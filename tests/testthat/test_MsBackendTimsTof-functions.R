test_that(".valid_required_columns works", {
    df <- data.frame()
    expect_null(.valid_required_columns(df))
    df <- data.frame(c1 = 1, c2 = 2)
    expect_null(.valid_required_columns(df))
    expect_match(.valid_required_columns(df, columns = "c3"), "Required column")
})

test_that(".valid_frames works", {
    frames <- data.frame(frameId = c(1, 2), file = c(1, 2), other = c("a", "b"))
    expect_null(.valid_frames(frames))
    frames <- frames[, c("frameId", "other")]
    expect_match(.valid_frames(frames), "Required column")
})

test_that(".valid_indices works", {
    expect_null(.valid_indices(be))

    tmp <- be
    tmp@indices[1, "file"] = 4
    expect_equal(.valid_indices(tmp),
                 c("Some file indices are out of bounds",
                   "Some indices are out of bounds"))
})

test_that(".valid_fileNames works", {
    tmpf <- tempfile()
    write("hello", file = tmpf)
    expect_match(.valid_fileNames(setNames(c(1L, 3L), c(NA, tmpf))),
                 "of 'fileNames' are not allowed")
    expect_match(.valid_fileNames(setNames(c(1L, 3L), c("x", tmpf))),
                 "not found")
    expect_null(.valid_fileNames(setNames(3L, tmpf)))
    expect_match(.valid_fileNames(NA), "'NA' values in 'fileNames'")
})

test_that(".get_tims_columns works", {
    res <- .get_tims_columns(be, c("tof"))
    expect_true(is.list(res))
    expect_identical(length(res), length(be))
    expect_true(all(lengths(res) > 0))

    res <- .get_tims_columns(be, c("tof", "inv_ion_mobility"))
    expect_identical(length(res), length(be))
    expect_equal(colnames(res[[1]]), c("tof", "inv_ion_mobility"))

    expect_error(.get_tims_columns(be, "bla"), "'bla' not available")

    ## mz and intensity are always first.
    be_sub <- be[200:300]
    res <- .get_tims_columns(be_sub, c("intensity", "mz", "tof"))
    expect_equal(colnames(res[[1L]]), c("intensity", "mz", "tof"))

    res <- .get_tims_columns(be_sub, c("tof", "intensity"))
    expect_equal(colnames(res[[1L]]), c("tof", "intensity"))

    res <- .get_tims_columns(be_sub, c("tof", "inv_ion_mobility"))
    expect_equal(colnames(res[[1L]]), c("tof", "inv_ion_mobility"))

    ## random order
    idx <- sample(seq_along(be))
    be_2 <- be[idx]
    res <- .get_tims_columns(be, "inv_ion_mobility")
    res_2 <- .get_tims_columns(be_2, "inv_ion_mobility")
    expect_equal(unlist(res[idx]), unlist(res_2))

    ## duplicated entries
    idx <- c(3, 5, 13, 5, 3, 3, 1)
    be_2 <- be[idx]
    res <- .get_tims_columns(be, c("tof", "mz"))
    res_2 <- .get_tims_columns(be_2, c("tof", "mz"))
    expect_equal(res_2[[1L]], res_2[[5L]])
    expect_equal(res_2[[1L]], res_2[[6L]])
    expect_equal(res_2[[2L]], res_2[[4L]])
    expect_equal(res_2[[1L]], res[[3L]])
    expect_equal(res_2[[2L]], res[[5L]])
    expect_equal(res_2[[3L]], res[[13L]])

    res <- .get_tims_columns(be, "intensity")
    res_2 <- .get_tims_columns(be_2, "intensity")
    expect_true(is.numeric(res_2[[1L]]))
    expect_equal(res_2[[1L]], res_2[[5L]])
    expect_equal(res_2[[1L]], res_2[[6L]])
    expect_equal(res_2[[2L]], res_2[[4L]])
    expect_equal(res_2[[1L]], res[[3L]])
    expect_equal(res_2[[2L]], res[[5L]])
    expect_equal(res_2[[3L]], res[[13L]])
})

test_that(".get_frame_columns works", {
    res <- .get_frame_columns(be, c("rtime", "polarity"))
    expect_true(is.data.frame(res))
    expect_identical(nrow(res), length(be))
    expect_equal(colnames(res), c("rtime", "polarity"))

    expect_error(.get_frame_columns(be, "bla"), "'bla' not available")
})

test_that(".format_polarity works", {
    res <- .format_polarity(c("Pos", "pos", "+", "neg", "?"))
    expect_equal(res, c(1L, 1L, 1L, 0L, NA_integer_))
    res <- .format_polarity(c("-", NA))
    expect_equal(res, c(0L, NA_integer_))
})

test_that("MsBackendTimsTof works", {
    res <- MsBackendTimsTof()
    expect_equal(length(res), 0)
    validObject(res)
})

test_that(".list_tims_columns works", {
    res <- .list_tims_columns(path_d_folder)
    expect_true(all(c("mz", "frame", "scan", "tof", "intensity") %in% res))
})

test_that(".get_msLevel works", {
    MsMsType <- .get_frame_columns(be, "MsMsType")
    res <- .get_msLevel(be)
    expect_identical(which(res == 1L), which(MsMsType == 0L))
    expect_identical(which(res == 2L), which(MsMsType == 8L))
    expect_identical(.get_msLevel(MsMsType, isMsMsType = TRUE), res)
    expect_identical(.get_msLevel(c(8L, NA, 0L), TRUE), c(2L, NA, 1L))
    expect_warning(.get_msLevel(c(8L, 2L, 0L), TRUE), "not recognized")

    tmp <- be
    tmp@frames <- tmp@frames[, colnames(tmp@frames) != "MsMsType"]
    res <- .get_msLevel(tmp)
    expect_true(all(is.na(res)))
    expect_true(is.integer(res))
    expect_equal(length(res), length(tmp))
})

test_that(".query_tims works", {
    ## With a character.
    res <- .query_tims(path_d_folder, frames = 1,
                       columns = c("frame", "scan", "mz", "intensity"))
    expect_true(is.data.frame(res))
    expect_equal(colnames(res), c("frame", "scan", "mz", "intensity"))
    expect_true(all(res$frame == 1L))

    ## With a OpenTIMS
    tm <- OpenTIMS(path_d_folder)
    res <- .query_tims(tm, frames = 2,
                       columns = c("frame", "scan", "inv_ion_mobility"))
    expect_true(is.data.frame(res))
    expect_equal(colnames(res), c("frame", "scan", "inv_ion_mobility"))
    expect_true(all(res$frame == 2L))

    ## Errors
    expect_error(.query_tims(tm, columns = c("frame", "scan", "other")), "not")

    opentimsr::CloseTIMS(tm)
})

test_that(".initialize works", {
    res <- .initialize(MsBackendTimsTof(),
                       file = c(path_d_folder, path_d_folder))
    expect_true(validObject(res))
    expect_s4_class(res, "MsBackendTimsTof")
    expect_true(length(res@fileNames) == 2)
    a <- res@indices[res@indices[, "file"] == 1L, -3]
    b <- res@indices[res@indices[, "file"] == 2L, -3]
    row.names(a) <- NULL
    row.names(b) <- NULL
    expect_equal(a, b)
})

test_that(".inv_ion_mobility works", {
    res <- .inv_ion_mobility(be)
    expect_true(is.numeric(res))
    expect_true(length(res) == length(be))

    ## Random order
    idx <- sample(seq_along(be))
    be_2 <- be[idx]
    res_2 <- .inv_ion_mobility(be_2)
    expect_equal(res_2, res[idx])
})

test_that(".spectra_data works", {
    b <- MsBackendTimsTof()
    res <- .spectra_data(b)
    expect_true(nrow(res) == 0)
    expect_identical(colnames(res), spectraVariables(b))
    res <- .spectra_data(b, c("msLevel", "rtime"))
    expect_identical(colnames(res), c("msLevel", "rtime"))

    expect_error(spectraData(be, "not spectra variable"), "not available")

    res_all <- .spectra_data(be)
    expect_identical(colnames(res_all), spectraVariables(be))
    expect_identical(res_all$mz, mz(be))
    expect_identical(res_all$intensity, intensity(be))
    expect_identical(res_all$polarity, .get_frame_columns(be, "polarity"))
    expect_identical(res_all$scanIndex, be@indices[, "scan"])
    expect_identical(res_all$dataStorage, dataStorage(be))

    ## selecting only a few columns
    res <- .spectra_data(be, columns = c("msLevel", "rtime"))
    expect_identical(colnames(res), c("msLevel", "rtime"))
    expect_identical(res$msLevel,
                     match(.get_frame_columns(be, "MsMsType"), c(0L, 8L)))
    expect_identical(res$rtime, rtime(be))
    ## only tims col
    res <- .spectra_data(be, columns = "tof")
    expect_identical(colnames(res), "tof")
    expect_identical(nrow(res), length(be))
    res <- .spectra_data(be, columns = "intensity")
    expect_identical(colnames(res), "intensity")
    expect_identical(res$intensity, intensity(be))
    res <- .spectra_data(be, columns = "inv_ion_mobility")
    expect_identical(colnames(res), "inv_ion_mobility")
    expect_true(is.numeric(res$inv_ion_mobility))
    res_2 <- spectraData(be, columns = c("mz", "inv_ion_mobility"))
    expect_identical(colnames(res_2), c("mz", "inv_ion_mobility"))
    expect_identical(nrow(res_2), length(be))
    expect_true(is.numeric(res_2$inv_ion_mobility))
    expect_equal(res$inv_ion_mobility, res_2$inv_ion_mobility)

    ## only frames col
    res <- .spectra_data(be, columns = "TimsId")
    expect_identical(colnames(res), "TimsId")
    expect_identical(nrow(res), length(be))

    ## frames and tims cols
    res <- .spectra_data(be, columns = c("TimsId", "intensity"))
    expect_identical(colnames(res), c("TimsId", "intensity"))
    expect_identical(nrow(res), length(be))

    ## dataStorage, dataOrigin
    res <- .spectra_data(be, columns = c("msLevel","dataStorage", "dataOrigin"))
    expect_identical(colnames(res), c("msLevel", "dataStorage", "dataOrigin"))
    expect_true(is.integer(res$msLevel))
    expect_identical(res$msLevel, msLevel(be))
    expect_equal(dataStorage(be), res$dataStorage)
    expect_equal(dataOrigin(be), res$dataOrigin)
})

test_that(".analysis_tdf works", {
    expect_error(res <- .analysis_tdf(tempdir()), "not found")
    res <- .analysis_tdf(path_d_folder)
    expect_s4_class(res, "SQLiteConnection")
    dbDisconnect(res)
})

test_that(".ms2_frames works", {
    res <- .ms2_frames(be)
    expect_true(is.list(res))
    expect_equal(length(res), length(be@fileNames))
})

test_that(".ms2_cols works", {
    res <- .ms2_cols()
    expect_equal(res, character())
    res <- .ms2_cols("precursorMz")
    expect_equal(res, "LargestPeakMz")
    res <- .ms2_cols("isolationWindowUpperMz")
    expect_equal(res, c("IsolationMz", "IsolationWidth"))
    res <- .ms2_cols(c("precursorMz", "isolationWindowTargetMz",
                       "precursorCharge", "precursorIntensity",
                       "collisionEnergy"))
    expect_equal(sort(res), sort(c("LargestPeakMz", "Intensity",
                                   "CollisionEnergy", "Charge",
                                   "IsolationMz", "IsolationWidth")))
})

test_that(".ms2_d works", {
    res <- .ms2_d(path_d_folder, c(31, 32))
    rownames(res) <- NULL
    expect_true(is.data.frame(res))
    expect_equal(colnames(res), c("frame", "scan", "precursorMz",
                                  "precursorIntensity", "collisionEnergy",
                                  "precursorCharge", "isolationWindowTargetMz",
                                  "isolationWindowLowerMz",
                                  "isolationWindowUpperMz"))
    expect_equal(unique(res$frame), c(31L, 32L))

    res_2 <- .ms2_d(path_d_folder, c(32))
    ref <- res[res$frame == 32, ]
    rownames(ref) <- NULL
    expect_equal(res_2, ref)

    res_s <- .ms2_d(path_d_folder, c(31, 32),
                    c("isolationWindowLowerMz", "precursorCharge"))
    expect_equal(colnames(res_s), c("frame", "scan", "precursorCharge",
                                    "isolationWindowLowerMz"))
    expect_equal(res$frame, res_s$frame)
    expect_equal(res$scan, res_s$scan)
    expect_equal(res$precursorCharge, res_s$precursorCharge)
    expect_equal(res$isolationWindowLowerMz, res_s$isolationWindowLowerMz)
})

test_that(".ms2_spectra_data works", {
    res <- .ms2_spectra_data(be, columns = c("precursorMz"))
    expect_true(is.data.frame(res))
    expect_equal(colnames(res), "precursorMz")
    expect_equal(nrow(res), length(be))
    ## data at the right places
    expect_equal(!is.na(res[, 1L]), msLevel(be) == 2)

    ## No MS2 data.
    b <- be[c(5:20)]
    res <- .ms2_spectra_data(b)
    expect_equal(nrow(res), length(b))
    expect_true(all(is.na(res$precursorMz)))
    expect_equal(colnames(res), c("precursorMz", "precursorIntensity",
                                  "collisionEnergy", "precursorCharge",
                                  "isolationWindowTargetMz",
                                  "isolationWindowLowerMz",
                                  "isolationWindowUpperMz"))
    ## With MS2 data
    b <- be[msLevel(be) == 2L]
    res <- .ms2_spectra_data(b, c("precursorCharge", "precursorIntensity"))
    expect_equal(nrow(res), length(b))
    expect_equal(colnames(res), c("precursorCharge", "precursorIntensity"))
    expect_equal(res$precursorIntensity[1:50], res$precursorIntensity[51:100])
})

test_that(".precursor_mz_column works", {
    res <- .precursor_mz_column()
    expect_true(is.character(res))
    expect_equal(length(res), 1L)
})

test_that(".ms2_d_empty", {
    res <- .ms2_d_empty()
    expect_true(is.data.frame(res))
    expect_true(nrow(res) == 0L)
    res <- .ms2_d_empty(c("collisionEnergy", "precursorMz"))
    expect_equal(colnames(res), c("frame", "scan", "collisionEnergy",
                                  "precursorMz", "file"))
})
