library(testthat)
library(opentimsr)
tms <- OpenTIMS(path_d_folder)

test_that("backendInitialize,MsBackendTimsTof works", {
    expected_frames <- rbind(cbind(tms@frames, file = 1),
                             cbind(tms@frames, file = 2))
    colnames(expected_frames)[1:3] <- c("frameId", "rtime", "polarity")
    expected_frames$polarity <- .format_polarity(expected_frames$polarity)
    expect_equal(be@frames, expected_frames)
    expect_equal(be@fileNames,
                 setNames(c(1L, 2L), rep(normalizePath(path_d_folder), 2)))
    tmp <- unique(query(tms, unique(be@frames$frame), c("frame", "scan")))
    tmp <- rbind(cbind(tmp, file = 1),
                 cbind(tmp, file = 2))
    a <- be@indices
    row.names(a) <- NULL
    expect_equal(a, as.matrix(tmp, rownames.force = FALSE))

    expect_equal(be@nspectra, nrow(be@indices))
    expect_equal(be@spectraVariables, c(.TIMSTOF_COLUMNS,
                                        colnames(be@frames), "dataOrigin"))
    expect_equal(nrow(be@localData), be@nspectra)
    expect_equal(ncol(be@localData), 0L)

    bla <- backendInitialize(MsBackendTimsTof(), path_d_folder)
    expect_equal(dataOrigin(bla), dataStorage(bla))
})

test_that("[,MsBackendTimsTof works", {
    expect_error(MsBackendTimsTof()[1])

    res <- be[1]
    expect_true(validObject(res))
    expect_equal(res@indices, be@indices[1, , drop = FALSE])
    expect_equal(res@frames, be@frames[1, , drop = FALSE])
    expect_equal(names(res@fileNames), normalizePath(path_d_folder))

    res <- be[c(TRUE, rep(FALSE, length(be) - 1))]
    expect_true(validObject(res))
    expect_equal(res@indices, be@indices[1, , drop = FALSE])
    expect_equal(res@frames, be@frames[1, , drop = FALSE])
    expect_equal(names(res@fileNames), normalizePath(path_d_folder))

    res <- be[c(1000, 1, 222)]
    expect_true(validObject(res))
    expect_equal(res@indices, be@indices[c(1000, 1, 222), ])
    expect_equal(res@frames$frameId, unique(res@indices[, "frame"]))
    expect_equal(res@frames$file, c(1L, 1L, 1L))
    expect_equal(names(res@fileNames), normalizePath(path_d_folder))
    expect_equal(rtime(res), rtime(be)[c(1000, 1, 222)])
    expect_equal(mz(res), mz(be)[c(1000, 1, 222)])

    ## arbitrary order with duplication
    res <- be[c(1000, 2790, 1, 222, 1, 2790)]
    expect_true(validObject(res))
    expect_equal(res@indices, be@indices[c(1000, 2790, 1, 222, 1, 2790), ])
    expect_equal(res@frames$frameId, unique(res@indices[, "frame"]))
    expect_equal(res@frames$file, c(1L, 1L, 1L, 1L))
    expect_equal(names(res@fileNames), normalizePath(path_d_folder))
    expect_equal(rtime(res), rtime(be)[c(1000, 2790, 1, 222, 1, 2790)])
    expect_equal(mz(res), mz(be)[c(1000, 2790, 1, 222, 1, 2790)])
    expect_equal(intensity(res), intensity(be)[c(1000, 2790, 1, 222, 1, 2790)])

    expect_error(be[10^7], "index out of bounds")
})

qry <- query(tms, tms@frames$Id, c("frame", "scan", "mz", "intensity"))
sample_idxs <- seq(length(be), 1, by = -1000)

test_that("mz,MsBackendTimsTof works", {
    expect_equal(mz(MsBackendTimsTof()), NumericList(compress = FALSE))

    res <- mz(be)
    expect_equal(length(res), length(be))
    exp_res <- NumericList(lapply(sample_idxs, function(i) {
        qry[qry$frame == be@indices[i, "frame"] &
                qry$scan == be@indices[i, "scan"], "mz"]}), compress = FALSE)
    expect_equal(res[sample_idxs], exp_res)
})

test_that("intensity,MsBackendTimsTof works", {
    expect_equal(intensity(MsBackendTimsTof()), NumericList(compress = FALSE))

    res <- intensity(be)
    expect_equal(length(res), length(be))
    exp_res <- NumericList(lapply(sample_idxs, function(i) {
        qry[qry$frame == be@indices[i, "frame"] &
                qry$scan == be@indices[i, "scan"], "intensity"]}),
        compress = FALSE)
    expect_equal(res[sample_idxs], exp_res)
})

test_that("peaksData,MsBackendTimsTof works", {
    expect_equal(peaksData(MsBackendTimsTof()), list())

    res <- peaksData(be)
    expect_equal(length(res), length(be))
    exp_res <- lapply(sample_idxs, function(i) {
        as.matrix(qry[qry$frame == be@indices[i, "frame"] &
                          qry$scan == be@indices[i, "scan"],
                      c("mz", "intensity")],
                  rownames.force = FALSE) * 1})
    expect_equal(res[sample_idxs], exp_res)

    res <- peaksData(be, c("tof", "mz", "inv_ion_mobility"))
    expect_equal(colnames(res[[1L]]), c("tof", "mz", "inv_ion_mobility"))
})

test_that("rtime,MsBackendTimsTof works", {
    expect_true(is.numeric(rtime(MsBackendTimsTof())))
    expect_true(length(rtime(MsBackendTimsTof())) == 0)

    res <- rtime(be)
    expect_equal(length(res), length(be))
    expect_equal(res[1], be@frames[1, "rtime"])
    expect_equal(res[sum(be@frames[be@frames$Id == 1, "NumScans"]) + 1],
                 be@frames[1, "rtime"])
    expect_equal(rtime(be[sample_idxs]), res[sample_idxs])
})

test_that("dataStorage,MsBackendTimsTof works", {
    expect_equal(dataStorage(MsBackendTimsTof()), character(0))

    res <- dataStorage(be)
    expect_equal(length(res), length(be))
    expect_equal(res, names(be@fileNames[be@indices[, "file"]]))

    expect_equal(dataStorage(be), dataOrigin(be))
})

test_that("spectraVariables,MsBackendTimsTof works", {
    expect_identical(spectraVariables(MsBackendTimsTof()),
                     unique(c(names(Spectra:::.SPECTRA_DATA_COLUMNS),
                              .TIMSTOF_COLUMNS)))
    res <- spectraVariables(be)
    expect_true(all(res %in% c(colnames(be@frames), .TIMSTOF_COLUMNS,
                               names(Spectra:::.SPECTRA_DATA_COLUMNS))))
})

test_that("spectraData,MsBackendTimsTof works", {
    b <- MsBackendTimsTof()
    res <- spectraData(b)
    expect_true(nrow(res) == 0)
    expect_identical(colnames(res), spectraVariables(b))

    expect_error(spectraData(be, "not spectra variable"), "not available")

    res_all <- spectraData(be)
    expect_identical(colnames(res_all), spectraVariables(be))
    expect_identical(res_all$mz, mz(be))
    expect_identical(res_all$intensity, intensity(be))
    expect_identical(res_all$polarity, .get_frame_columns(be, "polarity"))
    expect_identical(res_all$scanIndex, be@indices[, "scan"])
    expect_identical(res_all$dataStorage, dataStorage(be))

    ## selecting only a few columns
    res <- spectraData(be, columns = c("msLevel","rtime"))
    expect_identical(colnames(res), c("msLevel","rtime"))
    expect_identical(res$msLevel,
                     match(.get_frame_columns(be, "MsMsType"), c(0L, 8L)))
    expect_identical(res$rtime, rtime(be))
    res <- spectraData(be, columns = "tof")
    expect_identical(colnames(res), "tof")
    expect_identical(nrow(res), length(be))

    ## ion mobility.
    res <- spectraData(be, columns = c("inv_ion_mobility"))
    expect_identical(colnames(res), "inv_ion_mobility")
    expect_identical(nrow(res), length(be))
    expect_true(is.numeric(res$inv_ion_mobility))

    res_2 <- spectraData(be, columns = c("mz", "inv_ion_mobility"))
    expect_identical(colnames(res_2), c("mz", "inv_ion_mobility"))
    expect_identical(nrow(res_2), length(be))
    expect_true(is.numeric(res_2$inv_ion_mobility))
    expect_equal(res$inv_ion_mobility, res_2$inv_ion_mobility)

    ## Random order
    idx <- sample(seq_along(be))
    be_2 <- be[idx]

    res <- spectraData(be_2)
    expect_equal(res, res_all[idx, ])

    ## duplicated values
    be_2 <- be[c(2, 2, 1, 2)]
    res <- spectraData(be_2)
    expect_equal(res, res_all[c(2, 2, 1, 2), ])
})

test_that("msLevel,MsBackendTimsTof works", {
    expect_identical(msLevel(MsBackendTimsTof()), integer(0))

    MsMsType <- .get_frame_columns(be, "MsMsType")
    res <- msLevel(be)
    expect_identical(length(res), length(be))
    expect_identical(res, match(.get_frame_columns(be, "MsMsType"), c(0L, 8L)))
})

test_that("$,MsBackendTimsTof works", {
    res_all <- spectraData(be)

    res <- be$inv_ion_mobility
    expect_equal(res, res_all$inv_ion_mobility)

    res <- be$mz
    expect_equal(res, res_all$mz)

    expect_equal(be$dataOrigin, dataOrigin(be))
    expect_equal(be$dataStorage, dataStorage(be))
})

test_that("peaksVariables works", {
    res <- peaksVariables(MsBackendTimsTof())
    expect_equal(res, c("mz", "intensity"))

    res <- peaksVariables(be)
    expect_true(length(res) > 2)
    expect_true(all(c("frame", "scan", "tof", "intensity") %in% res))
})

test_that("$<-,MsBackendTimsTof works", {
})

test_that("selectSpectraVariables works", {
    res <- selectSpectraVariables(
        be, spectraVariables = c("msLevel", "rtime", "mz", "TimsId"))
    expect_true(validObject(res))
    expect_true(all(c("msLevel", "rtime", "mz", "TimsId") %in%
                    spectraVariables(res)))
    expect_true(length(spectraVariables(res)) < length(spectraVariables(be)))
    expect_equal(colnames(res@frames), c("frameId", "rtime", "MsMsType",
                                         "TimsId", "file"))
    sdat <- spectraData(res)
    expect_true(all(lengths(sdat$intensity) == 0))
    expect_true(all(is.na(res$dataOrigin)))
    expect_true(all(is.na(sdat$dataOrigin)))

    ## intensity has to be empty, but no error
    expect_true(all(lengths(intensity(res)) == 0))
    expect_equal(mz(res), sdat$mz)

    res <- selectSpectraVariables(be, c("TimsId"))
    sdat <- spectraData(res)
    expect_true(all(is.na(rtime(res))))
    expect_true(all(lengths(intensity(res)) == 0))
    expect_true(all(lengths(mz(res)) == 0))
    expect_equal(sdat$intensity, intensity(res))
    expect_equal(sdat$mz, mz(res))
    expect_true(all(is.na(res$rtime)))

    ## peaksData; that tests might be tricky as it's not totally clear what
    ## they should return.

    ## Remove/select added (cached) spectra variable
    tmp <- be
    tmp$new_var <- "G"
    res <- selectSpectraVariables(tmp, c("rtime", "msLevel", "tof"))
    sdat <- spectraData(res)
    expect_true("new_var" %in% spectraVariables(tmp))
    expect_false("new_var" %in% spectraVariables(res))
    expect_false("new_var" %in% colnames(sdat))
    expect_error(res$new_var, "not available")
})

test_that("$<-,MsBackendTimsTof works", {
    ## Errors
    expect_error(be$mz <- 3, "not supported")
    expect_error(be$frameId <- 5L, "not supported")

    ## Add a new spectra variable
    res <- be
    res$new_var <- "G"
    expect_true(all(res$new_var == "G"))
    expect_identical(colnames(res@localData), c("new_var"))
    res$new_var <- seq_along(res)
    expect_equal(spectraData(res, "new_var")[, 1L], seq_along(res))

    ## Replace an existing spectra variable
    res$rtime <- rtime(res) + 10
    expect_equal(res$rtime, rtime(be) + 10)
    expect_equal(rtime(res), rtime(be) + 10)
    expect_equal(spectraData(res, "rtime")[, 1L], rtime(be) + 10)

    res$dataOrigin <- "Z"
    expect_true(all(dataOrigin(res) == "Z"))
    expect_equal(dataOrigin(res), res$dataOrigin)
    expect_equal(spectraData(res)$dataOrigin, res$dataOrigin)
})

test_that("precScanNum,MsBackendTimsTof works", {
    res <- precScanNum(be)
    expect_true(all(is.na(res)))
    expect_true(is.integer(res))
})

test_that("tic,MsBackendTimsTof works", {
    a <- tic(be, initial = FALSE)
    expect_equal(length(a), length(be))
    expect_true(is.numeric(a))

    b <- tic(be, initial = TRUE)
    expect_true(all(is.na(b)))
    expect_equal(length(b), length(be))
})

test_that("spectraNames,MsBackendTimsTof works", {
    res <- spectraNames(MsBackendTimsTof())
    expect_true(length(res) == 0)

    res <- spectraNames(be)
    expect_true(length(res) == length(be))
    expect_true(is.character(res))
    expect_true(length(res) == length(unique(res)))

    be_sub <- be[c(4, 14, 30)]
    expect_equal(spectraNames(be_sub), c("4", "14", "30"))
})
