library(opentimsr)

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
                 "'NA' values in fileNames")
    expect_match(.valid_fileNames(setNames(c(1L, 3L), c("x", tmpf))), "not found")
    expect_null(.valid_fileNames(setNames(3L, tmpf)))
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
})

test_that(".get_tims_columns_p works", {
    register(SerialParam())
    res <- .get_tims_columns_p(be, c("tof"))
    expect_true(is.list(res))
    expect_identical(length(res), length(be))
    expect_true(all(lengths(res) > 0))

    res <- .get_tims_columns_p(be, c("tof", "inv_ion_mobility"))
    expect_identical(length(res), length(be))
    expect_equal(colnames(res[[1]]), c("tof", "inv_ion_mobility"))

    expect_error(.get_tims_columns_p(be, "bla"), "'bla' not available")

    ## mz and intensity are always first.
    be_sub <- be[200:300]
    res <- .get_tims_columns_p(be_sub, c("intensity", "mz", "tof"))
    expect_equal(colnames(res[[1L]]), c("intensity", "mz", "tof"))

    res <- .get_tims_columns_p(be_sub, c("tof", "intensity"))
    expect_equal(colnames(res[[1L]]), c("tof", "intensity"))

    res <- .get_tims_columns_p(be_sub, c("tof", "inv_ion_mobility"))
    expect_equal(colnames(res[[1L]]), c("tof", "inv_ion_mobility"))

    ## random order
    idx <- sample(seq_along(be))
    be_2 <- be[idx]
    res <- .get_tims_columns_p(be, "inv_ion_mobility")
    res_2 <- .get_tims_columns_p(be_2, "inv_ion_mobility")
    expect_equal(unlist(res[idx]), unlist(res_2))
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
    expect_equal(res@indices[res@indices[, "file"] == 1L, -3],
                 res@indices[res@indices[, "file"] == 2L, -3])
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
