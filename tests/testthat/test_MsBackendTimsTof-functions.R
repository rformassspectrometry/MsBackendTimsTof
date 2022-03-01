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

    res <- .get_tims_columns(be, c("tof", "inv_ion_mobility"))
    expect_identical(length(res), length(be))
    expect_equal(colnames(res[[1]]), c("tof", "inv_ion_mobility"))

    expect_error(.get_tims_columns(be, "bla"), "'bla' not available")
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

