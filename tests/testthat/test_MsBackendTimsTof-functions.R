test_that(".valid_required_columns works", {
  df <- data.frame()
  expect_null(.valid_required_columns(df))
  df <- data.frame(c1 = 1, c2 = 2)
  expect_null(.valid_required_columns(df))
  expect_match(.valid_required_columns(df, columns = "c3"), "Required column")
})

test_that(".valid_frames works", {
  frames <- data.frame(Id = c(1, 2), file = c(1, 2), other = c("a", "b"))
  expect_null(.valid_frames(frames))
  frames <- frames[, c("Id", "other")]
  expect_match(.valid_frames(frames), "Required column")
})

test_that(".valid_indices works", {
  expect_null(.valid_indices(be))

  be2 <- be
  be2@indices[1, "file"] = 4
  expect_equal(.valid_indices(be2),
               c("Some file indices are not valid",
                 "Some indices in x@indices are not compatible with x@frames"))
})

test_that(".valid_fileNames works", {
  tmpf <- tempfile()
  write("hello", file = tmpf)
  expect_match(.valid_fileNames(c(NA, tmpf)), "'NA' values in fileNames")
  expect_match(.valid_fileNames(c("x", tmpf)), "not found")
  expect_null(.valid_fileNames(tmpf))
})

test_that(".get_frame_columns works", {
    res <- .get_frame_columns(be, c("Time", "Polarity"))
    expect_true(is.data.frame(res))
    expect_identical(nrow(res), length(be))
    expect_equal(colnames(res), c("Time", "Polarity"))

    expect_error(.get_frame_columns(be, "bla"), "'bla' not available")
})

test_that(".format_polarity works", {
    res <- .format_polarity(c("Pos", "pos", "+", "neg", "?"))
    expect_equal(res, c(1L, 1L, 1L, 0L, NA_integer_))
    res <- .format_polarity(c("-", NA))
    expect_equal(res, c(0L, NA_integer_))
})
