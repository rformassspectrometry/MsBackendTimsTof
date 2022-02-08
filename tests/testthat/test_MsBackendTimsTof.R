library(testthat)
#setup_bruker_so(path_lib)
tms <- OpenTIMS(path_d_folder)

test_that("backendInitialize,MsBackendTimsTof works", {
  expected_frames <- cbind(tms@frames, file = 1)
  colnames(expected_frames)[2:3] <- c("rtime", "polarity") 
  expected_frames$polarity <- .format_polarity(expected_frames$polarity)
  expect_equal(be@frames, expected_frames)
  
  nr <- nrow(expected_frames) 
  expected_frames_2 <- expected_frames[rep(seq_len(nr), 2), ]
  expected_frames_2[seq(nr + 1, 2 * nr), "file"] <- 2
  rownames(expected_frames_2) <- seq_len(nrow(expected_frames_2))
  expect_equal(be_2@frames, expected_frames_2)
})


test_that("[,MsBackendTimsTof works", {
  expect_error(MsBackendTimsTof()[1])
  
  res <- be[1]
  expect_true(validObject(res))
  expect_equal(res@indices, be@indices[1, , drop = FALSE])
  expect_equal(res@frames$Id, be@frames[1, "Id"])
  expect_equal(res@fileNames, normalizePath(path_d_folder))
  
  res <- be[c(TRUE, rep(FALSE, length(be) - 1))]
  expect_true(validObject(res))
  expect_equal(res@indices, be@indices[1, , drop = FALSE])
  expect_equal(res@frames$Id, be@frames[1, "Id"])
  expect_equal(res@fileNames, normalizePath(path_d_folder))
  
  res <- be[c(600, 1, 1)]
  expect_true(validObject(res))
  expect_equal(res@indices, be@indices[c(600, 1, 1), ])
  expect_equal(res@frames$Id, c(2, 1))
  expect_equal(res@frames$file, c(1L, 1L))
  expect_equal(res@fileNames, normalizePath(path_d_folder))
})

# mz <- mz(be)
# intensity <- intensity(be)
# peaks <- peaksData(be)