library(testthat)
#setup_bruker_so(path_lib)
tms <- OpenTIMS(path_d_folder)

test_that("backendInitialize,MsBackendTimsTof works", {
  expected_frames <- cbind(tms@frames, file = 1)
  expect_equal(be@frames, expected_frames)
  
  expected_frames <- rbind(cbind(tms@frames, file = 1),
                           cbind(tms@frames, file = 2))
  rownames(expected_frames) <- seq_len(nrow(expected_frames))
  expect_equal(be_2@frames, expected_frames)
})


test_that("[,MsBackendTimsTof works", {
  expect_error(MsBackendTimsTof()[1])
  
  res <- be[1]
  expect_true(validObject(res))
  expect_equal(res@indices, be@indices[1, , drop = FALSE])
  expect_equal(res@frames, be@frames[1, , drop = FALSE])
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