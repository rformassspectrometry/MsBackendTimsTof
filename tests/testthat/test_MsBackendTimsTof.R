library(testthat)
#setup_bruker_so(path_lib)
tms <- OpenTIMS("Methanolpos-1-TIMS_108_1_2007.d/")

test_that("backendInitialize,MsBackendTimsTof works", {
  file <- "Methanolpos-1-TIMS_108_1_2007.d/"
  be <- backendInitialize(new("MsBackendTimsTof"), file)
  expected_frames <- cbind(tms@frames, file = 1)
  expect_equal(be@frames, expected_frames)
  
  files <- rep("Methanolpos-1-TIMS_108_1_2007.d/", 2)
  # this was to test initializing an object with two files but in this case if 
  # the files have the same name maybe I should add them only once by taking 
  # unique(files) in .initialize function?
  be <- backendInitialize(new("MsBackendTimsTof"), files)
  expected_frames <- rbind(cbind(tms@frames, file = 1), cbind(tms@frames, file = 2))
  rownames(expected_frames) <- seq_len(nrow(expected_frames))
  expect_equal(be@frames, expected_frames)
})


test_that("[,MsBackendTimsTof works", {
  be <- MsBackendTimsTof()
  expect_error(be[1])
  
  file <- "Methanolpos-1-TIMS_108_1_2007.d/"
  be <- backendInitialize(new("MsBackendTimsTof"), file)
  res <- be[1]
  expect_true(validObject(res))
  expect_equal(res@indices, be@indices[1, , drop = FALSE])
  expect_equal(res@frames, be@frames[1, , drop = FALSE])
  expect_equal(res@fileNames, normalizePath(file))
  
  res <- be[c(600, 1, 1)]
  expect_true(validObject(res))
  expect_equal(res@indices, be@indices[c(600, 1, 1), ])
  expect_equal(res@frames$Id, c(2, 1))
  expect_equal(res@frames$file, c(1L, 1L))
  expect_equal(res@fileNames, normalizePath(file))
})
