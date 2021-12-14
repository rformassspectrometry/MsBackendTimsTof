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
  
  # 2 files with 2 and 3 frames respectively (each frame has 3 spectra)
  be@frames <- data.frame(Id = c(1L:2L, 1:3), 
                          file = rep(c(1L, 2L), c(2, 3)))
  be@fileNames <- c("file1", "file2")
  be@indices <- cbind(frame = c(rep(1L:2L, each = 3), rep(1L:3L, each = 3)),
                      scan = rep(1L:3L, 5),
                      file = c(rep(1L, 2*3), rep(2L, 3*3)))
  
  res <- be[1]
  # expect_true(validObject(res))
  expect_equal(res@indices, be@indices[1, , drop = FALSE])
  expect_equal(res@frames, be@frames[1, , drop = FALSE])
  expect_equal(res@fileNames, "file1")
  
  res <- be[c(5, 1, 1)]
  # expect_true(validObject(res))
  expect_equal(be@indices[c(5, 1, 1), ], res@indices)
  expect_equal(res@frames$Id, c(2, 1))
  expect_equal(res@frames$file, c(1L, 1L))
  expect_equal(res@fileNames, "file1")
})
