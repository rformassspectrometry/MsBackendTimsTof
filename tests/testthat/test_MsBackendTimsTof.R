library(testthat)
library(opentimsr)
tms <- OpenTIMS(path_d_folder)

test_that("backendInitialize,MsBackendTimsTof works", {
  expected_frames <- cbind(tms@frames, file = 1)
  colnames(expected_frames)[1:3] <- c("frameId", "rtime", "polarity") 
  expected_frames$polarity <- .format_polarity(expected_frames$polarity)
  expect_equal(be@frames, expected_frames)
  
  nr <- nrow(expected_frames) 
  expected_frames_2 <- expected_frames[rep(seq_len(nr), 2), ]
  expected_frames_2[seq(nr + 1, 2 * nr), "file"] <- 2
  rownames(expected_frames_2) <- seq_len(nrow(expected_frames_2))
  expect_equal(be2@frames, expected_frames_2)
})


test_that("[,MsBackendTimsTof works", {
  expect_error(MsBackendTimsTof()[1])
  
  res <- be2[1]
  expect_true(validObject(res))
  expect_equal(res@indices, be2@indices[1, , drop = FALSE])
  expect_equal(res@frames, be2@frames[1, , drop = FALSE])
  expect_equal(res@fileNames, normalizePath(path_d_folder))
  
  res <- be2[c(TRUE, rep(FALSE, length(be2) - 1))]
  expect_true(validObject(res))
  expect_equal(res@indices, be2@indices[1, , drop = FALSE])
  expect_equal(res@frames, be2@frames[1, , drop = FALSE])
  expect_equal(res@fileNames, normalizePath(path_d_folder))
  
  res <- be2[c(600, 1, 1)]
  expect_true(validObject(res))
  expect_equal(res@indices, be2@indices[c(600, 1, 1), ])
  expect_equal(res@frames$frameId, c(2, 1))
  expect_equal(res@frames$file, c(1L, 1L))
  expect_equal(res@fileNames, normalizePath(path_d_folder))
   
  expect_error(be2[10^7], "index out of bounds")
})

be2sub <- be2[which(be2@indices[, "frame"] %in% c(1, 2))]
be2sub <- be2sub[seq(1, length(be2sub), by = 500)]
qry <- query(tms, c(1, 2), c("frame", "scan", "mz", "intensity"))

test_that("mz,MsBackendTimsTof works", {
  expect_equal(mz(MsBackendTimsTof()), NumericList(compress = FALSE))

  res <- mz(be2sub)
  expect_equal(length(res), length(be2sub))
  exp_res <- NumericList(lapply(seq_len(length(be2sub)), function(i) {
    qry[qry$frame == be2sub@indices[i, "frame"] &
          qry$scan == be2sub@indices[i, "scan"], "mz"]}), compress = FALSE)
  expect_equal(res, exp_res)

  re <- c(3, 2, 5, 4, 1)
  expect_equal(mz(be2sub[re]), res[re])
})

test_that("intensity,MsBackendTimsTof works", {
  expect_equal(intensity(MsBackendTimsTof()), NumericList(compress = FALSE))

  res <- intensity(be2sub)
  expect_equal(length(res), length(be2sub))
  exp_res <- NumericList(lapply(seq_len(length(be2sub)), function(i) {
    qry[qry$frame == be2sub@indices[i, "frame"] &
          qry$scan == be2sub@indices[i, "scan"], "intensity"]}),
    compress = FALSE)
  expect_equal(res, exp_res)

  re <- c(3, 2, 5, 4, 1)
  expect_equal(intensity(be2sub[re]), res[re])
})

test_that("peaksData,MsBackendTimsTof works", {
  expect_equal(peaksData(MsBackendTimsTof()), list())

  res <- peaksData(be2sub)
  expect_equal(length(res), length(be2sub))
  exp_res <- lapply(seq_len(length(be2sub)), function(i) {
    as.matrix(qry[qry$frame == be2sub@indices[i, "frame"] &
          qry$scan == be2sub@indices[i, "scan"], c("mz","intensity")],
          rownames.force = FALSE)})
  exp_res[[1]] <- exp_res[[1]] * 1
  expect_equal(res, exp_res)

  re <- c(3, 2, 5, 4, 1)
  expect_equal(peaksData(be2sub[re]), res[re])
})

test_that("rtime,MsBackendTimsTof works", {
  expect_error(rtime(MsBackendTimsTof()), "'rtime' not available")

  res <- rtime(be2sub)
  expect_equal(length(res), length(be2sub))
  expect_equal(res, be2sub@frames[c(1, 1, 2, 3, 4), "rtime"])

  re <- c(3, 2, 5, 4, 1)
  expect_equal(rtime(be2sub[re]), res[re])
})

test_that("dataStorage,MsBackendTimsTof works", {
  expect_equal(dataStorage(MsBackendTimsTof()), character(0))

  res <- dataStorage(be2sub)
  expect_equal(length(res), length(be2sub))
  expect_equal(res, be2sub@fileNames[be2sub@indices[, "file"]])
})

test_that("spectraVariables,MsBackendTimsTof works", {
  expect_equal(spectraVariables(MsBackendTimsTof()),
               names(Spectra:::.SPECTRA_DATA_COLUMNS))
  
  res <- spectraVariables(be)
  expect_true(all(res %in% c(colnames(be@frames),
                             names(Spectra:::.SPECTRA_DATA_COLUMNS))))
})

test_that("spectraData,MsBackendTimsTof works", {
  b <- MsBackendTimsTof()
  res <- spectraData(b)
  expect_true(nrow(res) == 0)
  expect_identical(colnames(res), spectraVariables(b))
  
  expect_error(spectraData(be2sub, "not spectra variable"), "not available")
  
  res <- spectraData(be2sub)
  expect_identical(colnames(res), spectraVariables(be2sub))
  expect_identical(res$mz, mz(be2sub))
  expect_identical(res$intensity, intensity(be2sub))
  expect_identical(res$polarity, .get_frame_columns(be2sub, "polarity"))
  expect_identical(res$scanIndex, be2sub@indices[, "scan"])
  expect_identical(res$dataStorage, dataStorage(be2sub))
})
