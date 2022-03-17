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
    frame_scan <- cbind(frame = rep(tms@frames$Id, tms@frames$NumScans),
                        scan = sequence(tms@frames$NumScans))
    expect_equal(be@indices, rbind(cbind(frame_scan, file = 1),
                                   cbind(frame_scan, file = 2)))
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
    expect_equal(res@frames$frameId, c(2, 1))
    expect_equal(res@frames$file, c(1L, 1L))
    expect_equal(names(res@fileNames), normalizePath(path_d_folder))
    expect_equal(rtime(res), rtime(be)[c(1000, 1, 222)])
    expect_equal(mz(res), mz(be)[c(1000, 1, 222)])

    ## arbitrary order with duplication
    res <- be[c(1000, 2790, 1, 222, 1, 2790)]
    expect_true(validObject(res))
    expect_equal(res@indices, be@indices[c(1000, 2790, 1, 222, 1, 2790), ])
    expect_equal(res@frames$frameId, c(2, 4, 1))
    expect_equal(res@frames$file, c(1L, 1L, 1L))
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
})

test_that("rtime,MsBackendTimsTof works", {
    expect_error(rtime(MsBackendTimsTof()), "'rtime' not available")

    res <- rtime(be)
    expect_equal(length(res), length(be))
    expect_equal(res[1], be@frames[1, "rtime"])
    expect_equal(res[be@frames$NumScans[1] + 1], be@frames[2, "rtime"])
    expect_equal(res[sum(be@frames[be@frames$Id == 1, "NumScans"]) + 1],
                 be@frames[1, "rtime"])
    expect_equal(rtime(be[sample_idxs]), res[sample_idxs])
})

test_that("dataStorage,MsBackendTimsTof works", {
    expect_equal(dataStorage(MsBackendTimsTof()), character(0))

    res <- dataStorage(be)
    expect_equal(length(res), length(be))
    expect_equal(res, names(be@fileNames[be@indices[, "file"]]))
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

    res <- spectraData(be)
    expect_identical(colnames(res), spectraVariables(be))
    expect_identical(res$mz, mz(be))
    expect_identical(res$intensity, intensity(be))
    expect_identical(res$polarity, .get_frame_columns(be, "polarity"))
    expect_identical(res$scanIndex, be@indices[, "scan"])
    expect_identical(res$dataStorage, dataStorage(be))

    ## selecting only a few columns
    res <- spectraData(be, columns = c("msLevel","rtime"))
    expect_identical(colnames(res), c("msLevel","rtime"))
    expect_identical(res$msLevel,
                     match(.get_frame_columns(be, "MsMsType"), c(0L, 8L)))
    expect_identical(res$rtime, rtime(be))
    res <- spectraData(be, columns = "tof")
    expect_identical(colnames(res), "tof")
    expect_identical(nrow(res), length(be))
})

test_that("msLevel,MsBackendTimsTof works", {
    expect_identical(msLevel(MsBackendTimsTof()), integer(0))

    MsMsType <- .get_frame_columns(be, "MsMsType")
    res <- msLevel(be)
    expect_identical(length(res), length(be))
    expect_identical(res, match(.get_frame_columns(be, "MsMsType"), c(0L, 8L)))
})
