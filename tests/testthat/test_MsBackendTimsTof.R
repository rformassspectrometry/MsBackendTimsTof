library(testthat)
#setup_bruker_so(path_lib)
tms <- OpenTIMS("Methanolpos-1-TIMS_108_1_2007.d/")


test_that("backendInitialize,MsBackendDataFrame works", {
  be <- new("MsBackendTimsTof")
  
  be <- backendInitialize(object, "Methanolpos-1-TIMS_108_1_2007.d/")
  expect_equal(be@frames, tms@frames)
})