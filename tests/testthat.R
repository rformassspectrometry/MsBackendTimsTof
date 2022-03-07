library("testthat")
library("MsBackendTimsTof")

path_d_folder <- system.file("ddaPASEF.d",
                             package = "MsBackendTimsTof")

be <- backendInitialize(new("MsBackendTimsTof"), rep(path_d_folder, 2))

test_check("MsBackendTimsTof")
