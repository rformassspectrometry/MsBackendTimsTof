library("testthat")
library("MsBackendTimsTof")

so_folder <- tempdir()
library(opentimsr)
so_file <- download_bruker_proprietary_code(so_folder, method = "wget")
setup_bruker_so(so_file)
path_d_folder <- system.file("ddaPASEF.d",
                             package = "MsBackendTimsTof")

be <- backendInitialize(new("MsBackendTimsTof"), rep(path_d_folder, 2))

test_check("MsBackendTimsTof")
