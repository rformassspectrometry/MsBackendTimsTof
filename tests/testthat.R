library("testthat")
so_folder <- tempdir()
library(opentimsr)
so_file <- download_bruker_proprietary_code(so_folder, method = "wget")
setup_bruker_so(so_file)
path_d_folder <- system.file("ddaPASEF.d",
                             package = "MsBackendTimsTof")

library("MsBackendTimsTof")
be <- backendInitialize(new("MsBackendTimsTof"), rep(path_d_folder, 2))

test_check("MsBackendTimsTof")

be <- be[1800:1830]
## Run additional tests from Spectra:
test_suite <- system.file("test_backends", "test_MsBackend",
                          package = "Spectra")

test_dir(test_suite, stop_on_failure = TRUE)
