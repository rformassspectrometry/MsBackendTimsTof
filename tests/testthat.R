library("testthat")
library("MsBackendTimsTof")
library(opentimsr)
so_folder <- tempdir()
so_file <- download_bruker_proprietary_code(so_folder, method = "wget")

## For offline tests...
## so_file <- "/home/jo/data/TimsTOF/so/unix/libtimsdata.so"
setup_bruker_so(so_file)

register(SerialParam())
path_d_folder <- system.file("ddaPASEF.d",
                             package = "MsBackendTimsTof")

be <- backendInitialize(new("MsBackendTimsTof"), rep(path_d_folder, 2))

test_check("MsBackendTimsTof")

be <- be[1800:1830]
## Run additional tests from Spectra:
test_suite <- system.file("test_backends", "test_MsBackend",
                          package = "Spectra")

test_dir(test_suite, stop_on_failure = TRUE)
