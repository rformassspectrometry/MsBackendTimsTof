library("testthat")
library("MsBackendTimsTof")

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
