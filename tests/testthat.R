library("testthat")
library("MsBackendTimsTof")

path_d_folder <- system.file("ddaPASEF_CaptiveSpray_Calibrants_denoised_1.d/",
                             package = "MsBackendTimsTof")
be <- backendInitialize(new("MsBackendTimsTof"), rep(path_d_folder, 2))

test_check("MsBackendTimsTof")