library("testthat")
library("MsBackendTimsTof")

#path_d_folder <- "Methanolpos-1-TIMS_108_1_2007.d/"
path_d_folder <- system.file("Methanolpos-1-TIMS_108_1_2007.d", 
                             package = "MsBackendTimsTof")
be <- backendInitialize(new("MsBackendTimsTof"), path_d_folder)
be_2 <- backendInitialize(new("MsBackendTimsTof"), rep(path_d_folder, 2))

test_check("MsBackendTimsTof")