#' @importFrom opentimsr setup_bruker_so
#'
#' @noRd
.onAttach <- function(libname, pkgname) {
    requireNamespace("opentimsr", quietly = TRUE)
    so <- getOption("TIMSTOF_LIB", default = NA)
    if (is.na(so))
        so <- Sys.getenv("TIMSTOF_LIB", unset = NA)
    if (is.na(so))
        packageStartupMessage("Location of the TimsTOF library not known.\n",
                              "You need to use 'opentimsr::setup_bruker_so' ",
                              "to set/load this library in order to be able ",
                              "to read TimsTOF files. See the package ",
                              "vignette for more information.")
    else opentimsr::setup_bruker_so(so)
    ## could use: LargestPeakMz, AverageMz, IsolationMz as precursor m/z
    pmz <- getOption("TIMSTOF_PRECURSOR_MZ", default = NA)
    if (is.na(pmz))
        pmz <- Sys.getenv("TIMSTOF_PRECURSOR_MZ", unset = NA)
    if (is.na(pmz))
        pmz <- "LargestPeakMz"
    options(TIMSTOF_PRECURSOR_MZ = pmz)
}
