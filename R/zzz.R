#' @importFrom opentimsr setup_bruker_so
#'
#' @noRd
.onAttach <- function(libname, pkgname) {
    requireNamespace("opentimsr", quietly = TRUE)
    setup_converter_library()
    ## could use: LargestPeakMz, AverageMz, IsolationMz as precursor m/z
    pmz <- getOption("TIMSTOF_PRECURSOR_MZ", default = NA)
    if (is.na(pmz))
        pmz <- Sys.getenv("TIMSTOF_PRECURSOR_MZ", unset = NA)
    if (is.na(pmz))
        pmz <- "LargestPeakMz"
    options(TIMSTOF_PRECURSOR_MZ = pmz)
}
