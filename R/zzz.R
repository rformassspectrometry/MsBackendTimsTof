#' @importFrom opentimsr setup_bruker_so
#'
#' @noRd
.onLoad <- function(libname, pkgname) {
    requireNamespace("opentimsr", quietly = TRUE)
    so <- getOption("TIMSTOF_LIB", default = NA)
    if (is.na(so))
        so <- Sys.getenv("TIMSTOF_LIB", unset = NA)
    if (is.na(so))
        message("Location of the TimsTOF library not known.\nYou need to ",
                "use 'opentimsr::setup_bruker_so' to set/load this library ",
                "in order to be able to read TimsTOF files. See the package ",
                "vignette for more information.")
    else opentimsr::setup_bruker_so(so)
}
