#' @importFrom opentimsr setup_bruker_so
#'
#' @noRd
.onAttach <- function(libname, pkgname) {
    requireNamespace("opentimsr", quietly = TRUE)
    setup_converter_library()
}
