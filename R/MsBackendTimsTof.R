#' @title TimsTOF data backend
#'
#' @name MsBackendTimsTof
#'
#' @author Andrea Vicini, Johannes Rainer
#'
#' @noRd
#'
#' @exportClass MsBackendTimsTof
setClass("MsBackendTimsTof",
         contains = "MsBackend",
         slots = c(frames = "data.frame",
                   indexes = "matrix"),
         prototype = prototype(frames = data.frame(), # or DataFrame?
                               indexes = matrix(),
                               readonly = TRUE,
                               version = "0.1"))


setMethod("backendInitialize", signature = "MsBackendTimsTof",
          function(object, files, ...) {
            if (missing(files) || !length(files))
              stop("Parameter 'files' is mandatory for 'MsBackendMzR'")
            if (!is.character(files))
              stop("Parameter 'files' is expected to be a character vector",
                   " with the files names from where data should be",
                   " imported")
            files <- normalizePath(files, mustWork = FALSE)
            msg <- Spectra:::.valid_ms_backend_files_exist(files)
            if (length(msg))
              stop(msg)
            # despite the code above I assume that files is a character(1) 
            # because maybe it's better to discuss the multiple files case 
            # before.
            object <- .header(object, files) 
            # validObject(object)
            object
          })


#' @exportMethod length
#'
#' @rdname MsBackendTimsTof
setMethod("length", "MsBackendTimsTof", function(x) {
  nrow(x@indexes)
})
