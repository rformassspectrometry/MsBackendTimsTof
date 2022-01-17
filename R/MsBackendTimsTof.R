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
                   indices = "matrix",
                   fileNames = "character"),
         prototype = prototype(frames = data.frame(), # or DataFrame?
                               indices = matrix(),
                               fileNames = character(),
                               readonly = TRUE,
                               version = "0.1"))

setValidity("MsBackendTimsTof", function(object) {
  msg <- .valid_fileNames(object@fileNames)
  msg <- c(msg, .valid_frames(object@frames))
  msg <- c(msg, .valid_indices(object))
  if (length(msg)) msg
  else TRUE
})

setMethod("backendInitialize", signature = "MsBackendTimsTof",
          function(object, files, ..., BPPARAM = bpparam()) {
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
            object <- .initialize(object, files, BPPARAM) 
            validObject(object)
            object
          })

#' @exportMethod length
#'
#' @rdname MsBackendTimsTof
setMethod("length", "MsBackendTimsTof", function(x) {
  nrow(x@indices)
})

## show?

#' @rdname hidden_aliases
# setMethod("peaksData", "MsBackendTimsTof", function(object) {
#   do.call(c, lapply(object@fileNames, FUN = .timstof_peaks))
# })
setMethod("peaksData", "MsBackendTimsTof", function(object) {
  do.call(c, lapply(object@fileNames, 
                    function(x) .read_frame_col(x, c("mz", "intensity"))))
})


#' @importFrom IRanges NumericList
#' 
#' @rdname hidden_aliases
# setMethod("mz", "MsBackendTimsTof", function(object) {
#   NumericList(lapply(peaksData(object), "[", , 1), compress = FALSE)
# })
setMethod("mz", "MsBackendTimsTof", function(object) {
  NumericList(do.call(c, lapply(object@fileNames, 
                                function(x) .read_frame_col(x, "mz"))),
              compress = FALSE)
})

#' @importFrom IRanges NumericList
#' 
#' @rdname hidden_aliases
# setMethod("intensity", "MsBackendTimsTof", function(object) {
#   NumericList(lapply(peaksData(object), "[", , 2), compress = FALSE)
# })
setMethod("intensity", "MsBackendTimsTof", function(object) {
  NumericList(do.call(c, lapply(object@fileNames, 
                                function(x) .read_frame_col(x, "intensity"))),
              compress = FALSE)
})

#' @importFrom MsCoreUtils i2index
#'
#' @rdname hidden_aliases
setMethod("[", "MsBackendTimsTof", function(x, i, j, ..., drop = FALSE) {
  if (missing(i))
    return(x)
  i <- i2index(i, length(x))
  slot(x, "indices", check = FALSE) <- x@indices[i, , drop = FALSE]
  slot(x, "frames", check = FALSE) <- 
    x@frames[match(unique(paste0(x@indices[, "frame"], x@indices[, "file"])),
                   paste0(x@frames$Id, x@frames$file)), , drop = FALSE]
  slot(x, "fileNames", check = FALSE) <- x@fileNames[unique(x@frames$file)]
  x
}) 

#' @rdname hidden_aliases
setMethod("dataStorage", "MsBackendTimsTof", function(object) {
  if("file" %in% colnames(object@indices))
    return (object@fileNames[object@indices[, "file"]])
  character(0)
})

