#' @title TimsTOF data backend
#'
#' @name MsBackendTimsTof
#'
#' @aliases MsBackendTimsTof MsBackendTimsTof-class
#'
#' @description
#' The `MsBackendTimsTof` class supports Bruker TimsTOF data files. New objects
#' are created with the `MsBackendTimsTof` function.
#'
#' @section Available methods:
#'
#' The following methods are implemented:
#'
#' - `[`: subset the backend. Only subsetting by element (*row*/`i`) is
#'   allowed. First the `@indices` slot of `object` is subsetted and then the
#'   `frames` and `fileNames` slots are subsetted accordingly. Note that `[`
#'   does not update the values of `frames` variables (such as `"MaxIntensity"`,
#'   `"SummedIntensities"`, `"NumScans"` and `"NumPeaks"`).
#'
#' - `backendInitialize`: initializes `object` (the `MsBackendTimsTof` object)
#'   using TimsTOF data files whose path is specified by `files`. This method
#'   is supposed to be called right after creating a `MsBackendTimsTof` object
#'   with `MsBackendTimsTof` function.
#'
#' - `dataStorage`: gets a `character` of length equal to the number of spectra
#'   in `object` with the names of the '*.d' folders where each spectrum is
#'   stored.
#'
#' - `intensity`: gets the intensity values from the spectra in the backend.
#'   Returns a [NumericList()] of `numeric` vectors (intensity values for each
#'   spectrum). The length of the list is equal to the number of
#'   spectra in `object`.
#'
#' - `mz`: gets the mass-to-charge ratios (m/z) from the spectra in the backend.
#'   Returns a [NumericList()] of `numeric` vectors (m/z values for each
#'   spectrum). The length of the list is equal to the number of spectra in
#'   `object`.
#'
#' - `peaksData`: gets the peak matrices of the spectra in the backend.
#'   Returns a `list` of `matrix` with columns `"mz"` and `"intensity"`.
#'   The length of the `list` is equal to the number of spectra in `object`.
#'
#' - `rtime`: gets the retention times for each spectrum. Returns a `numeric`
#'   vector (length equal to the number of spectra) with the retention time
#'   for each spectrum.
#'   
#' - `spectraData`: gets spectra variables (specified by `columns`) from
#'   `object`.
#'
#' - `spectraVariables`: returns a `character` vector with the spectra variables
#'   names of core spectra variables defined in the Spectra package and other
#'   additional variables contained in `object`. Note that also `"mz"` and
#'   `"intensity"` (which are by default not returned by the
#'   `spectraVariables,Spectra` method) are returned.
#'
#' @param BPPARAM Parameter object defining the parallel processing
#' setup to import data in parallel. Defaults to `BPPARAM = bpparam()`.
#' See [bpparam()] for more information.
#' 
#' @param columns For `spectraData`: names of the spectra variables to extract
#'   from `object`.
#'
#' @param drop For `[`: not considered.
#'
#' @param files `character` specifying TimsTOF ’*.d’ folders names.
#'
#' @param i For `[`: `integer`, `logical` to subset the object.
#'
#' @param j For `[`: not supported.
#'
#' @param object `MsBackendTimsTof` object.
#'
#' @param x `MsBackendTimsTof` object.
#'
#' @param ... Additional arguments.
#'
#' @author Andrea Vicini, Johannes Rainer
#'
#' @rdname MsBackendTimsTof
#'
#' @exportClass MsBackendTimsTof
setClass("MsBackendTimsTof",
         contains = "MsBackend",
         slots = c(frames = "data.frame",
                   indices = "matrix",
                   fileNames = "integer"),
         prototype = prototype(frames = data.frame(),
                               indices = matrix(nrow = 0, ncol = 3,
                                                dimnames = list(NULL, 
                                                                c("frame",
                                                                  "scan",
                                                                  "file"))),
                               fileNames = integer(),
                               readonly = TRUE,
                               version = "0.1"))

#' @importFrom methods validObject
setValidity("MsBackendTimsTof", function(object) {
    msg <- .valid_fileNames(object@fileNames)
    msg <- c(msg, .valid_frames(object@frames))
    msg <- c(msg, .valid_indices(object))
    if (length(msg)) msg
    else TRUE
})

#' @importFrom BiocParallel bplapply
#'
#' @rdname MsBackendTimsTof
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

#' @rdname MsBackendTimsTof
setMethod("peaksData", "MsBackendTimsTof", function(object) {
    .get_tims_columns(object, c("mz", "intensity"))
})


#' @importFrom IRanges NumericList
#'
#' @rdname MsBackendTimsTof
setMethod("mz", "MsBackendTimsTof", function(object) {
    NumericList(.get_tims_columns(object, "mz"), compress = FALSE)
})

#' @importFrom IRanges NumericList
#'
#' @rdname MsBackendTimsTof
setMethod("intensity", "MsBackendTimsTof", function(object) {
    NumericList(.get_tims_columns(object, "intensity"), compress = FALSE)
})

#' @rdname MsBackendTimsTof
setMethod("rtime", "MsBackendTimsTof", function(object) {
    .get_frame_columns(object, "rtime")
})

#' @importFrom methods "slot<-"
#'
#' @importFrom MsCoreUtils i2index
#'
#' @rdname MsBackendTimsTof
setMethod("[", "MsBackendTimsTof", function(x, i, j, ..., drop = FALSE) {
    if (missing(i))
        return(x)
    i <- i2index(i, length(x))
    slot(x, "indices", check = FALSE) <- x@indices[i, , drop = FALSE]
    ff_indices <- paste(x@indices[, "frame"], x@indices[, "file"])
    slot(x, "frames", check = FALSE) <-
        x@frames[match(unique(ff_indices),
                       paste(x@frames$frameId, x@frames$file)), , drop = FALSE]
    slot(x, "fileNames", check = FALSE) <-
        x@fileNames[x@fileNames %in% unique(x@frames$file)]
    x
})

#' @rdname MsBackendTimsTof
setMethod("dataStorage", "MsBackendTimsTof", function(object) {
    if("file" %in% colnames(object@indices) && length(object@fileNames))
        return (names(object@fileNames[match(object@indices[, "file"],
                                             object@fileNames)])) 
    character(0)
})

#' @rdname MsBackendTimsTof
setMethod("spectraVariables", "MsBackendTimsTof", function(object) {
    unique(c(names(Spectra:::.SPECTRA_DATA_COLUMNS), .TIMSTOF_COLUMNS,
             colnames(object@frames)))
})

#' @rdname MsBackendTimsTof
setMethod("spectraData", "MsBackendTimsTof",
          function(object, columns = spectraVariables(object)) {
              .spectra_data(object, columns)
          })

