#' @title TimsTOF MS data backend
#'
#' @name MsBackendTimsTof
#'
#' @aliases MsBackendTimsTof MsBackendTimsTof-class
#'
#' @description
#'
#' The `MsBackendTimsTof` class adds support for Bruker TimsTOF data files (*.d*
#' files) to [Spectra()] objects. New objects are created with the
#' `MsBackendTimsTof()` function and initialized using the `backendInitialize()`
#' function. To ensure a small memory footprint, only general information is
#' kept in memory (such as number of frames and scans) and all data
#' (specifically the peaks data) are retrieved from the original data files
#' on-the-fly. `MsBackendTimsTof` directly extends the [MsBackendCached()]
#' backend from the *Spectra* package and supports therefore adding or locally
#' caching spectra variables.
#'
#' @details
#'
#' The (DDA) MS2 data for MS2 spectra is retrieved from the *analytis.tdf*
#' SQLite databases within the *.d* folder. Per default, the m/z of the largest
#' peak in the isolation window is reported as `precursorMz` (SQLite database
#' column `"LargestPeakMz"`). Alternatively, the content of database columns
#' `"AverageMz"` or `"IsolationMz"` could be reported instead. To use e.g. the
#' `"AverageMz"` as `precursorMz` set
#' `options(TIMSTOF_PRECURSOR_MZ = "AverageMz")`.
#'
#' The reported `isolationWindowLowerMz` and `isolationWindowUpperMz` are
#' calculated based on the `"IsolationMz"` and `"IsolationWidth"` database
#' columns, i.e., `isolationWindowLowerMz <- IsolationMz - IsolationWidth / 2`
#' and `isolationWindowUpperMz <- IsolationMz + IsolationWidth / 2`.
#'
#' @section Available methods:
#'
#' The methods listed below are specifically implemented for
#' `MsBackendTimsTof`. The backend inherits and thus supports all other methods
#' from the [Spectra::MsBackend] class.
#'
#' - `$`: access any of the `spectraVariables` of the backend.
#'
#' - `$<-`: add a new spectra variable or change values for an existing spectra
#'   variables. Values can be changed for any spectra variable except *peaks
#'   variables* ([Spectra::peaksVariables()]) or special internal variables
#'   `"file"` and `"frameId"`.
#'   Note that changes to spectra variables are only cached within
#'   the object but not propagated to the original data files.
#'
#' - `[`: subset the backend. Only subsetting by element (*row*/`i`) is
#'   allowed. First the `@indices` slot of `object` is subsetted and then the
#'   `frames` and `fileNames` slots are subsetted accordingly. Note that `[`
#'   does not update the values of `frames` variables (such as `"MaxIntensity"`,
#'   `"SummedIntensities"`, `"NumScans"` and `"NumPeaks"`).
#'
#' - `backendInitialize()`: initializes `object` (the `MsBackendTimsTof` object)
#'   using TimsTOF data files whose path is specified by `files`. This method
#'   is supposed to be called right after creating a `MsBackendTimsTof` object
#'   with `MsBackendTimsTof()` function.
#'
#' - `dataStorage()`: gets a `character` of length equal to the number of
#'   spectra in `object` with the names of the '*.d' folders where each
#'   spectrum is stored.
#'
#' - `intensity()`: gets the intensity values from the spectra in the backend.
#'   Returns a [NumericList()] of `numeric` vectors (intensity values for each
#'   spectrum). The length of the list is equal to the number of
#'   spectra in `object`.
#'
#' - `mz()`: gets the mass-to-charge ratios (m/z) from the spectra in the
#'   backend. Returns a [NumericList()] of `numeric` vectors (m/z values for
#'   each spectrum). The length of the list is equal to the number of spectra in
#'   `object`.
#'
#' - `peaksData()`: gets the peak matrices of the spectra in the backend.
#'   Returns a `list` of `matrix` with columns defined by parameter `columns`
#'   (which defaults to `columns = c("mz", "intensity")`. Use `peaksVariables()`
#'   to list all supported and available columns for a backend.
#'   The length of the `list` is equal to the number of spectra in `object`.
#'
#' - `peaksVariables()`: gets the supported peak variables (columns) for the
#'   backend.
#'
#' - `rtime()`: gets the retention times for each spectrum. Returns a `numeric`
#'   vector (length equal to the number of spectra) with the retention time
#'   for each spectrum.
#'
#' - `selectSpectraVariables()`: reduces the available spectra variables to the
#'   ones specified with parameter `spectraVariables`. For *core spectra
#'   variables* ([coreSpectraVariables()]) only their values will be removed,
#'   but not the variable itself.
#'
#' - `spectraData()`: gets spectra variables (specified by `columns`) from
#'   `object`.
#'
#' - `spectraNames()`: returns an *ID*/name for each spectrum. As IDs the index
#'   of the spectrum within the object after the initialization is used. This
#'   index/spectra name is unique and stable for each spectrum within the same
#'   object.
#'
#' - `spectraVariables()`: returns a `character` vector with the spectra
#'   variables names of core spectra variables defined in the Spectra package
#'   and other additional variables contained in `object`.
#'   Note that also `"mz"` and `"intensity"` (which are by default not returned
#'   by the `spectraVariables,Spectra` method) are returned.
#'
#' - `tic()`: calculates the total ion count from the intensities of each
#'   spectrum (for `initial = FALSE`). For `initial = TRUE` `NA` is returned
#'   for all spectra.
#'
#' @param BPPARAM Parameter object defining the parallel processing
#'   setup to import data in parallel. Defaults to `BPPARAM = bpparam()`.
#'   See [bpparam()] for more information.
#'
#' @param columns For `spectraData()`: names of the spectra variables to extract
#'   from `object`. For `peaksData()`: names of the peak variables to extract.
#'   Defaults to `columns = c("mz", "intensity")`.
#'
#' @param drop For `[`: not considered.
#'
#' @param files `character` specifying TimsTOF ’*.d’ folders names.
#'
#' @param initial For `tic()`: `logical(1)` whether the original total ion count
#'     should be returned (`initial = TRUE`, the default) or whether it
#'     should be calculated on the spectras' intensities (`initial = FALSE`).
#'
#' @param i For `[`: `integer`, `logical` to subset the object.
#'
#' @param j For `[`: not supported.
#'
#' @param name For `$`: the name of the variable to access.
#'
#' @param object `MsBackendTimsTof` object.
#'
#' @param spectraVariables `character` with the names of the spectra variables
#'     that should be retained in the returned object.
#'
#' @param value For `$<-`: the value to assign to the spectra variable.
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
#'
#' @examples
#'
#' ## Define the test file
#' fl <- system.file("ddaPASEF.d", package = "MsBackendTimsTof")
#'
#' ## Create a MsBackend instance for that file
#' be <- backendInitialize(MsBackendTimsTof(), fl)
#' be
#'
#' ## Available spectra variables
#' spectraVariables(be)
#'
#' ## Available peaks variables
#' peaksVariables(be)
#'
#' ## Subset to 10 randomly selected spectra.
#' be_sub <- be[sort(sample(seq_along(be), 10))]
#' rtime(be_sub)
#'
#' ## Get the peaks data, i.e. m/z, intensity as well as tof and ion mobility
#' pd <- peaksData(be_sub,
#'     columns = c("mz", "intensity", "tof", "inv_ion_mobility"))
#' pd
#'
#' ## Filter to data set to MS2 spectra only
#' be_ms2 <- filterMsLevel(be, 2L)
#' be_ms2
#'
#' ## Get the full spectra data (including peaks variables)
#' spectraData(be_ms2)
#'
#' ## Add a new spectra variable
#' be$new_var <- seq_along(be)
#'
#' head(be$new_var)
#'
#' ## Changing values of a spectra variable. Note that these are only changed
#' ## *locally* within the object, but not in the original data files.
#' head(rtime(be))
#' be$rtime <- rtime(be) + 10
#' head(rtime(be))
setClass(
    "MsBackendTimsTof",
    contains = "MsBackendCached",
    slots = c(frames = "data.frame",
              indices = "matrix",
              fileNames = "integer"),
    prototype = prototype(
        frames = data.frame(),
        indices = matrix(nrow = 0, ncol = 3,
                         dimnames = list(NULL, c("frame", "scan", "file"))),
        fileNames = integer(),
        readonly = TRUE,
        version = "0.2"))

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
#' @importMethodsFrom Spectra backendInitialize
#'
#' @importClassesFrom Spectra MsBackendCached
#'
#' @rdname MsBackendTimsTof
setMethod("backendInitialize", signature = "MsBackendTimsTof",
          function(object, files, ..., BPPARAM = bpparam()) {
              if (missing(files) || !length(files))
                  stop("Parameter 'files' is mandatory for 'MsBackendTimsTof'",
                       call. = FALSE)
              if (!is.character(files))
                  stop("Parameter 'files' is expected to be a character vector",
                       " with the files names from where data should be",
                       " imported", call. = FALSE)
              files <- normalizePath(files, mustWork = FALSE)
              msg <- Spectra:::.valid_ms_backend_files_exist(files)
              if (length(msg))
                  stop(msg, call. = FALSE)
              object <- .initialize(object, files, BPPARAM)
              object <- callNextMethod(
                  object, nspectra = nrow(object@indices),
                  ## Define all spectra variables that can be retrieved from
                  ## the .d file(s).
                  spectraVariables = unique(c(.TIMSTOF_COLUMNS,
                                       colnames(object@frames),
                                       .MS2_COLUMNS, "dataOrigin",
                                       "msLevel", "scanIndex")))
              validObject(object)
              object
          })

#' @exportMethod length
#'
#' @rdname MsBackendTimsTof
setMethod("length", "MsBackendTimsTof", function(x) {
    x@nspectra
})

#' @rdname MsBackendTimsTof
setMethod(
    "peaksData", "MsBackendTimsTof",
    function(object, columns = c("mz", "intensity")) {
        .get_tims_columns(object, columns)
})

#' @rdname MsBackendTimsTof
#'
#' @importMethodsFrom Spectra peaksVariables
setMethod(
    "peaksVariables", "MsBackendTimsTof",
    function(object) {
        ## TODO: should we return all available, or just the one
        ## in @spectraVariables: `selectSpectraVariables()` should subset
        ## the ones in @spectraVariables. Would need to check how it behaves
        ## when we subset peaks variables - check other backends behave.
        if (length(object@fileNames)) {
            .list_tims_columns(names(object@fileNames)[1L])
        } else c("mz", "intensity")
})

#' @importFrom IRanges NumericList
#'
#' @rdname MsBackendTimsTof
setMethod("mz", "MsBackendTimsTof", function(object) {
    if ("mz" %in% object@spectraVariables)
        NumericList(.get_tims_columns(object, "mz"), compress = FALSE)
    else spectraData(object, "mz")[, 1L]
})

#' @importFrom IRanges NumericList
#'
#' @rdname MsBackendTimsTof
setMethod("intensity", "MsBackendTimsTof", function(object) {
    if ("intensity" %in% object@spectraVariables)
        NumericList(.get_tims_columns(object, "intensity"), compress = FALSE)
    else spectraData(object, "intensity")[, 1L]
})

#' @rdname MsBackendTimsTof
setMethod("rtime", "MsBackendTimsTof", function(object) {
    if ("rtime" %in% colnames(object@localData))
        object@localData$rtime
    else {
        if ("rtime" %in% object@spectraVariables)
            .get_frame_columns(object, "rtime")
        else spectraData(object, "rtime")[, 1L]
    }
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
    extractByIndex(x, i)
})

#' @importMethodsFrom ProtGenerics extractByIndex
#'
#' @rdname MsBackendTimsTof
setMethod("extractByIndex", c("MsBackendTimsTof", "ANY"), function(object, i) {
    object <- .subset_backend(object, i)
    object <- callNextMethod(object, i = i)
    object
})

#' @importMethodsFrom Spectra dataStorage
#'
#' @rdname MsBackendTimsTof
setMethod("dataStorage", "MsBackendTimsTof", function(object) {
    if("file" %in% colnames(object@indices) && length(object@fileNames))
        names(object@fileNames[match(object@indices[, "file"],
                                     object@fileNames)])
    else character(0)
})

#' @importMethodsFrom Spectra spectraData
#'
#' @rdname MsBackendTimsTof
setMethod("spectraData", "MsBackendTimsTof",
          function(object, columns = spectraVariables(object)) {
              .spectra_data(object, columns)
          })

#' @importFrom utils capture.output
#'
#' @rdname MsBackendTimsTof
setMethod("show", "MsBackendTimsTof", function(object) {
    n <- length(object)
    cat(class(object), "with", n, "spectra\n")
    if (n) {
        idx <- unique(c(1L:min(6L, n), max(1L, n-5L):n))
        spd <- spectraData(object[idx, ],
                           c("msLevel", "precursorMz", "polarity"))
        if (!length(rownames(spd)))
            rownames(spd) <- idx
        txt <- capture.output(print(spd))
        cat(txt[-1], sep = "\n")
        sp_cols <- spectraVariables(object)
        cat(" ...", length(sp_cols) - 3, "more variables/columns.\n", "Use ",
            "'spectraVariables' to list all of them.\n")
    }
})

#' @rdname MsBackendTimsTof
setMethod("$", "MsBackendTimsTof", function(x, name) {
    if (!name %in% spectraVariables(x))
        stop("spectra variable '", name, "' not available")
    if (name == "inv_ion_mobility" &&
        "inv_ion_mobility" %in% x@spectraVariables)
        .inv_ion_mobility(x)
    else
        spectraData(x, name)[, 1L]
})

#' @rdname MsBackendTimsTof
setMethod(
    "selectSpectraVariables", "MsBackendTimsTof",
    function(object, spectraVariables = spectraVariables(object)) {
        req_cols <- c("frameId", "file")
        if ("msLevel" %in% spectraVariables) req_cols <- c(req_cols, "MsMsType")
        keep <- colnames(object@frames) %in% union(spectraVariables, req_cols)
        object@frames <- object@frames[, keep, drop = FALSE]
        callNextMethod(object, spectraVariables)
    })

#' @rdname MsBackendTimsTof
#'
#' @export
setReplaceMethod("$", "MsBackendTimsTof", function(x, name, value) {
    if (name %in% union(peaksVariables(x), c("file", "frameId")))
        stop("Replacing spectra/peaks variable \"", name,
             "\" is not supported.", call. = FALSE)
    callNextMethod()
})

#' @rdname MsBackendTimsTof
#'
#' @importMethodsFrom ProtGenerics precScanNum
#'
#' @exportMethod precScanNum
setMethod("precScanNum", "MsBackendTimsTof", function(object) {
    spectraData(object, "precScanNum")[, 1L]
})

#' @exportMethod spectraNames
#'
#' @importMethodsFrom ProtGenerics spectraNames
#'
#' @rdname MsBackendTimsTof
setMethod("spectraNames", "MsBackendTimsTof", function(object) {
    row.names(object@indices)
})

#' @importMethodsFrom ProtGenerics tic
#'
#' @importFrom Spectra intensity
#'
#' @importFrom MsCoreUtils vapply1d
#'
#' @exportMethod tic
#'
#' @rdname MsBackendTimsTof
setMethod("tic", "MsBackendTimsTof", function(object, initial = TRUE) {
    if (initial)
        rep(NA_real_, length(object))
    else vapply1d(intensity(object), sum, na.rm = TRUE)
})
