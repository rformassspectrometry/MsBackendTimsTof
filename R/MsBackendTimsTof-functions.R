#' @description
#'
#' Initialize the `MsBackendTimsTOF` object `x`:
#' - iterate over the files, for each file:
#' - get the *frame* information and build the `@indices` matrix with the
#'   frame index, scan index and file index columns.
#' - fix polarity information
#'
#' @author Andrea Vicini, Johannes Rainer
#'
#' @importFrom BiocParallel bplapply
#'
#' @importFrom MsCoreUtils rbindFill
#'
#' @importFrom opentimsr OpenTIMS CloseTIMS
#'
#' @return initialized `MsBackendTimsTOF` object
#'
#' @importFrom BiocParallel bpparam
#'
#' @importFrom stats setNames
#'
#' @noRd
.initialize <- function(x, file = character(), BPPARAM = bpparam()) {
    L <- bplapply(seq_len(length(file)), function(fl_idx) {
        tms <- opentimsr::OpenTIMS(file[fl_idx])
        on.exit(opentimsr::CloseTIMS(tms))
        frames <- cbind(tms@frames, file = fl_idx)
        indices <- unique(.query_tims(tms, frames = frames$Id,
                                      columns = c("frame", "scan")))
        indices$file <- fl_idx
        list(frames, as.matrix(indices, rownames.force = FALSE))
    }, BPPARAM = BPPARAM)
    x@frames <- as.data.frame(rbindlist(lapply(L, "[[", 1), fill = TRUE))
    idx <- match(colnames(x@frames), .SPECTRA_VARIABLE_MAPPINGS)
    not_na <- !is.na(idx)
    colnames(x@frames)[not_na] <- names(.SPECTRA_VARIABLE_MAPPINGS)[idx[not_na]]
    if (any(colnames(x@frames) == "polarity"))
        x@frames$polarity <- .format_polarity(x@frames$polarity)
    x@indices <- do.call(rbind, lapply(L, "[[", 2))
    row.names(x@indices) <- seq_len(nrow(x@indices)) # do we need rownames?
    x@fileNames <- setNames(seq_len(length(file)), file)
    x
}

#' @description
#'
#' Check if a `matrix`/`data.frame` has all required columns.
#'
#' @param x `matrix`/`data.frame`.
#'
#' @param columns `character` specifying the required columns.
#'
#' @noRd
.valid_required_columns <- function(x, columns = character(0)) {
    if (nrow(x)) {
        missing_cn <- setdiff(columns, colnames(x))
        if (length(missing_cn))
            return(paste0("Required column(s): ",
                          paste(missing_cn, collapse = ", "),
                          " is/are missing"))
    }
    NULL
}

#' @description
#'
#' Checks if `data.frame` `x` is compatible to be the `@frames` slot of a
#' `MsBackendTimsTof` object.
#'
#' @param x `data.frame`.
#'
#' @noRd
.valid_frames <- function(x) {
    .valid_required_columns(x, c("frameId", "file"))
}

#' @description
#'
#' Checks if the `@indices` slot in `x` is valid.
#'
#' @param x `MsBakendTimsTof` object.
#'
#' @noRd
.valid_indices <- function(x) {
    msg <- .valid_required_columns(x@indices, c("frame", "file"))
    if ("file" %in% colnames(x@indices) && !setequal(x@indices[, "file"],
                                                     x@fileNames))
        msg <- c(msg, "Some file indices are out of bounds")
    if (any(!paste0(x@indices[, "frame"], x@indices[, "file"]) %in%
            paste0(x@frames$frameId, x@frames$file)))
        msg <- c(msg, "Some indices are out of bounds")
    msg
}

#' @description
#'
#' Checks if the named `integer` `x` is compatible to be the `@fileNames` slot
#' of a `MsBackendTimsTof` object.
#'
#' @param x `character` with folder names.
#'
#' @noRd
.valid_fileNames <- function(x) {
    msg <- NULL
    if (anyNA(x))
        msg <- "'NA' values in 'fileNames' are not allowed."
    nms <- names(x)
    if (anyNA(nms))
        msg <- "'NA' values in 'names()' of 'fileNames' are not allowed."
    msg <- c(msg, Spectra:::.valid_ms_backend_files_exist(unique(nms)))
    msg
}

#' @importFrom methods new
#'
#' @rdname MsBackendTimsTof
#'
#' @export
MsBackendTimsTof <- function() {
    new("MsBackendTimsTof")
}

#' @description
#'
#' Get `x@all_columns` variables (including "`mz`" and "`intensity`") from `x`
#' split by spectra. At least one variable among `x@all_columns` and
#' different from `"frame"` and "`scan`" has to be provided via `columns`
#' parameter.
#'
#' @param x `MsBackendTimsTof` object.
#'
#' @param columns `character` with the names of the columns to extract.
#'
#' @param drop `logical` if TRUE and `columns` has length 1 the result is
#'   returned as list of `numeric` instead of as list of 1-column `matrix`.
#'
#' @importFrom opentimsr OpenTIMS CloseTIMS query opentims_set_threads
#'
#' @importFrom MsCoreUtils rbindFill
#'
#' @noRd
.get_tims_columns <- function(x, columns, drop = TRUE) {
    ## Disable parallel processing in opentimsr as that breaks BiocParallel
    opentims_set_threads(1L)
    res <- vector(mode = "list", length(x))
    nms <- names(x@fileNames)
    for (i in seq_len(length(nms))) { # bplapply instead to fetch in parallel?
        I <- which(x@indices[, "file"] == x@fileNames[i])
        i_frame <- x@indices[I, "frame"]
        tmp <- .query_tims(nms[i], unique(i_frame), columns)
        ## subset tmp if we're about to extract only few scans.
        if (length(i_frame) < (nrow(tmp) / 10)) {
            i_scan <- x@indices[I, "scan"]
            tmp <- tmp[tmp$scan %in% i_scan, ]
            rownames(tmp) <- NULL
            ids <- paste(i_frame, i_scan)
        } else ids <- paste(i_frame, x@indices[I, "scan"])
        ## Not quite sure why we had the code below; .query_tims would return
        ## nothing if it would not find the selected frames - but these frames
        ## are read from the .d file, so they should be there.
        ## if (!nrow(tmp))
        ##     tmp <- rbindFill(tmp, data.frame(frame = 0L))
        f <- factor(paste(tmp$frame, tmp$scan), levels = unique(ids))
        if (anyDuplicated(ids)) {
            if (length(columns) == 1)
                res[I] <- unname(split(tmp[, columns, drop], f)[ids])
            else
                res[I] <- unname(
                    split.data.frame(as.matrix(tmp[, columns, drop]), f)[ids])
        } else {
            if (length(columns) == 1) {
                res[I] <- unname(split(tmp[, columns, drop], f))
            } else {
                res[I] <- unname(
                    split.data.frame(as.matrix(tmp[, columns, drop]), f))
            }
        }
    }
    res
}

#' @description
#'
#' Extract the inv_ion_mobility information from the TimsTOF file.
#'
#' @author Johannes Rainer
#'
#' @noRd
.inv_ion_mobility <- function(x, BPPARAM = bpparam()) {
    f <- as.factor(x@indices[, "file"])
    res <- bplapply(split.data.frame(x@indices, f), function(z, x) {
        fn <- names(x@fileNames)[match(z[1, "file"], x@fileNames)]
        tmp <- unique(.query_tims(
            fn, unique(z[, "frame"]), c("frame", "scan", "inv_ion_mobility")))
        ids <- paste(z[, "frame"], z[, "scan"])
        tmp_ids <- paste(tmp$frame, tmp$scan)
        tmp[match(ids, tmp_ids), "inv_ion_mobility"]
    }, x = x, BPPARAM = BPPARAM)
    unsplit(res, f)
}

#' @description
#'
#' Function to use the `query()` function from *opentimsr* to retrieve data
#' from a **single** file.
#'
#' @param x `opentimsr::OpenTIMS` object or `character(1)`.
#'
#' @param frames `integer` with the IDs (indices) of the frames to retrieve
#'     data from.
#'
#' @param columns `character` defining the columns to retrieve.
#'
#' @return `data.frame` with the requested data.
#'
#' @author Johannes Rainer
#'
#' @noRd
.query_tims <- function(x, frames, columns) {
    if (is.character(x)) {
        x <- OpenTIMS(x)
        on.exit(opentimsr::CloseTIMS(x))
    }
    if (any(notin <- !columns %in% x@all_columns)) {
        msg <- paste0("'", columns[notin], "'", collapse = ", ")
        stop("Column(s) ", msg, " not available.", call. = FALSE)
    }
    sd <- setdiff(columns, c("frame", "scan"))
    query(x, unique(frames), c("frame", "scan", sd))
}

#' @description
#'
#' Lists all available peak columns in TIMS file
#'
#' @param x `character(1)` with the file name.
#'
#' @author Johannes Rainer
#'
#' @noRd
.list_tims_columns <- function(x) {
    tms <- OpenTIMS(x)
    on.exit(opentimsr::CloseTIMS(tms))
    tms@all_columns
}

#' @description
#'
#' Extract columns from @frames given the @indices in `x`. The function takes
#' care of eventually duplicating values.
#'
#' @param x `MsBackendTimsTOF`
#'
#' @param columns `character` with the column names.
#'
#' @param drop `logical` if TRUE and `columns` has length 1 the result is
#'   returned as `numeric` instead of as 1-column `data.frame`.
#'
#' @author Andrea Vicini, Johannes Rainer
#'
#' @noRd
.get_frame_columns <- function(x, columns, drop = TRUE) {
    if (!all(columns %in% colnames(x@frames))) {
        msg <- paste0("'", columns[!columns %in% colnames(x@frames)],
                      "'", collapse = ", ")
        stop("Column(s) ", msg, " not available.", call. = FALSE)
    }
    idx <- match(paste(x@indices[, "frame"], x@indices[, "file"]),
                 paste(x@frames$frameId, x@frames$file))
    x@frames[idx, columns, drop]
}

#' @description
#'
#' Mapping of spectra variables to frames column names.
#'
#' @noRd
.SPECTRA_VARIABLE_MAPPINGS <- c(
    rtime = "Time",
    polarity = "Polarity",
    frameId = "Id"
)

.format_polarity <- function(x) {
    xn <- rep(NA_integer_, length(x))
    xn[grep("^(p|\\+)", x, ignore.case = TRUE)] <- 1L
    xn[grep("^(n|-)", x, ignore.case = TRUE)] <- 0L
    xn
}

#' @description
#'
#' Get the msLevel for each spectra. `x` is interpreted either as
#' `MsBackendTimsTof` if `isMsMsType` is `FALSE` or as `numeric` with the
#' MsMsType of each spectra if `isMsMsType` is `TRUE`.
#'
#' @noRd
.get_msLevel <- function(x, isMsMsType = FALSE) {
    # msLevel=1 should correspond to MsMsType = 0. msLevel=2 to MsMsType = 8?
    if (!isMsMsType) {
        if (!"MsMsType" %in% colnames(x@frames))
            return(rep(NA_integer_, length(x)))
        else x <- .get_frame_columns(x, "MsMsType")
    }
    map <- c(0L, 8L)
    if (any(!is.na(x) & !x %in% map))
        warning("msLevel not recognized for some spectra and set to NA.")
    match(x, map)
}

# can we assume that tms@all_coulmns is the same for all the TimsTOF?
.TIMSTOF_COLUMNS <- c("mz", "intensity", "tof", "inv_ion_mobility")

#' @description
#'
#' Main function to extract the `spectraData` from the backend. The
#' `.ms2_spectra_data()` function is used to retrieve the MS2 information for
#' DDA MS2 data from the *analysis.tdf* SQLite databases of the .d files.
#'
#' @param x `MsBackendTimsTof`
#'
#' @param columns `character` with the column names.
#'
#' @importFrom methods as callNextMethod getMethod
#'
#' @importFrom S4Vectors DataFrame extractCOLS
#'
#' @importFrom S4Vectors cbind.DataFrame make_zero_col_DFrame
#'
#' @importFrom Spectra coreSpectraVariables
#'
#' @importMethodsFrom Spectra spectraVariables
#'
#' @return `DataFrame` with columns identical to `columns`, always.
#'
#' @author Andrea Vicini, Johannes Rainer
#'
#' @noRd
.spectra_data <- function(x, columns = spectraVariables(x)) {
    if (length(miss <- setdiff(columns, spectraVariables(x)))) {
        msg <- paste0("\"", miss, "\"")
        stop("Column(s) ", msg, " not available.", call. = FALSE)
    }
    ## Get cached data and data for core variables not provided through .d
    res <- getMethod("spectraData", "MsBackendCached")(x, columns = columns)
    if (is.null(res))
        res <- make_zero_col_DFrame(length(x))
    ## define columns that are not retrieved from the cache.
    cols <- setdiff(columns, colnames(res))
    ## Columns stored in @frames
    frames_cols <- intersect(cols, colnames(x@frames))
    ## Columns retrieved with querying through opentimsr
    tims_cols <- intersect(cols, setdiff(.TIMSTOF_COLUMNS, "inv_ion_mobility"))

    if ("scanIndex" %in% cols)
        res$scanIndex <- x@indices[, "scan"]
    if (length(frames_cols))
        res <- cbind.DataFrame(
            res, .get_frame_columns(x, frames_cols, drop = FALSE))
    if (length(tims_cols)) {
        if ("inv_ion_mobility" %in% cols) {
            pks <- .get_tims_columns(x, c(tims_cols, "inv_ion_mobility"),
                                     drop = FALSE)
            res$inv_ion_mobility <- vapply(
                pks, function(m) unname(m[1L, "inv_ion_mobility"]), numeric(1))
        } else
            pks <- .get_tims_columns(x, tims_cols, drop = FALSE)
        tms <- vector("list", length(tims_cols))
        names(tms) <- tims_cols
        for (col in tims_cols)
            tms[[col]] <- NumericList(lapply(pks, function(m) unname(m[, col])),
                                      compress = FALSE)
        res <- cbind.DataFrame(res, tms)
    } else {
        if ("inv_ion_mobility" %in% cols)
            res$inv_ion_mobility <- .inv_ion_mobility(x)
    }
    if ("msLevel" %in% cols) {
        if ("MsMsType" %in% frames_cols)
            res[["msLevel"]] <- .get_msLevel(res[["MsMsType"]], TRUE)
        else
            res[["msLevel"]] <- .get_msLevel(x)
    }
    if ("dataOrigin" %in% cols)
        res[["dataOrigin"]] <- dataStorage(x)
    ## DDA MS2 columns
    if (length(ms2_cols <- cols[cols %in% .MS2_COLUMNS]))
        res <- cbind(res, .ms2_spectra_data(x, columns = ms2_cols))
    extractCOLS(res, columns)
}

#' @description
#'
#' Subset the `MsBackendTimsTof` by index `i`.
#'
#' `@indices` and `@frames` slots are subset and ordered according to `i`.
#' `@fileNames` is only subset but **not** re-ordered! Also, the contents
#' of the `matrix` are not updated, i.e. the content of columns `"frame"`,
#' `"scan"` and `"file"` are unchanged, only the rows are subset and reordered.
#'
#' @param x `MsBackendTimsTof`
#'
#' @param i `integer` vector with the **same** length than `x`.
#'
#' @return subset `MsBackendTimsTof`.
#'
#' @author Johannes Rainer
#'
#' @noRd
.subset_backend <- function(x, i) {
    slot(x, "indices", check = FALSE) <- x@indices[i, , drop = FALSE]
    ff_indices <- paste(x@indices[, "frame"], x@indices[, "file"])
    ## would a `left_join` be faster?
    slot(x, "frames", check = FALSE) <-
        x@frames[match(unique(ff_indices),
                       paste(x@frames$frameId,
                             x@frames$file)), , drop = FALSE]
    slot(x, "fileNames", check = FALSE) <-
        x@fileNames[x@fileNames %in% unique(x@frames$file)]
    x
}

#' @description
#'
#' Get a `DBconnection` to the *analysis.tdf* SQLite database of the provided
#' *.d* directory.
#'
#' @param x `character(1)` with the path and directory name of **one** TimsTof
#'     *.d* file.
#'
#' @importFrom RSQLite SQLite
#'
#' @importMethodsFrom DBI dbConnect
#'
#' @noRd
.analysis_tdf <- function(x) {
    fl <- file.path(x, c("analysis.tdf", "Analysis.tdf"))
    fl <- fl[file.exists(fl)]
    if (!length(fl))
        stop("\"analysis.tdf\" not found in \"", x, "\"", .call = FALSE)
    dbConnect(SQLite(), fl[1L]) # non-case sensitive FS will report both paths
}

#' @description
#'
#' Get the frame IDs with (DDA) MS2 scans.
#'
#' @param x (initialized) `MsBackendTimsTof`.
#'
#' @return `list` of `integer` with the IDs of the frames.
#'
#' @noRd
.ms2_frames <- function(x, ms_ms_type = 8L) {
    sel <- which(x@frames$MsMsType == ms_ms_type)
    split(x@frames$frameId[sel], as.factor(x@frames$file[sel]))
}

#' @description
#'
#' Helper function to get the SQLite column name to report as precursor m/z.
#'
#' @noRd
.precursor_mz_column <- function() {
    getOption("TIMSTOF_PRECURSOR_MZ")
}

#' The column names for which we must query the analysis.tdf SQLite database
#'
#' @noRd
.MS2_COLUMNS <- c("precursorMz", "precursorIntensity", "collisionEnergy",
                  "precursorCharge", "isolationWindowTargetMz",
                  "isolationWindowLowerMz", "isolationWindowUpperMz")

#' @description
#'
#' Get (DDA) MS2 information from a **single** .d file. Needs the file name and
#' the IDs (indices) of the frames for which to extract the data.
#'
#' @param d `character(1)` with the path/file name of **one** .d file.
#'
#' @param frames `integer` with the IDs of the frames that contain MS2 scans.
#'
#' @param columns `character` with the columns/information that should be
#'     returned. Matches the `coreSpectraVariables()` with MS2 content.
#'
#' @return a `data.frame` with the MS2 information for **all** (potential)
#'     scans in each of the provided `frames`. The returned contains all
#'     columns defined with `columns` as well as `"frame"` and `"scan"` that
#'     can then be used to join the `data.frame` with the `@indices`. Note that
#'     the order of the columns does **not** match the order in `columns`!
#'
#' @author Johannes Rainer
#'
#' @importMethodsFrom DBI dbDisconnect dbGetQuery
#'
#' @noRd
.ms2_d <- function(d = character(), frames = integer(),
                   columns = .MS2_COLUMNS) {
    con <- .analysis_tdf(d)
    on.exit(dbDisconnect(con))
    cols <- c("Frame", "ScanNumBegin", "ScanNumEnd", .ms2_cols(columns))
    db <- dbGetQuery(
        con, paste0("select ", paste0(cols, collapse = ","), " from ",
                    "PasefFrameMsMsInfo join Precursors on ",
                    "(PasefFrameMsMsInfo.Precursor = Precursors.Id) where ",
                    "Frame in (", paste0(frames, collapse = ","), ")"))
    ## define the number of times we need to replicate each
    rp <- db$ScanNumEnd - db$ScanNumBegin + 1L
    ## build the base results data.frame
    res <- data.frame(frame = rep(db$Frame, rp),
                      scan = sequence(rp, from = db$ScanNumBegin))
    ## add info depending on `columnns`
    if ("precursorMz" %in% columns)
        res$precursorMz <- rep(db[[.precursor_mz_column()]])
    if ("precursorIntensity" %in% columns)
        res$precursorIntensity <- rep(as.numeric(db$Intensity), rp)
    if ("collisionEnergy" %in% columns)
        res$collisionEnergy <- rep(as.numeric(db$CollisionEnergy), rp)
    if ("precursorCharge" %in% columns)
        res$precursorCharge <- rep(as.integer(db$Charge), rp)
    if ("isolationWindowTargetMz" %in% columns)
        res$isolationWindowTargetMz <- rep(as.numeric(db$IsolationMz), rp)
    if ("isolationWindowLowerMz" %in% columns)
        res$isolationWindowLowerMz <- rep(
            db$IsolationMz - db$IsolationWidth / 2.0, rp)
    if ("isolationWindowUpperMz" %in% columns)
        res$isolationWindowUpperMz <- rep(
        db$IsolationMz + db$IsolationWidth / 2.0, rp)
    res
}

#' @description
#'
#' Helper to create an *empty* `data.frame` for the `.ms2_spectra_data()`
#' function.
#'
#' @author Johannes Rainer
#'
#' @noRd
.ms2_d_empty <- function(columns = .MS2_COLUMNS) {
    data.frame(frame = integer(),
               scan = integer(),
               precursorMz = numeric(),
               precursorIntensity = numeric(),
               collisionEnergy = numeric(),
               precursorCharge = integer(),
               isolationWindowTargetMz = numeric(),
               isolationWindowLowerMz = numeric(),
               isolationWindowUpperMz = numeric(),
               file = integer())[, c("frame", "scan", columns, "file")]
}

#' @description
#'
#' Helper function to retrieve (DDA) MS2 data from the SQLite databases of the
#' .d files in an `MsBackendTimsTof` for all spectra.
#'
#' @param x `MsBackendTimsTof`
#'
#' @param columns `character` with the names of the columns to extract.
#'
#' @param drop `logical(1)` whether the dimensions of the returned `data.frame`
#'     should be dropped in case a single column is requested.
#'
#' @return `data.frame` with columns `columns` (in the order of `columns`). The
#'     row order matches the order of spectra in `MsBackendTimsTof`.
#'
#' @importFrom dplyr left_join
#'
#' @importFrom data.table rbindlist
#'
#' @importFrom BiocParallel bpmapply bpparam
#'
#' @author Johannes Rainer, Roger Gine
#'
#' @noRd
.ms2_spectra_data <- function(x, columns = .MS2_COLUMNS, drop = FALSE,
                              BPPARAM = bpparam()) {
    frms <- .ms2_frames(x)
    frms <- frms[lengths(frms) > 0]
    if (length(frms)) {
        res <- as.data.frame(rbindlist(
            bpmapply(function(fr, index, fileNames, columns) {
                d <- names(fileNames)[which(fileNames == index)]
                ms2 <- .ms2_d(d, frames = fr, columns = columns)
                ms2$file <- rep(index, nrow(ms2))
                ms2
            }, frms, as.integer(names(frms)),
            MoreArgs = list(fileNames = x@fileNames, columns = columns),
            SIMPLIFY = FALSE, USE.NAMES = FALSE, BPPARAM = BPPARAM)))
    } else res <- .ms2_d_empty(columns)
    left_join(as.data.frame(x@indices), res,
              by = c(file = "file", frame = "frame",
                     scan = "scan"))[, columns, drop = drop]
}

#' @description
#'
#' Helper function to define the SQLite column names required for specified
#' spectra variables.
#'
#' @param x `character` with (core spectra) variables to request
#'
#' @return `character` with the database column names needed for these
#'     variables.
#'
#' @noRd
.ms2_cols <- function(x = character()) {
    cols <- character()
    if ("precursorMz" %in% x)
        cols <- c(cols, .precursor_mz_column())
    if ("precursorIntensity" %in% x)
        cols <- c(cols, "Intensity")
    if ("collisionEnergy" %in% x)
        cols <- c(cols, "CollisionEnergy")
    if ("precursorCharge" %in% x)
        cols <- c(cols, "Charge")
    if (any(c("isolationWindowTargetMz", "isolationWindowLowerMz",
              "isolationWindowUpperMz") %in% x))
        cols <- c(cols, c("IsolationMz", "IsolationWidth"))
    cols
}


#' @title Setup converter library for Bruker files
#'
#' @description
#'
#' Function to setup the required built-in open-source converters or the Bruker
#' library to convert tof-to-mz and scan-to-inv_ion_mobility.
#' When the Burker option is selected the library is downloaded automatically
#' using `opentimsr::download_bruker_proprietary_code()`.
#'
#' @note
#'
#' This function is called during package startup, thus it most cases it is not
#' required to be used. This will use the [opentimsr::setup_opensource()]
#' function. To use Bruker's proprietary library, call
#' `setup_converter_library()` with parameter `opensource = FALSE`. This will
#' download the library with [opentimsr::download_bruker_proprietary_code()] and
#' cache the file in the local *BiocFileCache*.
#'
#' @param opensource `logical(1)` determine if use the built-in open-source
#'     converters of *opentimsr* (defaults) or the proprietary library from
#'     Bruker. Defaults to `opensource = TRUE`.
#'
#' @param path `character(1)` where save the Bruker library. If `NULL` the
#'     library is cached using *BiocFileCache*. Used only with the Bruker
#'     library. Defaults to `path = NULL`.
#'
#' @param force `logical(1)` force to redownload the Bruker library (default:
#'     `FALSE`).
#'
#' @importFrom opentimsr download_bruker_proprietary_code
#'
#' @importFrom opentimsr setup_bruker_so
#'
#' @importFrom opentimsr setup_opensource
#'
#' @author Gabriele Tomè
#'
#' @examples
#'
#' ## To setup the open-source built-in library
#' setup_converter_library()
#'
#' ## Use `setup_converter_library(opensource = FALSE)` to download and use the
#' ## proprietary library from Bruker. The library file is downloaded and
#' ## cached with *BiocFileCache* package. Alternatively, to download the
#' ## library to a specific local path use the `path` parameter.
#' #'
#' @export
setup_converter_library <- function(opensource = TRUE, path = NULL,
                                    force = FALSE) {
    if(opensource) {
        setup_opensource()
    } else {
        bruker_libs_name <- c("libtimsdata.so", "timsdata.dll")
        if(is.null(path)) {
            ## Cache the library with BiocFileCache
            if(!requireNamespace("BiocFileCache", quietly = TRUE))
                stop("The *BiocFileCache* package is required if ",
                    "`path = NULL`. Please install it and try again.",
                    call. = FALSE)

            bfc <- BiocFileCache::BiocFileCache()
            cached <- BiocFileCache::bfcquery(bfc, bruker_libs_name,
                                              exact = TRUE)
            if(!nrow(cached) | force){
                if(nrow(cached) & force)
                    BiocFileCache::bfcremove(bfc, cached$rid)

                bruker_library <- download_bruker_proprietary_code(tempdir())
                bruker_location <- BiocFileCache::bfcadd(bfc,
                                        rname = basename(bruker_library),
                                        fpath = bruker_library, action = "copy",
                                        fname = "exact")
            } else {
                bruker_location <- BiocFileCache::bfcpath(bfc, cached[1, "rid"])
            }
        } else {
            cached <- list.files(path, pattern = bruker_libs_name,
                                 full.names = TRUE)
            if(!length(cached) | force){
                if(length(cached) & force)
                    file.remove(cached)

                bruker_location <- download_bruker_proprietary_code(path)
            } else {
                bruker_location <- cached[1]
            }
        }
        setup_bruker_so(bruker_location)
    }
}
