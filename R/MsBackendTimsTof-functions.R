#' Initialize the `MsBackendTimsTOF` object `x`
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
    x@frames <- do.call(rbindFill, lapply(L, "[[", 1))
    idx <- match(colnames(x@frames), .SPECTRA_VARIABLE_MAPPINGS)
    not_na <- !is.na(idx)
    colnames(x@frames)[not_na] <- names(.SPECTRA_VARIABLE_MAPPINGS)[idx[not_na]]
    if (any(colnames(x@frames) == "polarity"))
        x@frames$polarity <- .format_polarity(x@frames$polarity)
    x@indices <- do.call(rbind, lapply(L, "[[", 2))
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
#'
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
        msg <- "'NA' values in fileNames names are not allowed."
    nms <- names(x)
    if (anyNA(nms))
        msg <- "'NA' values in fileNames names are not allowed."
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

#' Get `x@all_columns` variables (including "`mz`" and "`intensity`") from `x`
#' splitted by spectra. At least one variable among `x@all_columns` and
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
    for (i in seq_len(length(nms))) {
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
        if (!nrow(tmp))
            tmp <- rbindFill(tmp, data.frame(frame = 0L))
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

#' Extract the inv_ion_mobility from the TimsTOF file.
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

#' Function to use the `query` function from opentimsr to retrieve data from
#' a single file.
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
#' @noRd
#'
#' @author Johannes Rainer
.query_tims <- function(x, frames, columns) {
    if (is.character(x)) {
        x <- OpenTIMS(x)
        on.exit(opentimsr::CloseTIMS(x))
    }
    if (any(notin <- !columns %in% x@all_columns))
        stop("Column(s) ",
             paste0("'", columns[notin], "'", collapse = ", "),
             " not available.", call. = FALSE)
    sd <- setdiff(columns, c("frame", "scan"))
    query(x, unique(frames), c("frame", "scan", sd))
}

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
    if (!all(columns %in% colnames(x@frames)))
        stop("Column(s) ",
             paste0("'", columns[!columns %in% colnames(x@frames)],
                    "'", collapse = ", "), " not available.", call. = FALSE)
    idx <- match(paste(x@indices[, "frame"], x@indices[, "file"]),
                 paste(x@frames$frameId, x@frames$file))
    x@frames[idx, columns, drop]
}

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

#' Get the msLevel for each spectra. `x` is interpreted either as
#' `MsBackendTimsTof` if `isMsMsType` is `FALSE` or as `numeric` with the
#' MsMsType of each spectra if `isMsMsType` is `TRUE`.
#'
#' @noRd
.get_msLevel <- function(x, isMsMsType = FALSE) {
    # msLevel=1 should correspond to MsMsType = 0. msLevel=2 to MsMsType = 8?
    if (!isMsMsType) {
        if (!"MsMsType" %in% colnames(x@frames))
            return(rep(as(NA, "integer"), length(x)))
        else x <- .get_frame_columns(x, "MsMsType")
    }
    map <- c(0L, 8L)
    if (any(!is.na(x) & !x %in% map))
        warning("msLevel not recognized for some spectra and set to NA.")
    match(x, map)
}

# can we assume that tms@all_coulmns is the same for all the TimsTOF?
.TIMSTOF_COLUMNS <- c("mz", "intensity", "tof", "inv_ion_mobility")

.TIMSTOF_MS2_COLUMNS <- c("precursorMz", "precursorCharge",
                          "precursorIntensity", "collisionEnergy",
                          "isolationWindowLowerMz", "isolationWindowTargetMz",
                          "isolationWindowUpperMz")

#' @importFrom methods as
#'
#' @importFrom S4Vectors DataFrame
#'
#' @importFrom Spectra coreSpectraVariables
.spectra_data <- function(x, columns = spectraVariables(x)) {
    if (!all(present <- columns %in% spectraVariables(x)))
        stop("Column(s) ", paste0("\"", columns[!present], "\"",
                                  collapse = ", "),
             " not available.", call. = FALSE)
    res <- vector(mode = "list", length = length(columns))
    names(res) <- columns
    core_cols <- columns[columns %in% names(coreSpectraVariables())]
    frames_cols <- columns[columns %in% colnames(x@frames)]
    tims_cols <- columns[
        columns %in% .TIMSTOF_COLUMNS[!.TIMSTOF_COLUMNS == "inv_ion_mobility"]]
    if ("scanIndex" %in% columns) {
        res[["scanIndex"]] <- x@indices[, "scan"]
        core_cols <- core_cols[core_cols != "scanIndex"]
    }
    if ("frameId" %in% frames_cols) {
        res[["frameId"]] <- x@indices[, "frame"]
        frames_cols <- frames_cols[frames_cols != "frameId"]
    }
    if ("file" %in% frames_cols) {
        res[["file"]] <- x@indices[, "file"]
        frames_cols <- frames_cols[frames_cols != "file"]
    }
    if (length(frames_cols)) {
        res[frames_cols] <- .get_frame_columns(x, frames_cols, drop = FALSE)
        core_cols <- setdiff(core_cols, frames_cols)
    }
    if (length(tims_cols)) {
        if ("inv_ion_mobility" %in% columns) {
            pks <- .get_tims_columns(x, c(tims_cols, "inv_ion_mobility"),
                                     drop = FALSE)
            res[["inv_ion_mobility"]] <-
                vapply(pks, function(m) unname(m[1L, "inv_ion_mobility"]),
                       numeric(1))
        } else
            pks <- .get_tims_columns(x, tims_cols, drop = FALSE)
        for (col in tims_cols)
            res[[col]] <- NumericList(lapply(pks, function(m) unname(m[, col])),
                                      compress = FALSE)
        core_cols <- setdiff(core_cols, tims_cols)
    } else {
        if ("inv_ion_mobility" %in% columns)
            res[["inv_ion_mobility"]] <- .inv_ion_mobility(x)
    }
    if ("msLevel" %in% columns) {
        if ("MsMsType" %in% frames_cols) {
            res[["msLevel"]] <- .get_msLevel(res[["MsMsType"]], TRUE)
        } else {
            res[["msLevel"]] <- .get_msLevel(x)
        }
        core_cols <- core_cols[core_cols != "msLevel"]
    }
    if ("dataStorage" %in% columns) {
        res[["dataStorage"]] <- dataStorage(x)
        core_cols <- core_cols[core_cols != "dataStorage"]
    }
    if (any(core_cols %in% .TIMSTOF_MS2_COLUMNS)) {
        res_ms2_data <- .calculate_core_ms2_information(x)
        for (col in core_cols[core_cols %in% .TIMSTOF_MS2_COLUMNS]) {
            res[[col]] <- res_ms2_data[, which(.TIMSTOF_MS2_COLUMNS == col)]
            core_cols <- core_cols[core_cols != col]
        }
    }
    if (length(core_cols)) {
        res[core_cols] <- lapply(coreSpectraVariables()[core_cols],
                                 function(z, n) rep(as(NA, z), n), length(x))
    }
    if (length(res))
        as(res, "DataFrame")
    else DataFrame()
}

#' @importFrom BiocParallel bpmapply bpparam
.calculate_core_ms2_information <- function(x){
    if (is.null(names(x@fileNames))){
        return(matrix(numeric(), nrow = 0, ncol = length(.TIMSTOF_MS2_COLUMNS)))
    }
    tbls <- bpmapply(FUN = .do_calculate_core_ms2_information,
                     x = names(x@fileNames),
                     indices = lapply(split(x@indices[,1:2], f = x@indices[,3]),
                                      matrix, ncol = 2),
                     SIMPLIFY = FALSE,
                     BPPARAM = bpparam())
    output <- do.call(rbind, tbls)
    output[order(order(x@indices[,3])), ]
}

#' @importFrom opentimsr OpenTIMS CloseTIMS table2df
#' @importFrom MsCoreUtils between
.do_calculate_core_ms2_information <- function(x, indices){
    tms <- opentimsr::OpenTIMS(x)
    on.exit(opentimsr::CloseTIMS(tms))
    if (missing(indices)) {
        indices <- unique(.query_tims(tms, frames = tms@frames$Id,
                                      columns = c("frame", "scan")))
    }
    output <- matrix(NA_real_,
                     nrow = nrow(indices),
                     ncol = length(.TIMSTOF_MS2_COLUMNS))
    tbl <- opentimsr::table2df(tms, c("PASEFFrameMsMsInfo", "Precursors"))
    for (i in seq(nrow(tbl[[1]]))) {
        row <- tbl[[1]][i, ]
        prec <- tbl[[2]][row$Precursor,]
        target_rows <- which(indices[,1] == row$Frame &
                             MsCoreUtils::between(indices[,2],
                                                  c(row$ScanNumBegin,
                                                    row$ScanNumEnd)))
        if (length(target_rows)) {
            output[target_rows, ] <- 
                rep(c(prec$MonoisotopicMz,                       ## precursorMz
                         prec$Charge,                            ## precursorCharge
                         prec$Intensity,                         ## precursorIntensity
                         row$CollisionEnergy,                    ## collisionEnergy
                         row$IsolationMz - row$IsolationWidth,   ## isolationWindowLowerMz
                         row$IsolationMz,                        ## isolationWindowTargetMz
                         row$IsolationMz + row$IsolationWidth),  ## isolationWindowUpperMz
                     each = length(target_rows))
        }
    }
    output
}
