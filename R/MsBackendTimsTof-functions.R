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
#' @noRd
.initialize <- function(x, file = character(), BPPARAM = bpparam()) {
    L <- bplapply(seq_len(length(file)), function(fl_idx) {
        tms <- opentimsr::OpenTIMS(file[fl_idx])
        on.exit(opentimsr::CloseTIMS(tms))
        frames <- cbind(tms@frames, file = fl_idx)
        indices <- cbind(
            frame = rep(tms@frames$Id, tms@frames$NumScans),
            scan = sequence(tms@frames$NumScans),
            file = fl_idx)
        list(frames, indices)
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
#' @importFrom opentimsr OpenTIMS CloseTIMS query
#'
#' @noRd
.get_tims_columns <- function(x, columns) {
    res <- vector(mode = "list", length(x))
    nms <- names(x@fileNames)
    for (i in seq_len(length(nms))) {
        I <- which(x@indices[, "file"] == x@fileNames[i])
        tms <- OpenTIMS(nms[i])
        on.exit(opentimsr::CloseTIMS(tms))
        if (any(notin <- !columns %in% tms@all_columns))
            stop("Column(s) ",
                 paste0("'", columns[notin], "'", collapse = ", "),
                 " not available.", call. = FALSE)
        if (!length(sd <- setdiff(columns, c("frame", "scan"))))
            stop("At least one column value different from 'frame' and",
                 " 'scan' required")
        tmp <- query(tms, unique(x@indices[I, "frame"]), c("frame", "scan", sd))
        f <- factor(paste(tmp$frame, tmp$scan),
                    levels = paste(x@indices[I, "frame"], x@indices[I, "scan"]))
        if (length(sd) == 1)
            res[I] <- unname(split(tmp[, sd], f))
        else res[I] <- unname(split.data.frame(as.matrix(tmp[, sd]), f))
    }
    res
}

#' Extract columns from @frames given the @indices in `x`. The function takes
#' care of eventually duplicating values.
#'
#' @param x `MsBackendTimsTOF`
#'
#' @param columns `character` with the column names.
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

# can we assume that tms@all_coulmns is the same for all the TimsTOF? 
.TIMSTOF_COLUMNS <- c("mz", "intensity", "tof", "inv_ion_mobility")

.spectra_data <- function(x, columns = spectraVariables(x)) {
    if (!all(present <- columns %in% spectraVariables(x)))
        stop("Column(s) ", paste0("\"", columns[!present], "\"",
                                  collapse = ", "),
             " not available.", call. = FALSE)
    res <- vector(mode = "list", length = length(columns))
    names(res) <- columns
    core_cols <- columns[columns %in% names(Spectra:::.SPECTRA_DATA_COLUMNS)]
    frames_cols <- columns[columns %in% colnames(x@frames)]
    tims_cols <- columns[columns %in% .TIMSTOF_COLUMNS]
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
        res[tims_cols] <- lapply(tims_cols, function(col)
            NumericList(lapply(.get_tims_columns(x, tims_cols),
                               function(m) unname(m[, col])), compress = FALSE))
        core_cols <- setdiff(core_cols, tims_cols)
    }
    if ("dataStorage" %in% columns) {
        res[["dataStorage"]] <- dataStorage(x)
        core_cols <- core_cols[core_cols != "dataStorage"]
    }
    if (length(core_cols)) {
        res[core_cols] <- lapply(Spectra:::.SPECTRA_DATA_COLUMNS[core_cols],
                                 function(z, n) rep(as(NA, z), n), length(x))
    }
    if (length(res))
        as(res, "DataFrame")
    else NULL
}
