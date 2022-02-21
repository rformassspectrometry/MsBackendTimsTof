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
    x@fileNames <- file
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
  if ("file" %in% colnames(x@indices) &&
      any(!x@indices[, "file"] %in% seq_len(length(x@fileNames))))
    msg <- c(msg, "Some file indices are not compatible with x@fileNames")
  if (any(!paste0(x@indices[, "frame"], x@indices[, "file"]) %in%
          paste0(x@frames$frameId, x@frames$file)))
    msg <- c(msg, "Some indices in x@indices are not compatible with x@frames")
  msg
}

#' @description
#'
#' Checks if the `character` `x` is compatible to be the `@fileNames` slot of a
#' `MsBackendTimsTof` object.
#'
#' @param x `character` with folder names.
#'
#' @noRd
.valid_fileNames <- function(x) {
  msg <- NULL
  if (anyNA(x))
    msg <- "'NA' values in fileNames are not allowed."
  msg <- c(msg, Spectra:::.valid_ms_backend_files_exist(unique(x)))
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

#' Read variables from the `OpenTIMS` object (`OpenTIMS(x)`) created from data
#' stored in folder `x`. At least one variable in `OpenTIMS(x)@all_columns`
#' different from `"frame"` and "`scan`" has to be provided via `columns`
#' parameter.
#'
#' @param x `character(1)` path to the '*.d' folder to read from.
#'
#' @param columns `character` with the names of the columns to extract.
#'
#' @param indices `matrix` of indices. It contains columns `"frame"` and
#' `"scan"` to select which rows of data to read from `OpenTIMS(x)`.
#'
#' @return `list` of `numeric` (if a single variable is read) or `list`
#' of `matrix` (otherwise). Each element of the list corresponds to certain
#' `"frame"` and `"scan"` indeces from `indeces`.
#'
#' @importFrom opentimsr OpenTIMS CloseTIMS query
#'
#' @noRd
# Maybe I didn't name this function very well. Maybe .read_tims_columns?
.read_frame_col <- function(x, columns, indices) { 
  tms <- OpenTIMS(x)
  on.exit(opentimsr::CloseTIMS(tms))
  if (any(!columns %in% tms@all_columns))
    stop("Invalid value for columns")
  if (!length(sd <- setdiff(columns, c("frame", "scan"))))
    stop("At least one column value different from 'frame' and 'scan' required")
  tmp <- query(tms, unique(indices[, "frame"]), c("frame", "scan", sd))
  f <- factor(paste(tmp$frame, tmp$scan),
              levels = paste(indices[, "frame"], indices[, "scan"]))
  if (length(sd) == 1)
    unname(split(tmp[, sd], f))
  else unname(split.data.frame(as.matrix(tmp[, sd]), f))
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
.get_frame_columns <- function(x, columns) {
  if (!all(columns %in% colnames(x@frames)))
    stop("Column(s) ", paste0("'", columns[!columns %in% colnames(x@frames)],
                              "'", collapse = ", "), " not available.",
         call. = FALSE)
  idx <- match(paste(x@indices[, "frame"], x@indices[, "file"]),
               paste(x@frames$frameId, x@frames$file))
  x@frames[idx, columns]
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

.spectra_data <- function(x, columns = spectraVariables(x)) {
  if (!all(present <- columns %in% spectraVariables(x)))
    stop("Column(s) ", paste0("\"", columns[!present], "\"", collapse = ", "),
         " not available.", call. = FALSE)
  res <- vector(mode = "list", length = length(columns))
  names(res) <- columns
  core_cols <- columns[columns %in% names(Spectra:::.SPECTRA_DATA_COLUMNS)]
  frames_cols <- columns[columns %in% colnames(x@frames)]
  if (length(frames_cols)) {
    res[frames_cols] <- .get_frame_columns(x, frames_cols)
    core_cols <- core_cols[!core_cols %in% frames_cols]
  }
  if ("mz" %in% columns) {
    res[["mz"]] <- mz(x)
    core_cols <- core_cols[core_cols != "mz"]
  }
  if ("intensity" %in% columns) {
    res[["intensity"]] <- intensity(x)
    core_cols <- core_cols[core_cols != "intensity"]
  }
  if ("scanIndex" %in% columns) {
    res[["scanIndex"]] <- x@indices[, "scan"]
    core_cols <- core_cols[core_cols != "scanIndex"]
  }
  if("dataStorage" %in% columns) {
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

