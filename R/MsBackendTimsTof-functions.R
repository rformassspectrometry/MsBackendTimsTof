#' Initialize the MsBackendTimsTOF object `x`
#'
#' @author Andrea Vicini, Johannes Rainer
#'
#' @importFrom MsCoreUtils rbindFill
#'
#' @return initialized MsBackendTimsTOF object
#'
#' @noRd
.initialize <- function(x, file = character(), BPPARAM = bpparam()) {
  L <- bplapply(seq_len(length(file)), function(fl_idx) {
    tms <- OpenTIMS(file[fl_idx])
    frames <- cbind(tms@frames, file = fl_idx)
    indices <- cbind(
      frame = rep(tms@frames$Id, tms@frames$NumScans),
      scan = unlist(lapply(tms@frames$NumScans, seq_len)),
      file = fl_idx)
    list(frames, indices)
  }, BPPARAM = BPPARAM)
  x@frames <- do.call(rbindFill, lapply(L, "[[", 1))
  x@indices <- do.call(rbind, lapply(L, "[[", 2))
  x@fileNames <- file
  x
}

#' @description
#'
#' Check if a matrix/data.frame has all required columns.
#'
#' @noRd
#'
#' @param x matrix/data.frame
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
#' Check the frames slot of the MsBakendTimsTof object.
#'
#' @noRd
#'
#' @param x frames data.frame
.valid_frames <- function(x) {
  .valid_required_columns(x, c("Id", "file"))
}

#' @description
#'
#' Check the indices slot of the MsBakendTimsTof object.
#'
#' @noRd
#'
#' @param x MsBakendTimsTof object
.valid_indices <- function(x) {
  msg <- .valid_required_columns(x@indices, c("frame", "file"))
  if ("file" %in% colnames(x@indices) &&
      any(!x@indices[, "file"] %in% seq_along(length(x@fileNames))))
    msg <- c(msg, "Some file indices are not valid")
  if (any(!paste0(x@indices[, "frame"], x@indices[, "file"]) %in%
          paste0(x@frames$Id, x@frames$file)))
    msg <- c(msg, "Some indices in x@indices are not compatible with x@frames")
  msg
}

#' @description
#'
#' Check the fileNames slot of the MsBakendTimsTof object.
#'
#' @noRd
#'
#' @param x character with file names
.valid_fileNames <- function(x) {
  msg <- NULL
  if (anyNA(x))
    msg <- "'NA' values in fileNames are not allowed."
  msg <- c(msg, Spectra:::.valid_ms_backend_files_exist(unique(x)))
  msg
}


MsBackendTimsTof <- function() {
  new("MsBackendTimsTof")
}

#' Read peaks from a single .d folder.
#'
#' @param x `character(1)` indicating the file to read from.
#'
#' @noRd
# .timstof_peaks <- function(x) {
#   tms <- OpenTIMS(x)
#   tmp <- query(tms, tms@frames$Id, c("frame", "scan", "mz", "intensity"))
#   # the result below should be a list of matrices (and not data.frames), right?
#   unname(split.data.frame(as.matrix(tmp[, c("mz", "intensity")]), 
#                           factor(paste(tmp$frame, tmp$scan))))
# }

#' Read columns (mz, intensity or both) from a single file
#' 
#' @param x `character(1)` indicating the file to read from.
#'
#' @noRd
.read_frame_col <- function(x, columns) {
  tms <- OpenTIMS(x)
  if (any(!columns %in% tms@all_columns))
    stop("Invalid value for columns")
  if (!length(sd <-setdiff(columns, c("frame", "scan"))))
    stop("At least one column value different from 'frame' and 'scan' required")
  tmp <- query(tms, tms@frames$Id, c("frame", "scan", sd))
  f <- factor(paste(tmp$frame, tmp$scan))
  if (length(sd) == 1)
    unname(split(tmp[, sd], f))
  else unname(split.data.frame(as.matrix(tmp[, sd]), f))
}
