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


 
# basically this is the same as Spectra:::.valid_spectra_data_required_columns. 
# Maybe it is possible to use directly that one or maybe then the name is
# misleading?
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
      any(!x@indices[, "file"] %in% seq_along(length(x@fileNames)))) # or unique(x@fileNames) if we don't want to have the same file twice
    msg <- c(msg, "Some file indices are not valid")
  # if (any(!paste0(x@indices[, "frame"], x@indices[, "file"]) %in% 
  #         paste0(x@frames$Id, x@frames$file)))
  #   msg <- c(msg, "Some indices in x@indices are not compatible with x@frame")
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
  msg <- character(0)
  if (anyNA(x))
    msg <- "'NA' values in fileNames are not allowed."
  msg <- c(msg, Spectra:::.valid_ms_backend_files_exist(unique(x)))
  msg
}


MsBackendTimsTof <- function() {
  new("MsBackendTimsTof")
}