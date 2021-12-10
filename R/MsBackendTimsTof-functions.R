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
    frames <- cbind(tms@frames, dataStorage = file[fl_idx])
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

MsBackendTimsTof <- function() {
  new("MsBackendTimsTof")
}