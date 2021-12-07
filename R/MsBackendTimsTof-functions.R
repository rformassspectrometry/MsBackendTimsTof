.header <- function(x, file = character()) {
  tms <- OpenTIMS(file)
  x@frames <- tms@frames
  x@indexes <- cbind(
    frame_index = rep(tms@frames$Id, tms@frames$NumScans),
    scan_index = unlist(lapply(tms@frames$NumScans, seq_len)),
    spectrum_index = seq_len(sum(tms@frames$NumScans)))
  #should also a column with the file name be added? Also in view of reading 
  #multiple files and to get the dataStorage?
  x
}

MsBackendTimsTof <- function() {
  new("MsBackendTimsTof")
}