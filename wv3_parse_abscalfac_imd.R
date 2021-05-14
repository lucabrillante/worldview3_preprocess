#read wv3 metadata
#
wv3_parse_abscalfac_imd <- function(path) {
  #path = path to .imd metadata file
  imd <- suppressWarnings(readLines(path))
  
  band_lines <- grep("BAND", imd)
  band_lines <- imd[band_lines[seq(1, length(band_lines), by = 2)]]
  band_lines <- sapply(strsplit(band_lines, "BAND_"), function(x) x[2])  
  
  abscalf_lines <- grep("absCalFactor", imd)
  abscalf_lines <- sapply(strsplit(imd[abscalf_lines], "Factor = "), function(x) x[2])
  abscalf_lines <- sapply(strsplit(abscalf_lines, ";"), function(x) x[1])
  abscalf_lines <- as.double(abscalf_lines)
  
  eff_bw_lines <- grep("effectiveBandwidth", imd)
  eff_bw_lines <- sapply(strsplit(imd[eff_bw_lines], "Bandwidth = "), function(x) x[2])
  eff_bw_lines <- sapply(strsplit(eff_bw_lines, ";"), function(x) x[1])
  eff_bw_lines <- as.double(eff_bw_lines)
  
  data.frame(spectral_band = band_lines, abscal_factor = abscalf_lines, 
             eff_bandwidth_imd = eff_bw_lines)
}