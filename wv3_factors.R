wv3_factors <- function(band = c("P", "C", "B", "G", "Y", "R", "RE", "N", 
                                 "N2", "S1", "S2", "S3","S4", "S5", "S6", "S7", 
                                 "S8", "mult", "swir", "all"), path = NULL) {
  require(dplyr)
  band_call <- band #this is for the message at line 22
  band <-  wv3_check_band(band)
  cbw  <-  wv3_center_bandwidth(band)
  gof  <-  wv3_gain_offset(band)
  irr  <-  wv3_irradiance(band)
  
  cbw_gof <- left_join(cbw, gof, by = "spectral_band")
  cbw_gof_irr <- left_join(cbw_gof, irr, by = "spectral_band")
  
  if (is.null(path)) {
    message("For including abscal factor please provide path to imd file\n")
    return(cbw_gof_irr)
  } else {
    imd <- wv3_parse_abscalfac_imd(path)
    imd <- imd[imd$spectral_band %in% band,]
    cbw_gof_irr_imd <- left_join(cbw_gof_irr, imd, by = "spectral_band")
    missing_bands <- cbw_gof_irr_imd[is.na(cbw_gof_irr_imd$abscal_factor) & 
                                   is.na(cbw_gof_irr_imd$eff_bandwidth_imd), "spectral_band"]
    if (length(missing_bands) == nrow(cbw_gof_irr_imd)) stop("Specified bands are not contained in the provided .imd file!\n")
    if (length(missing_bands) > 0) message(paste("band =", paste(band_call, collapse = ", "),
                                          "was called, but the .imd file does not contain", 
                                          paste(missing_bands, collapse = ", "),
                                          "spectral bands. Returning output for available bands.\n",
                                          sep = " "))
    return(na.omit(cbw_gof_irr_imd))
  }
}
