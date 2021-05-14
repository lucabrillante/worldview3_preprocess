#' wv3_center_bandwidth
#' 
#' @description Look-up table 1 in the WV3 reference to find the center wavelength and the effective bandwidth
#'  in the WV3 report.
#'
#' @param band string, specifying the band or set of bands to work on. 
#' Values can be "P" panchromatic, "C" coastal , "B" blue, "G" green, 
#' "Y" yellow, "R" red, "RE" red-edge, "N" nir, "N2" nir2,
#' "S1" to "S8" for swir 1 to swir 8, "S2", "S3","S4", "S5", "S6", "S7", "S8",
#' "mult" includes all bands except P and swir bands
#' "swir", includes all swir bands, 
#' "all" are all bands.)  
#'
#' @return a dataframe with the corresponding values of center wavelength and
#' effective bandwidth for the selected bands.
#' 
wv3_center_bandwidth <- function(band = c("P", "C", "B", "G", "Y", "R", "RE", "N", 
                                             "N2", "S1", "S2", "S3","S4", "S5", "S6", "S7", 
                                             "S8", "mult", "swir", "all")) {
  band <- wv3_check_band(band)
  
  #this is table 1 of Radiometric Use of WorldView-3 Imagery, technical note 2016
  spectral_band <- c("P", "C", "B", "G", "Y", "R", "RE", "N", 
                     "N2", "S1", "S2", "S3","S4", "S5", "S6", "S7", 
                     "S8")
  center_wv <- c(649.4, 427.4, 481.9, 547.1, 604.3, 660.1, 722.7, 824.0, 913.6,
                 1209.1, 1571.6, 1661.1, 1729.5, 2163.7, 2202.2, 2259.3, 2329.2)
  eff_bandwidth <- c(0.2896, 0.0405, 0.0540, 0.0618, 0.0381, 0.0585, 0.0387,
                          0.1004, 0.0889, 0.0330, 0.0397, 0.0373, 0.0416, 0.0389,
                          0.0409, 0.0476, 0.0679)
  table1 <- data.frame(spectral_band, center_wv, eff_bandwidth)
  
  output <- table1[which(spectral_band %in% band), ]
  output[match(band, output$spectral_band), ]
}
