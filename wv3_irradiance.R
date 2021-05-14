#Top of atmosphere reflectance

#to do we can make one single function for all tables

wv3_irradiance <- function(band = c("P", "C", "B", "G", "Y", "R", "RE", "N", 
                                    "N2", "S1", "S2", "S3","S4", "S5", "S6", "S7", 
                                    "S8", "mult", "swir", "all")) {
  band <- wv3_check_band(band)
  
  #this is table 3 of Radiometric Use of WorldView-3 Imagery, technical note 2016
  spectral_band <- c("P", "C", "B", "G", "Y", "R", "RE", "N", 
                     "N2", "S1", "S2", "S3","S4", "S5", "S6", "S7", 
                     "S8")
  thuillier <- c(1574.41, 1757.89, 2004.61, 1830.18, 1712.07, 1535.33, 1348.08, 1055.94,
                 858.77, 479.019, 263.797, 225.283, 197.552, 90.4178, 85.0642, 76.9507,
                 68.0988)
  chkur <- c(1578.28, 1743.9, 1974.53, 1858.1, 1748.87, 1550.58, 1303.4, 1063.92, 858.632, 
             478.873, 257.55, 221.448, 191.583, 86.5651, 82.0035, 74.7411, 66.3906)
  wrc <- c(1583.58, 1743.81, 1971.48, 1856.26, 1749.4, 1555.11, 1343.95, 1071.98, 863.296, 
           494.595, 261.494, 230.518, 196.766, 80.365, 74.7211, 69.043, 59.8224)
  
  table3 <- data.frame(spectral_band, thuillier, chkur, wrc)
  
  output <- table3[which(spectral_band %in% band), ]
  output[match(band, output$spectral_band), ]
}

#TO DO: include solar_irradiance_model option
#solar_irradiance_model = c("thuillier", "chkur", "wrc")
#thuillier being default, and also having "all" as an option