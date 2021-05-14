#Convert World View 3 imagery to surface reflectance
wv3_gain_offset <- function(band = c("P", "C", "B", "G", "Y", "R", "RE", "N", 
                                     "N2", "S1", "S2", "S3","S4", "S5", "S6", "S7", 
                                     "S8", "mult", "swir", "all")) {
  band <- wv3_check_band(band)
  
  #this is table 2 of Radiometric Use of WorldView-3 Imagery, technical note 2016
  spectral_band <- c("P", "C", "B", "G", "Y", "R", "RE", "N", 
                     "N2", "S1", "S2", "S3","S4", "S5", "S6", "S7", 
                     "S8")
  
  gain <- c(0.923, 0.863, 0.905, 0.907, 0.938, 0.945, 0.980, 0.982, 0.954, 1.160, 1.184,
            1.173, 1.187, 1.286, 1.336, 1.340, 1.392)
  offset <- c(-1.7, -7.154, -4.189, -3.287, -1.816, -1.350, -2.617, -3.752, -1.507,
              -4.479, -2.248, -1.806, -1.507, -0.622, -0.605, -0.423, -0.302)

  # # #https://dg-cms-uploads-production.s3.amazonaws.com/uploads/document/file/209/ABSRADCAL_FLEET_2016v0_Rel20170606.pdf             
  # gain <- c(0.950, 0.905, 0.940, 0.938, 0.962, 0.964, 1, 0.961,0.978,1.2,1.227,1.199,
  #           1.196, 1.262, 1.314, 1.346, 1.376)
  # offset <- c(-3.629, -8.604, -5.809, -4.996, -3.649, -3.021, -4.521, -5.522, -2.992,
  #             -5.546, -2.6, -2.309, -1.676, -0.705, -0.669, -0.512, -0.372)
  table2 <- data.frame(spectral_band, gain, offset)
  
  output <- table2[which(spectral_band %in% band), ]
  output[match(band, output$spectral_band), ]
}
