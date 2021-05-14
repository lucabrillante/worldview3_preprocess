#This function still need work. 
wv3_surf_ref <- function(wv3_raster, 
                         path_to_imd = NULL, 
                         remove_zeros = TRUE, 
                         dodn_band = 2, 
                         do_bands = "mult", 
                         dos_adjust = 0.01) {
  require(terra)
  require(raster)
  require(satellite)
  
  message("DOS methods should be applied to VIS-NIR only bands. Provide_the appropriate raster. As default uses WV3 Multispectral image (8 bands)")
  message("DOS methods need raw imagery (Digital Numbers data)")
  if (is.character(wv3_raster)) wv3_raster <- rast(wv3_raster)
  stopifnot("wv3_raster must be of class SpatRaster from package terra" = class(wv3_raster) == "SpatRaster")
  if (is.null(path_to_imd)) stop("path_to_imd is required.")

  do_bands <- wv3_check_band(do_bands)
  
  # TO DO: improve the way to ask for the band used to calculate the do, currently using the number identifying the band in the stack
  wv3_b <- wv3_raster[[dodn_band]]
  #these were developed for landsat 8 bit data, wv3 acquires 11bit data in vis-nir and 14-bit in swir, scale
  wv3_b <- (wv3_b * 255)/max(values(wv3_b))
  if (remove_zeros) {
  #default is FALSE as the satellite::calcDODN function only uses values > 0
    message("Assigning NA to 0 values in the image")
    wv3_raster[wv3_raster == 0] <- NA
  }
  shv <- calcDODN(raster(wv3_b))
  #this is equivalent to RStoolbox::estimateHaze(raster(wv3_b), maxSlope = FALSE)
  #RStoolbox::estimateHaze(raster(wv3_b), maxSlope = FALSE)
  #TO DO: this should be applied to visible bands only by default. Improve the function to include some defense mechanism on this
  radm_wv3 <- wv3_gain_offset(do_bands)$gain
  rada_wv3 <- wv3_gain_offset(do_bands)$offset
  szen_wv3 <- wv3_parse_time_sun(imd_path)["sun_angle"]
  esun_wv3 <- wv3_irradiance(do_bands)$thuillier
  band_wls_wv3 <- wv3_center_bandwidth(do_bands)
  band_wls_wv3 <- within(band_wls_wv3, {
    lmax <- center_wv + eff_bandwidth
    lmin <- center_wv - eff_bandwidth
    rm(spectral_band, center_wv, eff_bandwidth)
  })
  
  #TO DO: this function currently has only implemented "DOS2" (the "COSTZ" Chavez 1996 does not works)
  #check this https://github.com/environmentalinformatics-marburg/satellite/issues/38 to be updated 
  #we could implement radiocorr from the landsat package. Need to check what is the 
  #satellite zenith angle though for world view 3.
  #This implements equation 7 in the paper of Song et al., 2001 http://www.sciencedirect.com/science/article/pii/S0034425700001693
  #
  #TO DO!!!: currently using this function however this does not consider that to convert
  #DN to radiance in world view 3 you need to include abscal factor and effective bandwidth check this paper. 
  ##Modify line 217 of calcPathRadDos() when calculating lpmin using equation at page five of the radiometric coefficient.
  # http://www.sciencedirect.com/science/article/pii/0034425788900193
  #and the equation 
  img_path_rad <- calcPathRadDOS(shv, 
                                 bnbr = dodn_band, 
                                 band_wls = band_wls_wv3, 
                                 radm = radm_wv3,
                                 rada = rada_wv3,
                                 szen = szen_wv3, 
                                 esun = esun_wv3,
                                 model = "DOS2", 
                                 scat_coef = assign_scat_coef(shv),
                                 dos_adjust = dos_adjust)
  
  wv3_raster_rad <- wv3_toa_rad(raster_path,
                                band = do_bands,
                                path_to_imd = imd_path,
                                wv3_raster_bands = do_bands, 
                                remove_zeros = remove_zeros)
  
  
  #sum(na.omit(values(wv3_raster_rad$B)) < 0)
  #TO DO, insert a message of error when readLines the imd file tryCatch(...)
  
  #This implements equation 6 of Song with tv = 1, tz = 1 and Edown = 0
  calcAtmosCorr(raster(wv3_raster_rad$B), img_path_rad[2], 
                esun = wv3_irradiance("B")$thuillier,
                szen = szen_wv3,
                model = "DOS2")
  
}


