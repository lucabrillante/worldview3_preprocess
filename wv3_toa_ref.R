wv3_toa_ref <- function(wv3_raster, 
                        band = c("P", "C", "B", "G", "Y", "R", "RE", "N1", "N2",
                                 "S1","S2", "S3","S4", "S5", "S6", "S7", "S8",
                                 "mult", "swir", "all"),
                        wv3_raster_bands = NULL,
                        path_to_imd = NULL, 
                        keep_band_names = TRUE, 
                        remove_zeros = TRUE, 
                        solar_irradiance_model = c("thuillier", "chkur", "wrc"), 
                        toa_rad = FALSE) {
  if (keep_band_names) wv3_raster_names <- names(wv3_raster)
  if (!toa_rad) {
    message("toa_rad = FALSE. Assuming to start with raw imagery and performing conversion to TOA radiance.\n")
    wv3_raster <- wv3_toa_rad(wv3_raster = wv3_raster, band = band, path_to_imd = path_to_imd,
                            keep_band_names =  keep_band_names, remove_zeros = remove_zeros, 
                            wv3_raster_bands = wv3_raster_bands)
  } else {
    stopifnot("wv3_raster must be of class SpatRaster from package terra and already converted to TOA radiance." = class(wv3_raster) == "SpatRaster")
  }
  
  tryCatch(solar_irradiance_model <- match.arg(solar_irradiance_model,
                                               c("thuillier", "chkur", "wrc")), 
                                               error = function(x) {
    stop("Solar irradiance model must be one of thuillier, chkur, wrc.\n")
  })
  
  irradiance <- wv3_irradiance(band)[, c("spectral_band", solar_irradiance_model)]
  irradiance <- irradiance[match(names(wv3_raster), irradiance$spectral_band),]
  sun_angle_des <- wv3_parse_time_sun(path_to_imd)
  
  toa_ref_fun_wrap <- function(L, E) toa_ref_fun(L, E, des = sun_angle_des["des"], sun_angle = sun_angle_des["sun_angle"])
  
  wv3_raster <- lapply(wv3_raster@ptr$names, function(x) wv3_raster[[x]])
  wv3_raster <- mapply(toa_ref_fun_wrap, wv3_raster, irradiance[[solar_irradiance_model]])
  wv3_raster <- rast(wv3_raster)
  
  if (keep_band_names) {
    names(wv3_raster) <- wv3_raster_names
  } else {
    names(wv3_raster) <- irradiance$spectral_band
  }
  
  wv3_raster
}
