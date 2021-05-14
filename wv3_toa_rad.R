#absolute radiometric correction of worldview-3 products and conversion to top-of-atmosphere
#spectral radiance

wv3_toa_rad <- function(wv3_raster, 
                        band = c("P", "C", "B", "G", "Y", "R", "RE", "N1", "N2",
                                 "S1","S2", "S3","S4", "S5", "S6", "S7", "S8",
                                 "mult", "swir", "all"),
                        wv3_raster_bands = NULL,
                        path_to_imd = NULL, 
                        keep_band_names = FALSE, 
                        remove_zeros = TRUE) {
  
  require(terra)
  if (is.character(wv3_raster)) wv3_raster <- rast(wv3_raster)
  stopifnot("wv3_raster must be of class SpatRaster from package terra" = class(wv3_raster) == "SpatRaster")
  if (is.null(path_to_imd)) stop("path_to_imd is required.")
  if (keep_band_names) wv3_raster_names <- names(wv3_raster)
  band <- wv3_check_band(band)
  wv3_raster_bands <- wv3_check_band(wv3_raster_bands, "wv3_raster_band")
  factor_df <- wv3_factors(band, path_to_imd)
  
  if (is.null(wv3_raster_bands)) {
    message(paste0("Argument wv3_raster_bands is NULL. Assuming order of bands in ",
                   "wv3_raster is == to band argument: ", 
                   paste(factor_df$spectral_band, collapse = ", "), 
                   ". Double check it!\n"))
    names(wv3_raster) <- factor_df$spectral_band
  } else {
    if (sum(!wv3_raster_bands %in% band) >= 1) { 
      stop(paste("wv3_raster_bands:", 
                 paste(wv3_raster_bands[!wv3_raster_bands %in% band], collapse = ", "), 
                 "are not in band argument.\n"))
    } 
    if (length(names(wv3_raster)) != length(wv3_raster_bands)) {
        stop("Number of bands provided in wv3_raster_bands < raster layers in wv3_raster.\n")
      } else {
      names(wv3_raster) <- wv3_raster_bands
    }
  }
  
  if (remove_zeros) {
    message("Assigning NA to 0 values in the image")
    wv3_raster[wv3_raster == 0] <- NA
  }
  
  factor_df <- factor_df[match(names(wv3_raster), factor_df$spectral_band),]
  #convert this to a list of SpatRaster to iterate through with mapply
  wv3_raster <- lapply(wv3_raster@ptr$names, function(x) wv3_raster[[x]])
  wv3_raster <- mapply(toa_rad_fun, wv3_raster, factor_df$gain, factor_df$abscal_factor,
                    factor_df$eff_bandwidth, factor_df$offset)
  wv3_raster <- rast(wv3_raster)
  if (keep_band_names) {
    names(wv3_raster) <- wv3_raster_names
  } else {
    names(wv3_raster) <- factor_df$spectral_band
  }
  wv3_raster
}
