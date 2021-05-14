wv3_check_band <- function(band = c("P", "C", "B", "G", "Y", "R", "RE", "N", 
                                    "N2", "S1", "S2", "S3","S4", "S5", "S6", "S7", 
                                    "S8", "mult", "swir", "all"), 
                           type = c("band", "wv3_raster_band")) {
  
  type <- match.arg(type)

  tryCatch(band <- match.arg(band, several.ok = TRUE), error = function(x) {
    if (type == "band") {
      stop("band argument should be one of: P for pan, C for coastal, B for blue, G for green, Y for yellow, R for red,RE for red_edge, N for nir1, N2 for nir2, S1 for swir1, S2 swir2, S3 for swir3, S4 for swir4, S5 for swir5, S6 for swir6, S7 for swir7, S8 for swir8, or mult for vis-nir bands, swir for swir bands, all for all bands.\n")
    }
    if (type == "wv3_raster_band") {
      stop("wv3_raster_band argument should be one of: P for pan, C for coastal, B for blue, G for green, Y for yellow, R for red,RE for red_edge, N for nir1, N2 for nir2, S1 for swir1, S2 swir2, S3 for swir3, S4 for swir4, S5 for swir5, S6 for swir6, S7 for swir7, S8 for swir8, or mult for vis-nir bands, swir for swir bands, all for all bands.\n")
    }
  })
  
  if (length(band) > 1 & (sum(band  %in% "mult") >= 1 | sum(band  %in% "swir") >= 1 |
                          sum(band  %in% "all") >= 1)) stop("Combining individuals bands (e.g. P, C, B etc.) with aggregate options (mult, swir, all) is not currently supported.")
  
  if (length(band) != length(unique(band))) {
    if (type == "band") {
      stop("Check the band argument, it is not unique.")
    } else {
      stop("Check the wv3_raster_band argument, it is not unique.")
    }
  }
    
  if (band[1] == "mult") band <- c("C", "B", "G", "Y", "R", "RE", "N", "N2")
  if (band[1] == "swir") band <- c("S1", "S2", "S3","S4", "S5", "S6", "S7", "S8")
  if (band[1] == "all")  band <- c("P", "C", "B", "G", "Y", "R", "RE", "N", "N2", 
                                   "S1", "S2", "S3","S4", "S5", "S6", "S7", "S8")
  
  band
}
