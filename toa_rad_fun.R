#function page 5 of Radiometric Use of WorldView-3 Imagery, technical note 2016
toa_rad_fun <- function(dn, gain, abscal_factor, eff_bandwidth, offset) {
  gain * dn * (abscal_factor/eff_bandwidth) + offset
}