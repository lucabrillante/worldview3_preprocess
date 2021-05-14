#absolute radiometric correction of worldview-3 products and conversion to top-of-atmosphere
#spectral radiance

wv3_toa_rad <- function(gain, offset_value, dn, abscal_factor, eff_bandwidth) {
  gain * dn * (abscal_factor/eff_bandwidth) + offset_value
}

