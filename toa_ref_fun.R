toa_ref_fun <- function(L, E, des = sun_angle_des["des"], sun_angle = sun_angle_des["sun_angle"]) {
  (L*des**2*pi)/(E*cos(sun_angle*pi/180))
}