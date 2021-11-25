
# Julian Day on UT timescale
# Observer Longitude, +E, -W
# Observer latitude, +N, -S
# ra is a vector of three right ascension values for day-1, day, and day+1 * UT0
# dec is a vector of three declination values for day-1, day, and day+1 * UT0
# z0 is the zenith distance in degrees
# delta_t is the difference in seconds between UT and TD
rise_set_events <- function(jd_ut, obs_long, obs_lat, ra, dec, z0, deltat)
{
  # Calculate cosH0
  numerator <- cos(z0*DEG2RAD) - sin(obs_lat*DEG2RAD) * sin(dec[2])
  denom <- cos(obs_lat*DEG2RAD) * cos(dec[2])
  cosH0 <- numerator / denom
  
  t_rise <- 0
  t_set <- 0
  t_transit <- 0
  rise <- ""
  set <- ""
  
  # Check if body is circumpolar
  if (cosH0 < -1 & z0 >= 96){
    rise = "Circumpolar"
    set = "Circumpolar"
  }
  
  # Check if body never rises
  if (cosH0 > 1 & z0 >= 96){
    rise = "Never rises"
    set = "Never rises"
  }
  
  H0 <- acos(cosH0)
  H0 <- amodulo(H0, PI)
  
  # Calculate the sidereal time
  st <- sidereal_time(jd_ut)
  
  # Calculate first approximation to the transit time
  m0 <- (ra[2] - obs_long*DEG2RAD - st[2]*HR2RAD) / PI2
  m0 <- amodulo(m0, 1)
  
  # Calculate first approximation to the rise time
  m1 <- m0 - H0 / PI2
  m1 <- amodulo(m1, 1)
  
  # Calculate first approximation to the set time
  m2 <- m0 + H0 / PI2
  m2 <- amodulo(m2, 1)
  
  # Interpolate RA - transit
  theta0 <- st[2]*HR2RAD + 6.300388093 * m0
  n0 <- m0 + deltat/86400
  ra0 <- interpolate(ra, n0)
  
  #Update the hour angle
  st0 <- st[2]*HR2RAD + 6.300388093 * m0
  H_transit <- st0 + obs_long*DEG2RAD - ra0
  H_transit <- amodulo(H_transit, PI2)
  if (H_transit > PI) {H_transit <- H_transit - PI2}
  delta_m0 <- - H_transit / PI2
  m0 <- m0 + delta_m0
  
  
  # Interpolate RA and Dec - rise
  theta1 <- st[2]*HR2RAD + 6.300388093 * m1
  n1 <- m1 + deltat/86400
  ra1 <- interpolate(ra, n1)
  dec1 <- interpolate(dec, n1)
  st1 <- st[2]*HR2RAD + 6.300388093 * m1
  H_rise <- st1 + obs_long*DEG2RAD - ra1
  h1 <- asin(sin(obs_lat*DEG2RAD) * sin(dec1) 
             + cos(obs_lat*DEG2RAD) * cos(dec1) * cos(H_rise))
  delta_m1 <- (h1 - PI/2 + z0*DEG2RAD) / 
    (PI2 * cos(dec1) * cos(obs_lat*DEG2RAD) * sin(H_rise))
  m1 <- m1 + delta_m1

  # Interpolate RA and Dec - set
  theta2 <- st[2]*HR2RAD + 6.300388093 * m2
  n2 <- m2 + deltat/86400
  ra2 <- interpolate(ra, n2)
  dec2 <- interpolate(dec, n2)
  st2 <- st[2]*HR2RAD + 6.300388093 * m2
  H_set <- st2 + obs_long*DEG2RAD - ra2
  h2 <- asin(sin(obs_lat*DEG2RAD) * sin(dec2) 
             + cos(obs_lat*DEG2RAD) * cos(dec2) * cos(H_set))
  delta_m2 <- (h2 - PI/2 + z0*DEG2RAD) / 
    (PI2 * cos(dec2) * cos(obs_lat*DEG2RAD) * sin(H_set))
  m2 <- m2 + delta_m2
  
  return(c(rise, set, m0, m1, m2))
  
}