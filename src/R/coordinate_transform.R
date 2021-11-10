# Calculate the observer position vector using the geodetic latitude (phi),
# geodetic longitude (lambda), and the height above sea level
# Phi is measured positive north of the equator and negative south of the equator in degrees
# Lambda is positive for east longitudes and negative for west longitudes in degrees
# Height is in meters
observer_position_vector <- function(lat, long, height)
{
  f <- (1-FLAT)*(1-FLAT)
  coslat <- cos(lat*DEG2RAD)
  cos2lat <- coslat * coslat
  sinlat <- sin(lat*DEG2RAD)
  sin2lat <- sinlat * sinlat
  C <- 1/sqrt(cos2lat + f * sin2lat)
  S <- f * C
  
  pos_obs <- c(0.0, 0.0, 0.0)
  pos_obs[1] <- (EARTHRADAU * C + height) * coslat * cos(long*DEG2RAD)
  pos_obs[2] <- (EARTHRADAU * C + height) * coslat * sin(long*DEG2RAD)
  pos_obs[3] <- (EARTHRADAU * S + height) * sinlat
  
  return(pos_obs)
}

# Convert a position vector (x, y, z) to polar angles (r, phi, theta)
polar_angles <- function(pos)
{
  rho_sqr <- pos[1] * pos[1] + pos[2] * pos[2]
  m_r <- sqrt(rho_sqr + pos[3] * pos[3])
  m_phi <- 0.0
  
  if (pos[1] == 0.0 & pos[2] == 0.0) {
    m_phi <- 0.0
  } else {
    m_phi <- atan2(pos[2], pos[1])
  }
  
  if (m_phi < 0.0) {m_phi <- m_phi + PI2}
  
  rho <- sqrt(rho_sqr)
  m_theta <- 0.0
  
  if (pos[3] == 0.0 & rho == 0.0) {
    m_theta <- 0.0
  } else {
    m_theta <- atan2(pos[3], rho)
  }

  return(c(m_r, m_phi, m_theta))
}

# Convert equatorial position vector to ecliptical position vector
# Obliquity is in radians
equatorial2ecliptical <- function(pos, obliquity)
{
  pos1 <- rotation_matrix(1, obliquity) %*% pos
  
  return(pos1)
}

# Convert ecliptical position vector to equatorial position vector
# Obliquity is in radians
ecliptical2equatorial <- function(pos, obliquity)
{
  pos1 <- rotation_matrix(1, -obliquity) %*% pos
  
  return(pos1)
}

# Convert equatorial coordinates (hour angle, declination) in radians to
# horizon coordinates (azimuth, altitude) in radians.
equatorial2horizon <- function(hour_angle, dec, obs_lat)
{
  vec1 <- c(hour_angle, dec, 1)
  vec2 <- rotation_matrix(2, (PI/2 - obs_lat)) %*% vec1
  
  azimuth <- vec2[1]
  altitude <- vec2[2]
  
  return(c(azimuth, altitude))
}

horizon2equatorial <- function(azimuth, altitude, obs_lat)
{
  vec1 <- c(azimuth, altitude, 1)
  vec2 <- rotation_matrix(2, -(PI/2 - obs_lat)) %*% vec1
  
  hour_angle <- vec2[1]
  dec <- vec2[2]
  
  return(c(hour_angle, dec))
}
