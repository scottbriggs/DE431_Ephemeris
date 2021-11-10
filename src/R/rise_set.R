
sin_alt <- function(hour_angle, obs_lat, dec)
{
  sinalt <- cos(obs_lat) * cos(dec) * cos(hour_angle) + sin(obs_lat) * sin(dec)
  
  return(sinalt)
}

