
# Calculate the position and velocity of the Earth with respect to the SSB

position_earth_ssb <- function(jd)
{
  # Get the position and velocity of the earth-moon barycenter and moon
  emb_pos_vel <- position_emb_ssb(jd)
  moon_pos_vel <- position_moon_geo(jd)
  
  # Calculate the position and velocity of the earth
  earth_pos_vel <- data.frame(matrix(0.0, nrow=3, ncol=2))
  temp <- 1 + EMRAT
  earth_pos_vel[1,1] <- emb_pos_vel[1,1] - (moon_pos_vel[1,1] / temp)
  earth_pos_vel[2,1] <- emb_pos_vel[2,1] - (moon_pos_vel[2,1] / temp)
  earth_pos_vel[3,1] <- emb_pos_vel[3,1] - (moon_pos_vel[3,1] / temp)
  earth_pos_vel[1,2] <- emb_pos_vel[1,2] - (moon_pos_vel[1,2] / temp)
  earth_pos_vel[2,2] <- emb_pos_vel[2,2] - (moon_pos_vel[2,2] / temp)
  earth_pos_vel[3,2] <- emb_pos_vel[3,2] - (moon_pos_vel[3,2] / temp)
  
  return (earth_pos_vel)
}