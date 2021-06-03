
# Calculates the light time correction for a given body - Mercury, Venus,
# Mars, Jupiter, Saturn, Uranus, Neptune, Pluto, Sun
light_time_correction <- function(con, jd, body)
{
  mag_earth_hel <- sqrt(earth_hel[1,1] * earth_hel[1,1] +
                        earth_hel[1,2] * earth_hel[1,2] +
                        earth_hel[1,3] * earth_hel[1,3])
  
  tau <- 0.0
  while(jd - tau < 1e-9) {
    
    body_df <- position_body_ssb(con, (jd-tau), body)
    earth_df <- position_earth_ssb(con, jd)
    sun_df <- position_body_ssb(con, jd, 'Sun')
    sun_df2 <- position_body_ssb(con, (jd-tau), 'Sun')
    
    body_pos_geo <- body_df - earth_df
    earth_pos_helio <- earth_df - sun_df
    body_pos_helio <- body_df - sun_df2
    
  }
}