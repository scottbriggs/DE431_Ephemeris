
test_cases <- function()
{
  jd <- julian_day(1, 1, 1989)
  
  pos <- c(0.6616929, -0.6699028, -0.2904395)
  ap <- apparent_place_sun(jd)
  
  pos_p <- polar_angles(pos)
  ra <- deg2dms(pos_p[2]*RAD2HR)
  dec <- deg2dms(pos_p[3]*RAD2DEG)
  
  nut <- nutation_angles(jd)
  ep <- obliquity(jd, nut)
  
  ec <- equatorial2ecliptical(pos, ep[1])
  pos_p <- polar_angles(ec)
  
  ecliptic_lat <- deg2dms(pos_p[3]*RAD2DEG)
  ecliptic_long <- deg2dms(pos_p[2]*RAD2DEG)
  
  pos <- c(0.99992571, 0.01218922, 0.00001132)
  ec <- equatorial2ecliptical(pos, ep[1])
  pos_p <- polar_angles(pos)
  ecliptic_lat <- deg2dms(pos_p[3]*RAD2DEG)
  ecliptic_long <- deg2dms(pos_p[2]*RAD2DEG)
  
  pos <- c(0.99992571, 0.01218922, 0.00001132)
  
}
