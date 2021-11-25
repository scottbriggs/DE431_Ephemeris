
# Convert degrees, minutes, and seconds to decimal degrees
dms2deg <- function(deg, min, sec)
{
    sign <- 0
    
    if ( (deg<0) | (min<0) | (sec<0) ){
        sign <- -1
    }else {
        sign <- 1
    }
    
    return( sign * (abs(deg) + abs(min) /60 + abs(sec) / 3600))
}

# Convert decimal degrees to degrees, minutes, and seconds
deg2dms <- function(deg)
{
    x <- abs(deg)
    D <- as.integer(x)
    x <- (x - D) * 60.0
    M <- as.integer(x)
    S <- (x - M) * 60.0
    
    if (deg < 0.0) {
        if (D != 0) {
            D <- D * -1
        }else if (M != 0) {
            M <- M * -1
        }else {
            S <- S * -1
        }
    }
    
    vec <- c(D, M, S)
    
    return(vec)
}

hms2hr <- function(hr, min, sec)
{
  sign <- 0
  
  if ( (hr<0) | (hr<0) | (hr<0) ){
    sign <- -1
  }else {
    sign <- 1
  }
  
  return( sign * (abs(hr) + abs(min) /60 + abs(sec) / 3600))
}

delta_t <- function(year, month)
{
  delta_t <- 0.0
  
  y <- year + (month - 0.5) / 12
  
  if (year < -500) {
    u <- (year - 1820) / 100
    delta_t <- -20 + (32 * u * u)
  } else if (year >= -500 & year < 500) {
    u <- y/100
    delta_t <- 10583.6 + u*(-1014.41 + u*(33.78311 + u*(-5.952053 + 
                         u*(-0.1798452 + u*(0.022174192 + u*(0.0090316521))))))
  } else if (year >= 500 & year < 1600) {
    u = (y - 1000)/100
    delta_t <- 1574.2 + u*(-556.01 + u*(71.23472 + u*(0.319781 + 
                        u*(-0.8503463 + u*(-0.005050998 + u*(0.0083572073))))))
  } else if (year >= 1600 & year < 1700) {
    t <- y - 1600
    delta_t <- 120 + t*(-0.9808 + t*(-0.01532 + t*(1/7129)))
  } else if (year >= 1700 & year < 1800) {
    t <- y - 1700
    delta_t <- 8.83 + t*(0.1603 + t*(-0.0059285 + t*(0.00013336 +
                      t*(-1/1174000))))
  } else if (year >= 1800 & year < 1860) {
    t <- y - 1800
    delta_t <- 13.72 + t*(-0.332447 + t*(0.0068612 + t*(0.00411116 +
                       t*(-0.00037436 + t*(0.0000121272 + t*(-0.0000001699 +
                       t*(0.000000000875)))))))
  } else if (year >= 1860 & year < 1900) {
    t <- y - 1860
    delta_t <- 7.62 + t*(0.5737 + t*(-0.251754 + t*(0.01680668 +
                      t*(-0.0004473624 + t*(1/233174)))))
  } else if (year >= 1900 & year < 1920) {
    t <- y - 1900
    delta_t <- -2.79 + t*(1.494119 + t*(-0.0598939 + t*(0.0061966 +
                       t*(-0.000197))))
  } else if (year >= 1920 & year < 1941) {
    t <- y - 1920
    delta_t <- 21.20 + t*(0.84493 + t*(-0.076100 + t*(0.0020936)))
  } else if (year >= 1941 & year < 1961) {
    t <- y - 1950
    delta_t <- 29.07 + t*(0.407 + t*(-2/233)) + t*(1/2547)
  } else if (year >= 1961 & year < 1986) {
    t <- y - 1975
    delta_t <- 45.45 + t*(1.067 + t*(-1/260)) + t*(-1/718)
  } else if (year >= 1986 & year < 2005) {
    t <- y - 2000
    delta_t <- 63.86 + t*(0.3345 + t*(-0.060374 + t*(0.0017275 +
                      t*(0.000651814 + t*(0.00002373599)))))
  } else if (year >= 2005 & year < 2050) {
    t <- y - 2000
    delta_t <- 62.92 + t*(0.32217 + t*(0.005589))
  } else if (year >= 2050 & year < 2150) {
    t <- y - 2000
    delta_t <- -20 + 32 * ((y - 1829)/100) * ((y - 1829)/100) - 
                    0.5628 * (2150 - y)
  } else {
    u <- (year - 1820) / 100
    delta_t <- -20 + (32 * u * u)
  }
  
  return(delta_t)
}

# Calculates the julian day for both universal time and dynamical time by
# calculating delta t. The month, day, and year given are universal time.
jd_ut_dt <- function(month, day, year)
{
  jd_ut <- julian_day(month, day, year)
  deltat <- delta_t(year, month)
  jd_td <- jd_ut + deltat / 86400
  
  
  return(c(jd_ut, jd_td))
}
