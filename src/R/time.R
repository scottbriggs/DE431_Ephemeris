
# Convert decimal hours to hours, minutes, and seconds
HHMMSS <- function (HHdd)
{
    intpart1 <- trunc(HHdd)
    fracpart1 <- HHdd - intpart1
    hh <- intpart1
    temp <- fracpart1 * 60
    mm <- trunc(temp)
    ss <- trunc((temp - mm) * 60)
    
    return(c(hh, mm, ss))
}

# Converts hours, minutes, and seconds to decimal hours
HHdd <- function (hr, min, sec)
{
    decimal_hours <- hr + min  /60 + sec / 3600
    return(decimal_hours)
}

# Calculate the TDB julian date corresponding to the epoch of observation
julian_date_TDB <- function(julian_date)
{
    t_prime <- (julian_date - 2451545.0) / 36525
    m <- (357.528 + 35999.050 * t_prime) * 2 * 3.14159265 / 360
    s <- 0.001658 * sin(m + 0.01671 * sin(m))
    t <- t_prime + s / 86400
    return(t)
}

