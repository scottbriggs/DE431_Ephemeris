
# Convert decimal hours to hours, minutes, and seconds
HHMMSS <- function (HHdd)
{
    intpart1 <- trunc(HHdd)
    fracpart1 <- HHdd - intpart1
    hh <- intpart1
    temp <- fracpart1 * 60
    mm <- trunc(temp)
    ss <- (temp - mm) * 60
    
    vec1 <- data.frame(matrix(0.0, nrow=3, ncol=1))
    vec1[1] = hh
    vec1[2] = mm
    vec1[3] = ss
    
    return(vec1)
}

# Converts hours, minutes, and seconds to decimal hours
HHdd <- function (hr, min, sec)
{
    decimal_hours <- hr + min  /60 + sec / 3600
    return(decimal_hours)
}

# Convert radians to degrees
rad2deg <- function(rad) {
    return(rad * 180 / 3.14159265)
}

# Convert degrees to radians
deg2rad <- function(deg){
    return(deg * 3.14159265 / 180)
}

# Convert decimal degrees to degrees, arcminutes, and arcseconds
DDMMSS <- function (DDdd)
{
    intpart1 <- trunc(DDdd)
    fracpart1 <- DDdd - intpart1
    dd <- intpart1
    temp <- fracpart1 * 60
    arc_mm <- trunc(temp)
    arc_ss <- (temp - arc_mm) * 60
    
    vec1 <- data.frame(matrix(0.0, nrow=3, ncol=1))
    vec1[1] = dd
    vec1[2] = arc_mm
    vec1[3] = arc_ss
    
    return(vec1)
}

DDdd <- function(dd, arc_min, arc_sec)
{
    decimal_degreess <- dd + arc_min  /60 + arc_sec / 3600
    return(decimal_degreess)
}




