
# Convert decimal hours to hours, minutes, and seconds
HHMMSS <- function (HHdd)
{
    intpart1 <- trunc(HHdd)
    fracpart1 <- HHdd - intpart1
    hh = intpart1
    temp = fracpart1 * 60
    mm <- trunc(temp)
    ss = trunc((temp - mm) * 60)
    
    return(c(hh, mm, ss))
}

# Converts hours, minutes, and seconds to decimal hours
HHdd <- function (hr, min, sec)
{
    decimal_hours <- hr + min  /60 + sec / 3600
    return(decimal_hours)
}
