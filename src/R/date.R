
# Calculates the Julian Day given the month, day, and year. The algorithm works
# for any date in the common era (CE) or before the common era (BCE).
# The Julian Day is calculated for a calendar date at 12 noon, which means that
# the day is an integer, i.e., there is no fractional day allowed.
julianDay <- function(month, day, year)
{
    # Calculates the julian day for a calendar date at 12 noon. This means that 
    #the day is an integer

    # Gregorian calendar adopted October 15, 1582
    IGREG <- (15 + 31 * (10 + 12 * 1582))
    jm <- 0
    
    jy <- year
    
    if (jy == 0) { print("There is no year zero")}
    
    # This code causes the julian day to be in error for years
    # less than zero
    #if (jy < 0) { jy <- jy + 1}
    
    if (month > 2)
    { 
        jm <- month + 1
    } else {
        jy <- jy - 1
        jm <- month + 13
    }
    
    julday <- floor(365.25 * jy) + floor(30.6001 * jm) + day + 1720995
    
    if (day + 31 * (month + 12 * year) >= IGREG)
    {
        ja <- trunc(0.01 * jy)
        julday <- julday + 2 - ja + trunc(0.25 * ja)
    }
    
    return (julday)
}

# Calculates the Julian Day given the month, day, and year. The algorithm works
# for any date in the common era (CE) or before the common era (BCE).
# The Julian Day is calculated for a calendar date that allows for a decimal
# day.
julianDayFrac <- function(month, day, year)
{
    # Day is a decimal 
    int_day <- trunc(day)
    frac_day <- day - int_day
    
    julday <- julianDay(month, int_day, year)
    
    if (julday >= 0){
        julday <- julday + frac_day - 0.5
    } else {
        julday <- julday - frac_day + 0.5    
        }
    
    return(julday)
}

calendarDate1 <- function(julday)
{
    julday <- -57
    IGREG <- 2299161
    jalpha <- 0
    ja <- 0
    jb <- 0
    jc <- 0
    jd <- 0
    je <- 0

    if (julday >= IGREG) {
        jalpha <- trunc((julday - 1867216 - 0.25) / 36524.25)
        ja <- julday + 1 + jalpha - trunc(0.25 * jalpha)
    } else if (julday < 0) {
        ja <- julday + 36525 * (1 - julday / 36525)
    } else {ja <- julday}
    
    jb <- ja + 1524
    
    jc <- trunc(6680 + (jb - 2439870 - 122.1) / 365.25)
    
    jd <- trunc(365 * jc + (0.25 * jc))
    
    je <- trunc((jb - jd) / 30.6001)
    
    day <- jb -jd - trunc(30.6001 * je)
    
    month <- je - 1
    
    if (month > 12) {month = month - 12}
    
    year <- jc - 4715
    
    if (month > 2) {year <- year - 1}
    
    # if (year <= 0) {year <- year - 1}
    
    if (julday < 0) {year <- year - 100 * (1 - julday / 36525)}
}

# Calculate the calendar date given a julian day number.
# Works for julian and calendar dates from January 1, -4712.
calendarDate <- function(jd)
{
    jd <- jd + 0.50
    
    jd_int <- trunc(jd)
    jd_frac <- jd - jd_int
    
    a <- 0
    
    if (jd_int < 2299161)
    {
        a <- jd_int
    } else
    {
        alpha <- trunc((jd_int - 1867216.25) / 36524.25)
        a <-jd_int + 1 + alpha - trunc(alpha / 4)
    }
    
    b <- a + 1524
    c <- trunc((b - 122.1) / 365.25)
    d <- trunc(365.25 * c)
    e <- trunc((b - d) / 30.6001)
    
    day <- b - d - trunc(30.6001 * e) + jd_frac
    month <- 0
    
    if (e < 14)
    {
        month <- e - 1
    }else if (e == 14 | e == 15)
    {
        month <- e - 13
    }
    
    year <- 0
    
    if (month > 2)
    {
        year <- c - 4716
    }else if (month == 1 | month == 2)
    {
        year <- c - 4715
    }
    
    c(year, month, day)
}

# Determines the day of the week given a calendar date. Works for Julian and 
# Gregorian calendars.
dayOfWeek <- function(year, month, day)
{
    d <- trunc(day)
    jd <- julianDay(year, month, d)
    
    dayNum <- (jd + 1.5) %% 7
    
    dayStr <- ""
    
    if (dayNum == 0) {
        dayStr <- "Sunday"
    } else if (dayNum == 1){
        dayStr <- "Monday"
    } else if (dayNum == 2){
        dayStr <- "Tuesday"
    } else if (dayNum == 3){
        dayStr <- "Wednesday"
    } else if (dayNum == 4){
        dayStr <- "Thursday"
    } else if (dayNum == 5){
        dayStr <- "Friday"
    } else {dayStr = "Saturday"}
    
    return(dayStr)
}

# Calculates the date of Christian Easter
dateOfEaster <- function(year)
{
    month <- 0
    day <- 0
    
    # Julian Calendar
    if (year < 1583)
    {
        a <- year %% 4
        b <- year %% 7
        c <- year %% 19
        d <- (19 * c + 15) %% 30
        e <- (2 * a + 4 * b - d + 34) %% 7
        temp <- d + e + 114
        month <- trunc(temp/31)
        day <- (temp %% 31) + 1
        # Gregorian Calendar
    } else {
        a <- year %% 19
        b <- trunc(year / 100)
        c <- year %% 100
        d <- trunc(b / 4)
        e <- b %% 4
        f <- trunc((b+8) / 25)
        g <- trunc((b - f + 1) / 3)
        h <- (19 * a + b - d - g + 15) %% 30
        i <- trunc(c / 4)
        k <- c %% 4
        l <- (32 + 2 * e + 2 * i - h - k) %% 7
        m <- trunc((a + 11 * h + 22 * l) / 451)
        temp <- h + l - 7 * m + 114
        month <- trunc(temp / 31)
        day <- (temp %% 31) + 1
    }
    
    monthStr <- ""
    
    if (month == 3) {
        monthStr <- "March"
    } else { monthStr <- "April" }
    
    c(monthStr, day)
}
