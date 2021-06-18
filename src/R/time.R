
# Convert degrees, minutes, and seconds to decimal degrees
ddmmss2deg <- function(deg, min, sec)
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
deg2ddmmss <- function(deg)
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

