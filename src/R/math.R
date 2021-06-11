
# Various math routines that are needed for calculations

# Calculate the dot product of two vectors (x1, y1, z1) and (y1, y2, y3)
dot_product <- function(x, y)
{
  result <- 0.0
  for (i in 1:length(x)){
    result <- result + x[i] * y[i]
  }
  
  return(result)
}

# Calculate the cross product of two vectors (x1, y1, z1) and (y1, y2, y3)
cross_product <- function(x, y)
{
  result <- c(0.0, 0.0, 0.0)
  for (i in 1:length(x)){
    result[1] <- x[2] * y[3] - x[3] * y[2]
    result[2] <- x[3] * y[1] - x[1] * y[3]
    result[3] <- x[1] * y[2] - x[2] * y[1]
  }
  
  return(result)
}

# Calculate the magnitude of a vector (x1, y1, z1)
magnitude <- function(x)
{
  return(sqrt(x[1] * x[1] + x[2] * x[2] + x[3] * x[3]))
}

# Calculate a unit vector for a given vector (x1, x2, x3)
unit_vector <- function(x)
{
  mag <- magnitude(x)
  result <- c(0.0, 0.0, 0.0)
  result[1] <- x[1] / mag
  result[2] <- x[2] / mag
  result[3] <- x[3] / mag
  
  return (result)
}

# Convert position and velocity data from KM and KM/Sec to AU and AU/day
# The function will work for a 6 element data frame containing both position 
# and velocity data
km_sec2AU_day <- function(df)
{
  KM2AU <- 149597870.7
  sec2day <- 60*60*24
  
  pos <- data.frame(df[,1])
  vel <- data.frame(df[,2])
  
  pos1 <- apply(pos, 1, function(x) {x/KM2AU})
  vel1 <- apply(vel, 1, function(x) {x*sec2day/KM2AU})
  df1 <- data.frame(pos1, vel1)
  
  return(df1)
}
