
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

ra_dec <- function(vec)
{
  # Convert declination to degs, mins, and secs
  dec <- deg2ddmmss(asin(vec[3]) * RAD2DEG)
  
  # Convert right ascension to hours, mins, and secs
  ra_tmp <- atan2(vec[2], vec[1]) * RAD2DEG
  if (vec[2] < 0) {ra_tmp <- ra_tmp + 360.0}
  ra_tmp <- ra_tmp / 15
  ra <- deg2ddmmss(ra_tmp)
  
  mat <- matrix(0.0, nrow=3, ncol=3)
  
  for (i in 1:3) {
    mat[1,i] = ra[i]
    mat[2,i] = dec[i]
  }
  
  return(mat)
}
