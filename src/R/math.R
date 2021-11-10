
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

amodulo <- function(a, b)
{
  return(x <- a - b * floor(a/b))
}

# Returns a rotation matrix based on the axis (x, y, or z) and the angle phi
# The angle phi is expected to be in radians
rotation_matrix <- function(axis, phi)
{
  mat <- matrix(0.0, nrow = 3, ncol = 3)
  cosphi <- cos(phi)
  sinphi <- sin(phi)

  if (axis == 1) {
    mat[1,1] <- 1.0
    mat[2,2] <- cosphi
    mat[3,3] <- cosphi
    mat[2,3] <- sinphi
    mat[3,2] <- -sinphi
  } else if (axis == 2) {
    mat[1,1] <- cosphi
    mat[1,3] <- -sinphi
    mat[2,2] <- 1.0
    mat[3,1] <- sinphi
    mat[3,3] <- cosphi
  } else if (axis == 3){
    mat[1,1] <- cosphi
    mat[2,2] <- cosphi
    mat[3,3] <- 1.0
    mat[2,1] <- -sinphi
    mat[1,2] <- sinphi
  } else {print("Axis is wrong value")}
  
  return (mat)
}

quadratic_interpolation <- function(y_minus, y_0, y_plus)
{
  # Coefficients of interpolating parabola
  a <- 0.5 * (y_plus + y_minus) - y_0
  b <- 0.5 * (y_plus - y_minus)
  c <- y_0
  
  # Find extreme value
  xe <- -b / (2 * a)
  ye <- (a * xe + b) * xe + c
  dis <- b * b - 4 * a * c
  
  dx <- 0
  num_roots <- 0
  root1 <- 0
  root2 <- 0
  
  if (dis >= 0) {
    dx <- 0.5 * sqrt(dis) / fabs(a)
    root1 <- xe - dx
    root2 <- xe + dx
    
    if (fabs(root1) <= 1) {num_roots <- num_roots + 1}
    if (fabs(root2) <= 1) {num_roots <- num_roots + 1}
    if (root1 < -1) {root1 <- root2}
  }
  
  return(c(num_roots, root1, root2))
}
