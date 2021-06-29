
# Create the nutation matrix
nutation_matrix <- function(jd)
{
  nut_angles <- nutation_angles(jd)
  obliq <- obliquity(jd, nut_angles)
  
  # Calculate the nutation matrix elements
  cos_long <- cos(nut_angles[[1]])
  sin_long <- sin(nut_angles[[1]])
  sin_mean_obliquity <- sin(obliq[[1]])
  cos_mean_obliquity <- cos(obliq[[1]])
  sin_true_obliquity <- sin(obliq[[2]])
  cos_true_obliquity <- cos(obliq[[2]])
  
  nut_mat <- matrix(0.0, nrow=3, ncol=3)
  nut_mat[1,1] <- cos_long
  nut_mat[1,2] <- -sin_long * cos_mean_obliquity
  nut_mat[1,3] <- -sin_long * sin_mean_obliquity
  nut_mat[2,1] <- sin_long * cos_true_obliquity
  nut_mat[2,2] <- cos_long * cos_true_obliquity * cos_mean_obliquity + 
    sin_true_obliquity * sin_mean_obliquity
  nut_mat[2,3] <- cos_long * cos_true_obliquity * sin_mean_obliquity -
    sin_true_obliquity * cos_mean_obliquity
  nut_mat[3,1] <- sin_long * sin_true_obliquity
  nut_mat[3,2] <- cos_long * sin_true_obliquity * cos_mean_obliquity -
    cos_true_obliquity * sin_mean_obliquity
  nut_mat[3,3] <- cos_long * sin_true_obliquity * sin_mean_obliquity +
    cos_true_obliquity * cos_mean_obliquity
  
  return(nut_mat)
}