
# Populate the database for all de431 solar system bodies plus the header
# and body data
populate_database <- function(con)
{
  filelist <- c("ascm01000.rds",
                "ascm02000.rds",
                "ascm03000.rds",
                "ascm04000.rds",
                "ascm05000.rds",
                "ascm06000.rds",
                "ascm07000.rds",
                "ascm08000.rds",
                "ascm09000.rds",
                "ascm10000.rds",
                "ascm11000.rds",
                "ascm12000.rds",
                "ascm13000.rds",
                "ascp00000.rds",
                "ascp01000.rds",
                "ascp02000.rds",
                "ascp03000.rds",
                "ascp04000.rds",
                "ascp05000.rds",
                "ascp06000.rds",
                "ascp07000.rds",
                "ascp08000.rds",
                "ascp09000.rds",
                "ascp10000.rds",
                "ascp11000.rds",
                "ascp12000.rds",
                "ascp13000.rds",
                "ascp14000.rds",
                "ascp15000.rds",
                "ascp16000.rds")
  
  for (i in seq(from = 1, to = 30, by = 1)){
    populate_mercury_data(filelist[i], con)
    populate_venus_data(filelist[i], con)
    populate_emb_data(filelist[i], con)
    populate_mars_data(filelist[i], con)
    populate_jupiter_data(filelist[i], con)
    populate_saturn_data(filelist[i], con)
    populate_uranus_data(filelist[i], con)
    populate_neptune_data(filelist[i], con)
    populate_pluto_data(filelist[i], con)
    populate_moon_data(filelist[i], con)
    populate_sun_data(filelist[i], con)
    populate_nutation_data(filelist[i], con)
  }
  
}