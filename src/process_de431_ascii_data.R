
# This script will take a de431 ascii file, parse the file so that each coordinate
# and Julian Day are stored in a single column, and output as a RDS file.

proces_de431_ascii_data <- function(filename)
{
  # Read in the ascii file
  ascii_data <- readLines(here("data", "raw", filename))
  DT <- as.data.table(ascii_data)
  
  # Create data table for output file
  m <- matrix(0, nrow = 11643300, ncol=1)
  DT1 <- data.table(m)
  
  # Extract all data from the file and add to data frame
  # k indexes the rows of the target data table
  k <- as.integer(1)
  for (i in seq(from = 2, to = 3892176, by = 341)){
    for (j in seq(from = 0, to = 339, by = 1)){
       
      # First data element on the row
      tmpstr1 <- as.character(substr(DT[j+i], 4, 26))
      tmpstr1 <- chartr(old = "D", new = "E", tmpstr1)
      tmpstr1 <- as.numeric(tmpstr1)
      set(DT1,k,1L,tmpstr1)
      k <- k + 1L
      
      # Second data element on the row
      tmpstr2 <- as.character(substr(DT[j+i], 30, 52))
      tmpstr2 <- chartr(old = "D", new = "E", tmpstr2)
      tmpstr2 <- as.numeric(tmpstr2)
      set(DT1,k,1L,tmpstr2)
      k <- k + 1L
      
      # Third data element on the row
      tmpstr3 <- as.character(substr(DT[j+i], 56, 78))
      tmpstr3 <- chartr(old = "D", new = "E", tmpstr3)
      tmpstr3 <- as.numeric(tmpstr3)
      set(DT1,k,1L,tmpstr3)
      k <- k + 1L
    }
  }
  
  # Write RDS file
  new_filename <- substr(filename, 1, 10)
  new_filename <- paste(new_filename, "rds", sep = "")
  saveRDS(DT1, here("data", "processed", new_filename))
}