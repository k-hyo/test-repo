pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  file_ext <- ".csv"
  tmp <- NA
  
  for(i in id) {
    if((i > 0) && (i < 10)) {
      file_name <- paste("00", i, file_ext, sep="")
    } else if((i >= 10) && (i < 100)){
      file_name <- paste("0", i, file_ext, sep="")
    } else if((i >= 100) && (i < 1000)){
      file_name <- paste(i, file_ext, sep="")
    } else {
      return(NA)
    }
    d <- read.csv(paste(directory, file_name, sep="/"))
    tmp <- c(tmp, d[,pollutant])
    tmp <- tmp[!is.na(tmp)]
  }
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  return(mean(tmp))
}