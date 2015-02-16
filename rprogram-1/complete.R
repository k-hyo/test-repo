complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  file_ext <- ".csv"
  NOBS <- NULL
  
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
    sub_d <- subset(d, complete.cases(d))
    NOBS <- c(NOBS, nrow(sub_d))
  }
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  return(data.frame(id=id, nobs=NOBS))
}