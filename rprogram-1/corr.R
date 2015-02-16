corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  df <- complete(directory,)
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  file_ext <- ".csv"
  correlations <- vector('numeric')
  
  for(i in df$id){
    if(df$nobs[i] > threshold){
      if((i > 0) && (i < 10)) {
        file_name <- paste("00", i, file_ext, sep="")
      } else if((i >= 10) && (i < 100)){
        file_name <- paste("0", i, file_ext, sep="")
      } else if((i >= 100) && (i < 1000)){
        file_name <- paste(i, file_ext, sep="")
      }
      d <- read.csv(paste(directory, file_name, sep="/"))
      sub_d <- subset(d, complete.cases(d))
      correlations <- c(correlations, cor(sub_d$sulfate, sub_d$nitrate))
    }
  }
  
  ## Return a numeric vector of correlations
  return(correlations)
}