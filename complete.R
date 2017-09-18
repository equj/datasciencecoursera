complete <- function(directory, id = 1:332){
##this function takes a full name of directory with files
## and outputs total number of observations for which both pollutants are present
## function returns 2-column matrix 'nobs' that has monitor id in first column and
## the number of complete observations in second column
  
  setwd(directory)
  nobs <- matrix(nrow = length(id), ncol = 2)
  for(i in 1:length(id))
  {
    idname <- paste(c(sprintf("%03.f", id[i]), ".csv"), collapse = "")
    iddata <- read.csv(idname, header = TRUE)
    iddata[,4] <- (!is.na(iddata[,2])) * (!is.na(iddata[,3]))
    
      nobs[i,1] <- idname
      nobs[i,2] <- sum(iddata[,4], na.rm = TRUE)
    
  }
nobs

}


