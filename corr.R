corr <- function(directory, threshold = 0){
  ##this function takes a full name of directory with files
nobs<- complete(directory)
nobs_meas <- as.numeric(nobs[,2])
cr <- vector("numeric")
k <- 1
    for(i in 1:length(nobs_meas)){
    if(nobs_meas[i] > threshold){
          iddata <- read.csv(nobs[i,1], header = TRUE)
          cr[k] <- cor(iddata$nitrate, iddata$sulfate, use="complete.obs", method="pearson")
          k <- k +1      
         }
     else{
          paste(c("No monitors above threshold", threshold), collapse = " ")
         }
    }
cr
}

