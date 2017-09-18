pollutantmean <- function(directory, polutant, id = 1:332){
##this function takes a full name of directory with file
  ## and calculates means for each polutant across all monitors
  ## the calculated mean is weighted per number of observations
  ## for each monitor
  ## the functions excludes NAs and replaces resulted NaNs with NAs
  
  setwd(directory)
mpolutant <- matrix(nrow = length(id), ncol = 3)
   for(i in 1:length(id))
    {
     idname <- paste(c(sprintf("%03.f", id[i]), ".csv"), collapse = "")
     iddata <- read.csv(idname, header = TRUE)
     
              if (polutant == "nitrate") { 
                   p <-3
                  } else { 
                   p <- 2
                  }
              mpolutant[i,1] <- length(na.omit(iddata[,p]))
              mpolutant[i,2] <- mean(iddata[,p], na.rm = TRUE)
              mpolutant[i,3] <- mpolutant[i,1]* mpolutant[i,2]
              mpolutant[is.nan(mpolutant[,3]),3] <- NA
             

      }
##print(mpolutant)
print(sum(mpolutant[,3], na.rm = TRUE)/sum(mpolutant[,1], na.rm = TRUE))

##mpolutant[1, is.nan(mpolutant)]<-NA
##print(mpolutant)
##mtotal <- mean(mpolutant, na.rm = TRUE)
##print(paste(c("total mean PPM: ", mtotal), collapse=""))

}


