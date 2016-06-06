corr <- function(directory, threshold = 0){
    setwd(file.path(getwd(), directory))
    correlationVector = NULL
    
    for (i in 1:332){
        if (i < 10){
            data <- read.csv(paste("0", "0", as.character(i), ".csv", sep=""), header = T, na.strings=c("NA", "NaN", " "))
        } else if (i >= 10 & i <100) {
            data <- read.csv(paste("0", as.character(i), ".csv", sep=""), header = T, na.strings=c("NA", "NaN", " "))
        } else {
            data <- read.csv(paste(as.character(i), ".csv", sep=""), header = T, na.strings=c("NA", "NaN", " "))
        }
        
        data <- data[complete.cases(data[, c("sulfate", "nitrate")]),]
        
        if (nrow(data) > threshold){
            correlationVector = c(correlationVector, cor(data[,2], data[,3]))
        }
    }
    
    setwd("..")
    
    return(correlationVector)
}