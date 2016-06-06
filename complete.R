complete <- function(directory, id = 1:332){
    setwd(file.path(getwd(), directory))
    dataframe = NULL
    
    for (i in id){
        if (i < 10){
            data <- read.csv(paste("0", "0", as.character(i), ".csv", sep=""), header = T, na.strings=c("NA", "NaN", " "))
        } else if (i >= 10 & i <100) {
            data <- read.csv(paste("0", as.character(i), ".csv", sep=""), header = T, na.strings=c("NA", "NaN", " "))
        } else {
            data <- read.csv(paste(as.character(i), ".csv", sep=""), header = T, na.strings=c("NA", "NaN", " "))
        }
        
        data <- data[complete.cases(data[, c("sulfate", "nitrate")]),]
        data = as.matrix(data)
        dataframe = rbind(dataframe, c(i, nrow(data)))
    }
    
    setwd("..")
    dataframe = data.frame(dataframe)
    names(dataframe) = c('id', 'nobs')

    return(dataframe)
}