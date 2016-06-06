pollutantmean <- function(directory, pollutant, id = 1:332){
    mypath <- file.path(getwd(), directory)
    setwd(mypath)

    total <- 0
    observations <- 0
    mean <- 0.00
    
    for (i in id){
        if (i < 10){
            record <- read.csv(paste("0", "0", as.character(i), ".csv", sep=""), header = T, na.strings=c("NA", "NaN", " "))
        } else if (i >= 10 & i <100) {
            record <- read.csv(paste("0", as.character(i), ".csv", sep=""), header = T, na.strings=c("NA", "NaN", " "))
        } else {
            record <- read.csv(paste(as.character(i), ".csv", sep=""), header = T, na.strings=c("NA", "NaN", " "))
        }
        
        record <- record[complete.cases(record[, c("sulfate", "nitrate")]),]
        observations <- observations + nrow(record)
        
        if (pollutant == "sulfate"){
            total <- total + sum(record$sulfate)
        } else if (pollutant == "nitrate"){
            total <- total + sum(record$nitrate)
        }
    }
    
    setwd("..")
    
    mean <- total/observations

    print(mean)
}