#outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#head(outcome)
#outcome[, 11] <- as.numeric(outcome[, 11])
#hist(outcome[, 11])

rankhospital <- function(state, outcome, num="best"){
    ## if invalid outcome, stop
    outcomes = c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% outcomes){
        stop ("Invalid outcome choice")
    }
    
    ## test variables
    #state = "TX"
    #outcome = "heart failure"
    #num = 4
    
    ## corresponding column index for outcome
    ## heart attack -> 11
    ## heart failure -> 17
    ## pneumonia -> 23
    colindex <- ifelse (outcome == "heart attack", 11, ifelse (outcome == "heart failure", 17, 23))
    
    ## read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## surpress warnings for conversion to numeric
    data[, colindex] <- suppressWarnings(as.numeric(data[, colindex]))
    #data[, 11] <- as.numeric(data[, 11])
    
    ## remove NA's
    data <- na.omit(data)
    
    ## check that state and outcome are valid
    states <- unique(data$State)
    if (!state %in% states){
        stop ("Invalid state choice")
    }
        
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    # subset of data matching state
    subdata <- subset(data, State == state)
    # subdata <- subdata[order(subdata[, 2]), ]
    ordered_subdata <- subdata[order(subdata[,colindex], subdata[,2], na.last=TRUE), 2]
    #ordered_subdata <- subdata[order(subdata[,colindex], na.last=TRUE), 2]
    
    ## between best and worst
    num <- ifelse(num == "best", 1, ifelse(num == "worst", length(ordered_subdata), as.numeric(num)))
    
    num_best <- ordered_subdata[num]
    
    return(num_best)
}