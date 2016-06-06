#outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#head(outcome)
#outcome[, 11] <- as.numeric(outcome[, 11])
#hist(outcome[, 11])

rankall <- function(outcome, num="best"){
    ## if invalid outcome, stop
    outcomes = c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% outcomes){
        stop ("Invalid outcome choice")
    }
    
    ## test variables
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
    
    ## sort data based on colindex and name
    ordered_data <- data[order(data[,colindex], data[,2], na.last=TRUE), ]
    ordered_data <- ordered_data[!is.na(ordered_data[,colindex]),]

    ## remove duplicate state names
    states <- sort(unique(ordered_data[,7]))
    
    ## between best and worst
    num <- ifelse(num == "best", 1, ifelse(num == "worst", length(ordered_data), as.numeric(num)))

    ## for each state, find the hospital of the given rank
    hospital_given_rank <- function(state){
        hospital <- subset(ordered_data, State == state)
        #hospital <- hospital[num, c(2, 7, colindex)]
        hospital <- hospital[num, c(2, 7, colindex)]
        hospital$State <- state
        
        return (hospital)
    }
    
    ## return a data frame with the hospital names and the
    ## (abbreviated) state name        
    hospital_state_rank <- lapply(states, hospital_given_rank)
    
    ## call hospital_given_rank for states, based on colindex
    hospital_frame <- data.frame(do.call(rbind, lapply(states, hospital_given_rank)), row.names=states)
    colnames(hospital_frame) <- c("hospital", "state")
    
    return(hospital_frame)
}