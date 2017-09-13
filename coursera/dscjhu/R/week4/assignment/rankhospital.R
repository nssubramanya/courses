# rankhospital.R
# Return Lowest 30-day mortality rate for given outcome - "heart attack", "heart failure", 
# "pneumonia"
rankhospital <- function (state, outcome, rank) {
    
    # Read outcome data, convert relevant columns to numeric
    # outcome.data <- read.csv("outcome-of-care-measures.csv")
    
    
    
    # input argument check
    if (outcome == "heart attack") col <- 11
    else if (outcome == "heart failure") col <- 17
    else if (outcome == "pneumonia") col <- 23
    else {
        stop ("invalid outcome")
    }
    
    if (is.na(match(state, outcome.data$State)))
        stop("invalid state")
    
    hospitals.in.state <- outcome.data[outcome.data[,"State"] == state & !is.na(outcome.data[,col]), c(2,col)]
    hospitals.ordered <- hospitals.in.state[order(hospitals.in.state[2], hospitals.in.state[1]),]
    num.hospitals <- nrow(hospitals.ordered)
    
    if (rank == "best") rank <- 1
    else if (rank == "worst") rank <- num.hospitals
    else if (rank > num.hospitals) {return (NA)}
    
    return (hospitals.ordered[rank,1])
}


outcome.data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
outcome.data[,11] <- as.numeric(outcome.data[,11])
outcome.data[,17] <- as.numeric(outcome.data[,17])
outcome.data[,23] <- as.numeric(outcome.data[,23])
