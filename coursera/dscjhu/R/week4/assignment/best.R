# Return Lowest 30-day mortality rate for given outcome - "heart attack", "heart failure", 
# "pneumonia"
best <- function (state, outcome) {
    
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
    
    min.heart.rate.mortality <- min(outcome.data[outcome.data[,"State"] == state, col], na.rm = TRUE)
    hospitals <- outcome.data[outcome.data[,"State"] == state & !is.na(outcome.data[,col]) & outcome.data[,col] == min.heart.rate.mortality, c(2)]
    sort(hospitals)[1]
}


outcome.data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
outcome.data[,11] <- as.numeric(outcome.data[,11])
outcome.data[,17] <- as.numeric(outcome.data[,17])
outcome.data[,23] <- as.numeric(outcome.data[,23])
