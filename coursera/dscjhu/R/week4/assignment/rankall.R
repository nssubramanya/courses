# rankall.R

rankhosp <- function (outcome.data, outcome, rank){
    if (outcome == "heart attack") col <- 11
    else if (outcome == "heart failure") col <- 17
    else if (outcome == "pneumonia") col <- 23
    else {
        stop ("invalid outcome")
    }
    
    hospitals.in.state <- outcome.data[!is.na(outcome.data[,col]), c(2,7,col)]
    hospitals.ordered <- hospitals.in.state[order(hospitals.in.state[3], hospitals.in.state[1]),]
    num.hospitals <- nrow(hospitals.ordered)
    
    if (rank == "best") rank <- 1
    else if (rank == "worst") rank <- num.hospitals
    else if (rank > num.hospitals) {
        return (c(NA, hospitals.ordered$State [1]))
    }
    
    return (hospitals.ordered[rank,c(1,2)])
}

rankall <- function (outcome, num = "best") {
    
    data <- split(outcome.data, outcome.data$State)
    l <- lapply (data, rankhosp, outcome=outcome, rank = num)
    my.matrix <- do.call("rbind", l)
    df <- as.data.frame(my.matrix, stringsAsFactors=FALSE)
    #df <- na.omit(df)
    colnames(df) <- c("hospital", "state")
    
    return (df)
    
}


outcome.data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
outcome.data[,11] <- as.numeric(outcome.data[,11])
outcome.data[,17] <- as.numeric(outcome.data[,17])
outcome.data[,23] <- as.numeric(outcome.data[,23])


