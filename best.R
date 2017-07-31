best <- function(st, outcome) {
    ## Read outcome data
    raw <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    relevant <- raw[c(2, 7, 11, 17, 23)]
    relevant[relevant == "Not Available"] <- NA
    relevant[,3:5] <- sapply(relevant[,3:5], as.numeric)
    names(relevant) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
    #checks
    outcomes = c("heart attack", "heart failure", "pneumonia")
    states = unique(relevant$state)
    if(!is.element(outcome, outcomes))
    {
        return("No Such Outcome")
    }
    if(!is.element(st, states))
    {
        return("No Such state")
    }
    stateRelevantRows <- subset(relevant, relevant["state"] == st)
    stateRelevantRows <- subset(stateRelevantRows, !is.na(stateRelevantRows[outcome]))
    ## rate
    tempmin <- min(stateRelevantRows[outcome])
    tempmin
    temp <- subset(stateRelevantRows, stateRelevantRows[outcome] == tempmin)
     
    temp$name
    
}
 
