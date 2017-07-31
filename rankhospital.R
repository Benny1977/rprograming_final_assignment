rankhospital <- function(st, outcome, num = "best") {
    ## Read outcome data
    raw <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    relevant <- raw[c(2, 7, 11, 17, 23)]
    relevant[relevant == "Not Available"] <- NA
    relevant[,3:5] <- sapply(relevant[,3:5], as.numeric)
    names(relevant) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
    ## Check that state and outcome are valid
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
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    stateRelevantRows <- subset(relevant, relevant["state"] == st)
    stateRelevantRows <- subset(stateRelevantRows, !is.na(stateRelevantRows[outcome]))
    withRank <- stateRelevantRows
    withRank["r"] <- rank(stateRelevantRows[outcome])
    ordered <- withRank[order(withRank["r"], withRank["name"]),]
    ordered <- rerank(ordered)
    if(num == "best"){
        ordered[1,]$name 
    }
    else if(num == "worst") {
        ordered[nrow(ordered),]$name
    }
    else{
        ordered[num,]$name
    }    
    
}

rerank <- function(tempdf){
    for(i in 1:nrow(tempdf)){
        tempdf[i , "f"] <- i
       
    }
    tempdf
}