rankall <- function(outcome, num = "best") {
    ## Read outcome data
    raw <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    relevant <- raw[c(2, 7, 11, 17, 23)]
    relevant[relevant == "Not Available"] <- NA
    relevant[,3:5] <- sapply(relevant[,3:5], as.numeric)
    relevant[,2] <- sapply(relevant[,2], as.factor)
    names(relevant) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
    ## Check that outcome is valid
    outcomes = c("heart attack", "heart failure", "pneumonia")
    states = unique(relevant$state)
    if(!is.element(outcome, outcomes))
    {
        return("No Such Outcome")
    }
    #Need to change the logic to get rank per state
    #tapply(relevant, relevant$state, rankhospital)
    dflistbyState <- split(relevant, relevant$state)
    #data.frame(Reduce(rbind, dflistbyState[3]))
    #********************
    resultList <- data.frame(character(),character(),numeric())
    names(resultList) <- c("state", "name", "score")
     for(i in 1:length(dflistbyState)){
        if(num == "best"){
            resultList <- rbind(resultList, getStateLine(dflistbyState[i], 1, outcome))
            
        }
        else if(num == "worst") {
            resultList <- rbind(resultList, getStateLine(dflistbyState[i], "worst" , outcome))
            
        }
        else{
            resultList <- rbind(resultList, getStateLine(dflistbyState[i], num, outcome))
        }   
     }
     orderedResultList <- resultList[order(resultList["score"], decreasing = TRUE),]
     orderedResultList
    #resultList["score"]
   #order(resultList$score)
}

rerank <- function(tempdf){
    for(i in 1:nrow(tempdf)){
        tempdf[i , "f"] <- i
        
    }
    tempdf
}

getStateLine <- function(df, num, outcome){
    df <- data.frame(Reduce(rbind, df))
    names(df) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
    df <- subset(df, !is.na(df[outcome]))
    df["r"] <- rank(df[outcome])
    ordered <- df[order(df["r"], df["name"]),]
    ordered <- rerank(ordered)
   # ordered
    if(num == "worst"){
       num <- nrow(ordered) 
    }
    
    newdf <- data.frame(ordered[num,]$state, ordered[num,]$name, ordered[num,][outcome])
    
    names(newdf) <- c("state", "name", "score")
    newdf
    
    #print(paste(ordered[num,]$state, ordered[num,]$name, ordered[num,][outcome] , sep = "   "))
    
    #names2 <- c("state", "name", "score")
}