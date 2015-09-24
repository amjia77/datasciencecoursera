setwd ("~/R/Coursera-R Programming/Assignment 3")             


##### Task 3: Ranking hospitals by outcome in a state #############################################

rankhospital <- function(state, outcome, num = "best") {
  data <- read.csv('rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv', colClasses = "character")           ## Read outcome data
  
  if (state %in% data$State & outcome %in% c("heart attack", "heart failure", "pneumonia")) {       ## Check that state and outcome are valid
    data[, 11] <- as.numeric(data[, 11])                       ## convert to numeric
    data[, 17] <- as.numeric(data[, 17])
    data[, 23] <- as.numeric(data[, 23])
    
    if (outcome == "heart attack") {                           ## get the corresponding column number
      newoutcome <- 11
    } else if (outcome == "heart failure") {
      newoutcome <- 17
    } else if (outcome == "pneumonia") {
      newoutcome <- 23
    }
    
    newdata <- data[which(data$State == state), ]               ## subset of that state
    
    newdata1 <- na.omit(newdata[order(newdata[[newoutcome]], newdata[[2]]),])         ## sort this data by rate and then by name; hospitals that do not have data on a particular outcome should be excluded by using na.omit
    
    if (num == "best") {                                   ## Convert num variable to numeric if given num == "best" or "worst"
      num <- 1
    } else if (num == "worst") {
      num <- nrow(newdata1)
    }
    
    if (nrow(newdata1) < num) {
          NA
    } else {
      
          newdata1[num, 2]            ## Return hospital name in that state with the given rank
      
    }
    
    
  } else if (state %in% data$State == FALSE) {
    stop('invalid state')
  } else if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) {
    stop('invalid outcome')
  }
}
