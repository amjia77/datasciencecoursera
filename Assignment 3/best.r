setwd ("~/R/Coursera-R Programming/Assignment 3")             

#unzip("rprog-data-ProgAssignment3-data.zip", exdir = "rprog-data-ProgAssignment3-data")


######################## Task 1 ###################################################
# outcome <- read.csv('rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv', colClasses = "character")
# head(outcome)
# ncol(outcome)                             ## number of columns in this dataset
# names(outcome)                            ## the names of each column
#
# outcome[, 11] <- as.numeric(outcome[, 11])      ## coerces objects to be "numeric"
# hist(outcome[, 11])
#############################################################################################

##### Task 2: Finding the best hospital in a state #############################################

best <- function(state, outcome) {
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
            
            outcomedata <- data[which(data$State == state), newoutcome]              ## the outcome column of subset of the state
            newdata <- data[which(data$State == state), ]               ## subset of that state
            
            rowNumber <- which.min(outcomedata)               ## return the row number that has the lowest outcome 
            newdata[rowNumber, 2]            ## Return hospital name in that state with lowest 30-day death rate

      
      } else if (state %in% data$State == FALSE) {
            stop('invalid state')
      } else if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) {
            stop('invalid outcome')
      }
}

