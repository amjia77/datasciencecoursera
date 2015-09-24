setwd ("~/R/Coursera-R Programming/Assignment 3")             

## THIS ASSIGNMENT EXECUTES INCORRECTLY!!!!!!!!!! ##########################

##### Task 4: Ranking hospitals in all states #############################################

rankall <- function(outcome, num = "best") {
  data <- read.csv('rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv', colClasses = "character")           ## Read outcome data
  
  if (outcome %in% c("heart attack", "heart failure", "pneumonia")) {       ## Check that state and outcome are valid
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
    
    statedata <- split(data, data$State)                               # Split the "data" dataframe based on State, so "statedata" is a list with each element being a dataframe for each individual state
    
    ## For each element (a dataframe) of "statedata", sort by rate and then by name; hospitals that do not have data on a particular outcome should be excluded by using na.omit
    sortstatedata <- lapply(statedata, function(x) na.omit(x[order(x[[newoutcome]], x[[2]]),]))    
  
    name <- c()                   # create "name" vector to save hospitals name
    st <- c()                     # create "st" vector to save state
    
    ## handling each element (a dataframe) of the list "sortstatedata"
    for (i in 1:length(sortstatedata)) {                      ## so sortstatedata[[i]] is a dataframe for each individual state         
        
        if (num == "best") {                                   ## Convert num variable to numeric if given num == "best" or "worst"
            num <- 1
        } else if (num == "worst") {
            num <- nrow(sortstatedata[[i]])
        }
    
        if (nrow(sortstatedata[[i]]) < num) {
            name[i] <- NA
            st[i] <- sortstatedata[[i]][1, 7]
        } else {
      
            name[i] <- sortstatedata[[i]][num, 2]            ## Return hospital name in that state with the given rank
            st[i] <- sortstatedata[[i]][num, 7]
            
        }
    }
    
    data.frame(hospital=name, state=st)
    
  } else if (outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE) {
    stop('invalid outcome')
  }
}
