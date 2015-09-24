setwd ("~/R/Coursera-R Programming/Assignment 1")

# This function reads a directory full of files and reports the number of completely observed 
# cases in each data file. The funciton sould return a data frame where the first column is the
# name of the file and the seond column is the number of complete cases.

complete <- function(dir, iid=1:332) {                              ## 'dir' is a character vector of length 1 indicating the location of the CSV files
      files_full <- list.files(dir, full.names=TRUE)
      num <- vector("numeric", length=length(iid))
      j <- 1
      for (i in iid) {
          x <- read.csv(files_full[i])
          num[j] <- nrow(x[complete.cases(x), ])
          j <- j+1      
      }
      
      data.frame(id=iid, nobs=num)
}

# To test this function:
# complete("specdata", 1)
# complete("specdata", c(2, 4, 8, 10))