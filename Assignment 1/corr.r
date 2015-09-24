setwd ("~/R/Coursera-R Programming/Assignment 1")

# a function that takes a directory of data files and a threshold for complete cases and calculates
# the correlation between sulfate and nitrate for monitor locations where the number of complete observed
# cases (on all variables) is greater than the threshold. The function should return a vector of correlations
# for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then 
# the function should return a numeric vecctor of length 0.

corr <- function(dir, threshold=0) {
  comp <- complete(dir, 1:332)
  comp1 <- subset(comp, comp$nobs > threshold)
  corres <- vector("numeric", length=nrow(comp1))
  j <- 1
  for (no in comp1$id) {
    files_full <- list.files(dir, full.names=TRUE)
    x <- read.csv(files_full[no])
    corres[j] <- cor(x$sulfate, x$nitrate, use="complete")
    j <- j+1
  }
  corres
}


# To test this function:
# cr <- corr("specdata", 150)
# head(cr)
# summary(cr)