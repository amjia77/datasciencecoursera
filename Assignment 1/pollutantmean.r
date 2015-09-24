setwd ("~/R/Coursera-R Programming/Assignment 1") 

# This function calculates the mean of a pollutant (sulfate or titrate) across a specified list of monitors.
# The funciton takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers,
# 'pullutantmean' reads that monitor's particulate matter data from the directory specified in the 'directory'
# argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA.

pollutantmean <- function(dir, pol, id=1:332) {
      files_full <- list.files(dir, full.names=TRUE)
      dat <- data.frame()
      for (i in id) {
            dat <- rbind(dat, read.csv(files_full[i]))
      }
      mean(dat[, pol], na.rm=TRUE)
}

# To test this function:
# pullutantmean("specdata", "sulfate", 1:10)
# pullutantmean("specdata", "nitrate", 23)