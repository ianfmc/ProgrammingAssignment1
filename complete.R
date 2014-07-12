complete <- function(directory, id = 1:332) {
  
  complete_values = data.frame(ID=integer(0),
                               nobs=integer(0))
  
  ## loop through and find complete records afor the selected files
  
  for (i in id) {
    filename <- paste(directory, "/", sep="")
    filename <- paste(filename, sprintf("%03d", i), sep="")
    filename <- paste(filename, ".csv", sep="")
    
    current_values <- read.csv(filename, colClasses=c("character",
                                                      "numeric",
                                                      "numeric",
                                                      "integer"))  
    current_ID <- i
    complete_observations <- current_values[complete.cases(current_values),]

    complete_values <- rbind(complete_values,
                             data.frame(ID=current_ID,
                                        nobs=nrow(complete_observations)))
  }
  
  ## find and print number of complete observations
  ## for the selected files
  
  complete_values
}