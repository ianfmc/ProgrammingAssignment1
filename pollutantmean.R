pollutantmean <- function(directory, pollutant, id=1:332) {
  
  ## create a data frame to hold all of the values
  ## from the selected files

  if ((pollutant != "sulfate") && (pollutant != "nitrate")) {
    print("Pollutant must be sulfate or nitrate")
  } 
  else {
    all_values = data.frame(Date=character(0), 
                            sulfate=numeric(0), 
                            nitrate=numeric(0), 
                            ID=integer(0))
    
    if (pollutant == "sulfate") {
      
      ## loop through and append all values for the selected files
      ## and the selected pollutant
      
      for (i in id) {
        filename <- paste(directory, sprintf("%03d", i), sep="")
        filename <- paste(filename, ".csv", sep="")
        
        current_values <- read.csv(filename) 
        current_values_clean <- subset(current_values,!is.na(current_values$sulfate))
        
        all_values <- rbind(all_values, current_values_clean)
      } 
      pollutant_mean <- mean(all_values[,"sulfate"], na.rm=TRUE)
    }
    else {
      for (i in id) {
        
        ## loop through and append all values for the selected files
        ## and the selected pollutant
        
        filename <- paste(directory, sprintf("%03d", i), sep="")
        filename <- paste(filename, ".csv", sep="")
        
        current_values <- read.csv(filename) 
        current_values_clean <- subset(current_values,!is.na(current_values$nitrate))
        
        all_values <- rbind(all_values, current_values_clean)        
      }
      pollutant_mean <- mean(all_values[,"nitrate"], na.rm=TRUE)    
    }
    
    ## print the mean of the selected pollutant 
    sprintf("%4.3f", pollutant_mean)
  }
}