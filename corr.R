corr <- function(directory, threshold = 0) {
  
  corr_values = vector('numeric')
  
  ## loop through and find complete records afor the selected files
  
  for (i in 1:332) {
    filename <- paste(directory, sprintf("%03d", i), sep="")
    filename <- paste(filename, ".csv", sep="")
    
    current_values <- read.csv(filename, colClasses=c("character",
                                                      "numeric",
                                                      "numeric",
                                                      "integer"))  
    current_ID <- i
    complete_observations <- current_values[complete.cases(current_values),]
    
    if (nrow(complete_observations) < threshold) {
      next
    }
    else {
      cr <- cor(complete_observations[["sulfate"]], 
                complete_observations[["nitrate"]],
                method="pearson")
      corr_values <- c(corr_values, cr)
    }
  }
  
  ## find and print number of complete observations
  ## for the selected files
  
  return (corr_values)
}