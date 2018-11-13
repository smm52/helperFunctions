getBootSamples <- function(x,input,lengthInput,statistic){
  statistic(sample(input, 
                   size = lengthInput, 
                   replace = TRUE), na.rm = TRUE)
}

bootstrapCI <- function(input, numBoots = 10000, lowerBound = 0.025, upperBound = 0.975, myStatsFunction = median){
  res <- sapply(1:numBoots,getBootSamples,input,length(input),myStatsFunction)
  output <- c(myStatsFunction(input, na.rm = TRUE), quantile(res, probs = c(lowerBound, upperBound)))
  names(output) <- c(substitute(myStatsFunction),paste0(lowerBound * 100,'%') ,paste0(upperBound * 100,'%'))
  output
}
