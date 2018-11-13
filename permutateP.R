diff.means <- function(group1, group2){
  mean(group2, na.rm = TRUE) - mean(group1, na.rm = TRUE)
}

diff.geoMeans <- function(group1, group2){
  exp(mean(log(group2), na.rm = TRUE)) - exp(mean(log(group1), na.rm = TRUE))
}

diff.medians <- function(group1, group2){
  median(group2, na.rm = TRUE) - median(group1, na.rm = TRUE)
}

permute <- function(..., sample, m, statistic) {
  s <- sample(sample)
  statistic(s[1:m], s[-(1:m)])
}

permuteP <- function(input1, input2, statistic = diff.means, numPermutations = 10000){
  dataStats <- statistic(input1,input2)
  permutedStats<- sapply(1:numPermutations, permute, sample=c(input1,input2), m=length(input1), statistic=statistic)
  moreExtremeStats <- sum(abs(permutedStats) >= abs(dataStats))
  pValueStats <- moreExtremeStats/numPermutations
  pValueStats
}