
# Confidence Interval
ConfidenceInterval <- function(data, alpha = 0.05) {
  
  mean.value <- mean(data)
  n <- length(data)
  
  standard.deviation <- sd(data)
  standard.error <- standard.deviation / sqrt(n)
  
  # alpha = 0.05
  degrees.freedom = n - 1
  tscore = qt(p = alpha/2, df = degrees.freedom, lower.tail = F)
  
  margin.error <- tscore * standard.error
  
  lower.bound <- mean.value - margin.error
  upper.bound <- mean.value + margin.error
  
  statistical <- round(c(mean.value, standard.deviation, lower.bound, upper.bound), digits = 3)
  
  names(statistical) <- c("Mean", "StDev", "2.5%", "97.5%")
  
  return(statistical)
}

demo <- sample(1:100, 50)
ConfidenceInterval(demo)


# reference
# https://bookdown.org/logan_kelly/r_practice/p09.html