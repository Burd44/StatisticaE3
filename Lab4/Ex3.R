lambda1 <- 4
lambda2 <- 12  
prob_faster <- 3/4

N <- 10000  

generate_service_time <- function() {
  u <- runif(1)  
  
  if (u < prob_faster) {
    lambda <- lambda2  
  } else {
    lambda <- lambda1 
  }
  
  service_time <- rexp(1, rate = lambda)
  return(service_time)
}

service_times <- replicate(N, generate_service_time())
estimated_expectation <- mean(service_times)
cat("Estimated Expectation:", estimated_expectation, "\n")