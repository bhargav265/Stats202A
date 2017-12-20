############################################################# 
## Stat 202A - Homework 9
## Author: 
## Date : 
## Description: This script performs parallel computing in 
## R using the 'doParallel' package
#############################################################

#############################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names, 
## function inputs or outputs. You can add examples at the
## end of the script (in the "Optional examples" section) to 
## double-check your work, but MAKE SURE TO COMMENT OUT ALL 
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not use the function "setwd" anywhere
## in your code. If you do, I will be unable to grade your 
## work since R will attempt to change my working directory
## to one that does not exist.
#############################################################


## Load 'doParallel' package
library(doParallel)

## In this script, we will be computing a confidence interval
## for a sample statistic using bootstrapping and parallel computing

bootstrap_CI <- function(X, B = 10000){
  
  ## Input:
  ## X: An n-dimensional vector of numbers. 
  ## B: The number of bootstrap samples to draw
  
  ## Output:
  ## The function returns a 95% confidence interval for the 
  ## median of X, computed using the bootstrap
  
  ## For this function, fill in all of the missing code, 
  ## marked with '??' symbols.
  
  n <- length(X)

  ## Setup parallel backend to use multiple processors
  cl <- makeCluster(4)
  registerDoParallel(cl)
  
  ## Start program
  boostrap_results <- foreach(i = 1:B, .combine = cbind) %dopar% {
    
    boot_indices  <- sample(n,replace=T)
    boot_sample   <- X[boot_indices] 
    boot_stat     <- median(boot_sample)
    
    ## Combine all of our output into a list
    result <- list(median = boot_stat)
    result
    
  }
  
  ## Stop the cluster manually
  stopCluster(cl)
  
  my_results <- as.numeric(unlist(boostrap_results))
  CI_temp <- quantile(my_results, c(0.025, 0.975), names = F)
  CI_low     <- CI_temp[1]
  CI_high    <-CI_temp[2]
  CI         <- c(CI_low, CI_high)
  
  ## Output the 2-dimensional vector CI
  return(CI)
  
}

###########################################
## Optional examples (testing your code) ##
###########################################

# # ## Make up some data
#  n <- 1000
#  X <- rexp(n)
# 
#  ## Compute the 95% CI
#  low     <- round((n/2) - (1.96 * sqrt(n) / 2))
#  high    <- round(1 + (n/2) + (1.96 * sqrt(n) / 2))
#  CI      <- c(sort(X)[low], sort(X)[high])
#  boot_CI <- bootstrap_CI(X)


