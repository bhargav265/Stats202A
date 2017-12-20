#############################################################
## Stat 202A - Homework 7
## Author: Hariharan Shanmugavadivel
## Date : 11/28/17
## Description: This script implements the lasso
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

#####################################
## Function 1: Lasso solution path ##
#####################################

myLasso <- function(X, Y, lambda_all){

  # Find the lasso solution path for various values of
  # the regularization parameter lambda.
  #
  # X: n x p matrix of explanatory variables.
  # Y: n dimensional response vector
  # lambda_all: Vector of regularization parameters. Make sure
  # to sort lambda_all in decreasing order for efficiency.
  #
  # Returns a matrix containing the lasso solution vector
  # beta for each regularization parameter.

  #######################
  ## FILL IN CODE HERE ##
  #######################
  L = length(lambda_all)
  lambda_all = sort(lambda_all, decreasing = TRUE)

  p = dim(X)[2]
  X = cbind(rep(1,n),X)
  T = 10
  db = matrix(rep(0, p+1), nrow = p+1)
  beta = matrix(rep(0, p+1), nrow = p+1)
  beta_all = matrix(rep(0, (p+1)*L), nrow = p+1)
  R = Y
  ss = rep(0, p+1)
  for (j in 1:(p+1))
    ss[j] = sum(X[, j]^2)

  for (l in 1:L)
  {
    lambda = lambda_all[l]
    for (t in 1:T)
    {
      for (j in 2:(p+1))
      {
        db = sum(R*X[, j])/ss[j]
        b = beta[j]+db
        b=sign(b)*max(0,abs(b)-lambda/ss[j])
        db=b-beta[j]
        R=R-X[,j]*db
        beta[j]=b
      }
    }
    beta_all[, l] = beta

  }
  ## Function should output the matrix beta_all, the
  ## solution to the lasso regression problem for all
  ## the regularization parameters.
  ## beta_all is (p+1) x length(lambda_all)
  return(beta_all)

}

# n = 50
# p = 200
# s = 10
# lambda_all = (100:1)*10
#
# X = matrix(rnorm(n*p), nrow=n)
# beta_true = matrix(rep(0, p), nrow = p)
# beta_true[1:s] = 1:s
# Y = X %*% beta_true + rnorm(n)
# beta_all = myLasso(X, Y, lambda_all)
#
#
# L = length(lambda_all)
# err = rep(0, L)
# for(l in 1:L)
# {
#   err[l] = sum((beta_all[2:(p+1),l]-beta_true)^2)
# }
#
# plot(lambda_all, err, type = 'l',main='LASSO ERROR ESTIMATION',
#      xlab='Lambda',ylab='Error')
