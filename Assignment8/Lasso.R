#############################################################
## Stat 202A - Homework 7
## Author:
## Date :
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

  lambda_all = sort(lambda_all, decreasing = TRUE)

  p = dim(X)[2]
  L = length(lambda_all)

  T = 10

  beta = matrix(rep(0,p), nrow = p)
  beta_all = matrix(rep(0,(p*L)), nrow = p)
  err = rep(0,L)
  R = Y

  ss = rep(0,p)

  for(j in 1:p){
    ss[j] = sum(X[,j]^2)


  }

  for (l in 1:L){
    lambda = lambda_all[l]
    for(t in 1:T){
      for (j in 1:p){
        db = sum(R*X[,j])/ss[j]
        b = beta[j] + db
        b = sign(b) * max(0, abs(b) - lambda / ss[j])
        db = b - beta[j]
        R = R - X[,j] * db
        beta[j] = b
      }
    }
    beta_all[,l] = beta
    # print('beta')
    # print(dim(beta))
    # print('beta_true')
    # print(dim(beta_true))
    #err[l] = sum((beta - beta_true)^2)
  }

 
  #matplot(t(matrix(rep(1, p), nrow = 1)%*%abs(beta_all)), t(beta_all), type = 'l',main='LASSO BOOSTING',xlab='N',ylab='Beta')
  #plot(lambda_all, err, type = 'l',main='LASSO ERROR ESTIMATION',xlab='Lambda',ylab='Error')


  ## Function should output the matrix beta_all, the
  ## solution to the lasso regression problem for all
  ## the regularization parameters.
  ## beta_all is (p+1) x length(lambda_all)
  return(beta_all)

}

n = 50
p = 200
s = 10

X = matrix(rnorm(n * p), nrow=n)
lambda_all = (100 : 1) * 10
beta_true = matrix(rep(0, p), nrow = p)
beta_true[1:s] = 1 : s
Y = X %*% beta_true + rnorm(n)
beta_all = myLasso(X, Y, lambda_all)
#print(beta_all)
matplot(
  t(matrix(rep(1, p), nrow = 1) %*% abs(beta_all)),
  t(beta_all),
  type = 'l',
  main='Trace of Lassso Solution',
  xlab='N',
  ylab='All beta values'
)



