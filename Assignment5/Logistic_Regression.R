#########################################################
## Stat 202A - Homework 4
## Author: Bhargav Parsi
## Date : 11/2/2017
## Description: This script implements logistic regression
## using iterated reweighted least squares using the code 
## we have written for linear regression based on QR 
## decomposition
#########################################################

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


################ Optional ################ 
## If you are using functions from Rcpp, 
## uncomment and adjust the following two 
## lines accordingly. Make sure that your 
## cpp file is in the current working
## directory, so you do not have to specify
## any directories in sourceCpp(). For 
## example, do not write
## sourceCpp('John_Doe/Homework/Stats202A/QR.cpp')
## since I will not be able to run this.

## library(Rcpp)
## sourceCpp(name_of_cpp_file)

##################################
## Function 1: QR decomposition ##
##################################

myQR <- function(A){
  
  ## Perform QR decomposition on the matrix A
  ## Input: 
  ## A, an n x m matrix
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################  
  n=nrow(A)
  p=ncol(A)
  Q=matrix(nrow=n,ncol=p)
  R=matrix(nrow=n,ncol=p)
  U=matrix(0,nrow=n,ncol=p)
  U[,1]=A[,1]
  for (k in 2:p)
  {
    projection =matrix(0,nrow=n,ncol=1)
    for(i in 1:(k-1))
    {
      a=sum(A[,k]*U[,i])
      b=sum(U[,i]*U[,i])
      c=a/b
      ck__ =c*U[,i]
      projection = projection + ck__
    }  
    U[,k]=A[,k]-projection[,1]
  }
  sqa = matrix(0,n,1)
  tempo =matrix(0,nrow=n,ncol=1)
  Q=matrix(0,nrow=n,ncol=p)
  for(i in 1:p)
  {
    tempo =U[,i]*U[,i]
    sqa[i,1]=sqrt(sum(tempo))
    Q[,i]=U[,i]/sqa[i,1]
  }
  R= t(Q) %*% A
  result <- list(Q, R)
  names(result) <- c("Q", "R")
  result
  
  
  ## Function should output a list with Q.transpose and R
  ## Q is an orthogonal n x n matrix
  ## R is an upper triangular n x m matrix
  ## Q and R satisfy the equation: A = Q %*% R
  
}

###############################################
## Function 2: Linear regression based on QR ##
###############################################

myLM <- function(X, Y){
  
  ## Perform the linear regression of Y on X
  ## Input: 
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of responses
  ## Use myQR (or myQRC) inside of this function
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################  
   n=nrow(X)
  p=ncol(X)
  Z = cbind(X, Y)
  R = myQR(Z)$R
  R1 = R[1:(p), 1:(p)]
  Y1 = R[1:(p), p+1]
  #beta = mysolve(R1, Y1)
  
  beta_ls = solve(R1, Y1)
  
  
  
  ## Function returns the least squares solution vector
  return(beta_ls)
  
}

######################################
## Function 3: Logistic regression  ##
######################################

## Expit/sigmoid function
expit <- function(x){
  1 / (1 + exp(-x))
}

myLogistic <- function(X, Y){

  ## Perform the logistic regression of Y on X
  ## Input: 
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of binary responses
  ## Use myLM (or myLMC) inside of this function
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################
  
  n=nrow(X)
  p=ncol(X)
  
  beta = matrix(rep(0,p), nrow = p)
  
  epsilon = 1e-6
  
  repeat
  {
    ETA = X %*% beta
    pr = expit(ETA)
    z = ETA + (Y - pr)/(pr*(1-pr))
    sqw = sqrt(pr*(1-pr))
    mw = matrix(sqw, n, p)
    xw = mw*X
    yw = sqw*z
    
    beta_n = myLM(xw, yw)
    error = sum(abs(beta_n - beta))
    beta = beta_n
    if (error < epsilon)
      break
    
    
    
  }
  
  
  
  
  ## Function returns the logistic regression solution vector
  return (beta)  
    
}


# Optional testing (comment out!)
# n <- 100
# p <- 5
# 
# X    <- matrix(rnorm(n * p), nrow = n)
# beta <- rnorm(p)
# Y    <- 1 * (runif(n) < expit(X %*% beta))
# 
# logistic_beta <- myLogistic(X, Y)
# print(logistic_beta)
# print(glm(formula = Y ~ X + 0,  family=binomial("logit")))
