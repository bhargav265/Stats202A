############################################################# 
## Stat 202A - Homework 1
## Author: Bhargav Parsi
## Date : 10/12/2017
## Description: This script implements linear regression 
## using Gauss-Jordan elimination in both plain and
## vectorized forms
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
##
## Do not use the following functions for this assignment,
## except when debugging or in the optional examples section:
## 1) lm()
## 2) solve()
#############################################################


###############################################
## Function 1: Plain version of Gauss Jordan ##
###############################################


myGaussJordan <- function(A, m){
  
  # Perform Gauss Jordan elimination on A.
  # 
  # A: a square matrix.
  # m: the pivot element is A[m, m].
  # Returns a matrix with the identity matrix 
  # on the left and the inverse of A on the right. 

  #############################################
  ## FILL IN THE BODY OF THIS FUNCTION BELOW ##
  #############################################
  n = dim(A)[1]
  B = cbind(A,diag(rep(1,n)))

  for (k in 1:m)
  {
    a = B[k,k]
    for (j in 1 : (n*2))
    {
      B[k,j] <- B[k,j]/a
    }

    for (i in 1:n)
    {
      if (i!=k) {
        b <- B[i,k]
        for (j in 1:(n*2))
        {
          B[i,j] = B[i,j] - b*B[k,j]
        }
      }
      
    }

  }

  

  
  
  ## Function returns the matrix B
  return(B)
  
}

####################################################
## Function 2: Vectorized version of Gauss Jordan ##
####################################################

myGaussJordanVec <- function(A, m){
  
  # Perform Gauss Jordan elimination on A.
  # 
  # A: a square matrix.
  # m: the pivot element is A[m, m].
  # Returns a matrix with the identity matrix 
  # on the left and the inverse of A on the right.
  
  #############################################
  ## FILL IN THE BODY OF THIS FUNCTION BELOW ##
  #############################################
  n = dim(A)[1]
  B = cbind(A,diag(rep(1,n)))

  for (k in 1:m)
  {
    
    B[k,] <- B[k,]/B[k,k]
    

    for (i in 1:n)
    {
      if (i!=k)
      {
        B[i,] = B[i,] - B[k,]*B[i,k]


      }
      
    }

  }
  

  
  
  
  
  ## Function returns the matrix B
  return(B)
  
}



######################################################
## Function 3: Linear regression using Gauss Jordan ##
######################################################

myLinearRegression <- function(X, Y){
  
  # Find the regression coefficient estimates beta_hat
  # corresponding to the model Y = X * beta + epsilon
  # Your code must use one of the 2 Gauss Jordan 
  # functions you wrote above (either one is fine).
  # Note: we do not know what beta is. We are only 
  # given a matrix X and a vector Y and we must come 
  # up with an estimate beta_hat.
  # 
  # X: an 'n row' by 'p column' matrix of input variables.
  # Y: an n-dimensional vector of responses

  #############################################
  ## FILL IN THE BODY OF THIS FUNCTION BELOW ##
  #############################################
  
  ## Let me start things off for you...
  n <- nrow(X)
  p <- ncol(X)
  Z = cbind(rep(1,n),X,Y)

  A = t(Z) %*% Z

  S = myGaussJordanVec(A,p+1)
  
  
  beta_hat = S[1:(p+1),(p+2)]
  
  
  
  ## Function returns the (p+1)-dimensional vector 
  ## beta_hat of regression coefficient estimates
  return(beta_hat)
  
}

########################################################
## Optional examples (comment out before submitting!) ##
########################################################

# testing_Linear_Regression <- function(){
#   
#   ## This function is not graded; you can use it to 
#   ## test out the 'myLinearRegression' function 
# 
#   ## Define parameters
#   n    <- 100
#   p    <- 3
#   
#   ## Simulate data from our assumed model.
#   ## We can assume that the true intercept is 0
#   X    <- matrix(rnorm(n * p), nrow = n)
#   beta <- matrix(1:p, nrow = p)
#   Y    <- X %*% beta + rnorm(n)
#   
#   ## Save R's linear regression coefficients
#   R_coef  <- coef(lm(Y ~ X))
#   
#   ## Save our linear regression coefficients
#   my_coef <- myLinearRegression(X, Y)
#   
#   ## Are these two vectors different?
#   print(R_coef)
#   print(my_coef)
#   sum_square_diff <- sum((R_coef - my_coef)^2)
#   if(sum_square_diff <= 0.001){
#     return('Both results are identical')
#   }else{
#     return('There seems to be a problem...')
#   }
#   
# }
# 
# print(testing_Linear_Regression())
# 
