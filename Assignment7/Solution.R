############################################################# 
## Stat 202A - Homework 6
## Author: 
## Date : 
## Description: This script implements ridge regression as 
## well as piecewise linear spline regression.
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

## Source your Rcpp file (put in the name of your 
## Rcpp file)
library(Rcpp)
sourceCpp("D:/Downloads(Lenovo)/Downloads/Documents/Scholar Strategy docs/UCLA/Statistical programming/Assignment7/Sweep.cpp")

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
  
  n <- nrow(A)
  m <- ncol(A)
  Q <- diag(n)
  R <- A
  
  for(k in 1:(m - 1)){
    x      <- rep(0, n)
    x[k:n] <- R[k:n, k]
    s      <- -1 * sign(x[k])
    v      <- x
    v[k]   <- x[k] - s * norm(x, type = "2")
    u      <- v / norm(v, type = "2")
    
    R <- R - 2 * u %*% t(u) %*% R
    Q <- Q - 2 * u %*% t(u) %*% Q
    
  }
  
  ## Function should output a list with Q.transpose and R
  ## Q is an orthogonal n x n matrix
  ## R is an upper triangular n x m matrix
  ## Q and R satisfy the equation: A = Q %*% R
  return(list("Q" = t(Q), "R" = R))
  
}


#################################
## Function 2: Sweep operation ##
#################################

mySweep <- function(A, m){
  
  # Perform a SWEEP operation on A with the pivot element A[m,m].
  # 
  # A: a square matrix.
  # m: the pivot element is A[m, m].
  # Returns a swept matrix.
  
  ## Leave this function as is unless you want to make it 
  ## more efficient!
  
  n <- nrow(A)
  
  for(k in 1:m){ 
    for(i in 1:n)     
      for(j in 1:n)   
        if(i != k  & j != k)     
          A[i,j] <- A[i,j] - A[i,k]*A[k,j]/A[k,k]    
        
        for(i in 1:n) 
          if(i != k) 
            A[i,k] <- A[i,k]/A[k,k]  
          
          for(j in 1:n) 
            if(j != k) 
              A[k,j] <- A[k,j]/A[k,k]
            
            A[k,k] <- - 1/A[k,k]
  }
  
  return(A)
  
}


##################################
## Function 3: Ridge regression ##
##################################

myRidge <- function(X, Y, lambda){
  
  # Perform ridge regression of Y on X.
  # 
  # X: an n x p matrix of explanatory variables.
  # Y: an n vector of dependent variables. Y can also be a 
  # matrix, as long as the function works.
  # lambda: regularization parameter (lambda >= 0)
  # Returns beta, the ridge regression solution.
  
  ##################################
  ## FILL IN THIS SECTION OF CODE ##
  ##################################
  
  n <- dim(X)[1]
  p <- dim(X)[2]
  
  ## Define cross product matrix
  Z <- cbind(rep(1, n), X, Y)
  A <- t(Z) %*% Z
  
  ## Add ridge to cross product matrix
  D <- diag(rep(lambda, p + 2))
  D[p + 2, p + 2] <- 0
  
  ## Don't regularize intercept
  D[1, 1] <- 0
  A <- A + D
  S <- mySweepC(A, p + 1)
  beta_ridge <- S[1:(p + 1), p + 2]
  
  ## Function should output the vector beta_ridge, the 
  ## solution to the ridge regression problem. beta_ridge
  ## should have p + 1 elements.
  return(beta_ridge)
  
}


####################################################
## Function 4: Piecewise linear spline regression ##
####################################################


mySpline <- function(x, Y, lambda, p = 100){
  
  # Perform spline regression of Y on X.
  # 
  # x: An n x 1 vector or matrix of explanatory variables.
  # Y: An n x 1 vector of dependent variables. Y can also be a 
  # matrix, as long as the function works.
  # lambda: regularization parameter (lambda >= 0)
  # p: Number of cuts to make to the x-axis.
  
  ##################################
  ## FILL IN THIS SECTION OF CODE ##
  ##################################
  
  n <- length(x)
  X <- matrix(x, nrow = n)
  for(k in (1:(p-1))/p){
    X <- cbind(X, (x > k) * (x - k))
  }
  beta_spline <- myRidge(X, Y, lambda)
  Yhat <- cbind(rep(1,n), X) %*% beta_spline
  # plot(x, Y, ylim = c(-.2, 1.2), col = "red")
  # par(new = TRUE)
  # plot(x, Yhat, ylim = c(-.2, 1.2), type = 'l', col = "green")
  # 
  
  ## Function should a list containing two elements:
  ## The first element of the list is the spline regression
  ## beta vector, which should be p + 1 dimensional (here, 
  ## p is the number of cuts we made to the x-axis).
  ## The second element is y.hat, the predicted Y values
  ## using the spline regression beta vector. This 
  ## can be a numeric vector or matrix.
  output <- list(beta_spline = beta_spline, predicted_y = Yhat)
  return(output)
  
}

# Spline regression code
 n <- 100
 p <- 50
 x <- runif(n)
 x <- sort(x)
sigma <- .1
 lambda <- c(0,0.001, 0.01, 0.1, 1, 10, 50 ,100, 500, 1000)
 Y <- x^2 + rnorm(n) * sigma
 # results <- mySpline(x, Y, lambda)
 # Yhat <- results$predicted_y
 # beta <- results$beta_spline

 X <- matrix(x, nrow = n)
 for(k in (1:(p-1))/p){
   X <- cbind(X, (x>k) * (x-k))
 }
 for (l in lambda){
 beta <- myRidge(X, Y, l)
 Yhat <- cbind(rep(1,n), X) %*% beta
plot(x, Y, ylim = c(-.2, 1.2), col = "red")
par(new = TRUE)
plot(x, Yhat, ylim = c(-.2, 1.2), type = 'l', col = "green")
}
# 
# ## Coordinate descent
n <- 1000
p <- 200
s <- 10
X <- matrix(rnorm(n * p), nrow = n)
beta_true <- matrix(rep(0, p), nrow = p)
beta_true[1:s] <- 1:s
Y <- X %*% beta_true + rnorm(n)
beta <- matrix(rep(0, n), nrow = p)
R <- Y
ss <- rep(0, p)
for(j in 1:p){
  ss[j] <- sum(X[,j]^2)
}

IT <- 100
for(it in 1:IT){
  for(j in 1:p){
    db <- sum(R * X[, j]) / ss[j]
    beta[j] <- beta[j] + db
    R <- R - X[, j] * db
  }
}


