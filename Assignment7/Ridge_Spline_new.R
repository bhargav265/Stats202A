#############################################################
## Stat 202A - Homework 6
## Author: Aashna Agarwal
## Date : Nov 14
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
#library("Sweep.cpp")
#sourceCpp("Sweep.cpp")

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

  R = A
  n = dim(A)[1]
  m = dim(A)[2]

  Q = diag(rep(1,n))

  for(k in 1:(m-1))
  {
    X = matrix(0,n,1)
    X[k:n,1] = R[k:n,k]
    V = X
    V[k] = X[k] + sign(X[k,1])*norm(X, type = "F")
    S = norm(V, type = "F")
    u = V/S
    R = R - 2 * (u %*% (t(u) %*% R))
    Q = Q - 2 * (u %*% (t(u) %*% Q))
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

  ########################
  ## FILL IN CODE BELOW ##
  ########################

  B = A
  n = dim(B)[1]


  for(k in 1:m) {

    for(i in 1:n) {
      for(j in 1:n) {
        if(i!=k & j!=k)
          B[i,j] = B[i,j] - B[i,k]*B[k,j]/B[k,k]
      }
    }

    for(i in 1:n) {
      if(i!=k)
        B[i,k] = B[i,k]/B[k,k]
    }

    for(j in 1:n) {
      if(j!=k)
        B[k,j] = B[k,j]/B[k,k]
    }

    B[k,k] = -1/B[k,k]

  }

  ## The function outputs the matrix B
  return(B)

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

  n = dim(X)[1]
  p = dim(X)[2]
  Z = cbind(rep(1, n), X, Y)
  A = t(Z) %*% Z
  D = diag(rep(lambda, p+2))
  D[1, 1] = 0
  D[p+2, p+2] = 0
  A = A + D
  S = mySweep(A, p+1)
  beta_ridge = S[1:(p+1), p+2]

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
  n = dim(x)[1]
  temp = matrix(x, ncol = n)
  for (k in (1:(p-1))/p){
    x = cbind(x, t((temp>k)*(temp-k)))
  }
  x


  beta_spline = myRidge(x, Y, lambda)
  Yhat = cbind(rep(1, n), x)%*%beta_spline

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


error <- function(Yhat,Y)
{
  return (mean((Yhat-Y)^2))
}


n = 10
n_train = ceiling(2*n/3)
n_test = n-n_train
p = 3
sigma = .1
lambda = 10.
x = runif(n)

train_ind =sample(1:n,n_train)
test_ind = setdiff(1:n,train_ind)

#x_train1 = sample(x, size = n_train, replace = FALSE)
#x_train = sort(x_train1)
x_train = sort(x[train_ind]) # x
y_train = x_train^2 + rnorm(n_train)*sigma #n
Y_train = matrix(y_train, nrow=n_train) #n
x_test = sort(x[test_ind])
#x_test1 = setdiff(x,x_train1)
#x_test = sort(x_test1)
y_test = x_test^2 + rnorm(n_test)*sigma
Y_test = matrix(y_test, nrow=n_test)
X_train = matrix(x_train, nrow=n_train)
X_test = matrix(x_test, nrow=n_test)
error_test = NULL
error_train = NULL

for (k in (1:(p-1))/p){
  X_test = cbind(X_test, (x_test>k)*(x_test-k))
}

Y_total=NULL
for(lambda in 1:100){
  Y_total=mySpline(X_train, Y_train, lambda, p)

  beta_spline = Y_total[[1]]
  Yhat_train = Y_total[[2]]

  Yhat_test = cbind(rep(1, n_test), X_test)%*%beta_spline

  error_train = append(error_train,error(Yhat_train,Y_train))
  error_test = append(error_test,error(Yhat_test,Y_test))
}
lambda1 = 1:100

plot(lambda1,error_train,col="red",xlab="Lambda",ylab="Error",type="p")
par(new = TRUE)
plot(lambda1,error_test,col="green",xlab="Lambda",ylab="Error",type="p")
