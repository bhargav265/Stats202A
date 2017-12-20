#' Package to do Statistical Computing 
#' 
#' Functions available: Linear Regression, Logistic Regression, Sweep Operation,
#' QR Decomposition, Lasso Rigression and Ridge Regression.

#' 
"_PACKAGE"



#' QR Decomposition
#'
#' Performs QR decomposition on the matrix A.
#' Function should output a list with Q.transpose and R
#' Q is an orthogonal n x n matrix
#' R is an upper triangular n x m matrix
#' Q and R satisfy the equation: A = Q %*% R
#' @param A->
#' an n x m matrix
#' @export
#' @examples
#' myQR(A)

myQR <- function(A){
  
 
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

###############################################
## Function 2: Linear regression based on QR ##
###############################################

#' Linear Regression
#'
#' Performs the linear regression of Y on X. 
#' Function returns the 1 x (p + 1) vector beta_ls, the least squares solution vector.
#' @param X->
#'  X is an n x p matrix of explanatory variables
#' @param Y->
#'  Y is an n dimensional vector of responses
#' @export
#' @examples
#'  X    <- matrix(rnorm(n * p), nrow = n)
#'  beta <- matrix(1:p, nrow = p)
#'  Y    <- X %*% beta + rnorm(n)
#'  myLM(X,Y)

myLM <- function(X, Y){
  
  ## Performs the linear regression of Y on X
  ## Input: 
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of responses
  ## Use myQR inside of this function
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################  
  
  n <- nrow(X)
  p <- ncol(X)
  
  ## Stack (X, Y) and solve it by QR decomposition
  Z <- cbind(X, Y)
  R <- myQR(Z)$R
  
  R1 <- R[1:(p + 0), 1:(p + 0)]
  Y1 <- R[1:(p + 0), p + 1]
  
  beta_ls <- solve(R1) %*% Y1
  
  ## Function returns the 1 x (p + 1) vector beta_ls, 
  ## the least squares solution vector
  return(beta_ls)
  
}

######################################
## Function 3: Logistic regression  ##
######################################

## Expit/sigmoid function
# expit <- function(x){
#   1 / (1 + exp(-x))
# }

#' Logistic Regression
#'
#' Performs the logistic regression of Y on X
#' using iterated reweighted least squares obtained from linear regression based on QR decomposition.
#' Function returns the logistic regression solution vector.
#' @param X -> is an n x p matrix of explanatory variables
#' @param Y -> is an n dimensional vector of binary responses
#' @examples
#' X    <- matrix(rnorm(n * p), nrow = n)
#' beta <- matrix(rep(0, p), nrow = p)
#' Y    <- 1 * (runif(n) < (1 / (1 + exp(X %*% beta))))
#' logistic_beta <- myLogistic(X, Y)

myLogistic <- function(X, Y){

  ## Perform the logistic regression of Y on X
  ## Input: 
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of binary responses
  ## Use myLM (or myLMC) inside of this function
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################
  n <- nrow(X)
  p <- ncol(X)
  beta <- matrix(rep(0, p), nrow = p)
  epsilon <- 1e-8
  repeat
  {
    eta <- X%*%beta
    pr <- (1 / (1 + exp(-eta)))
    w <- pr*(1-pr)
    Z <- eta + (Y-pr)/w
    sw <- sqrt(w)
    mw <- matrix(sw, n, p)
    Xwork <- mw*X
    Ywork <- sw*Z
    beta_new <- myLM(Xwork,Ywork)
    beta_new <- beta_new[0:-1]
    err <- sum(abs(beta_new-beta))
    beta <- beta_new
    if (err<epsilon)
      break
  }
  return(beta)
  
  
  ## Function returns the logistic regression solution vector
  beta  
    
}

#' Principle Component Analysis
#'
#' Performs PCA on matrix A using  QR Decomposition .
#' Function should output a list with D and V. 
#' D is a vector of eigenvalues of A. 
#' V is the matrix of eigenvectors of A (in the 
#' same order as the eigenvalues in D.)
#' @param A-> Square matrix
#' @param numIter-> Number of iterations
#' @export
#' @examples
#' X = matrix(runif(n*p), nrow = n)
#' n = dim(X)[1]
#' p = dim(X)[2]
#' output_qr = myEigen_QR(t(X) %*% X, 1000)


myEigen_QR <- function(A, numIter = 1000){
  
  ## Perform PCA on matrix A using your QR function, myQR
  ## Input:
  ## A: Square matrix
  ## numIter: Number of iterations
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################  
  
  r <- nrow(A)
  c <- ncol(A)
  V <- matrix(runif(r * r), r, r)
  
  for(i in 1:numIter){
    Q <- myQR(V)$Q
    V <- A %*% Q
  }
  
  eigen_out <- myQR(V)
  Q <- eigen_out$Q
  R <- eigen_out$R
  
  ## Function should output a list with D and V
  ## D is a vector of eigenvalues of A
  ## V is the matrix of eigenvectors of A (in the 
  ## same order as the eigenvalues in D.)
  return(list("D" = diag(R), "V" = Q))
  
}

#' Lasso Regression
#'
#' Finds the lasso solution path for various values of 
#' the regularization parameter lambda. Returns a matrix containing the lasso solution vector 
#' beta for each regularization parameter.
#' @param X-> n x p matrix of explanatory variables.
#' @param Y: n dimensional response vector
#' @param lambda_all: Vector of regularization parameters.
#' @export
#' @examples
#'  n = 50
#'  p = 200
#'  s=10
#'  X = matrix(rnorm(n*p), nrow=n)
#'  X = cbind(rep(1,n), X)
#'  lambda_all = (0.1:100)/10
#'  beta_true = matrix(rep(0, p+1), nrow = p+1)
#'  beta_true[1:s] = 1:s
#'  Y = X %*% beta_true + rnorm(n)
#'  beta_all = myLasso(X,Y,lambda_all)


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
  lambda_all = sort(lambda_all, decreasing=TRUE)
  
  # Parameters
  p = dim(X)[2]
  L = length(lambda_all)
  
  # Constants
  T = 10
  
  # Beta
  beta = matrix(rep(0, p), nrow = p)
  beta_all = matrix(rep(0, p* L), nrow = p)

  R = Y
  ss = rep(0, p)
  for(j in 1 : p){
    ss[j] = sum(X[, j]^2)
  }

  for (l in 1:L){
    lambda = lambda_all[l]
    for (t in 1:T){
      for (j in 2:p){
        db = sum(R * X[ , j]) / ss[j]
        b = beta[j] + db
        b = sign(b) * max(0, abs(b) - lambda / ss[j])
        db = b - beta[j]
        R = R - X[ , j] * db
        beta[j] = b
      }
    }
    beta_all[ , l] = beta
  }  
  
  
  ## Function should output the matrix beta_all, the 
  ## solution to the lasso regression problem for all
  ## the regularization parameters. 
  ## beta_all is (p+1) x length(lambda_all)
  return(beta_all)
  
}

#' Sweep Operator
#'
#' Perform a SWEEP operation on A with the pivot element A[m,m].
#' Returns a swept matrix B (which is m by m).
#' @param A-> Square matrix
#' @param m-> the pivot element is A[m, m].
#' @export

mySweep <- function(A, m){
  
  # Perform a SWEEP operation on A with the pivot element A[m,m].
  # 
  # A: a square matrix.
  # m: the pivot element is A[m, m].
  # Returns a swept matrix B (which is m by m).
  
  #############################################
  ## FILL IN THE BODY OF THIS FUNCTION BELOW ##
  #############################################
  n <- dim(A)[1]
  for(k in 1:m) 
  {
    for(i in 1:n) 
    {
      for (j in 1:n)
      {
        if(i != k & j!= k)
          A[i,j] = A[i,j] - A[i,k] * A[k,j]/A[k,k]
      }
    }
    
    for (i in 1:n)
    {
      if(i != k) {
        A[i,k] = A[i,k]/A[k,k]
      }
    }

    for (j in 1:n)
    {
      if(j != k) {
        A[k,j] = A[k,j]/A[k,k]
      }
    }

    A[k,k] <- -1/A[k,k]
  }
  
  ## The function outputs the matrix B
  return(A)
  
}


#' Ridge Regression
#'
#' Perform ridge regression of Y on X. Returns beta, the ridge regression solution.
#' @param X-> a matrix of explanatory variables.
#' @param Y-> a vector of dependent variables. Y can also be a 
#' matrix, as long as the function works.
#' @param lambda-> regularization parameter (lambda >= 0)
#' @export
#' @examples
#'  my.data <- trees
#'  X <- as.matrix(trees[,1:2])
#'  Y <- trees$Volume
#'  beta_ridge <- myRidge(X, Y, 0)
#' 
#' 


myRidge <- function(X, Y, lambda){
  
  # Perform ridge regression of Y on X.
  # 
  # X: a matrix of explanatory variables.
  # Y: a vector of dependent variables. Y can also be a 
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

  D = diag(rep(lambda, p + 2))
  D[p+2, p+2] = 0
  D[1,1] = 0
  A = A + D

  S = mySweep(A, p + 1)
  beta_ridge = S[1:(p+1), p+2]
  
  ## Function should output the vector beta_ridge, the 
  ## solution to the ridge regression problem. Beta_ridge
  ## should have p + 1 elements.
  return(beta_ridge)
  
}
