\name{lasso}
\alias{myLasso}
\title{Lasso Regression}
\usage{
myLM(X, Y)
}
\description{
Find the lasso solution path

Find the lasso solution path for various values of the regularization parameter lambda.
Input: 
X: n x p matrix of explanatory variables.
Y: n dimensional response vector
lambda_all: Vector of regularization parameters

Returns a matrix containing the lasso solution vector
beta for each regularization parameter.
}
