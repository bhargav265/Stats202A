"""

 Stat 202A - Homework 2
 Author: Bhargav Parsi
 Date : 10/15/2017
 Description: This script implements linear regression 
 using the sweep operator

 INSTRUCTIONS: Please fill in the missing lines of code
 only where specified. Do not change function names, 
 function inputs or outputs. You can add examples at the
 end of the script (in the "Optional examples" section) to 
 double-check your work, but MAKE SURE TO COMMENT OUT ALL 
 OF YOUR EXAMPLES BEFORE SUBMITTING.

 Do not use any of Python's built in functions for matrix 
 inversion or for linear modeling (except for debugging or 
 in the optional examples section).
 
"""

import numpy as np

#from sklearn import linear_model
################################
## Function 1: Sweep operator ##
################################

def mySweep(A, m):
    
    """
    Perform a SWEEP operation on A with the pivot element A[m,m].
    
    :param A: a square matrix (np.array).
    :param m: the pivot element is A[m, m].
    :returns a swept matrix (np.array). Original matrix is unchanged.
    
    FILL IN THE BODY OF THIS FUNCTION BELOW
    """
    
    B = np.copy(A)   
    n,p = B.shape
    
    for k in range(m):
      for i in range(n):
        for j in range(n):
          if i != k and j != k:
            B[i,j] = B[i,j] - B[i,k]*B[k,j]/B[k,k]

      for i in range(n):
        if i != k:
          B[i,k] = B[i,k]/B[k,k]

      for j in range(n):
        if j != k:
          B[k,j] = B[k,j]/B[k,k]

      B[k,k] = (-1.0)/B[k,k]




    
    ## The function outputs the matrix (np.array) B
    return(B)
  



########################################################
## Function 2: Linear regression using Sweep operator ##
########################################################

def myLinearRegression(X, Y):
  
  """
  Find the regression coefficient estimates beta_hat
  corresponding to the model Y = X * beta + epsilon
  Your code must use the sweep operator you coded above.
  Note: we do not know what beta is. We are only 
  given a matrix X and a vector Y and we must come 
  up with an estimate beta_hat.
  
  X: an 'n row' by 'p column' matrix (np.array) of input variables.
  Y: an n-dimensional vector (np.array) of responses

  FILL IN THE BODY OF THIS FUNCTION BELOW
  """
  
  ## Let me start things off for you...
  n = X.shape[0]
  p = X.shape[1]
  
  
  z = np.hstack((np.ones(( n, 1)), X, Y))

  a = (z.T).dot(z)
  b = mySweep(a, p+1)
  beta_hat = b[0:p+1, p+1]
  
  
  
  ## Function returns the (p+1)-dimensional vector (np.array) 
  ## beta_hat of regression coefficient estimates
  return beta_hat
  


########################################################
## Optional examples (comment out before submitting!) ##
########################################################
# def testing_Linear_Regression():
#   n = 10
#   p = 3

#   X = np.random.rand(n,p)
#   beta = [1 for i in range(p)]

#   beta = np.array(beta)

#   beta = beta.reshape(p,1)

#   Y = X.dot(beta)
#   Y = np.add(Y,np.random.rand(n,1))

#   regr = linear_model.LinearRegression()

#   regr.fit(X,Y)

#   Py_coef = regr.coef_
#   my_coef = myLinearRegression(X,Y)
#   py_int = regr.intercept_

#   Py_coef = np.hstack((py_int,Py_coef[0,:]))

#   print("coeffcient of python are" + str(Py_coef))
#   #print("intercept of python is" + str(py_int))
#   print("coeffcient of myfunc are" + str(my_coef))
  


  
#   ## This function is not graded; you can use it to 
#   ## test out the 'myLinearRegression' function 

#   ## You can set up a similar test function as was 
#   ## provided to you in the R file.

# testing_Linear_Regression()
