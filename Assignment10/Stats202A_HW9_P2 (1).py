#########################################################
## Stat 202A - Homework 9
## Author: Bhargav Parsi
## Date : 12/8/2017
## Description: This script implements a support vector machine, an adaboost classifier/12
#########################################################

#############################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names,
## function inputs or outputs. You can add examples at the
## end of the script (in the "Optional examples" section) to
## double-check your work, but MAKE SURE TO COMMENT OUT ALL
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not use the function "os.chdir" anywhere
## in your code. If you do, I will be unable to grade your
## work since Python will attempt to change my working directory
## to one that does not exist.
#############################################################
import numpy as np
import sklearn.datasets as ds
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt


def prepare_data(valid_digits=np.array((6, 5))):
    ## valid_digits is a vector containing the digits
    ## we wish to classify.
    ## Do not change anything inside of this function
    if len(valid_digits) != 2:
        raise Exception("Error: you must specify exactly 2 digits for classification!")

    data = ds.load_digits()
    labels = data['target']
    features = data['data']

    X = features[(labels == valid_digits[0]) | (labels == valid_digits[1]), :]
    Y = labels[(labels == valid_digits[0]) | (labels == valid_digits[1]),]

    X = np.asarray(map(lambda k: X[k, :] / X[k, :].max(), range(0, len(X))))

    Y[Y == valid_digits[0]] = 0
    Y[Y == valid_digits[1]] = 1

    X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.25, random_state=10)
    Y_train = Y_train.reshape((len(Y_train), 1))
    Y_test = Y_test.reshape((len(Y_test), 1))

    return X_train, Y_train, X_test, Y_test


####################################################
## Function 1: Support vector machine  ##
####################################################

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Train an SVM to classify the digits data ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

def my_SVM(X_train, Y_train, X_test, Y_test, lamb=0.01, num_iterations=200, learning_rate=0.1):
    ## X_train: Training set of features
    ## Y_train: Training set of labels corresponding to X_train
    ## X_test: Testing set of features
    ## Y_test: Testing set of labels correspdonding to X_test
    ## lamb: Regularization parameter
    ## num_iterations: Number of iterations.
    ## learning_rate: Learning rate.

    ## Function should learn the parameters of an SVM.


    n = X_train.shape[0]
    p = X_train.shape[1] + 1
    X_train1 = np.concatenate((np.repeat(1, n, axis=0).reshape((n, 1)), X_train), axis=1)
    Y_train = 2 * Y_train - 1
    beta = np.repeat(0., p, axis=0).reshape((p, 1))

    ntest = X_test.shape[0]
    X_test1 = np.concatenate((np.repeat(1, ntest, axis=0).reshape((ntest, 1)), X_test), axis=1)
    Y_test = 2 * Y_test - 1

    acc_train = np.repeat(0., num_iterations, axis=0)
    acc_test = np.repeat(0., num_iterations, axis=0)

    #######################
    ## FILL IN CODE HERE ##
    #######################
    units = np.ones(shape=(n, 1))
    for i in range(num_iterations):
        s = np.dot(X_train1, beta)
        db = s * Y_train < 1

        temp = db * Y_train
        temp = (temp * X_train1) / n
        dbeta = np.dot(units.transpose(), temp)

        beta = beta + learning_rate * dbeta.transpose()
        beta[1:p] = beta[1:p] - lamb * dbeta.transpose()[1:p]

        acc_train[i] = np.mean(np.sign(s * Y_train))
        acc_test[i] = np.mean(np.sign(np.dot(X_test1, beta) * Y_test))

    ## Function should output 3 things:
    ## 1. The learned parameters of the SVM, beta
    ## 2. The accuracy over the training set, acc_train (a "num_iterations" dimensional vector).
    ## 3. The accuracy over the testing set, acc_test (a "num_iterations" dimensional vector).

    return beta, acc_train, acc_test


######################################
## Function 2: Adaboost ##
######################################

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Use Adaboost to classify the digits data ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

def my_Adaboost(X_train, Y_train, X_test, Y_test, num_iterations=200):
    ## X_train: Training set of features
    ## Y_train: Training set of labels corresponding to X_train
    ## X_test: Testing set of features
    ## Y_test: Testing set of labels correspdonding to X_test
    ## num_iterations: Number of iterations.

    ## Function should learn the parameters of an Adaboost classifier.

    n = X_train.shape[0]
    p = X_train.shape[1]
    threshold = 0.8

    X_train1 = 2 * (X_train > threshold) - 1

    Y_train = 2 * Y_train - 1


    X_test1 = 2 * (X_test > threshold) - 1
    Y_test = 2 * Y_test - 1

    beta = np.repeat(0., p).reshape((p, 1))
    w = np.repeat(1. / n, n).reshape((n, 1))


    weak_results = np.multiply(Y_train, X_train1) > 0


    acc_train = np.repeat(0., num_iterations, axis=0)
    acc_test = np.repeat(0., num_iterations, axis=0)

    #######################
    ## FILL IN CODE HERE ##
    #######################

    for i in range(num_iterations):

        w = w / np.sum(w)

        weighted_weak_results = w[:,0].reshape(n, 1) * weak_results

        weighted_accuracy = np.sum(weighted_weak_results, axis=0)

        e = 1 - weighted_accuracy

        j = np.argmin(e)


        dbeta = 0.5 * np.log((1 - e[j]) / e[j])

        beta[j] = beta[j] + dbeta


        w = w * np.exp((-1 * Y_train) * X_train1[:, j].reshape(n, 1) * dbeta)


        acc_train[i] = np.mean(np.sign(np.dot(X_train1, beta)) == Y_train)
        acc_test[i] = np.mean(np.sign(np.dot(X_test1, beta)) == Y_test)



    ## Function should output 3 things:
    ## 1. The learned parameters of the adaboost classifier, beta
    ## 2. The accuracy over the training set, acc_train (a "num_iterations" dimensional vector).
    ## 3. The accuracy over the testing set, acc_test (a "num_iterations" dimensional vector).
    return beta, acc_train, acc_test


############################################################################
## Testing your functions and visualize the results here##
############################################################################

X_train, Y_train, X_test, Y_test = prepare_data()
#print Y_train[1]

####################################################
## Optional examples (comment out your examples!) ##
####################################################

# x, y, z = my_SVM(X_train, Y_train, X_test, Y_test)
# plt.plot(y,'--',color = 'red', label = 'Training Accuracy')
# plt.plot(z, color = 'blue', label = 'Testing Accuracy')
# plt.xlabel('Iteration')
# plt.ylabel('Accuracy') 
# plt.legend(loc="upper right")
# plt.axis([-5,250,0,1.2])
# plt.title('SVM')
# plt.show()