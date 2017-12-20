#########################################################
## Stat 202A - Final Project
## Author: Bhargav Parsi
## Date : 12/12/2017
## Description: This script implements a two layer neural network in Tensorflow
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

############################################################################
## Implement a two layer neural network in Tensorflow to classify MNIST digits ##
############################################################################

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Train a two layer neural network to classify the MNIST dataset ##
## Use Relu as the activation function for the first layer. Use Softmax as the activation function for the second layer##
## z=Relu(x*W1+b1) ##
## y=Softmax(z*W2+b2)##
# Use cross-entropy as the loss function#
# Tip: be careful when you initialize the weight and bias parameters.
## You only need to install the CPU version of Tensorflow on your laptop, which is much easier.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import argparse
import sys
import os
os.environ["CUDA_VISIBLE_DEVICES"]=""

from tensorflow.examples.tutorials.mnist import input_data
import tensorflow as tf
# import numpy as np
# import matplotlib.pyplot as plt

FLAGS = None


def main(_):
  # Import data
  mnist = input_data.read_data_sets(FLAGS.data_dir, one_hot=True)

  # Create the model
  #######################
  ## FILL IN CODE HERE ##
  #######################
  tf.reset_default_graph() 
  n_nodes_hl1 = 300
  n_nodes_hl2 = 300
  n_nodes_hl3 = 500

  n_classes = 10
  batch_size = 100

  x = tf.placeholder('float',[None,784])
  y = tf.placeholder('float',)

  def neural_network_model(data):
      #hidden_1_layer = {'weights' : tf.Variable(tf.random_normal([784, n_nodes_hl1], stddev=0.03)),
       #                 'biases':tf.Variable(tf.random_normal([n_nodes_hl1]))}
      hidden_1_layer = {'weights' : tf.get_variable("W1", shape=[784, n_nodes_hl1],
                                 initializer=tf.contrib.layers.xavier_initializer()),
                        'biases':tf.get_variable("b1", shape=[n_nodes_hl1],
             initializer=tf.contrib.layers.xavier_initializer())}
      
      
      
      #hidden_2_layer = {'weights' : tf.Variable(tf.random_normal([n_nodes_hl1, n_nodes_hl2])),
       #                 'biases':tf.Variable(tf.random_normal([n_nodes_hl2]))}
     # hidden_3_layer = {'weights' : tf.Variable(tf.random_normal([n_nodes_hl2, n_nodes_hl3])),
      #                  'biases':tf.Variable(tf.random_normal([n_nodes_hl3]))}
      #output_layer = {'weights' : tf.Variable(tf.random_normal([n_nodes_hl1, n_classes], stddev=0.03)),
       #                 'biases':tf.Variable(tf.random_normal([n_classes]))}
      output_layer = {'weights' : tf.get_variable("W2", shape=[n_nodes_hl1, n_classes],
                                 initializer=tf.contrib.layers.xavier_initializer()),
                        'biases':tf.get_variable("b2", shape=[n_classes],
             initializer=tf.contrib.layers.xavier_initializer())}
      l1 = tf.add(tf.matmul(data, hidden_1_layer['weights']),hidden_1_layer['biases'])
      l1 = tf.nn.relu(l1)
      
      #l2 = tf.add(tf.matmul(l1, hidden_2_layer['weights']),hidden_2_layer['biases'])
      #l2 = tf.nn.softmax(l2)
      
      #l3 = tf.add(tf.matmul(l2, hidden_3_layer['weights']),hidden_3_layer['biases'])
      #l3 = tf.nn.relu(l3)
      
      output = tf.nn.softmax(tf.add(tf.matmul(l1, output_layer['weights']),output_layer['biases']))
      
      return output
  def train_neural_network(x):
      prediction = neural_network_model(x)
      cost = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits = prediction,labels = y))
      
      optimizer = tf.train.AdamOptimizer().minimize(cost)
      
      hm_epochs = 10

      # acc_train=np.repeat(0.,hm_epochs)
      # acc_test=np.repeat(0.,hm_epochs)
      
      with tf.Session() as sess:
          sess.run(tf.initialize_all_variables())
          for epoch in range(hm_epochs):
              epoch_loss = 0
              for i in range(int(mnist.train.num_examples/batch_size)):
                  epoch_x,epoch_y = mnist.train.next_batch(batch_size)
                  _,c = sess.run([optimizer,cost], feed_dict = {x:epoch_x, y:epoch_y})
                  epoch_loss+=c
              print('Epoch',epoch, 'completed out of', hm_epochs, 'loss:',epoch_loss)
              correct = tf.equal(tf.argmax(prediction,1),tf.argmax(y,1))
          
              accuracy = tf.reduce_mean(tf.cast(correct,'float'))
              print('Accuracy: ',accuracy.eval({x:mnist.test.images, y:mnist.test.labels}))
              # acc_test[epoch] = accuracy.eval({x:mnist.test.images, y:mnist.test.labels})
              # acc_train[epoch] = accuracy.eval({x:mnist.train.images, y:mnist.train.labels})
          #X_train, Y_train, X_test, Y_test = prepare_data()
#alpha,beta,acc_train,acc_test=my_NN(X_train,Y_train,X_test,Y_test,num_hidden=50,num_iterations=1000,learning_rate=1e-2)
          # plt.axis([0,10,0,1.3])
          # plt.plot(acc_train, label="Training Accuracy")
          # plt.plot(acc_test, label="Testing Accuracy")  

          # plt.legend()
          # plt.title('2 Layer Neural Network TF')
          # plt.show()

          
  train_neural_network(x)










if __name__ == '__main__':
  parser = argparse.ArgumentParser()
  parser.add_argument('--data_dir', type=str, default='/tmp/tensorflow/mnist/input_data',
                      help='Directory for storing input data')
  FLAGS, unparsed = parser.parse_known_args()
  tf.app.run(main=main, argv=[sys.argv[0]] + unparsed)




