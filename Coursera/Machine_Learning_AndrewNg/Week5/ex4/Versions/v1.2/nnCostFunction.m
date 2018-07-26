function [J grad] = nnCostFunction(nn_params, ...
                                   input_layer_size, ...
                                   hidden_layer_size, ...
                                   num_labels, ...
                                   X, y, lambda)
%NNCOSTFUNCTION Implements the neural network cost function for a two layer
%neural network which performs classification
%   [J grad] = NNCOSTFUNCTON(nn_params, hidden_layer_size, num_labels, ...
%   X, y, lambda) computes the cost and gradient of the neural network. The
%   parameters for the neural network are "unrolled" into the vector
%   nn_params and need to be converted back into the weight matrices. 
% 
%   The returned parameter grad should be a "unrolled" vector of the
%   partial derivatives of the neural network.
%

% Reshape nn_params back into the parameters Theta1 and Theta2, the weight matrices
% for our 2 layer neural network
Theta1 = reshape(nn_params(1:hidden_layer_size * (input_layer_size + 1)), ...
                 hidden_layer_size, (input_layer_size + 1));

Theta2 = reshape(nn_params((1 + (hidden_layer_size * (input_layer_size + 1))):end), ...
                 num_labels, (hidden_layer_size + 1));

% Setup some useful variables
m = size(X, 1);
         
% You need to return the following variables correctly 
J = 0;
Theta1_grad = zeros(size(Theta1));
Theta2_grad = zeros(size(Theta2));

% ====================== YOUR CODE HERE ======================
% Instructions: You should complete the code by working through the
%               following parts.
%
% Part 1: Feedforward the neural network and return the cost in the
%         variable J. After implementing Part 1, you can verify that your
%         cost function computation is correct by verifying the cost
%         computed in ex4.m
%
% Part 2: Implement the backpropagation algorithm to compute the gradients
%         Theta1_grad and Theta2_grad. You should return the partial derivatives of
%         the cost function with respect to Theta1 and Theta2 in Theta1_grad and
%         Theta2_grad, respectively. After implementing Part 2, you can check
%         that your implementation is correct by running checkNNGradients
%
%         Note: The vector y passed into the function is a vector of labels
%               containing values from 1..K. You need to map this vector into a 
%               binary vector of 1's and 0's to be used with the neural network
%               cost function.
%
%         Hint: We recommend implementing backpropagation using a for-loop
%               over the training examples if you are implementing it for the 
%               first time.
%
% Part 3: Implement regularization with the cost function and gradients.
%
%         Hint: You can implement this around the code for
%               backpropagation. That is, you can compute the gradients for
%               the regularization separately and then add them to Theta1_grad
%               and Theta2_grad from Part 2.
%#############################################
% part 1: Feedforward the neural network
%#############################################
%Add a column of 1's to X (the first column), and it becomes 'a1'.
a1 = [ones(m,1), X];
s1 = size(a1);
 
 %Multiply by Theta1 and you have 'z2'.
 z2 = a1 * Theta1' ;
 s2 = size(z2);

%Compute the sigmoid() of 'z2', then add a column of 1's, and it becomes 'a2'
a2 = sigmoid(z2);
a2 = [ones(m,1), a2];
s3 = size(a2);


 %Multiply by Theta2 and you have 'z3'.
 z3 = a2 * Theta2' ;
 s4 = size(z3);


%Compute the sigmoid() of 'z3', then add a column of 1's, and it becomes 'a3'
a3 = sigmoid(z3);
s5 = size(a3);
h  = a3;

% Tranform the y result vector into a matrix where 1s in the columns map to the corresponding values of y
y_matrix = eye(num_labels)(y,:) ;
s6 = size(y_matrix);

t = y_matrix .* log(h);
p = (1 - y_matrix) .* log(1-h);
J = -1/m * sum(sum(t + p));
     
     
     
% Implementing regularization
reg1 =  sum(sum(Theta1(:,2:size(Theta1,2)).^ 2));
reg2 =  sum(sum(Theta2(:,2:size(Theta2,2)).^ 2));
reg3 = reg1 + reg2;

reg = reg3 * lambda/(2 * m);
reg

J = J + reg;
J
%#############################################
% part 2: Backpropagation
%#############################################
% Step -1 : Feedforward
%  a1: 5000x401
%  z2: 5000x25
%  a2: 5000x26
%  a3: 5000x10
%  d3: 5000x10
%  d2: 5000x25
%  Theta1, Delta1 and Theta1grad: 25x401
%  Theta2, Delta2 and Theta2grad: 10x26

a1 = [ones(m,1), X];
z2 = a1 * Theta1' ;
a2 = sigmoid(z2);
a2 = [ones(m,1), a2];
z3 = a2 * Theta2' ;
a3 = sigmoid(z3);

% Step -2 For Output Layer

d3 = a3 - y_matrix;

% Step -3 For Hidden Layer

d2 = (Theta2(:,2:end)' * d3')' .* sigmoidGradient(z2);

% Step -4 Accumulate the Gradient
Theta1_grad = Theta1_grad + d2' * a1;
Theta2_grad = Theta2_grad + d3' * a2;

% Step -5 Get Unregularized Gradient
Theta1_grad = (1/m) * Theta1_grad;
Theta2_grad = (1/m) * Theta2_grad;

end
