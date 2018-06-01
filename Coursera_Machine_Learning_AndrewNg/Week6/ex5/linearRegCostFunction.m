function [J, grad] = linearRegCostFunction(X, y, theta, lambda)
%LINEARREGCOSTFUNCTION Compute cost and gradient for regularized linear 
%regression with multiple variables
%   [J, grad] = LINEARREGCOSTFUNCTION(X, y, theta, lambda) computes the 
%   cost of using theta as the parameter for linear regression to fit the 
%   data points in X and y. Returns the cost in J and the gradient in grad

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 
J = 0;
grad = zeros(size(theta));

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost and gradient of regularized linear 
%               regression for a particular choice of theta.
%
%               You should set J to the cost and grad to the gradient.
%
% The hypothesis is a vector, formed by multiplying the X matrix and the theta vector.
h = X * theta;

% The "errors vector" is the difference between the 'h' vector and the 'y' vector.
error = (h - y);

%The third line of code will compute the square of each of those error terms (using element-wise exponentiation),
error_sqr = (error .^2);

%compute the sum of the error_sqr vector, and scale the result (multiply) by 1/(2*m). That completed sum is the cost value J.
sqrErrors   = (1/2)* (1/m) * sum(error_sqr); 

% Note that you should not regularize the θ0 term. 
theta(1) = 0;
reg = lambda * (1/2) * (1/m) * sum(theta .^2);

% Calculate Cost
J = (sqrErrors + reg);

% =========================================================================
% calculate gradient
% The change in theta (the "gradient") is the sum of the product of X and the "errors vector", scaled by alpha and 1/m. 
grad1 = (1/m) * (X' * error);

% Add reg 
grad_reg = (lambda) * (1/m) * theta;
grad =  (grad1 + grad_reg);

grad = grad(:);

end
