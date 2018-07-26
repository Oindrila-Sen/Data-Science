function [J, grad] = cofiCostFunc(params, Y, R, num_users, num_movies, ...
                                  num_features, lambda)
%COFICOSTFUNC Collaborative filtering cost function
%   [J, grad] = COFICOSTFUNC(params, Y, R, num_users, num_movies, ...
%   num_features, lambda) returns the cost and gradient for the
%   collaborative filtering problem.
%

% Unfold the U and W matrices from params
X = reshape(params(1:num_movies*num_features), num_movies, num_features);
Theta = reshape(params(num_movies*num_features+1:end), ...
                num_users, num_features);

            
% You need to return the following values correctly
J = 0;
X_grad = zeros(size(X));
Theta_grad = zeros(size(Theta));

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost function and gradient for collaborative
%               filtering. Concretely, you should first implement the cost
%               function (without regularization) and make sure it is
%               matches our costs. After that, you should implement the 
%               gradient and use the checkCostFunction routine to check
%               that the gradient is correct. Finally, you should implement
%               regularization.
%
% Notes: X - num_movies  x num_features matrix of movie features
%        Theta - num_users  x num_features matrix of user features
%        Y - num_movies x num_users matrix of user ratings of movies
%        R - num_movies x num_users matrix, where R(i, j) = 1 if the 
%            i-th movie was rated by the j-th user
%
% You should set the following variables correctly:
%
%        X_grad - num_movies x num_features matrix, containing the 
%                 partial derivatives w.r.t. to each element of X
%        Theta_grad - num_users x num_features matrix, containing the 
%                     partial derivatives w.r.t. to each element of Theta
%

%Compute the predicted movie ratings for all users 
pred = X * Theta';
% Compute the movie rating error by subtracting Y from the predicted ratings.
error = pred - Y;
%Compute the "error_factor" my multiplying the movie rating error by the R matrix. 
%The error factor will be 0 for movies that a user has not rated
error_factor = error .* R;

%Calculate the cost
J = (1/2) * sum(sum(error_factor.^2));

% Calculate Gradient
%The X gradient is the product of the error factor and the Theta matrix.
X_grad = error_factor * Theta;

%The Theta gradient is the product of the error factor and the X matrix
Theta_grad = error_factor' * X;

%Calculate the regularized cost
%Compute the regularization term as the scaled sum of the squares of all terms in Theta and X. 
reg1 = (lambda/2) * (sum(sum(Theta.^2)));
reg2 = (lambda/2) * (sum(sum(X.^2)));

J = J + reg1 + reg2;

%Calculate the regularized gradient
%The X gradient regularization is the X matrix scaled by lambda
X_grad = X_grad + X * lambda;
%The Theta gradient regularization is the Theta matrix scaled by lambda.
Theta_grad = Theta_grad + Theta * lambda;

% =============================================================

grad = [X_grad(:); Theta_grad(:)];

end
