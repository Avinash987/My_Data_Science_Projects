# Gradient Descent is the process of minimizing a function by following the gradients of the cost function.
# In Machine learning a similar technique called stochastic gradient descent to minimize the error of a model on our training data can be used.
# gradient descent is a general algorithm, apply it to any problem that requires optimizing a cost function. 
# In the regression problem, the cost function that is often used is the mean square error (MSE).


# generate random data in which y is a noisy function of x

x <- runif(1000, -5, 5)
x
y <- x + rnorm(1000) + 3
y
# fit a linear model
res <- lm( y ~ x )
print(res)


# plot the data and the model
plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')
abline(res, col='red')

# taking the partial derivative of the cost function with respect to theta.

# squared error cost function
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}
# learning rate and iteration limit
alpha <- 0.01
num_iters <- 1000

# keep history
cost_history <- double(num_iters)
theta_history <- list(num_iters)

# initialize coefficients
theta <- matrix(c(0,0), nrow=2)

# add a column of 1's for the intercept coefficient
X <- cbind(1, matrix(x))
class(X)
head(X)


# gradient descent
for (i in 1:num_iters) {
  error <- (X %*% theta - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
}

print(theta)
theta

# As expected, theta winds up with the same values as lm returned. 
# Lets do some more plotting:


# plot data and converging fit
plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')
for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) {
  abline(coef=theta_history[[i]], col="red")
}
abline(coef=theta, col="blue",lwd=10)

cost_history
theta_history
theta

# Taking a look at how quickly the cost decreases
# This involves knowing the form of the cost as well as the derivative so that from a given point you know the gradient and can move in that direction, downhill towards the minimum value.

plot(cost_history, type='line', col='blue', lwd=2, main='Cost function', ylab='cost', xlab='Iterations')

