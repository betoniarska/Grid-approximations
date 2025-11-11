# **************************************************************
# ****** 2. Grid approximation: Gamma-Poisson ******************
# **************************************************************

# The Gamma-Poisson model can be stated as follows: 
# y_i ~ Pois(lambda)
# lambda ~ Gamma(1, 1)

# Where Pois is the Poisson distribution. Implement a grid approximation for this model.

# Apparently (https://en.wikipedia.org/wiki/Poisson_distribution), the number of chewing gum on a sidewalk tile is approximately Poisson distributed.
# Estimate the average number of gum on a Reykjavik side walk tile (lambda), using the data {2,7,4,3,5,2,7,5,5,5}.


# Data
y <- c(2,7,4,3,5,2,7,5,5,5)
n <- length(y)
sum_y <- sum(y)


lambda_grid <- seq(0, 15, length.out = 2000)

# remindwer: prior is before looking at the data, hence has no info on y
prior <- dgamma(lambda_grid, shape = 1, rate = 1)

# vrt. likelihood is where the data enters the picture:

#function(l) where l refers to each point in lambda_grid then does product of poisso on them
likelihood <- sapply(lambda_grid, function(l) prod(dpois(y, lambda = l)))

unstd_posterior <- likelihood * prior

# scale all for view
prior_scaled <- prior / sum(prior)
likelihood_scaled <- likelihood / sum(likelihood)
posterior <- unstd_posterior / sum(unstd_posterior)

posterior_mode <- lambda_grid[which.max(posterior)]

print('posterior mean')
print(posterior_mode)

# plot
plot(lambda_grid, prior_scaled, type = "l", lwd = 2, col = 'blue',
     ylim = c(0, max(posterior)),
     xlab = expression(lambda),
     ylab = "Density (scaled)",
     main = "Gamma–Poisson model for gum on tiles")
lines(lambda_grid, likelihood_scaled, col = 'red', lwd = 2)
lines(lambda_grid, posterior, col = 'green', lwd = 2)

abline(v = posterior_mode, col = 'gray', lwd = 2, lty = 2)

legend("topright",
       legend = c("Prior", "Likelihood", "Posterior", "Posterior mean"),
       col = c("blue", "red", "green", "gray"),
       lwd = 2, lty = c(1,1,1,2))

# most likely around 4.08 gum per tile (mode)




