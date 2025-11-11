# The following data is a collection of daily milk yield (in liters) for dairy cows.

# {30.25, 34.98, 29.66, 20.14, 23.92, 38.61, 36.89, 34.68, 25.83, 29.93}

# Estimate the average daily yield (mu) using the normal model. 

# Use the grid approximation and choose some non-uniform priors.
# Plot the marginal posterior for mu.
# Compute the 90% credible interval.
# What is the probability that the daily milk yield is more than 30 liters?

N <- c(30.25, 34.98, 29.66, 20.14, 23.92, 38.61, 36.89, 34.68, 25.83, 29.93)

# Generate data
sigma <- 1
mystery_mu <- rnorm(1, 0, 1) # Generate unknown mean parameter
x <- rnorm(N, mystery_mu, sigma)


# Define a grid of points for mu
delta <- 0.01
mu_grid <- seq(-3, 3, by = delta)
df <- data.frame(mu = mu_grid)

# Likelihood
for(i in 1:nrow(df)) {
  df[i, "likelihood"] <- prod(
    dnorm(x, mean = df[i, "mu"], sd = 1)
  )
}

# Prior: mu ~ N(0, 1)
df$prior <- dnorm(df[, "mu"], mean = 0, sd = 1)

# Posterior
df$posterior <- df$prior * df$likelihood
# Normalize
df$posterior <- df$posterior / sum(df$posterior * delta)


## Plot
df %>% 
  ggplot() + 
  geom_line(aes(x = theta, y = posterior), 
            color = posterior_color) + 
  geom_line(aes(x = theta, y = cdf))


# 90% CIs

x1 <- which.min(abs(df$cdf - 0.05))
x2 <- which.min(abs(df$cdf - 0.95))

CI90 <- c(theta_grid[x1], theta_grid[x2])

ggplot() +
  geom_line(data = df,
            aes(x = theta, y = posterior),
            color = posterior_color,
            linewidth = 1) +
  geom_hline(yintercept = c(0.05, 0.95)) +
  geom_vline(xintercept = CI90) +
  geom_point(data = data.frame(cdf_posterior, theta_grid),
             aes(x = theta_grid, y = cdf_posterior),
             size = 1)


