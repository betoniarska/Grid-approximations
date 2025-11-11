# **************************************************************
# ****** 3. Less data means bigger prior effect ****************
# **************************************************************
  
# Show that as the amount of available data increases, the effect of the prior decreases. 

# Instructions: 
# - Simulate a series of coin tosses:
# - Generate p ~ Uniform(0, 1). 
# - Simulate a sequence of 50 tosses with Pr(heads) = p. 
# - Fit the grid approximation using the first 1, 5, 10, 15,..., 50 tosses. 
# - Use a Beta prior for p.
# - Visualize the posteriors so that they can be compared to the prior.

# (This essentially replicates a figure from the first lecture's slides)

N <- 50

steps <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)

delta <- 0.01
theta_grid <- seq(0, 1, by = delta)

posterior_list <- c()

# generate the tosses

set.seed(123)
p_true <- runif(1, 0, 1) # n, min, max
y <- rbinom(N, size = 1, prob = p_true) # i.e. series of 0 and 1 indicating heads or tails


for (i in seq_along(steps)) {
  n <- steps[i]
  x <- sum(y[1:n])
  
  prior <- dbeta(theta_grid, 2, 3)
  
  likelihood <- dbinom(x = x, size = n, prob = theta_grid)
  
  posterior <- prior*likelihood
  posterior <- posterior / sum(posterior * delta) # maybe log version better?
  
  posterior_list[[i]] <- data.frame(
    theta = theta_grid,
    density = posterior,
    n = n
  )
}

posterior_df <- do.call(rbind, posterior_list) # put posteriors into df


ggplot(posterior_df, aes(x = theta, y = density, color = factor(n))) +
  geom_line(size = 1) +
  labs(x = expression(theta),
       y = "Posterior density",
       color = "Number of tosses (n)",
       title = "Tosses effect on prior influence") +
  theme_minimal()


