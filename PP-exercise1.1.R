# Task 1
# Generate data from the normal model with unknown mean (mu) and standard deviation (sigma) 1.
# Generate 1000 data points and implement the grid approximation plot the posterior of mu. 

# This is the same example as considered during lecture 2 but with larger sample size (see Topic 1 > code_examples for R code).
# What modifications do you need to make in the implementation?

#install.packages("tidyverse")
#install.packages("grafify")

library(tidyverse)
library(grafify)

N <- 1000

# Generate data
sigma <- 1
mystery_mu <- rnorm(1, 0, 1) # Generate unknown mean parameter
x <- rnorm(N, mystery_mu, sigma)


# Define a grid of points for mu

delta <- 0.01
mu_grid <- seq(-3, 3, by = delta)
df <- data.frame(mu = mu_grid)
df2 <- df

# Likelihood

for(i in 1:nrow(df)) {
  df2[i, "log_likelihood"] <- sum(
    dnorm(x, mean = df[i, "mu"], sd = 1, log = TRUE)
  )
}

df$likelihood <- exp(df2$log_likelihood - max(df2$log_likelihood))

# Prior: mu ~ N(0, 1)
df$prior <- dnorm(df[, "mu"], mean = 0, sd = 1)

# Posterior
df$posterior <- df$prior * df$likelihood
# Normalize
df$posterior <- df$posterior / sum(df$posterior * delta)

df$likelihood <- df$likelihood / sum(df$likelihood * delta)

# Wide --> long format
df_l <- df %>% 
  gather(key = "Function",value = "value", -mu)

df_l %>% 
  ggplot(aes(x = mu, y = value, color = Function)) + 
  geom_point() + 
  geom_line() +
  scale_color_grafify() + 
  geom_vline(xintercept = mystery_mu, linetype = "dashed")



# **************************************************************
# ****** 2. Grid approximation: Gamma-Poisson ******************
# **************************************************************
  
# The Gamma-Poisson model can be stated as follows: 
# y_i ~ Pois(lambda)
# lambda ~ Gamma(1, 1)

# Where Pois is the Poisson distribution. Implement a grid approximation for this model.

# Apparently (https://en.wikipedia.org/wiki/Poisson_distribution), the number of chewing gum on a sidewalk tile is approximately Poisson distributed.
# Estimate the average number of gum on a Reykjavik side walk tile (lambda), using the data {2,7,4,3,5,2,7,5,5,5}.






