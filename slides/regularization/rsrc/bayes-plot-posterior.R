### Simulate n samples from y = theta + eps where theta=1 and eps is N(0,1)
### Use 3 different values tau2 for Gaussian prior variances of theta
### Compute posterior of theta using empirical mean update formula
### compare MAP and ridge loss surface/minimizer

library(ggplot2)
library(ggpubr)

set.seed(10)

# Define prior variances
tau2 <- c(0.25^2, 0.5^2, 2^2)
taus <- sqrt(tau2)

# Simulate data
n <- 20
sigma2 <- 1
y <- rnorm(n, mean = 1, sd = sqrt(sigma2))
y_bar <- mean(y)

# Generate x-axis sequence for range of theta values
x_seq <- seq(-3, 3, length.out = 300)

# df for plotting priors
prior_df <- data.frame(
  x = rep(x_seq, times = length(taus)),
  tau = rep(taus, each = length(x_seq)),
  density = unlist(lapply(taus, function(tau) dnorm(x_seq, mean = 0, sd = tau)))
)

# Compute posterior parameters
posterior_var <- (sigma2 * tau2)/(sigma2+n*tau2)
posterior_sd <- sqrt(posterior_var)
posterior_mean <- (n * tau2 * y_bar + sigma2*0)/(sigma2 + n * tau2)

# df for plotting posteriors
posterior_df <- data.frame(
  x = rep(x_seq, times = length(taus)),
  tau = rep(taus, each = length(x_seq)),
  density = unlist(lapply(seq_along(posterior_sd), function(i) dnorm(x_seq, mean = posterior_mean[i], sd = posterior_sd[i])))
)

# Calculate loss function for range of theta values
theta_values <- seq(-1, 3, length.out = 300)
ridge_loss <- function(theta, y, lambda) {
  sapply(theta, function(t) sum((y - t)^2) + lambda * t^2)
}

# Calculate losses for each tau2
loss_df <- data.frame(
  theta = rep(theta_values, times = length(tau2)),
  tau = rep(taus, each = length(theta_values)),
  loss = unlist(lapply(seq_along(tau2), function(i) ridge_loss(theta_values, y, 1/tau2[i])))
)

# Max loss to scale second y-axis
max_loss <- max(loss_df$loss)

# Plot for prior densities
p1 <- ggplot(prior_df, aes(x = x, y = density, color = as.factor(tau))) +
  geom_line(linewidth=1.5) +
  labs(title = "Prior Densities", y = "Density", x = expression(theta), color = "Prior Std. Dev.") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, hjust=0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )

# Plot for posterior densities + ridge risk
p2 <- ggplot(posterior_df, aes(x = x, y = density, color = as.factor(tau))) +
  geom_line(linewidth = 1.5) +
  geom_vline(data = data.frame(tau = as.factor(taus), xintercept = posterior_mean), aes(xintercept = xintercept, color = tau), linetype = "dashed", size = 1.2) +
  geom_line(data = loss_df, aes(x = theta, y = loss / max_loss * max(posterior_df$density), color = as.factor(tau)), linetype = "solid", size=1) +
  labs(title = "Posterior Densities (MAP=Ridge)", y = "Density", x = expression(theta), color = "Std. Dev.") +
  scale_y_continuous(sec.axis = sec_axis(~ . / max(posterior_df$density) * max_loss, name = "Reg. Emp. Risk", breaks = scales::pretty_breaks())) +
  theme_minimal() +
  xlim(-1,3) +
  theme(
    plot.title = element_text(size = 20, hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )

# Combine plots and save
p.comb <- ggarrange(p1,p2, ncol=2, hjust=-0.5, common.legend=TRUE)
ggsave(filename = paste0("../figure/bayes-plot-posterior.png"), plot = p.comb, width = 12, height = 4) 

