### Maximum Likelihood Estimation

# 5-Minute Activity: Maximum Likelihood Estimation
# Goal: See how MLE finds the "most plausible" parameter values
install.packages("tidyr")
# Setup (30 seconds)
library(ggplot2)
library(dplyr)
library(tidyr)
set.seed(42)

#-------------------------------------------------------------------------------
#1) Generate some count data - imagine counting events per hour
y <- rpois(30, lambda = 2.3)  
print("Our observed counts:")
print(y)
print(paste("We observed", length(y), "time periods"))

#-------------------------------------------------------------------------------
# 2) Understanding the data
# TASK 1 (1 minute): Understanding the data
# What's the sample mean? This will be important!
sample_mean <- mean(y)
print(paste("Sample mean:",
            round(sample_mean, 2)))

#-------------------------------------------------------------------------------

# TASK 2 (1.5 minutes): Build the likelihood function
# For Poisson, log-likelihood = sum[y_i * log(λ) - λ] (ignoring constants)
# Complete the function:

calculate_loglik <- function(lambda_value, data) {
  # FILL IN: Calculate sum of [y_i * log(lambda) - lambda] for all observations
  result <- sum(data * log(lambda_value) - lambda_value)
  return(result)
}
# Test your function
test_loglik <- calculate_loglik(2.0, y)
print(paste("Log-likelihood at λ=2.0:", round(test_loglik, 2)))

#-------------------------------------------------------------------------------

# TASK 3 (1 minute): Find the MLE by trying different values
lambda_grid <- seq(0.5, 4, by = 0.1)

# FILL IN: Calculate log-likelihood for each lambda value
loglik_values <- sapply(lambda_grid, function(lam) {
  calculate_loglik(lam, y)
})

# Find the MLE
best_index <- which.max(loglik_values)
lambda_mle <- lambda_grid[best_index]

print(paste("MLE estimate:", round(lambda_mle, 2)))
print(paste("Sample mean:", round(sample_mean, 2)))
print("Notice: For Poisson, MLE = sample mean!")

#-------------------------------------------------------------------------------

# TASK 4 (1 minute): Visualize the likelihood surface
likelihood_data <- tibble(
  lambda = lambda_grid,
  loglik = loglik_values
)
ggplot(likelihood_data, aes(lambda, loglik)) +
  geom_line(linewidth = 1.2, color = "blue") +
  annotate("point", x =  lambda_mle, y = max(likelihood_data$loglik), color = "red", size = 3) +
  geom_vline(xintercept = lambda_mle, linetype = 2, color = "red") +
  annotate("text", x = lambda_mle + 0.3, y = max(loglik_values) - 5,
           label = paste("MLE =", round(lambda_mle, 2)), size = 4) +
  labs(title = "Log-Likelihood Function",
       subtitle = "MLE is where the curve peaks",
       x = "λ (Poisson rate parameter)", 
       y = "Log-Likelihood") +
  theme_minimal()
#-------------------------------------------------------------------------------

# TASK 5 (1 minute): Check the fit
# Compare what we observed vs what the MLE predicts

# Count frequencies in our data
observed <- table(y)
observed_df <- data.frame(
  count = as.numeric(names(observed)),
  observed_freq = as.numeric(observed),
  observed_prop = as.numeric(observed) / length(y)
)
observed_df
# FILL IN: What does our fitted Poisson model predict?
count_values <- 0:max(y)
predicted_probs <- dpois(count_values, lambda = _______)

predicted_df <- data.frame(
  count = count_values,
  predicted_prob = predicted_probs
)
predicted_df
#-------------------------------------------------------------------------------


# Combine and plot
comparison <- merge(observed_df, predicted_df, by = "count", all.y = TRUE)
comparison$observed_prop[is.na(comparison$observed_prop)] <- 0

print("Observed vs Predicted:")
print(comparison[1:8, c("count", "observed_prop", "predicted_prob")])

# Quick visual check
barplot(rbind(comparison$observed_prop, comparison$predicted_prob),
        beside = TRUE,
        names.arg = comparison$count,
        legend.text = c("Observed", "MLE Fit"),
        col = c("lightblue", "red"),
        main = "Observed vs MLE Fitted Probabilities",
        xlab = "Count", ylab = "Probability")


