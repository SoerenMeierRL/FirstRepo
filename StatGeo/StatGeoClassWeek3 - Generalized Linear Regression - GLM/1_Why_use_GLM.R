### Why use GLM and not model transformed data 

# 5-Minute Practical: Poisson GLM vs Log-Linear Model
# Learning objective: Why GLMs matter for count data

install.packages(c("ggplot2","dplyr"))

# Setup (30 seconds)
library(ggplot2)
library(dplyr)
set.seed(42)

#-------------------------------------------------------------------------------
# 1) Generate count data (1 minute)
n <- 200
x <- runif(n, 0, 4)
true_mean <- exp(0.5 + 0.6*x)  # exponential relationship
y <- rpois(n, true_mean)       # Poisson counts
dat <- tibble(x, y)

# Quick look at the data
head(dat, 3)
summary(y)  # Notice: counts, no negatives, some zeros
#-------------------------------------------------------------------------------

# 2) Fit both models (1.5 minute)
# Common "workaround": log-transform + linear model
# TASK 1: Complete the log-linear model
lm_log <- lm(log(y+1) ~ x, data = dat)
lm_log
# TASK 2: Complete the Poisson GLM
# Hint: Use family = poisson(link = "log")
glm_poi <- glm(y ~ x, family = poisson, data = dat)
glm_poi
#-------------------------------------------------------------------------------

# 3) Compare predictions (2 minutes)
# Make predictions on test points
test_x <- c(1, 2, 3)
test_data <- tibble(x = test_x)

# GLM predictions (on response scale)
# TASK 3: Get predictions from the GLM on the response scale
pred_glm <- predict(glm_poi, newdata = test_data, type = "response")
pred_glm

# Log-linear predictions (back-transformed)
# TASK 4: Complete the back-transformation
# Hint: We added 1 before taking log, so subtract 1 after exp()
pred_lm <- exp(predict(lm_log, newdata = test_data)) - 1
pred_lm

# Show the difference
comparison <- tibble(
  x = test_x,
  GLM_pred = pred_glm,
  LogLM_pred = pred_lm,
  Difference = pred_glm - pred_lm
)
print(comparison)

#-------------------------------------------------------------------------------
# 4) Key insight: Check model assumptions (1.5 minute)
# TASK 5: Create residual plots for both models
# GLM residual plot
plot(fitted(glm_poi), residuals(glm_poi, type = "pearson"), 
     main = "GLM: Pearson Residuals", xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")

# Log-linear residual plot  
plot(fitted(lm_log), residuals(lm_log),
     main = "Log-Linear: Residuals", xlab = "Fitted", ylab = "Residuals")
abline(h = 0, col = "red")

#-------------------------------------------------------------------------------
# 5) The bottom line (1 minute)
cat("Key takeaway: For count data, Poisson GLM respects the data structure.\n")
cat("Log-transformation can work but may introduce bias and violate assumptions.\n")

# TASK 6: Calculate RMSE for both models
# Quick performance check
actual_fits_glm <- fitted(glm_poi)
actual_fits_lm <- exp(predict(lm_log)) - 1
rmse_glm <- sqrt(mean((y - actual_fits_glm)^2))
rmse_lm <- sqrt(mean((y - actual_fits_lm)^2))

cat("RMSE - Poisson GLM:", round(rmse_glm, 3), "\n")
cat("RMSE - Log-Linear:", round(rmse_lm, 3), "\n")


