# GLM Coefficient Interpretation
# 5-Minute Exercise: GLM Coefficient Interpretation
# Goal: See how the SAME coefficient means different things in different models

library(ggplot2)
library(dplyr)
set.seed(42)

#------------------------------------------------------------
# Generate some data 

# Setup: Same predictor x for all three models
n <- 200
set.seed(42)
x <- runif(n, 0, 5)

cat("=== COEFFICIENT INTERPRETATION CHALLENGE ===\n")
cat("We'll fit 3 models with the SAME predictor x\n")
cat("But coefficients mean different things!\n\n")

#------------------------------------------------------------
# MODEL 1: Linear Model (30 seconds)
cat("MODEL 1: Linear Regression\n")

# TASK 1: Complete the data generation
# Hint: Linear relationship, additive effect
y_linear <- 2 + 1.2*x + rnorm(n, sd = 1)

# Fit the model
model_lm <- lm(y_linear ~ x)
# Extract the Coefficient
beta1_lm <- _______(_______)["_______"]

cat("Coefficient Beta_1 =", round(_______, 3), "\n")
# FILL IN: adds/increases OR multiplies
cat("INTERPRETATION: Each +1 in x _______ the mean by", _______, "\n")


#------------------------------------------------------------
# MODEL 2: Poisson GLM (1.5 minutes)

cat("\nMODEL 2: Poisson GLM (count data)\n")

# TASK 2: Complete the Poisson model
# Generate count data with exponential relationship
mu_poisson <- exp(0.5 + 0.35*x)
y_counts <- rpois(n, mu_poisson)

# Fit Poisson GLM
model_pois <- glm(y_counts ~ x, family = _______(link = "_____"))
beta1_pois <- _______(_______)["_______"]

# TASK 3: Calculate the rate ratio
rate_ratio <- exp(______)   # Transform coefficient

cat("Coefficient Beta_1 =", round(beta1_pois, 3), "\n")
cat("Rate Ratio = exp(Beta_1) =", round(rate_ratio, 3), "\n")
cat("INTERPRETATION: Each +1 in x _______ the count by factor", round(rate_ratio, 3), "\n")
# FILL IN: adds OR multiplies

# Quick check: pick two x values
x_test1 <- 2.0
x_test2 <- 3.0  # +1 higher
pred1 <- exp(predict(model_pois, newdata = data.frame(x = x_test1)))
pred2 <- exp(predict(model_pois, newdata = data.frame(x = x_test2)))

cat("Verification: At x=2, predicted count ≈", round(pred1, 1), "\n",
    "             At x=3, predicted count ≈", round(pred2, 1), "\n",
    "             Ratio =", round(pred2/pred1, 2), "≈", round(rate_ratio, 2), "✓\n")

#------------------------------------------------------------
# MODEL 3: Logistic GLM (1.5 minutes)  
cat("\nMODEL 3: Logistic GLM (binary outcomes)\n")

# Generate binary data
log_odds <- -3 + 0.18*x
prob_success <- 1/(1 + exp(-log_odds))
y_binary <- rbinom(n, size = 1, prob = prob_success)

# TASK 4: Complete the logistic model
model_logit <- glm(y_binary ~ x, family = _______(link = "_____"))
beta1_logit <-_______(_______)["_______"]

# TASK 5: Calculate the odds ratio
odds_ratio <- exp(______)

cat("Coefficient Beta_1 =", round(beta1_logit, 3), "\n")
cat("Odds Ratio = exp(Beta_1) =", round(odds_ratio, 3), "\n")
cat("INTERPRETATION: Each +1 in x _______ the odds by factor", round(odds_ratio, 3), "\n")
# FILL IN: adds OR multiplies

#------------------------------------------------------------
# COMPARISON TABLE (1 minute)
cat("\n=== SUMMARY COMPARISON ===\n")
comparison <- data.frame(
  Model = c("Linear", "Poisson", "Logistic"),
  Family = c("gaussian", "poisson", "binomial"),
  Link = c("identity", "log", "logit"),
  Beta1 = round(c(beta1_lm, beta1_pois, beta1_logit), 3),
  Transform = c("none", "exp(Beta_1)", "exp(Beta_1)"),
  Interpretation = c(
    paste("adds", round(beta1_lm, 2)),
    paste("multiplies by", round(rate_ratio, 2)),
    paste("odds × by", round(odds_ratio, 2))
  )
)
print(comparison)

#------------------------------------------------------------
# VISUAL CHECK (1 minute)
cat("\n=== QUICK PLOTS ===\n")

# TASK 6: Create prediction grids and plot
x_grid <- seq(0, 5, length.out = 100)

# Linear predictions
pred_linear <- predict(model_lm, newdata = data.frame(x = x_grid))

# Poisson predictions (on response scale)
pred_poisson <- predict(model_pois, newdata = data.frame(x = x_grid), type = "_______")

# Logistic predictions (probability scale)  
pred_logistic <- predict(model_logit, newdata = data.frame(x = x_grid), type = "_______")

# Simple plots
par(mfrow = c(1, 3))
plot(x, y_linear, main = "Linear: Straight Line", pch = 16, col = "blue", cex = 0.7)
lines(x_grid, pred_linear, col = "red", lwd = 2)

plot(x, y_counts, main = "Poisson: Exponential Curve", pch = 16, col = "green", cex = 0.7)
lines(x_grid, pred_poisson, col = "red", lwd = 2)

plot(x, y_binary, main = "Logistic: S-Curve", pch = 16, col = "purple", cex = 0.7)
lines(x_grid, pred_logistic, col = "red", lwd = 2)
par(mfrow = c(1, 1))  # Reset plot layout
#------------------------------------------------------------