# When Linear Models Go WRONG
# 5-Minute Discovery Activity: When Linear Models Go WRONG!
# Goal: See why GLMs are necessary for non-normal data

library(ggplot2)
library(dplyr)
set.seed(7)

cat("=== DISCOVERY CHALLENGE ===\n")
cat("We'll fit linear models to 'problematic' data types\n")
cat("What could go wrong?? Let's find out!\n\n")

#===============================================================================
# SCENARIO 1: Binary Outcomes (2 minutes)

cat("SCENARIO 1: Predicting Success/Failure (0/1 data)\n")

# Generate binary data
n <- 150
x <- runif(n, -3, 3)
true_prob <- 1/(1 + exp(-(-0.8 + 0.9*x)))  # True probabilities
y_binary <- rbinom(n, 1, true_prob)

cat("Data preview - y values:", unique(y_binary), "\n")  # Should be 0s and 1s

#-------------------------------------------------------------------------------

# TASK 1: Fit both models and make predictions
# Try the "obvious" linear model first
model_linear <- lm(y_binary ~ x)

# TASK 2: What does the linear model predict?
predictions_lm <- predict(model_linear)
cat("Linear model predictions range:", round(range(predictions_lm), 3), "\n")
cat("PROBLEM: Probabilities should be between __ and __!\n")

#-------------------------------------------------------------------------------
# TASK 3: Fit the correct GLM
model_glm <- glm(y_binary ~ x, family = _______, data = data.frame(x, y_binary))
predictions_glm <- predict(model_glm, type = "_______")  # Get probabilities

cat("GLM predictions range:", round(range(predictions_glm), 3), "\n")

#-------------------------------------------------------------------------------
# TASK 4: Create comparison plot
x_grid <- seq(-3, 3, length.out = 100)
pred_lm_grid <- predict(model_linear, newdata = data.frame(x = x_grid))
pred_glm_grid <- predict(model_glm, newdata = data.frame(x = x_grid), type = "response")

plot(x, y_binary, pch = 16, col = "gray", main = "Binary Data: LM vs GLM",
     xlab = "x", ylab = "Probability", ylim = c(-0.5, 1.5))
lines(x_grid, pred_lm_grid, col = "red", lwd = 2, lty = 2, label = "Linear Model")
lines(x_grid, pred_glm_grid, col = "blue", lwd = 2, label = "GLM (Logistic)")
abline(h = c(0, 1), col = "black", lty = 3)  # Show valid probability bounds

legend("topleft", legend = c("Linear (WRONG!)", "Logistic GLM (CORRECT)"), 
       col = c("red", "blue"), lwd = 2, lty = c(2, 1))

cat("Notice: Red line goes outside [0,1] bounds - impossible probabilities!\n\n")

#===============================================================================
# SCENARIO 2: Count Data (2 minutes)

cat("SCENARIO 2: Predicting Counts (non-negative integers)\n")

# Generate count data
x2 <- runif(n, -2, 2)
true_rate <- exp(0.2 + 0.6*x2)  # Exponential relationship
y_counts <- rpois(n, true_rate)

cat("Data preview - y values:", head(y_counts, 8), "...\n")
cat("Notice: All counts are ≥ 0 (can't have negative events!)\n")

#-------------------------------------------------------------------------------

# TASK 5: Fit linear model to count data  
model_linear_counts <- lm(y_counts ~ x2)

# What does linear model predict?
x2_grid <- seq(-2, 2, length.out = 100)
pred_lm_counts <- predict(model_linear_counts, newdata = data.frame(x2 = x2_grid))

cat("Linear model prediction range:", round(range(pred_lm_counts), 3), "\n")
cat("PROBLEM: Can't have _______ counts!\n")

#-------------------------------------------------------------------------------

# TASK 6: Fit the correct Poisson GLM
model_poisson <- glm(y_counts ~ x2, family = _______, data = data.frame(x2, y_counts))
pred_glm_counts <- predict(model_poisson, newdata = data.frame(x2 = x2_grid), type = "_______")

cat("Poisson GLM prediction range:", round(range(pred_glm_counts), 3), "\n")

#-------------------------------------------------------------------------------

# TASK 7: Plot comparison
plot(x2, y_counts, pch = 16, col = "gray", main = "Count Data: LM vs GLM",
     xlab = "x", ylab = "Expected Count")
lines(x2_grid, pred_lm_counts, col = "red", lwd = 2, lty = 2)
lines(x2_grid, pred_glm_counts, col = "blue", lwd = 2)
abline(h = 0, col = "black", lty = 3)  # Show zero bound

legend("topleft", legend = c("Linear (can go negative!)", "Poisson GLM (stays ≥ 0)"), 
       col = c("red", "blue"), lwd = 2, lty = c(2, 1))

cat("Notice: Red line goes below zero - impossible counts!\n\n")

#===============================================================================
# THE BIG REVELATION (1 minute)
cat("=== THE BIG PICTURE ===\n")
cat("Linear models assume:\n")
cat("  1. Outcomes can be ANY real number (-∞ to +∞)\n")
cat("  2. Constant variance\n")
cat("  3. Normal distribution\n\n")

cat("But many real outcomes are CONSTRAINED:\n")
cat("  • Probabilities: must be [0, 1]\n")
cat("  • Counts: must be ≥ 0\n") 
cat("  • Proportions: must be [0, 1]\n")
cat("  • Rates: must be > 0\n\n")

cat("GLMs RESPECT these constraints!\n")
cat("  • Link functions map linear predictors to valid ranges\n")
cat("  • Appropriate error distributions (binomial, Poisson, etc.)\n")
cat("  • No impossible predictions!\n")

#===============================================================================
# QUICK QUIZ (30 seconds)

cat("\n=== QUICK QUIZ ===\n")
cat("1. For binary outcomes, use _______ GLM\n")
cat("2. For count data, use _______ GLM\n")
cat("3. Linear models can predict impossible values: TRUE/FALSE?\n")
cat("4. GLMs use _______ functions to stay in valid ranges\n")
#===============================================================================
