# Logistic Regression from Scratch

# 5-Minute Coding Exercise: Logistic Regression from Scratch
# Goal: Predict car engine type from fuel efficiency

library(ggplot2)
set.seed(42)

cat("=== CAR ENGINE PREDICTION CHALLENGE ===\n")
cat("Question: Can we predict engine type from fuel efficiency?\n")
cat("Data: mtcars - vs (0=V-engine, 1=straight engine) vs mpg\n\n")

# ------------------------------------------------------------------------------
# STEP 1: Data Exploration (30 seconds)

cat("STEP 1: Explore the data\n")

# Load the classic mtcars dataset
data(mtcars)

# TASK 1: Examine the key variables
cat("Engine type distribution (vs):\n")
table(mtcars$vs)  # 0 = V-engine, 1 = straight engine
cat("\nMPG range:", range(mtcars$mpg), "\n")
cat("Sample size:", nrow(mtcars), "cars\n\n")

# Quick peek at relationship
cat("Average MPG by engine type:\n")
aggregate(mpg ~ vs, data = mtcars, mean)
cat("Hypothesis: Straight engines (vs=1) might be more fuel efficient!\n\n")

# ------------------------------------------------------------------------------
# STEP 2: Fit Logistic Model (1.5 minutes)

cat("STEP 2: Build the logistic regression model\n")

# TASK 2: Complete the logistic regression
# Predict vs (engine type) from mpg (fuel efficiency)
model_logistic <- glm(vs ~ _____, family = _______, data = mtcars)

# Show the results
cat("Model summary:\n")
print(summary(model_logistic)$coefficients)

# TASK 3: Extract and interpret coefficients
intercept_coef <- coef(model_logistic)["(Intercept)"]
mpg_coef <- coef(model_logistic)["mpg"]

cat("\nRaw coefficients (on log-odds scale):\n")
cat("  Intercept (β₀):", round(intercept_coef, 3), "\n")
cat("  MPG slope (β₁):", round(mpg_coef, 3), "\n")

# TASK 4: Convert to odds ratios (more interpretable!)
odds_ratios <- exp(coef(_______))
cat("\nOdds ratios (exponentiated coefficients):\n")
print(round(odds_ratios, 3))

mpg_odds_ratio <- odds_ratios["mpg"]
cat("\nINTERPRETATION:\n")
cat("  Each +1 mpg increases odds of straight engine by", round((mpg_odds_ratio-1)*100, 1), "%\n\n")

# ------------------------------------------------------------------------------
# STEP 3: Make Predictions (1.5 minutes)
cat("STEP 3: Predict for specific cars\n")

# TASK 5: Predict for three different efficiency levels
test_mpgs <- c(15, 25, 35)  # Low, medium, high efficiency
test_data <- data.frame(mpg = test_mpgs)

# Get predictions on both scales
log_odds_pred <- predict(model_logistic, newdata = test_data, type = "_____")  # log-odds
prob_pred <- predict(model_logistic, newdata = test_data, type = "_____")      # probabilities

# TASK 6: Create prediction table
prediction_table <- data.frame(
  MPG = test_mpgs,
  Log_Odds = round(log_odds_pred, 3),
  Probability = round(prob_pred, 3),
  Prediction = ifelse(prob_pred > 0.5, "Straight", "V-engine")
)

cat("Predictions for different MPG values:\n")
print(prediction_table)

# TASK 7: Interpret the predictions
cat("\nPrediction interpretation:\n")
cat("  Low efficiency (15 mpg): ____% chance of straight engine\n")   # Fill: ~8%
cat("  High efficiency (35 mpg): ____% chance of straight engine\n")  # Fill: ~99%
cat("  Pattern: Higher MPG → ______ likely to be straight engine\n") # Fill: more

# ------------------------------------------------------------------------------

# STEP 4: Visualization (1 minute)

cat("STEP 4: Visualize the logistic curve\n")

# TASK 8: Create smooth prediction curve
mpg_grid <- seq(min(mtcars$mpg), max(mtcars$mpg), length.out = 100)
grid_data <- data.frame(mpg = mpg_grid)

# Get probability predictions for the grid
grid_probs <- predict(model_logistic, newdata = grid_data, type = "response")

# TASK 9: Create the plot
plot(mtcars$mpg, mtcars$vs, 
     xlab = "Miles per Gallon", ylab = "Probability of Straight Engine",
     main = "Logistic Regression: Engine Type vs Fuel Efficiency",
     pch = 16, col = "gray")

# Add the logistic curve
lines(mpg_grid, grid_probs, col = "___", lwd = 3)  # Fill: blue or red

# Add a reference line at 50% probability
abline(h = 0.5, lty = 2, col = "gray")

# TASK 10: Add interpretive text
# Find MPG where probability = 0.5 (decision boundary)
boundary_mpg <- -intercept_coef / mpg_coef
text(boundary_mpg, 0.6, 
     paste("50% cutoff ≈", round(boundary_mpg, 1), "mpg"), 
     col = "red")

cat("The S-shaped curve shows the logistic relationship!\n\n")

# ------------------------------------------------------------------------------
# STEP 5: Model Assessment (30 seconds)

cat("STEP 5: How good is our model?\n")

# TASK 11: Check significance and fit
p_value_mpg <- coef(summary(model_logistic))["mpg", "Pr(>|z|)"]
is_significant <- p_value_mpg < 0.05

cat("Statistical significance:\n")
cat("  MPG p-value:", round(p_value_mpg, 4), "\n")
cat("  Significant effect?", is_significant, "\n")

# TASK 12: Calculate prediction accuracy
fitted_probs <- predict(model_logistic, type = "response")
predicted_class <- ifelse(fitted_probs > 0.5, 1, 0)
actual_class <- mtcars$vs

accuracy <- mean(predicted_class == actual_class)
cat("  Prediction accuracy:", round(accuracy*100, 1), "%\n")

# Show confusion table
cat("\nConfusion matrix:\n")
table(Predicted = predicted_class, Actual = actual_class)

# ------------------------------------------------------------------------------
# STEP 6: Real-world Applications (30 seconds)

cat("\n=== REAL-WORLD CONCLUSIONS ===\n")

cat("Engineering insights:\n")
cat("1. Fuel efficiency ______ predicts engine type\n")      # strongly/weakly
cat("2. Straight engines tend to be more ______\n")          # efficient/powerful  
cat("3. At ~", round(boundary_mpg, 1), "mpg, it's 50-50 which engine type\n")

# ------------------------------------------------------------------------------
