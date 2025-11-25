# GLM Diagnostics Practical
# 5-Minute GLM Diagnostics Practical
# Goal: Learn to check if your Poisson GLM is working properly!

library(ggplot2)
library(dplyr)
library(MASS)
set.seed(123)

cat("=== GLM DETECTIVE WORK ===\n")
cat("Question: Is our species count model trustworthy?\n")
cat("Your job: Run diagnostics to find problems!\n\n")

# ------------------------------------------------------------------------------
# SETUP: Ecological Data (30 seconds)

cat("STEP 1: Load the data (species counts vs biomass + pH)\n")

# Pre-built ecological dataset
n <- 200
biomass <- runif(n, 0, 500)
pH <- runif(n, 4.5, 8.0)
# True nonlinear relationship (students don't see this!)
true_rate <- exp(-1.5 + 0.003*biomass + 0.3*(pH-6.5) - 0.05*(pH-6.5)^2)
species_count <- rpois(n, true_rate)

eco_data <- data.frame(biomass, pH, species = species_count)
cat("Data loaded:", nrow(eco_data), "sites\n")
cat("Species range:", range(species_count), "\n\n")

# ------------------------------------------------------------------------------
# STEP 2: Fit Models (1 minute)

cat("STEP 2: Fit competing models\n")

# TASK 1: Fit two models to compare
model_simple <- glm(species ~ biomass + pH, family = _______, data = eco_data)
model_interaction <- glm(species ~ biomass * pH, family = _______, data = eco_data)

cat("Models fitted! Now let's check if they're any good...\n\n")

# ------------------------------------------------------------------------------
# STEP 3: Model Comparison (1 minute)

cat("STEP 3: Which model is better?\n")

# TASK 2: Test if interaction improves the model
lrt_test <- anova(model_simple, model_interaction, test = "_____")
print(lrt_test)

interaction_pvalue <- lrt_test$`Pr(>Chi)`[2]
cat("\nInteraction p-value:", round(interaction_pvalue, 4), "\n")
cat("Significant interaction?", interaction_pvalue < 0.05, "\n\n")

# ------------------------------------------------------------------------------
# STEP 4: Overdispersion Check (1 minute)

cat("STEP 4: Check for overdispersion (CRITICAL for Poisson!)\n")

# TASK 3: Calculate dispersion ratios
dispersion_simple <- model_simple$deviance / model_simple$df.residual
dispersion_interaction <- model_interaction$deviance / model_interaction$df.residual

cat("Dispersion ratios (should be ≈ 1.0 for Poisson):\n")
cat("  Simple model:", round(dispersion_simple, 3), "\n")
cat("  Interaction model:", round(dispersion_interaction, 3), "\n")

# TASK 4: Diagnose the problem
if(dispersion_interaction > 1.5) {
  cat("  PROBLEM: Overdispersion detected!\n",
      "           Poisson assumes variance = mean, but data has MORE variance\n",
      " Solution: Try negative binomial GLM\n")
} else {
  cat("GOOD: No serious overdispersion\n")
}

# TASK 5: Try negative binomial if needed
if(dispersion_interaction > 1.5) {
  cat("\nFitting negative binomial model...\n")
  model_nb <- glm.nb(species ~ biomass * pH, data = eco_data)
  
  aic_comparison <- data.frame(
    Model = c("Poisson", "Negative Binomial"),
    AIC = c(AIC(model_interaction), AIC(model_nb))
  )
  print(aic_comparison)
  cat("Lower AIC = better model\n")
}

cat("\n")

# ------------------------------------------------------------------------------
# STEP 5: Residual Diagnostics (1.5 minutes)

cat("STEP 5: Check residual patterns (like GLM health check)\n")

# TASK 6: Create diagnostic data
best_model <- model_interaction  # Use interaction model
residuals_df <- data.frame(
  fitted_values = fitted(best_model),
  deviance_resid = residuals(best_model, type = "_______"),  # Fill: "deviance"
  biomass = eco_data$biomass,
  pH = eco_data$pH
)

# TASK 7: Plot residuals vs fitted values
cat("Plotting residuals vs fitted values...\n")
plot(residuals_df$fitted_values, residuals_df$deviance_resid,
     xlab = "Fitted Values", ylab = "Deviance Residuals",
     main = "Residuals vs Fitted", pch = 16, col = "gray")
abline(h = 0, col = "red", lty = 2)

# Add a smooth line to see patterns
lines(lowess(residuals_df$fitted_values, residuals_df$deviance_resid), col = "blue", lwd = 2)

cat("LOOK FOR: Random scatter around zero (good) vs patterns (bad)\n")

# TASK 8: Check residuals vs predictors
cat("\nChecking residuals vs biomass...\n")
plot(residuals_df$biomass, residuals_df$deviance_resid,
     xlab = "Biomass", ylab = "Deviance Residuals", 
     main = "Residuals vs Biomass", pch = 16, col = "gray")
abline(h = 0, col = "red", lty = 2)
lines(lowess(residuals_df$biomass, residuals_df$deviance_resid), col = "blue", lwd = 2)

cat("LOOK FOR: No obvious curves or patterns\n\n")

# ------------------------------------------------------------------------------

# STEP 6: Model Predictions Check (1 minute)
cat("STEP 6: Visual reality check - do predictions make sense?\n")

# TASK 9: Create prediction grid
biomass_grid <- seq(0, 500, length.out = 100)
prediction_data <- data.frame(
  biomass = biomass_grid,
  pH = median(eco_data$pH)  # Hold pH constant
)

# TASK 10: Get predictions
predicted_counts <- predict(best_model, newdata = prediction_data, type = "_____")  # Fill: "response"

# Plot observed vs predicted
plot(eco_data$biomass, eco_data$species, 
     xlab = "Biomass", ylab = "Species Count",
     main = "Observed vs Model Predictions", pch = 16, col = "gray", cex = 0.7)
lines(biomass_grid, predicted_counts, col = "red", lwd = 3)
legend("topright", legend = "Model Fit", col = "red", lwd = 3)

cat("LOOK FOR: Model curve follows the data trend\n\n")

# ------------------------------------------------------------------------------
# DIAGNOSTIC SUMMARY (30 seconds)

cat("=== DIAGNOSTIC CHECKLIST SUMMARY ===\n")

# TASK 11: Fill in the diagnostic summary
cat("1. Model comparison: Interaction _______ significant\n")  # is/is not
cat("2. Overdispersion: _______ (ratio =", round(dispersion_interaction, 2), ")\n")  # Problem/OK
cat("3. Residual patterns: Check plots for _______ scatter\n")  # random/systematic
cat("4. Predictions: Model fit looks _______\n")  # reasonable/poor