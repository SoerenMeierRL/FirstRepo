# Poisson GLM from Scratch
# 5-Minute Coding Exercise: Poisson GLM from Scratch
# Goal: Build, interpret, and test a Poisson model for species counts

library(ggplot2)
library(dplyr)
set.seed(123)

cat("=== SPECIES ECOLOGY CHALLENGE ===\n")
cat("Question: Do more productive sites have more plant species?\n")
cat("Data: Species counts vs biomass and soil pH\n\n")

#-------------------------------------------------------------------------------
# STEP 1: Data Setup (30 seconds)

cat("STEP 1: Generate ecological data\n")

# TASK 1: Complete the data simulation
n <- 200
biomass <- runif(n, 0, 500)    # productivity (g/m²)
pH <- runif(n, 4.5, 8.0)       # soil acidity

# True relationship (don't peek at this when interpreting!)
true_log_rate <- -1.5 + 0.004*biomass + 0.3*(pH - 6.5)
true_rate <- exp(true_log_rate)
species_count <- rpois(n, true_rate)

# Create dataset
ecological_data <- data.frame(
  biomass = biomass,
  pH = pH,
  species = species_count
)

cat("Data preview:\n")
print(head(ecological_data))
cat("Species count range:", range(species_count), "\n\n")

#-------------------------------------------------------------------------------
# STEP 2: Simple Poisson Model (1.5 minutes)

cat("STEP 2: Fit Poisson model for biomass effect\n")

# TASK 2: Fit Poisson GLM with biomass predictor
model_biomass <- glm(species ~ biomass, family = poisson, data = ecological_data)

# Extract coefficient and calculate rate ratio
beta_biomass <- coef(model_biomass)["biomass"]
rate_ratio_biomass <- exp(beta_biomass)

cat("Biomass coefficient (β₁):", round(beta_biomass, 4), "\n")
cat("Rate ratio per +1 g/m² biomass:", round(rate_ratio_biomass, 4), "\n")

# TASK 3: Interpret the rate ratio
# For +100 g/m² increase in biomass:
biomass_effect_100 <- rate_ratio_biomass^100  # Fill in: 100

cat("INTERPRETATION:\n")
cat("  +100 g/m² biomass → species count × by", round(biomass_effect_100, 2), "\n")
cat("  That's a", round((biomass_effect_100-1)*100, 1), "% increase!\n\n")

#-------------------------------------------------------------------------------

# STEP 3: Add pH Effect (1.5 minutes)

cat("STEP 3: Add pH to the model\n")

# TASK 4: Fit model with both predictors
model_both <- glm(species ~ biomass + pH, family = poisson, data = ecological_data)

# Show results
print(summary(model_both)$coefficients)

# TASK 5: Calculate rate ratios for both variables
beta_biomass_adj <- coef(model_both)["biomass"]
beta_pH_adj <- coef(model_both)["pH"]

rr_biomass_adj <- exp(beta_biomass_adj)
rr_pH_adj <- exp(beta_pH_adj)

cat("\nAdjusted effects (controlling for the other variable):\n")
cat("  Biomass rate ratio (per +1 g/m²):", round(rr_biomass_adj, 4), "\n")
cat("  pH rate ratio (per +1 pH unit):", round(rr_pH_adj, 3), "\n\n")

#-------------------------------------------------------------------------------
# STEP 4: Model Comparison (1 minute)

cat("STEP 4: Which model is better?\n")

# TASK 6: Compare models using likelihood ratio test
cat("Likelihood ratio test (does pH improve the model?):\n")
lrt_result <- anova(model_biomass, model_both, test = "Chisq")
print(lrt_result)

# TASK 7: Check model fit - overdispersion
dispersion_simple <- model_biomass$deviance / model_biomass$df.residual
dispersion_both <- model_both$deviance / model_both$df.residual

cat("Overdispersion check (should be ≈ 1.0 for Poisson):\n")
cat("  Biomass-only model:", round(dispersion_simple, 3), "\n")
cat("  Both-variables model:", round(dispersion_both, 3), "\n")

if(dispersion_both > 1.5) {
  cat("  WARNING: Overdispersion detected! Consider negative binomial.\n")
} else {
  cat("  GOOD: No serious overdispersion.\n")
}
cat("\n")

#-------------------------------------------------------------------------------
# STEP 5: Visualization (30 seconds)

cat("STEP 5: Visualize the results\n")

# TASK 8: Create predictions for plotting
biomass_grid <- seq(0, 500, length.out = 100)
pH_fixed <- median(pH)  # Hold pH constant

pred_data <- data.frame(
  biomass = biomass_grid,
  pH = pH_fixed
)

# TASK 9: Get predictions from the best model
predicted_counts <- predict(model_both, newdata = pred_data, type = "_______")

# Quick plot
plot(biomass, species_count, pch = 16, col = "gray", 
     xlab = "Biomass (g/m²)", ylab = "Species Count",
     main = "Species Richness vs Productivity")
lines(biomass_grid, predicted_counts, col = "red", lwd = 2)
legend("topright", legend = paste("pH fixed at", round(pH_fixed, 1)), 
       col = "red", lty = 1)


#-------------------------------------------------------------------------------
# STEP 6: Scientific Conclusions (30 seconds)
cat("\n=== SCIENTIFIC CONCLUSIONS ===\n")

# TASK 10: Fill in the ecological interpretation
significant_biomass <- coef(summary(model_both))["biomass", "Pr(>|z|)"] < 0.05
significant_biomass
significant_pH <- coef(summary(model_both))["pH", "Pr(>|z|)"] < 0.05
significant_pH

cat("Results summary:\n")
cat("1. Biomass effect: _______ (p < 0.05 =", significant_biomass, ")\n")  # significant/not significant
cat("2. pH effect: _______ (p < 0.05 =", significant_pH, ")\n")           # significant/not significant
cat("3. Overall: Species richness _______ with productivity\n")           # increases/decreases/unchanged

cat("\nEcological interpretation:\n")
cat("- Higher biomass sites support _______ species\n")  # more/fewer/same number of
cat("- Soil pH also matters: _______ pH = more species\n") # higher/lower/neutral
cat("- This supports/contradicts the productivity-diversity hypothesis\n")