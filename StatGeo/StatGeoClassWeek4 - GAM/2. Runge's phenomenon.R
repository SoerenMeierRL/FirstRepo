# =============================================================================
# IN-CLASS EXERCISE: Demonstrating Runge's Phenomenon
# Using Species Richness vs. Clump Area Data
# =============================================================================
#
# Objective: Understand why high-degree polynomial fitting can be dangerous
# and how it leads to wild oscillations (Runge's phenomenon)
#
# Background: Runge's phenomenon occurs when high-degree polynomials 
# fitted to data points oscillate wildly between the data points,
# especially near the boundaries of the data range.
#
# Instructions: Fill in the blanks to see this phenomenon in action!
# =============================================================================

# Load the Peake dataset
Peake <- read.csv("peake.csv")

# Extract variables and convert area to hectares
clump_area <- Peake$AREA/10000  # Convert to dm2 (hectares)
species_richness <- Peake$SPECIES

# Sort data by area for proper plotting
sorted_indices <- order(clump_area)
x_sorted <- clump_area[sorted_indices]
y_sorted <- species_richness[sorted_indices]

# Create fine grid for smooth polynomial evaluation
x_fine <- seq(min(clump_area), max(clump_area), length.out = 200)

# Set up plotting area
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

# =============================================================================
# PART 1: LOW-DEGREE POLYNOMIALS (Well-behaved)
# =============================================================================

# Degree 1 (Linear) - COMPLETED AS EXAMPLE
plot(clump_area, species_richness, 
     main = "Degree 1 (Linear)", 
     xlab = "Clump Area (dm2)", 
     ylab = "Species Richness",
     xlim = c(0, max(clump_area)), 
     ylim = c(0, 35))

# Fit polynomial of degree 1
poly_fit_1 <- lm(species_richness ~ poly(clump_area, 1, raw = TRUE))
y_pred_1 <- predict(poly_fit_1, newdata = data.frame(clump_area = x_fine))
lines(x_fine, y_pred_1, col = "blue", lwd = 2)

# Degree 2 (Quadratic)
plot(clump_area, species_richness, 
     main = "Degree 2 (Quadratic)", 
     xlab = "Clump Area (dm2)", 
     ylab = "",
     xlim = c(0, max(clump_area)), 
     ylim = c(0, 35))

# TASK 1: Fit a quadratic polynomial (degree 2)
# Hint: Use poly() function with degree = 2
poly_fit_2 <- lm(species_richness ~ poly(clump_area, 2, raw = TRUE))
y_pred_2 <- predict(poly_fit_2, newdata = data.frame(clump_area = x_fine))
lines(x_fine, y_pred_2, col = "blue", lwd = 2)

# Degree 3 (Cubic)
plot(clump_area, species_richness, 
     main = "Degree 3 (Cubic)", 
     xlab = "Clump Area (dm2)", 
     ylab = "",
     xlim = c(0, max(clump_area)), 
     ylim = c(0, 35))

# TASK 2: Fit a cubic polynomial (degree 3)
poly_fit_3 <- lm(species_richness ~ poly(clump_area, 3, raw = TRUE))
y_pred_3 <- predict(poly_fit_3, newdata = data.frame(clump_area = x_fine))
lines(x_fine, y_pred_3, col = "blue", lwd = 2)

# =============================================================================
# PART 2: HIGH-DEGREE POLYNOMIALS (Runge's Phenomenon begins!)
# =============================================================================

# Degree 8 (Getting dangerous...)
plot(clump_area, species_richness, 
     main = "Degree 8 - Oscillations Begin!", 
     xlab = "Clump Area (dm2)", 
     ylab = "Species Richness",
     xlim = c(0, max(clump_area)), 
     ylim = c(-10, 40))  # Extended y-limits to show oscillations

# TASK 3: Fit degree 8 polynomial - watch for oscillations!
# Hint: Notice we extended ylim to see negative predictions
poly_fit_8 <- lm(species_richness ~ poly(clump_area, 8, raw = TRUE))
y_pred_8 <- predict(poly_fit_8, newdata = data.frame(clump_area = x_fine))
lines(x_fine, y_pred_8, col = "red", lwd = 2)
# Add horizontal line at zero for reference
abline(h = 0, lty = 2, col = "gray")

# Degree 15 (Runge's phenomenon in full effect!)
plot(clump_area, species_richness, 
     main = "Degree 15 - Runge's Phenomenon!", 
     xlab = "Clump Area (dm2)", 
     ylab = "",
     xlim = c(0, max(clump_area)), 
     ylim = c(-50, 60))  # Much wider limits needed!

# TASK 4: Fit very high degree polynomial - prepare for wild oscillations!
poly_fit_15 <- lm(species_richness ~ poly(clump_area, 15, raw = TRUE))
y_pred_15 <- predict(poly_fit_15, newdata = data.frame(clump_area = x_fine))
lines(x_fine, y_pred_15, col = "blue", lwd = 2)
abline(h = 0, lty = 2, col = "gray")

# BONUS: Degree 20 (Complete madness - if you have 25 data points)
# Note: We have 25 data points, so degree 24 would interpolate perfectly
# but create extreme oscillations
plot(clump_area, species_richness, 
     main = "Degree 20 - Ecological Nonsense!", 
     xlab = "Clump Area (dm2)", 
     ylab = "",
     xlim = c(0, max(clump_area)), 
     ylim = c(-100, 100))

# TASK 5: The most dangerous fit - degree 20!
# What biological sense does this make?
poly_fit_20 <- lm(species_richness ~ poly(clump_area, 20, raw = TRUE))
y_pred_20 <- predict(poly_fit_20, newdata = data.frame(clump_area = x_fine))
lines(x_fine, y_pred_20, col = "darkred", lwd = 2)
abline(h = 0, lty = 2, col = "gray")

# Reset plotting parameters
par(mfrow = c(1, 1))

# =============================================================================
# QUANTITATIVE ANALYSIS
# =============================================================================

# Calculate R-squared for each fit to show the overfitting trap
cat("\n=== MODEL PERFORMANCE ===\n")
cat("Degree 1 R²:", sprintf("%.3f", summary(poly_fit_1)$r.squared), "\n")

# TASK 6: Calculate and compare R-squared values
# Hint: Use summary()$r.squared for each model
cat("Degree 2 R²:", round(summary(poly_fit_2)$r.squared, 3), "\n")
cat("Degree 3 R²:", round(summary(poly_fit_3)$r.squared, 3), "\n")
cat("Degree 8 R²:", round(summary(poly_fit_8)$r.squared, 3), "\n")
cat("Degree 15 R²:", round(summary(poly_fit_15)$r.squared, 3), "\n")
cat("Degree 20 R²:", round(summary(poly_fit_20)$r.squared, 3), "\n")


# =============================================================================
# CRITICAL THINKING QUESTIONS (Fill in after completing exercise):
# =============================================================================

print("=== DISCUSSION QUESTIONS ===")
print("1. What happens to polynomial curves as degree increases?")
print("   Answer: ___________")

print("2. Which polynomial degrees make biological sense for species-area data?")
print("   Answer: ___________")

print("3. Why do high-degree polynomials predict negative species richness?")
print("   Answer: ___________")

print("4. Where do the worst oscillations occur in the data range?")
print("   Answer: ___________")

print("5. How does this relate to overfitting?")
print("   Answer: ___________")

print("6. What pattern do you observe in the R² values as degree increases?")
print("   Answer: ___________")

print("7. Why is higher R² misleading in this context?")
print("   Answer: ___________")


# =============================================================================
# BIOLOGICAL INTERPRETATION EXERCISE
# =============================================================================

print("\n=== BIOLOGICAL REALITY CHECK ===")
print("Consider these predictions from the degree 20 model:")

# Find some problematic predictions
extreme_indices <- which(y_pred_20 < -10 | y_pred_20 > 50)
  cat("At area =", round(x_fine[extreme_indices[1]],2), 
      "dm2, predicted species =", round(y_pred_20[extreme_indices[1]]), "\n")
  cat("At area =", sprintf("%.2f", x_fine[extreme_indices[length(extreme_indices)]]), 
      "dm2, predicted species =", round(y_pred_20[extreme_indices[length(extreme_indices)]]), "\n")

print("\nCan you have negative species richness? Can you have 80+ species in a small area?")

