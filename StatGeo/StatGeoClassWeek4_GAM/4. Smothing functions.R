# =============================================================================
# 5-MINUTE GAM SMOOTHERS EXERCISE
# Quick Exploration of Different Smoother Functions
# =============================================================================
#
# Objective: Compare different GAM smoothers in 5 minutes!
# Dataset: Species richness vs. clump area (Peake data)
#
# Instructions: Fill blanks and run each smoother - see the differences!
# =============================================================================

# Load required library
library(mgcv)  # Main GAM package
# If not installed: install.packages("mgcv")

# Load data
Peake <- read.csv("peake.csv")
area <- Peake$AREA/10000  # Convert to hectares
species <- Peake$SPECIES

# Set up plotting area - 2x3 grid for 6 smoothers
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

# =============================================================================
# SMOOTHER 1: THIN PLATE SPLINE (Default)
# =============================================================================
# Most common and flexible smoother

# TASK 1: Fit GAM with thin plate spline
# Hint: Just use s(area) - this is the default!
gam1 <- gam(species ~ s(area))

# Plot
plot(area, species, main = "Thin Plate Spline (Default)", 
     xlab = "Area (hectares)", ylab = "Species")
# Predict smooth curve
area_seq <- seq(min(area), max(area), length = 100)
pred1 <- predict(gam1, newdata = data.frame(area = area_seq))
lines(area_seq, pred1, col = "red", lwd = 2)

# =============================================================================
# SMOOTHER 2: CUBIC REGRESSION SPLINE
# =============================================================================
# Fixed knots, good for smooth curves

# TASK 2: Fit GAM with cubic regression spline
# Hint: Use bs="cr" in s()
gam2 <- gam(species ~ s(area, bs = "cr"))

plot(area, species, main = "Cubic Regression Spline", 
     xlab = "Area (hectares)", ylab = "")
pred2 <- predict(gam2, newdata = data.frame(area = area_seq))
lines(area_seq, pred2, col = "blue", lwd = 2)

# =============================================================================
# SMOOTHER 3: P-SPLINE (PENALIZED B-SPLINES)
# =============================================================================
# Good for automatic smoothness selection

# TASK 3: Fit GAM with P-splines
# Hint: Use bs="ps"
gam3 <- gam(species ~ s(area, bs = "ps"))

plot(area, species, main = "P-Splines", 
     xlab = "Area (hectares)", ylab = "")
pred3 <- predict(gam3, newdata = data.frame(area = area_seq))
lines(area_seq, pred3, col = "green", lwd = 2)

# =============================================================================
# SMOOTHER 4: CYCLIC CUBIC SPLINE
# =============================================================================
# For circular/seasonal data - fun to see difference!

# TASK 4: Fit GAM with cyclic spline
# Hint: Use bs="cc"
gam4 <- gam(species ~ s(area, bs = "cc"))

plot(area, species, main = "Cyclic Cubic Spline", 
     xlab = "Area (hectares)", ylab = "Species")
pred4 <- predict(gam4, newdata = data.frame(area = area_seq))
lines(area_seq, pred4, col = "purple", lwd = 2)

# =============================================================================
# SMOOTHER 5: GAUSSIAN PROCESS SPLINE
# =============================================================================
# Bayesian approach - very smooth

# TASK 5: Fit GAM with Gaussian process
# Hint: Use bs="gp"
gam5 <- gam(species ~ s(area, bs = "gp"))

plot(area, species, main = "Gaussian Process", 
     xlab = "Area (hectares)", ylab = "")
pred5 <- predict(gam5, newdata = data.frame(area = area_seq))
lines(area_seq, pred5, col = "orange", lwd = 2)

# =============================================================================
# SMOOTHER 6: ADAPTIVE SPLINE
# =============================================================================
# Adapts flexibility to local data density

# TASK 6: Fit GAM with adaptive smoothing
# Hint: Use bs="ad" 
gam6 <- gam(species ~ s(area, bs = "ad"))

plot(area, species, main = "Adaptive Spline", 
     xlab = "Area (hectares)", ylab = "")
pred6 <- predict(gam6, newdata = data.frame(area = area_seq))
lines(area_seq, pred6, col = "brown", lwd = 2)

# Reset plotting
par(mfrow = c(1, 1))

# =============================================================================
# QUICK COMPARISON
# =============================================================================

print("=== SMOOTHER COMPARISON ===")

# TASK 7: Check smoothness (effective degrees of freedom)
cat("Effective degrees of freedom (higher = more wiggly):\n")
cat("Thin Plate:", round(summary(gam1)$edf, 2), "\n")
cat("Cubic Regression:", round(summary(___)$edf, 2), "\n")
cat("P-Splines:", round(summary(___)$edf, 2), "\n")
cat("Cyclic:", round(summary(___)$edf, 2), "\n")
cat("Gaussian Process:", round(summary(___)$edf, 2), "\n")
cat("Adaptive:", round(summary(___)$edf, 2), "\n")

# TASK 8: Compare model fits (AIC)
cat("\nModel comparison (AIC - lower is better):\n")
cat("Thin Plate:", round(AIC(___), 1), "\n")
cat("Cubic Regression:", round(AIC(___), 1), "\n")
cat("P-Splines:", round(AIC(___), 1), "\n")
cat("Cyclic:", round(AIC(___), 1), "\n")
cat("Gaussian Process:", round(AIC(___), 1), "\n")
cat("Adaptive:", round(AIC(___), 1), "\n")

# =============================================================================
# QUICK INSIGHTS (30 seconds to discuss!)
# =============================================================================

print("\n=== QUICK INSIGHTS ===")
print("1. Which smoother looks most realistic for ecology?")
print("   Usually: Thin plate or P-splines")
print("")
print("2. Which looks too wiggly/unrealistic?")
print("   Often: Cyclic (forces curve to connect at ends)")
print("")
print("3. Which is smoothest?")
print("   Usually: Gaussian process")
print("")
print("4. Best general-purpose smoother?")
print("   Default thin plate spline - works well for most data!")

# =============================================================================
# BONUS: Quick parameter control
# =============================================================================

print("\n=== BONUS: Control smoothness ===")

# Show how to control smoothness with k parameter
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

# TASK 9: Try different k values (basis dimensions)
# Very smooth (k=4)
gam_smooth <- gam(species ~ s(area, k = ___))
plot(area, species, main = "Very Smooth (k=4)")
pred_smooth <- predict(gam_smooth, newdata = data.frame(area = area_seq))
lines(area_seq, pred_smooth, col = "red", lwd = 2)

# Default smoothness
gam_default <- gam(species ~ s(area))
plot(area, species, main = "Default Smoothness")
pred_default <- predict(gam_default, newdata = data.frame(area = area_seq))
lines(area_seq, pred_default, col = "blue", lwd = 2)

# Very flexible (k=15)
gam_flexible <- gam(species ~ s(area, k = ___))
plot(area, species, main = "Very Flexible (k=15)")
pred_flexible <- predict(gam_flexible, newdata = data.frame(area = area_seq))
lines(area_seq, pred_flexible, col = "green", lwd = 2)

# Reset plotting
par(mfrow = c(1, 1))

print("\nKey message: Usually stick with defaults unless you have good reason!")
print("GAMs automatically choose appropriate smoothness via cross-validation.")
