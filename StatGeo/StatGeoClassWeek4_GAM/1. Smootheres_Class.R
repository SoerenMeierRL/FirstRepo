# =============================================================================
# IN-CLASS EXERCISE: Smoothing Methods for Species Richness Analysis
# =============================================================================
# 
# Objective: Plot the changes in Species richness as a function of Clump area
# using different smoothing techniques
#
# Instructions: Fill in the blanks (marked with ___) to complete the code
# =============================================================================

# Load the data
Peake <- read.csv("peake.csv")
str(Peake)

# Set up plotting parameters for a 2x3 grid
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

# Convert area to hectares (dm2) and extract variables
clump_area <- Peake$AREA/10000  # Convert from m2 to hectares (dm2)
species_richness <- Peake$SPECIES

# =============================================================================
# PANEL 1: LINEAR REGRESSION
# =============================================================================
# Hint: Use plot() to create scatter plot, then add a linear trend line

plot(clump_area, species_richness, 
     main = "Linear", 
     xlab = "", 
     ylab = "Species Richness",
     xlim = c(0, 2.5), 
     ylim = c(0, 30))

# Add linear regression line
# Hint: Use abline() with lm() function, set line type to dashed
abline(lm(species_richness ~ clump_area), lty = 2)

# =============================================================================
# PANEL 2: LOESS SMOOTHING (Local Polynomial Regression)
# =============================================================================
# Hint: LOESS is good for capturing non-linear trends

plot(clump_area, species_richness, 
     main = "Loess (span = 0.75)", 
     xlab = "", 
     ylab = "", 
     xlim = c(0, 2.5), 
     ylim = c(0, 30))

# Add LOESS smooth line
# Hint: Use lines() with lowess() function, set f parameter for smoothing span
lines(lowess(clump_area, species_richness, f = 0.75))

# =============================================================================
# PANEL 3: CUBIC SPLINE SMOOTHING
# =============================================================================
# Hint: Splines are flexible curves that pass close to data points

plot(clump_area, species_richness, 
     main = "Cubic Spline (spar = 0.75)", 
     xlab = "", 
     ylab = "", 
     xlim = c(0, 2.5), 
     ylim = c(0, 30))

# Add spline smooth line
# Hint: Use lines() with smooth.spline() function, set spar for smoothing parameter
lines(smooth.spline(clump_area, species_richness, spar = 0.75))

# =============================================================================
# PANEL 4: RUNNING MEDIAN SMOOTHING
# =============================================================================
# Hint: Running median uses local medians to reduce influence of outliers

plot(clump_area, species_richness, 
     main = "Running Median (k=5)", 
     xlab = "Clump area [dm2]", 
     ylab = "Species Richness", 
     xlim = c(0, 2.5), 
     ylim = c(0, 30))

# Sort data by x-values for proper line drawing
ord <- order(clump_area)

# Add running median line
# Hint: Use lines() with runmed() function, k is the window size (must be odd)
lines(clump_area[ord], runmed(species_richness[ord], k = 21))

# =============================================================================
# PANEL 5: KERNEL SMOOTHING
# =============================================================================
# Hint: Kernel smoothing uses weighted averages of nearby points

plot(clump_area, species_richness, 
     main = "Kernel (bandwidth=0.5)", 
     xlab = "Clump area [dm2]", 
     ylab = "", 
     xlim = c(0, 2.5), 
     ylim = c(0, 30))

# Add kernel smooth line
# Hint: Use lines() with ksmooth() function, bandwidth controls smoothness
lines(ksmooth(clump_area, species_richness, bandwidth = 0.5))

# =============================================================================
# DISCUSSION QUESTIONS (Answer after completing the exercise):
# =============================================================================
# 
# 1. Which smoothing method shows the strongest relationship between 
#    clump area and species richness?
#
# 2. Which method would be most appropriate if you suspected the relationship
#    was approximately linear?
#
# 3. Which method might be best for exploring potential non-linear patterns
#    without making strong assumptions about the shape?
#
# 4. How does the bandwidth/span parameter affect the smoothness of the curves?
#    What happens if you use very small vs. very large values?
#
# 5. Which method seems most robust to potential outliers in the data?
#
