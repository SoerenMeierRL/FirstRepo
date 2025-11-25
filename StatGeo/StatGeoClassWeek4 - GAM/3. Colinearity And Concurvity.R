# =============================================================================
# IN-CLASS EXERCISE: Collinearity and Concurvity
# Using Species Richness and Habitat Variables
# =============================================================================
#
# Objective: Understand how correlated predictors cause problems in regression
# and learn to detect and handle multicollinearity issues
#
# Background: 
# - COLLINEARITY: Linear relationships between predictors
# - CONCURVITY: Non-linear relationships between predictors
# Both cause unstable coefficients, inflated standard errors, and poor predictions
#
# Instructions: Fill in the blanks to explore these problems!
# =============================================================================

# Load required libraries
library(car)  # For VIF calculations
# If you don't have 'car' package: install.packages("car")

# Load the Peake dataset
Peake <- read.csv("peake.csv")
str(Peake)

# Create additional variables that will demonstrate collinearity
clump_area <- Peake$AREA/10000  # Convert to hectares
species_richness <- Peake$SPECIES
individuals <- Peake$INDIV
log_area <- log(clump_area + 0.001)  # Add small constant to avoid log(0)
log_species <- log(species_richness + 1)
log_individuals <- log(individuals + 1)

# Create some intentionally problematic variables
area_squared <- clump_area^2
area_cubed <- clump_area^3
area_times_two <- clump_area * 2  # Perfect linear relationship!
species_density <- species_richness / (clump_area + 0.001)  # Species per hectare

# =============================================================================
# PART 1: PERFECT COLLINEARITY (The Worst Case)
# =============================================================================

print("=== PART 1: PERFECT COLLINEARITY ===")

# TASK 1: Try to fit a model with perfectly collinear predictors
# Hint: area_times_two = 2 * clump_area exactly

cat("Attempting to fit model with perfect collinearity...\n")

# This should give us a warning or error
tryCatch({
  perfect_collinear_model <- lm(species_richness ~ clump_area + area_times_two + individuals)
  print("Model fitted - check for warnings!")
  print(summary(perfect_collinear_model))
}, warning = function(w) {
  cat("WARNING:", w$message, "\n")
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
})

# QUESTION: What happened? Why can't we estimate all coefficients?
print("\nQUESTION 1: What happened when we included perfectly collinear variables?")
print("ANSWER SPACE: ___________________________________________")

# =============================================================================
# PART 2: DETECTING COLLINEARITY WITH CORRELATION MATRIX
# =============================================================================

print("\n=== PART 2: CORRELATION DETECTIVE WORK ===")

# Create a correlation matrix of our predictors
predictors <- data.frame(
  clump_area = clump_area,
  log_area = log_area,
  area_squared = area_squared,
  individuals = individuals,
  log_individuals = log_individuals,
  species_density = species_density
)

# TASK 2: Calculate correlation matrix
# Hint: Use cor() function
correlation_matrix <- cor(predictors)
print("Correlation Matrix:")
print(round(correlation_matrix, 3))

# TASK 3: Create a visual correlation plot
# Hint: Use corrplot if available, or make heatmap with base R
par(mfrow = c(1, 1))
# Simple correlation heatmap
image(1:ncol(correlation_matrix), 1:nrow(correlation_matrix), 
      t(correlation_matrix), 
      col = colorRampPalette(c("blue", "white", "red"))(100),
      xlab = "Variables", ylab = "Variables",
      main = "Correlation Heatmap\n(Red = Positive, Blue = Negative)",
      axes = FALSE)
axis(1, at = 1:ncol(correlation_matrix), labels = colnames(correlation_matrix), 
     las = 2, cex.axis = 0.8)
axis(2, at = 1:nrow(correlation_matrix), labels = rownames(correlation_matrix), 
     las = 2, cex.axis = 0.8)

# Add correlation values to the plot
for(i in 1:nrow(correlation_matrix)) {
  for(j in 1:ncol(correlation_matrix)) {
    text(j, i, round(correlation_matrix[i,j], 2), cex = 0.7)
  }
}

# QUESTION: Which variables are highly correlated?
print("\nQUESTION 2: Which pairs of variables have |correlation| > 0.8?")
print("ANSWER SPACE: ___________________________________________")

# =============================================================================
# PART 3: VARIANCE INFLATION FACTOR (VIF) - THE GOLD STANDARD
# =============================================================================

print("\n=== PART 3: VIF ANALYSIS ===")

# Fit a model with potentially collinear predictors (excluding perfect ones)
collinear_model <- lm(species_richness ~ clump_area + log_area + 
                        area_squared + individuals + log_individuals)

print("Model with potentially collinear predictors:")
print(summary(collinear_model))

# TASK 4: Calculate VIF values
# Hint: Use vif() function from car package
print("\nVariance Inflation Factors:")
vif_values <- vif(collinear_model)
print(round(vif_values, 2))

# TASK 5: Identify problematic variables
high_vif <- vif_values[vif_values > 5]  # Fill in threshold
print("\nVariables with HIGH VIF (>5):")
print(high_vif)

# QUESTION: Which variables should we consider removing?
print("\nQUESTION 3: Which variables have problematic VIF values?")
print("ANSWER SPACE: ___________________________________________")

# =============================================================================
# PART 4: DEMONSTRATING THE PROBLEM - UNSTABLE COEFFICIENTS
# =============================================================================

print("\n=== PART 4: THE INSTABILITY PROBLEM ===")

# Fit models with and without collinear predictors
print("Model 1: With collinear predictors")
model_with_collinearity <- lm(species_richness ~ clump_area + log_area + area_squared)
print(summary(model_with_collinearity)$coefficients)

print("\nModel 2: Removing most collinear predictor")
# TASK 6: Create a simpler model removing the worst offender
# Hint: Remove the variable with highest VIF
model_reduced <- lm(species_richness ~ log_area + 
                      area_squared + individuals + log_individuals)
print(summary(model_reduced)$coefficients)

# Compare standard errors
print("\nCompare Standard Errors:")
print("With collinearity:")
print(summary(model_with_collinearity)$coefficients[, "Std. Error"])
print("Reduced model:")
print(summary(model_reduced)$coefficients[, "Std. Error"])

# QUESTION: What happened to standard errors?
print("\nQUESTION 4: How did removing collinear predictors affect standard errors?")
print("ANSWER SPACE: ___________________________________________")

# =============================================================================
# PART 5: CONCURVITY - THE NON-LINEAR VERSION
# =============================================================================

print("\n=== PART 5: CONCURVITY DEMONSTRATION ===")

# Create variables that are non-linearly related
area_log_transform <- log(clump_area + 0.001)
area_sqrt_transform <- sqrt(clump_area)
area_inverse <- 1/(clump_area + 0.001)

# These transformations are all non-linear functions of the same variable!
# Plot to show non-linear relationships
par(mfrow = c(2, 2))

# TASK 7: Create scatter plots showing concurvity
plot(clump_area, area_log_transform, main = "Area vs Log(Area)",
     xlab = "Clump Area", ylab = "Log(Area)")

plot(clump_area, area_sqrt_transform, main = "Area vs Sqrt(Area)",
     xlab = "Clump Area", ylab = "Sqrt(Area)")

plot(clump_area, area_inverse, main = "Area vs 1/Area",
     xlab = "Clump Area", ylab = "1/Area")

plot(area_log_transform, area_sqrt_transform, main = "Log(Area) vs Sqrt(Area)",
     xlab = "Log(Area)", ylab = "Sqrt(Area)")

# QUESTION: Do you see non-linear relationships?
print("\nQUESTION 5: Describe the relationships you see in the plots:")
print("ANSWER SPACE: ___________________________________________")


# =============================================================================
# PART 6: ECOLOGICAL INTERPRETATION
# =============================================================================

print("\n=== PART 7: ECOLOGICAL REALITY CHECK ===")

print("In ecological studies, common sources of collinearity include:")
print("1. Different measures of the same thing (area, log(area), sqrt(area))")
print("2. Causally related variables (habitat size -> species richness -> individuals)")
print("3. Composite indices that share components")
print("4. Spatial/temporal autocorrelation")

# TASK 10: Think about biological relationships
print("\nBIOLOGICAL THINKING EXERCISE:")
print("In our dataset, why might these variables be correlated?")
print("- Area and Log(Area): ___________")
print("- Species richness and Individuals: ___________") 
print("- Area and Species density: ___________")

print("\nWhich relationships make biological sense to include together?")
print("ANSWER SPACE: ___________________________________________")
