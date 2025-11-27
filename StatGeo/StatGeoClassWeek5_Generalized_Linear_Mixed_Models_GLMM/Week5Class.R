##Load data
rikz_data <- read.table("RIKZ.txt",sep="\t",header=TRUE)

#Check structure of data
str(rikz_data)

#Turn categorical predictors that are integers into factors for data analysis
rikz_data$Beach_factor <- as.factor(rikz_data$Beach)
rikz_data$Sample_factor <- as.factor(rikz_data$Sample)
rikz_data$Exposure_factor <- as.factor(rikz_data$Exposure)


plot(rikz_data)


##Applying a model without controlling for dependence of observations between samples

lm1 <- lm(Richness ~ NAP * Exposure_factor, data = rikz_data)
summary(lm1)

plot(lm1)

#plot points colored by exposure level
plot(rikz_data$NAP,rikz_data$Richness, col=rikz_data$Exposure_factor, pch=c(17:19))
#Add regression line (this will only plot for exposure level 8)
abline(lm1,lty=2)

#Better plotting
require(tidyverse)
ggplot(rikz_data, aes(NAP,Richness,col=Exposure_factor)) + geom_point() + geom_smooth(method="lm")

#Assess changes in residuals as a function of NAP

lm1_residuals <- resid(lm1)
plot(rikz_data$NAP,lm1_residuals,col=rikz_data$Exposure_factor)
abline(h=0)

rikz_data$Residuals <- lm1_residuals
boxplot(Residuals~Exposure_factor,data=rikz_data)
boxplot(Residuals~Beach_factor,data=rikz_data)


#Build model that controls for the dependence between observations
library(lme4)

#Random intecept models - 1,2
lmm1 <- lmer(Richness ~ NAP + (1|Exposure_factor/Beach_factor), data=rikz_data)
summary(lmm1)

lmm2 <- lmer(Richness ~ NAP + (1|Exposure_factor), data=rikz_data)
summary(lmm2)

#check if lmm1 model is needed (if effect of beach is needed)
anova(lmm1,lmm2)

#Random slope model - 3
lmm3 <- lmer(Richness ~ NAP + (NAP|Exposure_factor), data=rikz_data)
summary(lmm3)

#Check if slope model is needed
anova(lmm3,lmm2)
#Slope model is not needed, intercept only model is enough

