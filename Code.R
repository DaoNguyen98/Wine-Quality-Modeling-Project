library(readr)
library(olsrr)
library(mosaic)
library(tidyverse)
library(MASS)
library(car)

redWine <- read.csv("winequality-red.csv", sep = ";")
redWine <- redWine[1:150, ]
redWine <- as.data.frame(redWine)
redWine$color <- "red"
head(redWine)

whiteWine <- read.csv("winequality-white.csv", sep = ";")
whiteWine <- whiteWine[1:150, ]
whiteWine <- as.data.frame(whiteWine)
whiteWine$color <- "white"
head(whiteWine)

wines <- rbind(redWine, whiteWine)
head(wines)

write.csv(wines, "wines.csv", row.names = FALSE)

wines <- read_csv("wines.csv")
View(wines)
attach(wines)

#Get Basic statistics of every variable in a data set
summary(wines)

#Bar Plots for variables
fixed.acidity_table <- table(fixed.acidity)
barplot(fixed.acidity_table, xlab = "fixed.acidity", 
        ylab = "Frequency of fixed.acidity")

volatile.acidity_table <- table(volatile.acidity)
barplot(volatile.acidity_table, xlab = "volatile.acidity", 
        ylab = "Frequency of volatile.acidity")

citric.acid_table <- table(citric.acid)
barplot(citric.acid_table, xlab = "citric.acid", 
        ylab = "Frequency of citric.acid")

residual.sugar_table <- table(residual.sugar)
barplot(residual.sugar_table, xlab = "residual.sugar", 
        ylab = "Frequency of residual.sugar")

chlorides_table <- table(chlorides)
barplot(chlorides_table, xlab = "chlorides", 
        ylab = "Frequency of chlorides")

free.sulfur.dioxide_table <- table(free.sulfur.dioxide)
barplot(free.sulfur.dioxide_table, xlab = "free.sulfur.dioxide", 
        ylab = "Frequency of free.sulfur.dioxide")

total.sulfur.dioxide_table <- table(total.sulfur.dioxide)
barplot(total.sulfur.dioxide_table, xlab = "total.sulfur.dioxide", 
        ylab = "Frequency of total.sulfur.dioxide")

density_table <- table(density)
barplot(density_table, xlab = "density", 
        ylab = "Frequency of density")

pH_table <- table(pH)
barplot(pH_table, xlab = "pH", 
        ylab = "Frequency of pH")

sulphates_table <- table(sulphates)
barplot(sulphates_table, xlab = "sulphates", 
        ylab = "Frequency of sulphates")

alcohol_table <- table(alcohol)
barplot(alcohol_table, xlab = "alcohol", 
        ylab = "Frequency of alcohol")

#Correlation Matrix: Any |cor| > 0.8, potential collinearity issue.
cor(wines)

#Basic Scatterplots: ONLY USE FOR SIMPLE LINEAR REGRESSION
#Scatterplot for only 2 variables:
plot(quality, total.sulfur.dioxide)
plot(quality, total.sulfur.dioxide, xlab = "total.sulfur.dioxide", ylab = "quality", 
     main = "Scatterplot of total.sulfur.dioxide vs. quality")

#Partial Regression Plots (Partial Residual Plots):
library(car)
# the "~ ." means to use all other variables in the dataset"
full_model <- lm(quality ~ ., data=wines)
crPlots(full_model)

# STEP 1 =================================================================

# Overall F-test
# * for the future we should prob abreviate the names...
# * From chap 9 notes, what does overall test do...
# * Overall F-test: Does the entire set of individual variables contribute
# * significantly to the prediction of y (quality)?
# * tip! if the p-value is less than 5% alpha then it is significant
model <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + 
              residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide
            + density + pH + sulphates + alcohol)
summary(model)

# F-statistic is 12.98
# P-value < 2.2e-16 (very small)
# P-value is less than alpha so Rejct HO
# ~At least one~ of the variables have a significant overall regression 
# prediction on wine quality.
# Next step is to see if we need to remove some variables, and which those
# are. Using Model selection.

# STEP TWO ===============================================================
# Model Selection
# Backward Stepwise was not working correctly at no ones fault.
# the R program simply was not working properly and kept variables
# with an insignifcant p-value when it should have been removed for
# that reason.

model <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + 
              residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide
            + density + pH + sulphates + alcohol)
library(olsrr)

#Backward Selection Model
> model <- lm(quality ~ fixed.acidity	+volatile.acidity	+citric.acid
              +            +	residual.sugar+	chlorides+	free.sulfur.dioxide	+total.sulfur.dioxide	+density	+pH+	sulphates+	alcohol +color, data=wines)
> model_backward <- ols_step_backward_p(model,prem = 0.1 ,details=TRUE)



reduce_model<- lm(quality ~ density + volatile.acidity	+ alcohol +	fixed.acidity + free.sulfur.dioxide	+total.sulfur.dioxide	+	sulphates +color, data=wines)
plot(reduce_model)


######Forward Stepwise
model_forward <- ols_step_forward_p(model, penter = 0.1) #pvalue must be < .1 to enter.
print(model_forward)
plot(model_forward) 
# * plot for some reason opens in a new window seperate from R
# * called R graphics. it may do the same on your computer I'm not sure.
# Unlike Backward Stepwise, the forward stepwise kept all variables that
# have a significant p-value. (a p-value that is less than 5% alpha)
# For further confirmation of its accuracy the we take a look at the plots:
# The AIC is increasing, which confirms its accuracy.
# The Adjusted R^2 is increasing which also confirms its accuracy.
# The Errors are decreasing, which confirms its accuracy.
# Conclusion:
# The variables that do not contribute significantly to the quality of wine (y)
# are: residual.sugar, chlorides, pH, and sluphates. (in total 4 varaibles).



model <- lm(quality ~ fixed.acidity	+volatile.acidity	+citric.acid
            +	residual.sugar+	chlorides+	free.sulfur.dioxide	+total.sulfur.dioxide	+density	+pH+	sulphates+	alcohol +color, data=wines)


#Testing for Coincidence (red wine vs white wine)
full_modeltcp <- lm(quality ~ fixed.acidity + volatile.acidity + free.sulfur.dioxide + total.sulfur.dioxide + density + alcohol + sulphates + color + fixed.acidity*color + volatile.acidity*color + free.sulfur.dioxide*color + total.sulfur.dioxide*color + density*color + alcohol*color + sulphates*color)

reduced_modeltc <- lm(quality ~ fixed.acidity + volatile.acidity + free.sulfur.dioxide + total.sulfur.dioxide + density + alcohol + sulphates)

anova(reduced_modeltc, full_modeltcp)



#Testing for Parallelism (also red wine vs white wine)
full_modeltcp <- lm(quality ~ fixed.acidity + volatile.acidity + free.sulfur.dioxide + total.sulfur.dioxide + density + alcohol + sulphates + color + fixed.acidity*color + volatile.acidity*color + free.sulfur.dioxide*color + total.sulfur.dioxide*color + density*color + alcohol*color + sulphates*color)

reduced_modeltp <- lm(quality ~ fixed.acidity + volatile.acidity + free.sulfur.dioxide + total.sulfur.dioxide + density + alcohol + sulphates + color)

anova(reduced_modeltp, full_modeltcp)

#Plot
plot(model)
plot(reduced_model)

#Cook's Distance
print(sort(cooks.distance(reduce_model)))
#No points above 1.

#Leverage: Compare values to 2(k+1)/n. 
#Here, compare to 2(8+1)/300 = 0.06 on x-axis. 
print(sort(hatvalues(reduce_model)))

print(sort(studres(reduce_model)))
#Jackknife residuals follow t distribution with n-k-2 df.
t <- qt(.025, 300-8-2, lower.tail = FALSE)
print(t)

#VIF
ols_coll_diag(reduce_model) #no collinearity 
plot(model)




