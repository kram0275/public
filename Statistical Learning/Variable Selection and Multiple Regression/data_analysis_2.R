# Conduct a suite of regression diagnostics for this model, explaining your 
# choices. If there is evidence of multicollinearity in the data, refit the 
# model using an appropriate subset of the covariates. If transforming any of 
# the variables in the model (including the response variable) impacts your 
# diagnostics, apply a judiciously chosen transformation. Write up your findings 
# in a report, following the usual guidelines, and be sure to include your code 
# in an Appendix so that it can be reproduced. 

# Load packages and dataset
options(digits = 4)
library(ISLR)
library(car)
library(MASS) # Has Box-Cox transformation function to test for power law
rm(list = ls()) # Clear variable space
df <- Carseats  # Copy over data and rename for less typing
## summary(df) # Reveals ShelveLoc has 3 classes
# Though R is smart enough to handle, re-assign dummy variables for ShelveLoc
# for regression ease of interpretation.  "Bad" becomes the reference level.
df$ShelveLocGood <- numeric(length(df[ , 1]))
df$ShelveLocMedium <- numeric(length(df[ , 1]))
df$ShelveLocGood[which(df$ShelveLoc == "Good")] <- 1
df$ShelveLocMedium[which(df$ShelveLoc == "Medium")] <- 1
# Also re-code the binary variables urban and US to [0, 1]
df$numUrban <- numeric(length = length(df[ , 1]))
df$numUrban[which(df$Urban == "Yes")] <- 1
df$numUS <- numeric(length = length(df[ , 1]))
df$numUS[which(df$US == "Yes")] <- 1
df <- df[ , c(1:6, 13, 12, 8, 9, 14, 15)]
colnames(df)[11:12] <- c("Urban", "US")
## pairs(df)
## cor(df)
## write.csv(df, file = "Carseats.csv")
## ??Carseats # Read up on data set

# Generate histograms for each variable to examine distributions
par(mfrow = c(3, 4))
for(i in 1:12) {
  hist(df[ , i], xlab = colnames(df[i]), 
       main = paste0("Histogram of ", colnames(df[i])))
}
# Normally distributed: Sales, CompPrice, Price
# Skewed to the right: Education, Advertising
# Balanced: Age, Population, Income
# length(which(df$Urban == 1)) / 400 ## 282/400 = 70.5% are Urban
# length(which(df$US == 1)) / 400 ## 258/400 = 64.5% are US

# Generate full model with all predictors included
full.model <- lm(df$Sales ~ ., data = df)
sum.full.model <- summary(full.model); sum.full.model
anova.full.model <- anova(full.model); anova.full.model
# The full model indicates that CompPrice, Income, Advertising, Price, 
# ShelveLocMedium, ShelveLocGood, Age, and US are significant linear predictors 
### THIS DIFFERS FROM SLR MODELS: numUS is not included
### while CompPrice, ShelveLocMedium is included

# Generate 4-in-1 residuals plot
par(mfrow = c(2, 2))
qqnorm(full.model$residuals, main = "Normal Probability Plot of Raw Residuals")
qqline(full.model$residuals)
plot(fitted(full.model), rstudent(full.model), xlab = "Fitted Values",
     ylab = "Studentized Residuals", 
     main = "Studentized Residuals vs. Fitted Values")
abline(h = 0)
hist(full.model$residuals, xlab = "Raw Residuals", 
     main = "Histogram of the Residuals")
plot(full.model$residuals ~ seq(1:400), xlab = "Order", ylab = "Raw Residuals", 
     main = "Raw Residuals vs. Order")
lines(full.model$residuals ~ seq(1:400))

# Test for normality with Anderson-Darling
library(nortest)
ad.test(full.model$residuals) # AD = 0.19, p-value = 0.9
# Test for independence with ACF, PACF
library(forecast)
par(mfrow = c(1, 2))
acf(df$Sales, main = "Autocorrelation Function for Sales", 
    sub = "95% confidence intervals in blue")
pacf(df$Sales, main = "Partial Autocorrelation Function for Sales",
     sub = "95% confidence intervals in blue")
# Seems OK via visual inspection
# Test for equal variances with Breusch-Pagan
library(olsrr)
ols_test_breusch_pagan(full.model) # p-value = 0.7775

# Generate residuals vs. fits plot
plot(fitted(full.model), rstudent(full.model))
abline(a = 0, b = 0)
identify(fitted(full.model), rstudent(full.model), labels=row.names(df))
# Observation 358 may be an outlier, with |stud.res| > 3
# Heteroskedasticity may be a problem.  Check via abs(stud.res) vs. fits
plot(fitted(full.model), abs(rstudent(full.model)), xlab = "Fitted Values", 
     ylab = "|Studentized Residuals|", 
     main = "Absolute Studentized Residuals vs. Fitted Values for Full Model") 
lo <- loess(abs(rstudent(full.model)) ~ fitted(full.model))
## lines(predict(lo), col='red', lwd=2) # Horizontal line; not helpful
boxdf <- df[-175, ]
box <- boxcox(boxdf$Sales ~ ., data = boxdf, lambda = seq(-2, 2, 0.01), 
              plotit = TRUE) # Added 0.01 to deal with Sales = 0.00 observation
# Optimal lambda is ~1

## summary(full.model) # residual standard error = 1
## anova(full.model) # Reveals significant predictors include CompPrice, Income,
# Advertising, Price, ShelveLoc, and Age IN FULL MODEL
# Compare min and max residuals to 3 times the S.E.(res) as seen in example
min.res <- min(full.model[[2]])
max.res <- max(full.model[[2]])
abs(min.res) > 3; abs(max.res) > 3
which(abs(full.model$residuals) > 3) # Observation 358 may be outlier/influential

# Check for outliers and influential points
# Identify outliers in res vs. fits
plot(fitted.values(full.model), rstudent(full.model))
abline(a = 0, b = 0)
res.df <- data.frame(raw = resid(full.model), stud = rstudent(full.model))
identify(fitted.values(full.model), rstudent(full.model), 
         labels = row.names(df)) # Observation 358 may be outlier/influential
outlierTest(full.model) # Confirms observation 358

# Check observations vs. fits
plot(fitted.values(full.model), df$Sales)
abline(a = 0, b = 1)

# Investigate the predictor levels of observation 358
df[358,]  # No immediately concerning predictor levels

# Investigate high leverage points
res.df$lev <- hatvalues(full.model)
par(mfrow = c(1,1))
plot(res.df$lev, xlab = "Observation Number", ylab = "Leverage")
identify(res.df$lev)  # Observation 311 may be high leverage
lev.thresh <- 2 * sum(hatvalues(full.model)) / nrow(df)
which(hatvalues(full.model) > lev.thresh) # Returns observations 43, 311

# Investigate Cook's distances for influential points
res.df$cooks <- cooks.distance(full.model)
plot(res.df$cooks, xlab = "Observation Number", ylab = "Cook's Distance")
which(res.df$cooks > 1) # Returns 0

# Check for multicollinearity amongst predictors
pairs(df) # Inspection of scatterplot matrix does not reveal obvious collinearity
# in the data set, with the possible exception of CompPrice ~ Price
cor(df)
par(mfrow = c(1,2))
plot(df$Advertising ~ df$US, xlab = "US (0 = non-US, 1 = in US)", 
     ylab = "Advertising Budget($1000)")
plot(df$CompPrice ~ df$Price, xlab = "Price ($)", ylab = "Competitor Price ($)")

vif(full.model)  # No VIFs exceed 5 (investigation warranted) nor 10 (fix
# immediately), as taught in STAT 501
# Try the condition number method as explained by Example 2
X <- model.matrix(full.model)
Z <- X[ , -1]
Z <- scale(Z)
M <- t(Z) %*% Z
eigen(M)
talih.cond.number <- max(eigen(M)$values) / min(eigen(M)$values)
wiki.cond.number <- sqrt(max(eigen(M)$values) / min(eigen(M)$values))
# Regardless of cond.number calculation, multicollinearity does not seem to
# be too much of a worry

# Examine each SLR
slr.stats <- matrix(numeric(), nrow = 22, ncol = 4)
colnames(slr.stats) <- colnames(summary(full.model)$coefficients)
for (i in 1:11) {
  temp.model <- lm(df$Sales ~ df[ , (i + 1)] )
  slr.stats[(2*i - 1), ] <- summary(temp.model)$coefficients[1, ]
  slr.stats[2*i, ] <- summary(temp.model)$coefficients[2, ]}
slr.stats <- as.data.frame(slr.stats)
row.names(slr.stats)[2*(1:11) - 1] <- paste0(colnames(df)[2:12], " Intercept")
row.names(slr.stats)[2*(1:11)] <- colnames(df)[2:12]
round(slr.stats, 4) 
# SLRs indicate that Income, Advertising, Price, 
# ShelveLocGood, Age, and US are significant linear predictors IN SLR MODELS
### THIS DIFFERS FROM FULL MODEL: CompPrice, ShelveLocMedium are not included
### while US is included

### The variables which are not significant in both the full and SLR models are:
### Population, Education, and Urban.  Likely OK getting rid of these variables.




# Screw around with interactions
int.full.model <- lm(df$Sales ~ .^2, data = df)
summary(int.full.model)
which(summary(int.full.model)$coefficients[ , 4] < 0.05) # Extracts p-values
# Indicates that CompPrice:Income and Age:numUS are significant interactions
anova(int.full.model)
# Generate new model with only those 2 interactions
int2.model <- lm(df$Sales ~ . + df$CompPrice:df$Income + df$Age:df$US,
                 data = df)
summary(int2.model);
summary(full.model)

round(summary(full.model)$coefficients, 3)
round(summary(int2.model)$coefficients, 3)

# Remove non-significant predictors from int2.model
int3.model <- lm(Sales ~ . + CompPrice:Income + Age:US - 
                   Population - Education - Urban,
                 data = df)
round(summary(int3.model)$coefficients, 3)
summary(int3.model)
vif(int3.model) # Wayyyyyyyy too high
plot(int3.model$residuals ~ int3.model$fitted.values)

# Remove US?
int4.model <- lm(Sales ~ .^2,
                 data = trimmed.df)
summary(int4.model)
plot(int4.model$residuals ~ int4.model$fitted.values)
vif(int4.model)
summary(int4.model) # Still probably not a good idea.  Go back to int3.model.

# Best subsets
# Check out 
# https://educationalresearchtechniques.com/2017/02/24/subset-regression-in-r/
# for code inspiration.
library(leaps)
library(lmtest)
trimmed.df <- df[ , c(1:4, 6:9, 12)]
sub.fit<-regsubsets(Sales~.,trimmed.df)
best.summary<-summary(sub.fit)
# Assess via Mallows Cp
par(mfrow=c(1,2))
plot(best.summary$cp)
plot(sub.fit,scale = "Cp")
best.summary$cp # Suggests that a 7 predictor model is optimal
# Assess via BIC
par(mfrow=c(1,2))
plot(best.summary$bic)
plot(sub.fit,scale = "bic")
best.summary$bic # Also suggests that a 7 predictor model is optimal
# The optimal model includes CompPrice, Income, Advertising, Price, ShelveLocMedium, ShelveLocGood, and Age
optimal.1 <- lm(Sales ~ ., trimmed.df)
summary(optimal.1)
vif(optimal.1) # For trimmed, no price.diff

optimal.2 <- lm(Sales ~ ., price.trimmed.df)
summary(optimal.2)
vif(optimal.2)

optimal.3 <- lm(Sales ~ . - US, price.trimmed.df)
summary(optimal.3)
vif(optimal.3) # For trimmed, with price.diff, no US

sub.fit<-regsubsets(Sales~.,price.trimmed.df)
best.summary<-summary(sub.fit)
# Assess via Mallows Cp
par(mfrow=c(1,2))
plot(best.summary$cp)
plot(sub.fit,scale = "Cp")
best.summary$cp # Suggests that a 6 predictor model is optimal
# Assess via BIC
par(mfrow=c(1,2))
plot(best.summary$bic)
plot(sub.fit,scale = "bic")
best.summary$bic # Also suggests that a 6 predictor model is optimal

