# In this assignment, you will predict per capita crime rate in the Boston 
# dataset (part of the MASS library).

# To guide your analysis:
#   
#   Conduct an exploratory data analysis to identify any data peculiarities. 
#   Any possible colinearities? Any transformations needed (e.g., logarithm 
#   transformation)?
#   Perform best subset selection, the lasso, and ridge regression. Present and 
#   discuss results for the approaches that you consider.
# 
# Propose a model (or set of models) that seem to perform well on this dataset, 
# and justify your proposal. Make sure that you are evaluating model performance 
# using validation set error and/or cross-validation, as shown in ISLR Sections 
# 6.5 (Links to an external site.)Links to an external site. and 6.6 (Links to 
# an external site.)Links to an external site.. 
# 
# Does your chosen model involve all of the features in the data set? Why or 
# why not?
# Write up your findings in a report, following the usual guidelines. Please 
# include your R syntax in an Appendix so that it can be reproduced.

# ---------------Line 23 to 125 is Data Analysis 1---------------------------- #
# # Copied from data_analysis_1.R
# 
# ## Clear variables and load data set
# rm(list = ls())
# library(MASS)
# 
# df <- Boston
# 
# ## Understand structure of data set
# ?Boston  # Produces description of the data set under consideration 
# names(df)
# summary(df)
# which(is.na(df) == TRUE)
# # 1. Make some pairwise scatterplots of the predictors in this data set.  
# # Describe your findings.
# pairs(df) 
# cor_df <- data.frame(cor(df))
# cor_index <- which(abs(cor_df) > 0.7 & abs(cor_df) < 1, arr.ind = TRUE)
# cor_matrix <- matrix(nrow = 16, ncol = 1)
# cor_df[cor_index[, 1], cor_index[, 2]]
# 
# 
# # Scatterplot matrix contains many variables and hard to interpret
# 
# # 2. Are any of the predictors associated with per capita crime rate?  If so,
# # explain the relationship.
# # Generate individual pairwise plots
# for (i in 2:14) {
#   plot(df$crim ~ df[, i], xlab = colnames(df)[i])
# }
# highcrime <- df[which(df$crim > 3.677), ]
# dim(df[which(df$crim > 3.677), ])
# which(df$zn == 0 & df$indus == 18.10 & df$rad == 24 & df$tax == 666 & 
#         df$ptratio == 20.2 & df$crim > 3.67 & df$chas == 0)
# indus1810 <- df[which(df$indus == 18.10), ]
# summary(indus1810)
# # Variables related to crim include: medv, lstat, black (parabola), ptratio 
# # (high at ~21), tax (high at ~ 660), rad (high at ~25), dis (exp decay), age
# # (increasing variance with increasing age), nox (increased variance in nox of
# # ~0.53 to 0.77), chas (higher variance in chas = 0 ==> tract does not bound
# # river), indus (high at ~ 17), zn (high at ~0 ==> no residential land zoned
# # for lots over 25K sqft)
# 
# # 3. Do any of the suburbs of Boston appear to have particularly high crime 
# # rates?  Tax rates?  Pupil-teacher ratios?  Comment on the range of each
# # predictor.  You may find the summary() function useful.
# 
# plot(df$crim)
# plot(df$tax)
# plot(df$ptratio)
# plot(df$crim ~ df$tax)
# 
# # 4. How many of the suburbs in this data bound the Charles river?
# dim(df[df$chas == 1, ]) # [35 14] ==> 35 suburbs bound the Charles River
# # Via the scatterplot matrix, 
# 
# # 5. What is the median pupil-teacher ratio among the towns in this dataset?
# median(df$ptratio) # 19.0 ==> The median pupil-teacher ratio is 19.05
# hist(df$ptratio)
# summary(df$ptratio)
# 
# # 6. Which suburb of Boston has the lowest median value of owner-occupied homes?
# # What are the values of the other predictors for that suburb, and how do these
# # values compare to the overall ranges of those predictors?  Comment on your
# # findings.
# df[df$medv == min(df$medv), ] # Suburbs 399 and 406 have the lowest medv = 5.0
# # Calculate percentiles for suburbs with lowest median household value
# pctiles_legend <- as.data.frame(matrix(ncol = 14, nrow = 100))
# pctiles_min_medv <- as.data.frame(matrix(ncol = 14, nrow = 2))
# min_medv_index <- which(df$medv == min(df$medv))
# colnames(pctiles_legend) <- colnames(df)
# colnames(pctiles_min_medv) <- colnames(df)
# for (i in 1:14) {
#   for(k in 1:2) {
#     pctiles_legend[ , i] <- quantile(df[ , i], seq_len(100) * 0.01)
#     pctiles_min_medv[k, i] <- findInterval(df[min_medv_index[k], i], 
#                                            pctiles_legend[ , i])
#   }
# }
# write.csv(rbind(df[min_medv_index, ], pctiles_min_medv), 
#           file = "min_medv.csv")
# save(pctiles_legend, file = "pctiles.rda")
# 
# # 7. In this datset, how many of the suburbs average more than seven rooms per
# # dwelling?  More than eight rooms per dwelling?  Comment on the suburbs that
# # average more than eight rooms per dwelling?
# dim(df[df$rm > 7, ]) # [64 14] ==> 64 suburbs average more than 7 rooms/dwelling
# dim(df[df$rm > 8, ]) # [13 14] ==> 13 suburbs average more than 8 rooms/dwelling
# # Calculate percentiles for suburbs averaging more than 8 rooms/dwelling
# pctiles_morethan8 <- as.data.frame(matrix(ncol = 14, nrow = 13))
# morethan8_index <- which(df$rm > 8)
# colnames(pctiles_morethan8) <- colnames(df)
# for (i in 1:14) {
#   for(k in 1:13) {
#     pctiles_morethan8[k, i] <- findInterval(df[morethan8_index[k], i], 
#                                             pctiles_legend[ , i])
#   }
# }
# write.csv(rbind(df[morethan8_index, ], pctiles_morethan8), 
#           file = "morethan8.csv")
# 
# plot(df$nox ~ df$indus)
# ---------------Line 23 to 125 is Data Analysis 1---------------------------- #
## Clear variables and load data set
rm(list = ls())
library(MASS)
library(car) # Has VIF
library(glmnet) # For ridge regression
library(nortest) # Has Anderson-Darling
library(olsrr) # Has Breusch-Pagan
df <- Boston
cor(df)

full.model <- lm(df$crim ~ ., data = df)
summary(full.model) 
# *** = dis, rad; ** = medv; * = inter, zn, black; . = rm, lstat
# Benchmark numbers: SE(res) = 6.439, Mult R^2 = 45.4, Adj R^2 = 43.96
plot(studres(full.model) ~ full.model$fitted.values)
# Seemingly a healthy amount of outliers.  Linearity and equal variances 
# assumptions are also in doubt
vif(full.model) # rad and tax show high VIFs (7.16, 9.20) 
cor(df$rad, df$tax) # Returns 0.9102
par(mfrow = c(1, 1))
plot(df$rad ~ df$tax, cex = 0.5)
# identify(df$tax, df$rad)
full.model2 <- lm(df$crim ~. - rad, data = df)
summary(full.model2) # SE(res) = 6.718, Mult R^2 = 40.45, Adj R^2 = 39.0
vif(full.model2)
full.model3 <- lm(df$crim ~. - tax, data = df)
summary(full.model3)  # SE(res) = 6.436, Mult R^2 = 45.34, Adj R^2 = 44.01
vif(full.model3)
full.model4 <- lm(df$crim ~. - tax - rad, data = df)
summary(full.model4) # SE(res) = 7.124, Mult R^2 = 32.9, Adj R^2 = 31.41
vif(full.model4)

# Returning to data_analysis_1 concerning high crime suburbs
df.trimmed <- df
df.trimmed$rad24tax666 <- numeric(length = length(df[ , 1]))
df.trimmed$rad24tax666[which(df$rad == 24 & df$tax == 666)] <- 1
length(which(df$rad == 24 & df$tax == 666)) # Returns 132 / 506 = 26.1% of data
df.trimmed <- df.trimmed[ , -c(9:10)] 
cor(df.trimmed)
rad24tax666.full.model <- lm(df.trimmed$crim ~ ., data = df.trimmed)
summary(rad24tax666.full.model) # SE(res) = 6.417, Mult R^2 = 0.4567
vif(rad24tax666.full.model) # OK at this point.  Nox and Dis are a bit high (~4).
# Use BoxCox (lambda = 0.02)
rad24tax666.full.model2 <- lm(df.trimmed$crim^(0.02) ~ ., data = df.trimmed)
summary(rad24tax666.full.model2) # SE(res) = 0.01601, Mult R^2 = 86.46, Adj R^2 = 86.13
vif(rad24tax666.full.model2) # Pretty good.  Nox, dis are high-ish (~4).
plot(dis ~ nox, data = df) # Looks like an inverse relationship
plot(1/dis ~ nox, data = df)
noxdis.model <- lm(1/dis ~ nox, data = df)
summary(noxdis.model)
plot(noxdis.model$residuals ~ noxdis.model$fitted.values)
box.noxdis <- boxCox(noxdis.model)
box.noxdis
which(box.noxdis$y == max(box.noxdis$y)) # Returns 57
box.noxdis$x[57] # Returns 0.2626
# Back to BoxCox; try out nox/dis as variable
df.trimmed.noxdis <- df.trimmed
df.trimmed.noxdis$noxoverdis <- df.trimmed$nox / df.trimmed$dis
df.trimmed.noxdis <- df.trimmed.noxdis[ , -c(5, 8)]
rad24tax666.full.model3 <- lm(df.trimmed$crim^(0.02) ~ ., data = df.trimmed.noxdis)
summary(rad24tax666.full.model3)
vif(rad24tax666.full.model3) # Overall not bad, though probably not worth taking out nox and dis just yet

# Examine each SLR
slr.stats <- matrix(numeric(), nrow = 26, ncol = 4)
colnames(slr.stats) <- colnames(summary(full.model)$coefficients)
for (i in 1:13) {
  temp.model <- lm(df$crim ~ df[ , i] )
  slr.stats[(2*i - 1), ] <- summary(temp.model)$coefficients[1, ]
  slr.stats[2*i, ] <- summary(temp.model)$coefficients[2, ]
  rm(temp.model) # Clean up temp.model
}
slr.stats <- as.data.frame(slr.stats)
row.names(slr.stats)[2*(1:13) - 1] <- paste0(colnames(df)[2:14], " Intercept")
row.names(slr.stats)[2*(1:13)] <- colnames(df)[2:14]
round(slr.stats, 4) 
options(scipen = 999)
# All predictors significant in SLR except nox

full.model.int <- lm(df$crim ~ .^2, data = df)
summary(full.model.int) # lstat:medv, black:lstat, dis:rad, age:black, rm:lstat, rm:black, chas:rm
round(vif(full.model.int), 4)

# Generate 4-in-1 residuals plot for full.model
par(mfrow = c(2, 2))
qqnorm(full.model$residuals, main = "Normal Probability Plot of Raw Residuals")
qqline(full.model$residuals)
plot(fitted(full.model), rstudent(full.model), xlab = "Fitted Values",
     ylab = "Studentized Residuals", 
     main = "Studentized Residuals vs. Fitted Values")
abline(h = 0)
hist(full.model$residuals, xlab = "Raw Residuals", 
     main = "Histogram of the Residuals")
plot(full.model$residuals ~ seq(1:506), xlab = "Order", ylab = "Raw Residuals", 
     main = "Raw Residuals vs. Order")
lines(full.model$residuals ~ seq(1:506))
# Check observations vs. fits for full.model
par(mfrow = c(1, 1))
plot(df$crim ~ full.model$fitted.values)
abline(a = 0, b = 1)

# Try log transforms
log.full.model <- lm(log(df$crim) ~ ., data = df)
summary(log.full.model) # SE(res) = 0.7732, Mult R^2 = 87.54, Adj R^2 = 87.21
# Generate 4-in-1 residuals plot for log.full.model
par(mfrow = c(2, 2))
qqnorm(log.full.model$residuals, main = "Normal Probability Plot of Raw Residuals")
qqline(log.full.model$residuals)
plot(fitted(log.full.model), rstudent(log.full.model), xlab = "Fitted Values",
     ylab = "Studentized Residuals", 
     main = "Studentized Residuals vs. Fitted Values")
abline(h = 0)
hist(log.full.model$residuals, xlab = "Raw Residuals", 
     main = "Histogram of the Residuals")
plot(log.full.model$residuals ~ seq(1:506), xlab = "Order", ylab = "Raw Residuals", 
     main = "Raw Residuals vs. Order")
lines(log.full.model$residuals ~ seq(1:506))
# Huge improvements in non-linearity for log.full.model.  
# Check observations vs. fits for log.full.model
par(mfrow = c(1, 1))
plot(log(df$crim) ~ log.full.model$fitted.values)


# Box-cox on full.model
par(mfrow = c(1, 1))
box.full <- boxCox(full.model)
box.full
which(box.full$y == max(box.full$y))
box.full$x[51] # Returns lambda = 0.02 as suggested power transform
box.full.model <- lm(df$crim^(0.02) ~ ., data = df)
summary(box.full.model) # SE(res) = 0.01519, Mult R^2 = 87.84, Adj R^2 = 87.51
# Box = 0.02 suggests chas, rm, dis, tax, and medv are not significant
vif(box.full.model) # High for rad, tax
# due to the insignificance of rad in box.full.model and the high VIF, remove 
# rad for next model
# Generate 4-in-1 residuals plot for box.full.model
par(mfrow = c(1, 1))
qqnorm(box.full.model$residuals, main = "Normal Probability Plot of Raw Residuals")
qqline(box.full.model$residuals)
plot(fitted(box.full.model), rstudent(box.full.model), xlab = "Fitted Values",
     ylab = "Studentized Residuals", 
     main = "Studentized Residuals vs. Fitted Values")
abline(h = 0)
hist(box.full.model$residuals, xlab = "Raw Residuals", 
     main = "Histogram of the Residuals")
plot(box.full.model$residuals ~ seq(1:506), xlab = "Order", ylab = "Raw Residuals", 
     main = "Raw Residuals vs. Order")
lines(box.full.model$residuals ~ seq(1:506))
ad.test(box.full.model$residuals) # STILL HAVE NON-NORMALITY OF THE RESIDUALS

box.full.model2 <- lm(df$crim^(0.02) ~ . - rad, data = df)
summary(box.full.model2) # SE(res) = 0.01791, Mult R^2 = 83.05, Adj R^2 = 82.64
vif(box.full.model2) # Reasonable
# Check out Box with int's
box.full.model3 <- lm(df$crim^(0.02) ~ .^2, data = df)
summary(box.full.model3)



#### Tracking best models ####

# Box transformed response, without rad
box.full.model2 <- lm(df$crim^(0.02) ~ . - rad, data = df)
summary(box.full.model2) # SE(res) = 0.01791, Mult R^2 = 83.05, Adj R^2 = 82.64
vif(box.full.model2) # Reasonable

# Box transformed response, without rad, tax, with rad24tax666
rad24tax666.full.model2 <- lm(df.trimmed$crim^(0.02) ~ ., data = df.trimmed)
summary(rad24tax666.full.model2) # SE(res) = 0.01601, Mult R^2 = 86.46, Adj R^2 = 86.13
vif(rad24tax666.full.model2) # 

# Box transformed response, without rad, tax, dis, with triple
which(df$zn == 0 & df$indus == 18.10 & df$rad == 24 & df$tax == 666 &
        df$ptratio == 20.2 & df$chas == 0)
df.more.trimmed <- df
df.more.trimmed$highcrim <- numeric(length = length(df[ , 1]))
df.more.trimmed$highcrim[which(df$zn == 0 & df$indus == 18.10 & df$rad == 24 & 
                        df$tax == 666 & df$ptratio == 20.2 & df$chas == 0)] <- 1
df.more.trimmed <- df.more.trimmed[ , -c(2:4, 9:11)]
cor(df.more.trimmed)
more.trimmed.model <- lm(df.more.trimmed$crim^(0.02) ~ ., data = df.more.trimmed)
summary(more.trimmed.model) # SE(res) = 0.01718, Mult R^2 = 84.27, Adj R^2 = 84.02
vif(more.trimmed.model)

more.trimmed.model2 <- lm(df.more.trimmed$crim^(0.02) ~ . - rm, data = df.more.trimmed) # without room
summary(more.trimmed.model2) # SE(res) = 0.01719, Mult R^2 = 84.22, Adj R^2 = 84
vif(more.trimmed.model2)
pairs(df.more.trimmed, cex = 0.5, pch = 3)
cor(df.more.trimmed)

more.trimmed.model3 <- lm(df.more.trimmed$crim^(0.02) ~ . - rm - medv, data = df.more.trimmed) # without room and medv
summary(more.trimmed.model3) # SE(res) = 0.01722, Mult R^2 = 84.13, Adj R^2 = 83.94
vif(more.trimmed.model3)

# Nox, dis, and age might be able to be consolidated! #
summary(lm(crim^(0.02) ~ .^2, data = df.more.trimmed))
