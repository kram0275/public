## Clear variables and load data set
rm(list = ls())
library(MASS)
df <- Boston

## Understand structure of data set
?Boston  # Produces description of the data set under consideration 
names(df)
summary(df)
which(is.na(df) == TRUE)
# 1. Make some pairwise scatterplots of the predictors in this data set.  
# Describe your findings.
pairs(df) 
cor_df <- data.frame(cor(df))
cor_index <- which(abs(cor_df) > 0.7 & abs(cor_df) < 1, arr.ind = TRUE)
cor_matrix <- matrix(nrow = 16, ncol = 1)
cor_df[cor_index[, 1], cor_index[, 2]]


# Scatterplot matrix contains many variables and hard to interpret

# 2. Are any of the predictors associated with per capita crime rate?  If so,
# explain the relationship.
# Generate individual pairwise plots
for (i in 2:14) {
  plot(df$crim ~ df[, i], xlab = colnames(df)[i])
}
highcrime <- df[which(df$crim > 3.677), ]
dim(df[which(df$crim > 3.677), ])
which(df$zn == 0 & df$indus == 18.10 & df$rad == 24 & df$tax == 666 & 
        df$ptratio == 20.2 & df$crim > 3.67 & df$chas == 0)
indus1810 <- df[which(df$indus == 18.10), ]
summary(indus1810)
# Variables related to crim include: medv, lstat, black (parabola), ptratio 
# (high at ~21), tax (high at ~ 660), rad (high at ~25), dis (exp decay), age
# (increasing variance with increasing age), nox (increased variance in nox of
# ~0.53 to 0.77), chas (higher variance in chas = 0 ==> tract does not bound
# river), indus (high at ~ 17), zn (high at ~0 ==> no residential land zoned
# for lots over 25K sqft)

# 3. Do any of the suburbs of Boston appear to have particularly high crime 
# rates?  Tax rates?  Pupil-teacher ratios?  Comment on the range of each
# predictor.  You may find the summary() function useful.

plot(df$crim)
plot(df$tax)
plot(df$ptratio)
plot(df$crim ~ df$tax)

# 4. How many of the suburbs in this data bound the Charles river?
dim(df[df$chas == 1, ]) # [35 14] ==> 35 suburbs bound the Charles River
# Via the scatterplot matrix, 

# 5. What is the median pupil-teacher ratio among the towns in this dataset?
median(df$ptratio) # 19.0 ==> The median pupil-teacher ratio is 19.05
hist(df$ptratio)
summary(df$ptratio)

# 6. Which suburb of Boston has the lowest median value of owner-occupied homes?
# What are the values of the other predictors for that suburb, and how do these
# values compare to the overall ranges of those predictors?  Comment on your
# findings.
df[df$medv == min(df$medv), ] # Suburbs 399 and 406 have the lowest medv = 5.0
# Calculate percentiles for suburbs with lowest median household value
pctiles_legend <- as.data.frame(matrix(ncol = 14, nrow = 100))
pctiles_min_medv <- as.data.frame(matrix(ncol = 14, nrow = 2))
min_medv_index <- which(df$medv == min(df$medv))
colnames(pctiles_legend) <- colnames(df)
colnames(pctiles_min_medv) <- colnames(df)
for (i in 1:14) {
  for(k in 1:2) {
    pctiles_legend[ , i] <- quantile(df[ , i], seq_len(100) * 0.01)
    pctiles_min_medv[k, i] <- findInterval(df[min_medv_index[k], i], 
                                            pctiles_legend[ , i])
  }
}
write.csv(rbind(df[min_medv_index, ], pctiles_min_medv), 
          file = "min_medv.csv")
save(pctiles_legend, file = "pctiles.rda")

# 7. In this datset, how many of the suburbs average more than seven rooms per
# dwelling?  More than eight rooms per dwelling?  Comment on the suburbs that
# average more than eight rooms per dwelling?
dim(df[df$rm > 7, ]) # [64 14] ==> 64 suburbs average more than 7 rooms/dwelling
dim(df[df$rm > 8, ]) # [13 14] ==> 13 suburbs average more than 8 rooms/dwelling
# Calculate percentiles for suburbs averaging more than 8 rooms/dwelling
pctiles_morethan8 <- as.data.frame(matrix(ncol = 14, nrow = 13))
morethan8_index <- which(df$rm > 8)
colnames(pctiles_morethan8) <- colnames(df)
for (i in 1:14) {
  for(k in 1:13) {
    pctiles_morethan8[k, i] <- findInterval(df[morethan8_index[k], i], 
                                            pctiles_legend[ , i])
  }
}
write.csv(rbind(df[morethan8_index, ], pctiles_morethan8), 
          file = "morethan8.csv")

plot(df$nox ~ df$indus)
