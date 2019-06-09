rm(list = ls())
library(ISLR)
df <- Weekly
df$numDirection <- numeric(length(df[ , 1]))
df$numDirection[df$Direction == "Up"] <- 1
df <- df[ , -9]
colnames(df)[9] <- "Direction"

for(i in 1:9) {
  hist(df[ , i], 
       xlab = paste0(colnames(df)[i]),
       main = paste0("Histogram of ", colnames(df)[i])
  )
}
# Histograms suggest that Volume should be transformed
hist(log(df$Volume))

df.summ <- summary(df)
colnames(df.summ) <- gsub(" ", "", colnames(df.summ), fixed = TRUE)
df.summ

corr.matrix <- cor(df) 
corr.matrix
# Cor(Year, Volume) = 0.8419, Cor(Direction, Today) others insignificant
# pairs(df) 
# Correlation between Year:Volume also seen visually, though not strictly 
# linear. BoxCox would probably be a good bet for this one if needed.


## Logistic Regression
log.full.model <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, data = df)
log.full.summ <- summary(log.full.model)
log.full.summ
# Lag1, Lag2 (p = 0.1181, 0.0296) appear to be significant. By hierarchy
# principle, should probably include Lag1 with Lag2.

log.full.probs <- predict(log.full.model, type = "response")
log.full.probs[1:10]
# contrasts(df$Direction) # This only applies when Direction is qualitative
log.full.pred <- rep("Down",1089)
log.full.pred[log.full.probs > 0.5] <- "Up"
# Given the predictions, the table() function can be used to produce 
# a confusion matrix in order to determine how many observations were
# correctly or incorrectly classified.
table(log.full.pred, df$Direction)
# Confusion matrix gives an overall accuracy rate of ~56%.  Not particularly great.

train <- (df$Year <= 2008)
df.test <- df[!train, ]
Direction.test <- df$Direction[!train]
# We now fit a logistic regression model using only the subset of the 
# observations that correspond to dates before 2005, using the subset 
# argument.
log.train.model <- glm(Direction ~  Lag2, data = df, family = binomial, subset = train)
log.train.probs <- predict(log.train.model, df.test, type = "response")
log.train.pred <- rep("Down",104)
log.train.pred[log.train.probs > 0.5] <- "Up"
# Given the predictions, the table() function can be used to produce 
# a confusion matrix in order to determine how many observations were
# correctly or incorrectly classified.
table(log.train.pred, Direction.test)
# Overall correct prediction rate = (9 + 56) / (104) = 62.5%


## LDA
library(MASS)
lda.train.model <- lda(Direction ~ Lag2, data = df, subset = train)
lda.train.model
lda.train.pred <- predict(lda.train.model, df.test)
names(lda.train.pred)
# The LDA and logistic regression predictions are almost identical.
lda.train.class <- lda.train.pred$class
table(lda.train.class, Direction.test)
# Overall correct prediction rate = (9 + 56) / 104 = 62.5%.  This matches the logistic regression results.
mean(lda.train.class == Direction.test)

## QDA
qda.train.model <- qda(Direction ~ Lag2, data = df, subset = train)
qda.train.model
qda.train.pred <- predict(qda.train.model, df.test)
names(qda.train.pred)
qda.train.class <- qda.train.pred$class
table(qda.train.class, Direction.test)
# Overall correct prediction rate = (0 + 61) / 104 = 58.7%.  This is worse than both the logistic and LDA models.
mean(qda.train.class == Direction.test)


## Let's get weird
dfnew <- df[ , -c(1, 8)]
dfnew$lnVolume <- log(df$Volume)
for (i in 1:5) {
  dfnew[ , 8 + i] <- numeric(length(df[ , 1]))
  dfnew[ , 8 + i] <- log(dfnew[ , i] + 20)
}
colnames(dfnew)[9:13] <- c("lnLag1", "lnLag2", "lnLag3", "lnLag4", "lnLag5")
dfnew <- dfnew[ , c(1:5, 9:13, 6, 8, 7)]
## Logistic experimentation
log.train.model2 <- glm(Direction ~ ., data = dfnew[ , -c(1:5, 11)], family = binomial, subset = train)
summary(log.train.model2)

# library(bestglm)
# best.models <- bestglm(df[ , -c(1, 8)], family = binomial, IC = "CV")
# best.models$Subsets
df.test2 <- dfnew[!train, ]
Direction.test2 <- df.test2$Direction[!train]
  
log.train.probs2 <- predict(log.train.model2, df.test2[ , -c(1:5, 11)], type = "response")
log.train.pred2 <- rep("Down", 104)
log.train.pred2[log.train.probs2 > 0.5] <- "Up"
table(log.train.pred2, Direction.test2)

## LDA experimentation
lda.train.model2 <- lda(Direction ~ Today, data = df, subset = train)
lda.train.model2
lda.train.pred2 <- predict(lda.train.model2, df.test)
names(lda.train.pred2)
# The LDA and logistic regression predictions are almost identical.
lda.train.class2 <- lda.train.pred2$class
table(lda.train.class2, Direction.test)

## QDA experimentation
qda.train.model2 <- qda(Direction ~ Lag1 + Volume, data = df, subset = train)
qda.train.model2
qda.train.pred2 <- predict(qda.train.model2, df.test)
names(qda.train.pred2)
qda.train.class2 <- qda.train.pred2$class
table(qda.train.class2, Direction.test)



dim(df[!train,])

df.test <- df[!train, ]
