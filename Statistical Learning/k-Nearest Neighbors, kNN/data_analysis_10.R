rm(list = ls())
library(ISLR)
library(class)
df <- Weekly
df$numDirection <- numeric(length(df[ , 1]))
df$numDirection[df$Direction == "Up"] <- 1
df <- df[ , -9]
colnames(df)[9] <- "Direction"
df$logVol <- log(df$Volume)

train <- (df$Year <= 2008)
df.train <- df[train, ]
df.test <- df[!train, ]
Direction.train <- df$Direction[train]
Direction.test <- df$Direction[!train]

set.seed(8)
knn.pred <- knn(as.matrix(df.train$Lag2), as.matrix(df.test$Lag2), 
                Direction.train, k = 4)
conf.matrix <- table(knn.pred, Direction.test)
conf.matrix
(21 + 45) / 104
# Confusion matrix with k = 1 gives an overall accuracy rate of 50%.  Worst seen.

#### Optimization of k ####
predict.rate <- numeric(length = 100)
conf.list <- list()
for(i in 1:100) {
  set.seed(1)
  knn.pred <- knn(as.matrix(df.train$Lag2), as.matrix(df.test$Lag2), 
                  Direction.train, k = i)
  conf.matrix <- table(knn.pred, Direction.test)
  conf.list[[i]] <- conf.matrix
  predict.rate[i] <- (conf.matrix[1, 1] + conf.matrix[2, 2]) / 104
}
# conf.list
predict.rate
which(predict.rate == max(predict.rate))
plot(predict.rate, xlab = "k", ylab = "Overall Prediction Success Rate",
     main = "Dependence of KNN Success Rate on k")
####




#### Loop Experiment ####
predict.rate <- numeric(length = 9)
conf.list <- list()
best.k.list <- list()
predict.list <- list()
for(j in 1:50) {
  for(i in 1:9){
    set.seed(j)
    knn.pred <- knn(as.matrix(df.train$Lag2), as.matrix(df.test$Lag2), 
                    Direction.train, k = i)
    conf.matrix <- table(knn.pred, Direction.test)
    conf.list[[i]] <- conf.matrix
    predict.rate[i] <- (conf.matrix[1, 1] + conf.matrix[2, 2]) / 104
  }
  best.k.list[[j]] <- which(predict.rate == max(predict.rate))
  predict.list[[j]] <- predict.rate
}
best.k.list
best.k.table <- tabulate(unlist(best.k.list))

best.k.vector <- list()
for(i in 1:9){
  best.k.vector[[i]] <- rep(i, best.k.table[i])
}
hist(unlist(best.k.vector), xlab = "k", 
     main = "Frequency of k Producing Optimal Prediction Rate for set.seed(x), x = 1:50")
####

####
# max(unlist(predict.list))
m <- 1:450
predict.vector <- unlist(predict.list)
best.10 <- sort(unlist(predict.list), decreasing = TRUE)[1:10]
which(predict.vector == best.10[1]) # Returns 67 ==> x = 8, k = 4
# mean(predict.vector) # Returns 54.47
# range(predict.vector) # Returns 43.27, 63.46
hist(predict.vector, xlab = "Overall Prediction Rate")
hist(predict.vector[m %% 9 == 4], xlab = "Overall Prediction Rate")
####

#### Variable Transforms
# User manipulation of columns used in the knn() function allows investigation
# of predictor combinations
df$logVol <- log(df$Volume)
train <- (df$Year <= 2008)
df.train <- df[train, ]
df.test <- df[!train, ]

Direction.train <- df$Direction[train]
Direction.test <- df$Direction[!train]
predict.rate <- numeric(length = 9)
conf.list <- list()
for(i in 1:9) {
  set.seed(1)
  knn.pred <- knn(as.matrix(df.train[ , c(3, 10)]), as.matrix(df.test[ , c(3, 10)]), 
                  Direction.train, k = i)
  conf.matrix <- table(knn.pred, Direction.test)
  conf.list[[i]] <- conf.matrix
  predict.rate[i] <- (conf.matrix[1, 1] + conf.matrix[2, 2]) / 104
}
# conf.list
predict.rate
which(predict.rate == max(predict.rate))
# plot(predict.rate, xlab = "k", ylab = "Overall Prediction Success Rate",
#      main = "Dependence of KNN Success Rate on k")
#### End Variable Transforms


