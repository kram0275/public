rm(list = ls())
plot.new()
library(ISLR)
library(e1071)
library(car)
library(tree)
summary(OJ)
## Basic EDA
df <- OJ
# cor(df2[, -c(1, 13)])
# Categorical variables that need to be fixed include: StoreID (5 levels: 1:4, 7), Store (5 levels: 0:4).  These seem to be duplicate variables.
# plot(df$StoreID ~ df$STORE)
# Duplicate variables confirmed
# Create dummy variables for Store.  Use Store = 0 as reference.
df2 <- data.frame(df, store1 = numeric(1070), 
                  store2 = numeric(1070), 
                  store3 = numeric(1070), 
                  store4 = numeric(1070))
for(i in 1:1070) {
  for(j in 1:4){
    if(df2$STORE[i] == j){
      df2[i, 18 + j] <- 1
    }
  }
}
rm(i, j)
df2 <- df2[ , -c(3, 14, 18)]

# Create train:test, where n_train = 800
set.seed(2)
indices <- sample(1:1070, size = 800)
train <- df2[indices, ]
test <- df2[-indices, ]

#### Begin classification tree
tree.OJ <- tree(Purchase ~ ., train)
summary(tree.OJ)
tree.OJ
plot(tree.OJ)
text(tree.OJ, pretty = 0)
tree.pred1 <- predict(tree.OJ, test, type = "class")
table(tree.pred1, test[ , "Purchase"])
(147 + 62)/270 # Equals 77.41% prediction success rate
# Next, we consider whether pruning the tree might lead to improved results. The function cv.tree() performs cross-validation in order to determine the optimal level of tree complexity; cost complexity pruning is used in order to select a sequence of trees for consideration. We use the argument FUN = prune.misclass in order to indicate that we want the classification error rate to guide the CV and pruning process, rather than the default for the cv.tree() function, which is deviance. The cv.tree() function reports the number of terminal nodes of each tree considered (size) as well as the corresponding error rate and the value of the cost-complexity parameter used (k, which corresponds to alpha in 8.4)
set.seed(4)
cv.OJ <- cv.tree(tree.OJ, FUN = prune.misclass)
# names(cv.OJ)
# cv.OJ

# Note that, despite the name, dev corresponds to the CV error rate in this instance. The tree with 5 terminal nodes results in the lowest CV error rate, with 151 CV errors. We plot the error rate as a function of both size and k.
par(mfrow = c(1, 2))
plot(cv.OJ$size, cv.OJ$dev, type = "b")
plot(cv.OJ$k, cv.OJ$dev, type = "b")
# We now apply the prunce.misclass() function in order to prune the tree to obtain the five-node tree.
prune.OJ <- prune.misclass(tree.OJ, best = 5)
plot(prune.OJ)
text(prune.OJ, pretty = 0)
# How well does this pruned tree perform on the test data set? Once again, we apply the predict() function. 
tree.pred2 <- predict(prune.OJ, test, type = "class")
table(tree.pred2, test[ , "Purchase"])

#### Begin size optimization
which(cv.OJ$dev == min(cv.OJ$dev))

#### Begin split testing
error.matrix <- matrix(numeric(), nrow = 50, ncol = 4)
colnames(error.matrix) <- c("Unpruned Test Error Rate", "Unpruned Train Error Rate", "Pruned Test Error Rate", "Pruned Train Error Rate")
for(i in 1:50){
  # Create train:test, where n_train = 800
  set.seed(i)
  indices <- sample(1:1070, size = 800)
  train <- df2[indices, ]
  test <- df2[-indices, ]
  
  #### Begin classification tree
  tree.OJ <- tree(Purchase ~ ., train)
  tree.pred1 <- predict(tree.OJ, test, type = "class")
  rate1 <- table(tree.pred1, test[ , "Purchase"])
  error.matrix[i , 1] <- 1 - ((rate1[1, 1] + rate1[2, 2]) / 270)
  
  tree.OJ <- tree(Purchase ~ ., train)
  tree.pred2 <- predict(tree.OJ, train, type = "class")
  rate2 <- table(tree.pred2, train[ , "Purchase"])
  error.matrix[i , 2] <- 1 - ((rate2[1, 1] + rate2[2, 2]) / 800)
  
  set.seed(1)
  cv.OJ <- cv.tree(tree.OJ, FUN = prune.misclass)
  
  prune.OJ <- prune.misclass(tree.OJ, 
                             best = cv.OJ$size[which(cv.OJ$dev 
                                                     == min(cv.OJ$dev))])
  tree.pred3 <- predict(prune.OJ, test, type = "class")
  rate3 <- table(tree.pred3, test[ , "Purchase"])
  error.matrix[i , 3] <- 1 - ((rate3[1, 1] + rate3[2, 2]) / 270)
  
  prune.OJ <- prune.misclass(tree.OJ, best = cv.OJ$size[which(cv.OJ$dev 
                                                              == min(cv.OJ$dev))])
  tree.pred4 <- predict(prune.OJ, train, type = "class")
  rate4 <- table(tree.pred4, train[ , "Purchase"])
  error.matrix[i , 4] <- 1 - ((rate4[1, 1] + rate4[2, 2]) / 800)
}
error.matrix
# Graph error rates
par(mfrow = c(1, 2))
plot(error.matrix[ , 1], type = "l", col = "red", 
     xlab = "x in set.seed(x) for Split",
     ylab = "Test Error Rate")
lines(error.matrix[ , 3], col = "blue")

plot(error.matrix[ , 2], type = "l", col = "red", 
     xlab = "x in set.seed(x) for Split",
     ylab = "Training Error Rate")
lines(error.matrix[ , 4], col = "blue")
legend("topright",
       legend = c("Unpruned", "Pruned"),
       col = c("red", "blue"),
       pch = c("l", "l")
)

length(which(error.matrix[ , 1] > error.matrix[ , 3]))
length(which(error.matrix[ , 2] > error.matrix[ , 4]))

#### Examine CV Tweak
error.matrix2 <- matrix(numeric(), nrow = 50, ncol = 4)
colnames(error.matrix2) <- c("Unpruned Test Error Rate", "Unpruned Train Error Rate", "Pruned Test Error Rate", "Pruned Train Error Rate")
for(i in 1:50){
  # Create train:test, where n_train = 800
  set.seed(1)
  indices <- sample(1:1070, size = 800)
  train <- df2[indices, ]
  test <- df2[-indices, ]
  
  #### Begin classification tree
  tree.OJ <- tree(Purchase ~ ., train)
  tree.pred1 <- predict(tree.OJ, test, type = "class")
  rate1 <- table(tree.pred1, test[ , "Purchase"])
  error.matrix2[i , 1] <- 1 - ((rate1[1, 1] + rate1[2, 2]) / 270)
  
  tree.OJ <- tree(Purchase ~ ., train)
  tree.pred2 <- predict(tree.OJ, train, type = "class")
  rate2 <- table(tree.pred2, train[ , "Purchase"])
  error.matrix2[i , 2] <- 1 - ((rate2[1, 1] + rate2[2, 2]) / 800)
  
  set.seed(i)
  cv.OJ <- cv.tree(tree.OJ, FUN = prune.misclass)
  
  prune.OJ <- prune.misclass(tree.OJ, 
                             best = cv.OJ$size[which(cv.OJ$dev 
                                                     == min(cv.OJ$dev))])
  tree.pred3 <- predict(prune.OJ, test, type = "class")
  rate3 <- table(tree.pred3, test[ , "Purchase"])
  error.matrix2[i , 3] <- 1 - ((rate3[1, 1] + rate3[2, 2]) / 270)
  
  prune.OJ <- prune.misclass(tree.OJ, best = cv.OJ$size[which(cv.OJ$dev 
                                                              == min(cv.OJ$dev))])
  tree.pred4 <- predict(prune.OJ, train, type = "class")
  rate4 <- table(tree.pred4, train[ , "Purchase"])
  error.matrix2[i , 4] <- 1 - ((rate4[1, 1] + rate4[2, 2]) / 800)
}
error.matrix2
# Graph error rates
par(mfrow = c(1, 2))
plot(error.matrix2[ , 1], type = "l", col = "red", 
     xlab = "x in set.seed(x) for CV",
     ylab = "Test Error Rate")
lines(error.matrix2[ , 3], col = "blue")

plot(error.matrix2[ , 2], type = "l", col = "red", 
     xlab = "x in set.seed(x) for CV",
     ylab = "Training Error Rate")
lines(error.matrix2[ , 4], col = "blue")
legend("topright",
       legend = c("Unpruned", "Pruned"),
       col = c("red", "blue"),
       pch = c("l", "l")
)

length(which(error.matrix2[ , 1] > error.matrix2[ , 3]))
length(which(error.matrix2[ , 2] > error.matrix2[ , 4]))
