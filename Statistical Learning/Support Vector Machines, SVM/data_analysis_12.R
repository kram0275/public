rm(list = ls())
plot.new()
library(ISLR)
library(e1071)
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
df2 <- df2[ , -c(3, 18)]

# Create train:test, where n_train = 800
set.seed(1)
indices <- sample(1:1070, size = 800)
train <- df2[indices, ]
test <- df2[-indices, ]

# Assign design matrix as x
model1 <- lm(Purchase ~ ., data = train)
xtrain <- model.matrix(model1)[ , -1]
model2 <- lm(Purchase ~ ., data = test)
xtest <- model.matrix(model2)[ , -1]
rm(model1, model2)

# Create dat for svm()
traindat <- data.frame(x = xtrain, y = as.factor(train$Purchase))
testdat <- data.frame(x = xtest, y = as.factor(test$Purchase))

#### Begin support vector classifier
svmfit.svc <- svm(y ~., data = traindat, kernel = "linear", cost = 0.01, scale = TRUE)
summary(svmfit.svc)
# Optimize cost with tune()
set.seed(1)
tune.out.svc <- tune(svm, y ~ ., data = traindat, kernel = "linear", ranges = list(cost = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10)))
# We can access the CV errors for each of these models using the summary() command.
summary(tune.out.svc)
# We see that cost = 0.1 results in the lowest CV error rate. The tune() function stores the best model obtained, which can be accessed:
bestmod.svc <- tune.out.svc$best.model
summary(bestmod.svc)
# Now we predict the class labels of these test observations. Here we use the best model obtained through CV in order to make predictions.
ypred.svc <- predict(bestmod.svc, testdat)
test.table.svc <- table(predict = ypred.svc, truth = testdat$y)
# Test error rate is 1 - [(140 + 80) / 270] = 18.5%
ypred2.svc <- predict(bestmod.svc, traindat)
train.table.svc <- table(predict = ypred2.svc, truth = traindat$y)
# Train error rate is 1 - [(437 + 235) / 800] = 16%

#### Begin support vector machine with radial.  Use default gamma.
svmfit.svmr <- svm(y ~ ., data = traindat, kernel = "radial", cost = 0.01, scale = TRUE)
summary(svmfit.svmr)
# Optimize cost with tune()
set.seed(1)
tune.out.svmr <- tune(svm, y ~ ., data = traindat, kernel = "radial", ranges = list(cost = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10)))
summary(tune.out.svmr)
bestmod.svmr <- tune.out.svmr$best.model
summary(bestmod.svmr)
ypred.svmr <- predict(bestmod.svmr, testdat)
test.table.svmr <- table(predict = ypred.svmr, truth = testdat$y)
# Test error rate is 1 - [(140 + 80) / 270] = 18.5%
ypred2.svmr <- predict(bestmod.svmr, traindat)
train.table.svmr <- table(predict = ypred2.svmr, truth = traindat$y)
# Train error rate is 1 - [(437 + 235) / 800] = 16%

#### Begin support vector machine with polynomial. Use degree = 2.
svmfit.svmp <- svm(y ~ ., data = traindat, kernel = "polynomial", cost = 0.01, degree = 2)
summary(svmfit.svmp)
# Optimize cost with tune()
set.seed(1)
tune.out.svmp <- tune(svm, y ~ ., data = traindat, kernel = "polynomial", degree = 2, ranges = list(cost = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10)))
summary(tune.out.svmp)
bestmod.svmp <- tune.out.svmp$best.model
summary(bestmod.svmp)
ypred.svmp <- predict(bestmod.svmp, testdat)
test.table.svmp <- table(predict = ypred.svmp, truth = testdat$y)
# Test error rate is 1 - [(140 + 80) / 270] = 18.5%
ypred2.svmp <- predict(bestmod.svmp, traindat)
train.table.svmp <- table(predict = ypred2.svmp, truth = traindat$y)
# Train error rate is 1 - [(437 + 235) / 800] = 16%

load(file = "error_matrix.Rda")

#### Begin set.seed() loop.

# error.matrix <- matrix(numeric(), nrow = 10, ncol = 6)
# colnames(error.matrix) <- c("SVC Train", "SVC Test", "SVM (R) Train", 
#                             "SVM (R) Test", "SVM (P) Train", "SVM (P) Test")
# for(i in 1:10){
#   set.seed(i)
#   tune.out.svc <- tune(svm, y ~ ., data = traindat, kernel = "linear", 
#                        ranges = list(cost = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10)))
#   bestmod.svc <- tune.out.svc$best.model
#   ypred.svc <- predict(bestmod.svc, testdat)
#   test.table.svc <- table(predict = ypred.svc, truth = testdat$y)
#   ypred2.svc <- predict(bestmod.svc, traindat)
#   train.table.svc <- table(predict = ypred2.svc, truth = traindat$y)
#   
#   set.seed(i)
#   tune.out.svmr <- tune(svm, y ~ ., data = traindat, kernel = "radial", 
#                         ranges = list(cost = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10)))
#   bestmod.svmr <- tune.out.svmr$best.model
#   ypred.svmr <- predict(bestmod.svmr, testdat)
#   test.table.svmr <- table(predict = ypred.svmr, truth = testdat$y)
#   ypred2.svmr <- predict(bestmod.svmr, traindat)
#   train.table.svmr <- table(predict = ypred2.svmr, truth = traindat$y)
#   
#   set.seed(i)
#   tune.out.svmp <- tune(svm, y ~ ., data = traindat, kernel = "polynomial", 
#                         degree = 2, ranges = 
#                           list(cost = c(0.01, 0.05, 0.1, 0.5, 1, 5, 10)))
#   bestmod.svmp <- tune.out.svmp$best.model
#   ypred.svmp <- predict(bestmod.svmp, testdat)
#   test.table.svmp <- table(predict = ypred.svmp, truth = testdat$y)
#   ypred2.svmp <- predict(bestmod.svmp, traindat)
#   train.table.svmp <- table(predict = ypred2.svmp, truth = traindat$y)
#   
#   error.matrix[i, 1] <- 1 - ((train.table.svc[1, 1] 
#                               + train.table.svc[2, 2]) / 800)
#   error.matrix[i, 2] <- 1 - ((test.table.svc[1, 1] 
#                               + test.table.svc[2, 2]) / 270)
#   error.matrix[i, 3] <- 1 - ((train.table.svmr[1, 1] 
#                               + train.table.svmr[2, 2]) / 800)
#   error.matrix[i, 4] <- 1 - ((test.table.svmr[1, 1] 
#                               + test.table.svmr[2, 2]) / 270)
#   error.matrix[i, 5] <- 1 - ((train.table.svmp[1, 1] 
#                               + train.table.svmp[2, 2]) / 800)
#   error.matrix[i, 6] <- 1 - ((test.table.svmp[1, 1] 
#                               + test.table.svmp[2, 2]) / 270)
# }
# save(error.matrix, file = "error_matrix.Rda")
load("error_matrix.Rda")
?tune
