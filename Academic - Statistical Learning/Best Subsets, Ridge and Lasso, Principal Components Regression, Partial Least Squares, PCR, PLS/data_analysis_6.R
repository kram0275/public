## Clear variables and load data set
rm(list = ls())
library(MASS)
library(ISLR)
library(car) # Has VIF
library(glmnet) # For ridge regression
library(nortest) # Has Anderson-Darling
library(olsrr) # Has Breusch-Pagan
library(leaps)
library(glmnet) # For Lasso, Ridge
library(pls) # For PCA
df <- College
?College
str(College) # Private is binary, all others continuous numeric
cor(College[ , 2:18])
# pairs(df, cex = 0.5)
# High corr (say, > 0.8): Enroll-Accept, Enroll-F.Undergrad, 
# Top10perc-Top25Perc, PhD-Terminal
summary(College) # Private has 565 / (565 + 212) = 72.71%
names(df) <- tolower(names(df)) # For ease of use

for(i in 2:16) {
  hist(df[ , i], xlab = colnames(df)[i])
}
rm(i)
# Normal: Grad Rate, S.F.Ratio (approx), Room.Board, Outstate, Top25perc
# Skewed right: Expend, perc.alumni, Personal, Books (very long tail/outliers?),
#               P.Undergrad, F.Undergrad, Top10perc, Enroll, Accept
# Skewed left: Terminal, PhD

full.model <- lm(apps ~ ., data = df)
summary(full.model) # SE(res) = 1041, Adj R-Sq = 92.76
vif(full.model) # accept, top10perc, top25perc > 5, enroll, f.undergrad > 10

summary(lm(apps ~ . - enroll - f.undergrad, data = df))
vif(lm(apps ~ . - enroll - f.undergrad, data = df))

# Modify terminal and top25perc
df$top75to90 <- df$top25perc - df$top10perc
# df$nonphdterm <- df$terminal - df$phd
df <- df[ , c(1:5, 7:19)] 
cor(df$top75to90, df$top10perc)
# Re-run model with modified variables
full.model6 <- lm(apps ~ ., data = df)
summary(full.model6) # SE(res) = 1062, Adj R-Sq = 92.47
vif(full.model6) # Pretty reasonable
# Re-run model with modified variables, and without enroll, f.undergrad
full.model2 <- lm(apps ~ . - enroll - f.undergrad, data = df)
summary(full.model2) # SE(res) = 1062, Adj R-Sq = 92.47
vif(full.model2) # Pretty reasonable
# plot(full.model2)
2*sqrt((19 + 1)/(777 - 19 - 1)) # DFFITs threshold = 0.3251
which(dffits(full.model2) > 0.3251)
which(cooks.distance(full.model2) > 1) # Returns Rutgers at New Brunswick
# It seems very reasonable that Rutgers be taken out of the data
which(row.names(df) == "Rutgers at New Brunswick") # Row 484
df <- df[-484, ] # DELETE RUTGERS; New n = 776
# Re-run model with no Rutgers
full.model3 <- lm(apps ~ . - enroll - f.undergrad, data = df)
summary(full.model3) # SE(res) = 979.4, Adj R-Sq = 92.25; best error thus far
vif(full.model3) # About the same as VIF for 2
# plot(full.model3)
# Check out BoxCox
box.full3 <- boxCox(full.model3)
which(box.full3$y == max(box.full3$y)) # Returns 63
box.full3$x[63] # Returns 0.5050.....  Use SQRT transform!
# Modify df$apps
full.model4 <- lm(sqrt(apps) ~ . - enroll - f.undergrad, data =df)
summary(full.model4) # SE(res) = 8.426, Adj R-Sq = 90.04
vif(full.model4) # Pretty reasonable
plot(full.model4) 
# Most worrisome is Purdue-West Lafayette, but not high leverage, low Cook's
# distance
hist(sqrt(df$apps)) # Still a bit skewed right, but better
hist(log(df$apps)) # Significantly better than sqrt(apps)
# Try log transform
full.model5 <- lm(log(apps) ~ . - enroll - f.undergrad, data =df)
summary(full.model5) # SE(res) = 0.5028, Adj R-Sq = 77.82
vif(full.model5) # Pretty reasonable
# plot(full.model5) 

## Transform df to permanently exclude enroll and f.undergrad
df <- df[ , -c(4, 6)]
## Transform df to permanently have sqrt(apps) as response
df$sqrt.apps <- sqrt(df$apps)
df <- df[ , c(17, 1, 3:16)]

best.model <- lm(sqrt.apps ~ . - outstate - terminal - top75to90 - personal, 
                 data = df)
summary(best.model)
vif(best.model)

save(df, file = "mod_df.rda")

#### Move forward with full.model4 (sqrt(y), no enroll, no f.undergrad)
library(leaps)
# Split the data set into a training set and a test set
set.seed(1) # Originally equal to 1
train=sample(1:nrow(df), nrow(df)/2)
test=(-train)

y <- df$sqrt.apps
x <- model.matrix(sqrt.apps ~ ., data = df)[, -1]

# Fit model using least-squares
ols.model <- lm(sqrt.apps ~ ., data = df[train, ])
ols.beta <- matrix(ols.model$coefficients)
# Calculate test responses
pred <- model.matrix(sqrt.apps ~ ., data = df)[test,] %*% ols.beta
mse.ols <- mean((df$sqrt.apps[test] - pred)^2) # OLS MSE = 80.21417

# Use best subsets
regfit.best=regsubsets(sqrt.apps ~ . ,data=df[train, ], nvmax = 15)
best.test.mat=model.matrix(sqrt.apps ~ .,data=df[test,])
best.val.errors=rep(NA,15)

for(j in 1:15){
  coefi=coef(regfit.best,id=j)
  pred=best.test.mat[,names(coefi)]%*%coefi
  best.val.errors[j]=mean(((df$sqrt.apps[test] - pred)^2))
}
best.val.errors
coef(regfit.best, id = 11) 
# Include private, accept, top10perc, p.undergrad, room.board, books, phd, s.f.ratio, perc.alumni, expend, grad.rate
mse.subsets <- best.val.errors[which(best.val.errors == min(best.val.errors))]
# Best subsets MSE = 79.41252 (11 variables)

# Use ridge regression
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x[train, ],y[train],alpha=0,lambda=grid, thresh=1e-12)
# Get optimal lambda
ridge.cv.out=cv.glmnet(x[train, ],y[train],alpha=0)

ridge.bestlam=ridge.cv.out$lambda.min
ridge.bestlam # Returns best lambda = 2.64715
# Get predictions and MSE
ridge.pred=predict(ridge.mod,s=ridge.bestlam,newx=x[test, ])
mse.ridge <- mean((ridge.pred-y[test])^2) # Returns Ridge MSE = 81.77377

# Use lasso regression
lasso.mod=glmnet(x[train, ],y[train],alpha=1,lambda=grid)

set.seed(1)
# Get optimal lambda
lasso.cv.out=cv.glmnet(x[train, ],y[train],alpha=1)

lasso.bestlam=lasso.cv.out$lambda.min # Returns best lambda = 0.15869
lasso.pred=predict(lasso.mod,s=lasso.bestlam,newx=x[test, ])
mse.lasso <- mean((lasso.pred-y[test])^2) 
# Returns Lasso MSE = 80.83988; lower than Ridge, slightly better than OLS
lasso.out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(lasso.out,type="coefficients",s=lasso.bestlam)[1:15,]
lasso.coef 
lasso.coef[lasso.coef!=0] # Non-zero: terminal, outstate

# Use PCR
# The CV scores are sqrt(MSE)!!!  To get MSE, we must square.
# We then perform PCR on the training data and evaluate its test performance.
set.seed(1)
pcr.fit=pcr(sqrt.apps ~., data=df,subset=train,scale=TRUE, validation="CV")
summary(pcr.fit) # PCR M = 12 components for train set fit
# We can also plot cross-validation scores using the validationplot() function.
# val.type = "MSEP" gives the cross-validation MSE plot
validationplot(pcr.fit,val.type="MSEP")
pcr.pred=predict(pcr.fit,x[test,],
                 ncomp=which(pcr.fit$validation$PRESS == 
                               min(pcr.fit$validation$PRESS)))
mse.pcr <- mean((pcr.pred-y[test])^2) 
# Returns PCR MSE = 81.78060; almost as good as least squares

# We implement PLS using plsr()
set.seed(1)
pls.fit=plsr(sqrt.apps~., data=df,subset=train,scale=TRUE, validation="CV")
summary(pls.fit) # PLS M = 6 comps
validationplot(pls.fit,val.type="MSEP")
pls.pred=predict(pls.fit,x[test,],ncomp=which(pls.fit$validation$PRESS == 
                                                min(pls.fit$validation$PRESS)))
mse.pls <- mean((pls.pred-y[test])^2)
# Returns PLS MSE = 80.35265