rm(list = ls())
df <- read.csv(file = "dummydata.csv")
df <- df[ , -c(5:7)]
colnames(df)[1:4] <- c("sample", "ref", "m1", "m2")
df$bias1 <- df$m1 - df$ref
df$bias2 <- df$m2 - df$ref

rownames(df) <- df$sample
df <- df[ , -1]
df$ref[5:7] <- NA
str(df)

attach(df)

b1.model <- lm(bias1 ~ ref)
summary(b1.model)

b2.model <- lm(bias2 ~ ref)
summary(b2.model)

p1.val <- 1.98 # User-defined value
p2.val <- 1.67 # User-defined value

ref.predict1 <- (p1.val - b1.model$coefficients[1]) / 
  (b1.model$coefficients[2] + 1) 
                 
ref.predict2 <- (p2.val - b2.model$coefficients[1]) / 
  (b2.model$coefficients[2] + 1) 