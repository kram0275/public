rm(list = ls())

df <- read.csv(file = "dummy_data.csv")
# Data comprises 4 observations over 2 variables: the response *performance* 
# the predictor *ratio*
colnames(df) <- c("ratio", "perf")
# Change perf data type to numeric
df$perf <- as.numeric(df$perf)
# Create variable based off *ratio*
df$ratio.sq <- (df$ratio)^2
# Response in first column
df <- df[ , c(2, 1, 3)]

attach(df)

poly(ratio, 2, raw = TRUE)


# Being exploratory data analysis
plot(perf ~ ratio)

scaled.ratio <- poly(ratio, 2)
scaled.ratio
## attr(,"coefs")$alpha
## [1] 0.35 0.35

## attr(,"coefs")$norm2
## [1] 1e+00 4e+00 5e-02 4e-04

poly.scaled <- lm(perf ~ poly(ratio, 2), data = df)
summary(poly.scaled)

## beta_1 = 622.969
## beta_2 = -439
## ratio_crit = -(622.969) / 2 / (-439) = 0.7095

#### The unscaled critical ratio is 0.413462