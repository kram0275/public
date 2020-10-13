rm(list = ls())

df <- read.csv(file = "dummy_data.csv")
colnames(df) <- c("ratio", "performance")
df$ratio.sq <- ratio^2


attach(df)

plot(perf ~ ratio)

poly.unscaled <- lm(perf ~ ratio + ratio.sq, data = df)
summary(poly.unscaled)
# plot(poly.unscaled)

poly.scaled <- lm(perf ~ poly(ratio, 2), data = df)
summary(poly.scaled)


