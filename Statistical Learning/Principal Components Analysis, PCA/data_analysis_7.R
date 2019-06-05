## Perform PCA on the data in 'SRp'. Don't forget to center and scale all the variables. How many principal components would you need to adequately represent this data in lower dimension? Are the first few principal components capturing any interpretable combinations of the nutrients? 

rm(list = ls())
library(ggplot2)
library(ggfortify)
options(digits = 3, scipen = 999)

# Copy over pre-processing script and implement
SR = read.table("ABBREV.txt", header=F, row.names=1, sep="^", quote="~")
SR = na.omit(SR) # remove rows with missing values
SR = SR[row.names(SR) != "13352",] # remove "duplicate" entry
row.names(SR) = SR[,1] # set more meaningful row names
SR = SR[,-1]
names(SR) = c("Water_(g)", "Energ_Kcal", "Protein_(g)", "Lipid_Tot_(g)", "Ash_(g)", "Carbohydrt_(g)", "Fiber_TD_(g)", "Sugar_Tot_(g)", "Calcium_(mg)", "Iron_(mg)", "Magnesium_(mg)", "Phosphorus_(mg)", "Potassium_(mg)", "Sodium_(mg)", "Zinc_(mg)", "Copper_(mg)", "Manganese_(mg)", "Selenium_(µg)", "Vit_C_(mg)", "Thiamin_(mg)", "Riboflavin_(mg)", "Niacin_(mg)", "Panto_Acid_(mg)", "Vit_B6_(mg)", "Folate_Tot_(µg)", "Folic_Acid_(µg)", "Food_Folate_(µg)", "Folate_DFE_(µg)", "Choline_Tot_(mg)", "Vit_B12_(µg)", "Vit_A_IU", "Vit_A_RAE", "Retinol_(µg)", "Alpha_Carot_(µg)", "Beta_Carot_(µg)", "Beta_Crypt_(µg)", "Lycopene_(µg)", "Lut+Zea_(µg)", "Vit_E_(mg)", "Vit_D_µg", "Vit_D_IU", "Vit_K_(µg)", "FA_Sat_(g)", "FA_Mono_(g)", "FA_Poly_(g)", "Cholestrl_(mg)", "GmWt_1", "GmWt_Desc1", "GmWt_2", "GmWt_Desc2", "Refuse_Pct")
SRp = SR[,c(1:46)] # restrict to just the nutrient variables
# SRp is thus the data frame under investigation
df <- SRp

# Implement PCA
pr.out <- prcomp(df, scale = TRUE)
# Center and scale are means, SD's of the variables used for scaling prior to 
# implementing PCA
pr.out$center # == apply(df, 2, mean)
pr.out$scale # == apply(df, 2, sd)

table1 <- matrix(rbind(pr.out$center, pr.out$scale), nrow = 2, ncol = 46, byrow = FALSE)
colnames(table1) <- names(pr.out$center)
row.names(table1) <- c("Mean", "Std. Deviation")
table1

# The rotation matrix provides principal component loadings; each column of 
# pr.out$rotation contains the corresponding principal component loading vector
pr.out$rotation
# Recall there are generally min(n - 1, p) informative principal components

# With prcomp(), we do not need to explicitly multiply the data by the 
# principal component loading vectors to obtain the PC score vectors. 
# Instead, the 50 x 4 matrix x has as its columns the PC score vectors.  That 
# is, the kth column is the kth principal component score vector.
dim(pr.out$x) # Returns 2223, 46
pr.out$x

pr.summ <- summary(pr.out)
pr.summ$importance

# We can plot the first two principal components as follows:
par(mfrow = c(1,1))
biplot(pr.out, scale = 0, xlabs=rep("x", nrow(SRp)), ylabs=substr(colnames(SRp),1,3),
       cex = 0.3)
abline(h=0,v=0, lty=2)
# scale = 0 ensures that the arrows are scaled to represent the loadings
# Note that this is a mirror image of Fig. 10.1.  Recall that the PCs are 
# only unique up to a sign change, so we can reproduce Fig. 10.1 by making a 
# few small changes.
# pr.out$rotation <- -pr.out$rotation
# pr.out$x <- -pr.out$x
# biplot(pr.out, scale = 0)

# The prcomp() function also outputs the SD of each principal component.
pr.out$sdev
# The variance explained by each principal component is obtained by squaring
pr.var <- pr.out$sdev^2
pr.var

# The proportion of variance explained by each PC, we divide each variance by
# the total variance
pve <- pr.var/sum(pr.var)*100
pve

# We can then plot the PVE explained by each component, as well as the 
# cumulative PVE as follows:
plot(pve, xlab = "Principal Component", 
     ylab = "Pct of Variance Explained", ylim = c(0, 20), type = "l")
plot(cumsum(pve), xlab = "Principal Component", 
     ylab = "Cumulative Pct of Variance Explained", ylim = c(0, 100),
     type = "b")
# Note that the function cumsum() computes the cumulative sum of the elements 
# of a numeric vector


# For interpretation of biplots, check out:
# https://blog.bioturing.com/2018/06/18/how-to-read-pca-biplots-and-scree-plots/


## Method #1: Basic threshold
# To determine a quantitative method of selecting the number of principal components, implement a barrier to entry of percentage of variance explained > 100/46, which is the expected % of variance explained for each p
thresh.pct <- 100/46 # Returns 2.17%
which(pve > thresh.pct)
## This returns optimal amount of components = 14
# Alternatively, using a threshold of cumsum(pve) > 80%
thresh.cum <- 80
which(cumsum(pve) > thresh.cum) # Returns 16:46
## This returns optimal amount of components = 16 (min amount that )

## Method #2: Numerical differentiation and peaks of derivatives
# See https://dmpeli.math.mcmaster.ca/Matlab/Math4Q3/NumMethods/Lecture3-3.html
h <- 1
# Calculate 5-point central difference for components = 3:44
first.deriv <- numeric(length = 46)
for (i in 3:44) {
  first.deriv[i] <- (-pve[i + 2] + 8*pve[i + 1] - 8*pve[i -1] + pve[i - 2]) / 
    (12*h)
}

plot(-first.deriv, pch = 1, xlab = "Principal Component", 
     ylab = "dpve / dx", col = "red")
which(-first.deriv == max(-first.deriv)) 
## This returns optimal components = 4
## While this does not seem to coincide with the elbow in the PVE plot, this would 
## likely lend itself to easier interpretation
-first.deriv

second.deriv <- numeric(length = 46)
for (i in 3:44) {
  second.deriv[i] <- (pve[i + 1] - 2*pve[i] + pve[i - 1]) / h^2
}
plot(second.deriv[3:44], type = "l", xlab = "Principal Component", 
     ylab = "d2(pve) / d(comp)^2", col = "blue")

plot(pve)
# lines(-first.deriv, col = "red")
lines(second.deriv, col = "blue")
summary(second.deriv)
plot.new()
plot(second.deriv, col = "blue", type = "l")

## Method #3: Elbow Method with identify()
plot(pve, xlab = "Principal Component", 
     ylab = "Pct of Variance Explained", ylim = c(0, 20), col = "blue")
identify(pve)
plot(pve, xlab = "Principal Component", 
     ylab = "Pct of Variance Explained", ylim = c(0, 20), col = "blue")
text(x = 7, y = 3.3, labels = "7", col = "blue")
## This returns optimal components = 7, 7, 7 (three replicates)

## Method 4a: Plot z1 vs zx, x >1.  Look for lowest numbers with decent 
## correlations.
# We can plot the first few principal component score vectors in order to visualize the data.  
autoplot(pr.out, data = df, size = 0.5, 
         label = FALSE, label.size = 1.5, label.colour = "purple",
         label.label = 1:nrow(df),
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, cex = 0.5)
# ?autoplot
# ??ggbiplot

thresh.table <- matrix(ncol = 2, nrow = 3)
colnames(thresh.table) <- c("Selection Method", "No. of Principal Components")
thresh.table[1, ] <- c("Elbow of scree plot", 8)
thresh.table[2, ] <- c("Cum. % of variance explained > 80", 16)
thresh.table[3, ] <- c("% of variance explained > 2.17", 14)
thresh.table[4, ] <- c("Eigenvalue > 1", 14)

pve*46/100
