## tabler.lm()

## Will Kramlinger, 1/23/19

## The function takes as input an lm model and then gives as output a 
## matrix which holds the most relevant statistical output.

## The intent is to create a table which is easily displayed using, say, 
## data tables in Shiny, or with kableExtra in Markdown.

## Example usage:
## 
##  ##  the.model <- lm(y ~ x1 + x2 + x3, data = the.data)
##  ##  X <- tabler.lm(the.model)
##  
## X will then be the desired matrix.  Alternatively,
## 
##  ##  X <- tabler.lm(lm(y ~ x1 + x2 + x3, data = the.data))

tabler.lm.slr <- function(the_model) { 
  require(car)
  summ <- summary(the_model)
  n <- dim(summ$coefficients)[1] # Number of predictors including intercept
  full.matrix <- matrix(nrow = n + 6, ncol = 5)
  row.names(full.matrix) <- c(row.names(summ$coefficients), "----------", "s",
                              "Residual DF", "R Squared", "Adj. R Squared", 
                              "F, numDF, denomDF")
  colnames(full.matrix)[1:5] <- c("Coefficient", "Std. Error", "t-stat", 
                                  "P-value", "VIF")
  for(i in 1:n) {
    for(j in 1:4) {
      full.matrix[i, j] <- levels(as.factor(round(summ$coefficients[i, j], 4)))
    }
  }
  
  # full.matrix[2:n, 5] <- round(vif(the_model)[1:(n - 1)], 3)
  
  full.matrix[n + 1, ] <- levels(as.factor("----------"))
  full.matrix[n + 2, 1] <- levels(as.factor(round(summ$sigma, 4)))
  full.matrix[n + 3, 1] <- levels(as.factor(round(summ$df[2], 4)))
  full.matrix[n + 4, 1] <- levels(as.factor(round(summ$r.squared, 4)))
  full.matrix[n + 5, 1] <- levels(as.factor(round(summ$adj.r.squared, 4)))
  full.matrix[n + 6, 1] <- levels(as.factor(round(summ$fstatistic[1])))
  full.matrix[n + 6, 2] <- levels(as.factor(round(summ$fstatistic[2])))
  full.matrix[n + 6, 3] <- levels(as.factor(round(summ$fstatistic[3])))
  full.matrix[n + 2:5, 2:5] <- ""
  full.matrix[n + 6, 4:5] <- ""
  return(full.matrix)
}

save(tabler.lm.slr, file = "tabler_lm_slr.Rda")
