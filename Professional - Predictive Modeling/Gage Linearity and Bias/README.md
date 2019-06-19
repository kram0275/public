# public/Professional - Predictive Modeling/Gage Linearity and Bias

Report: https://kram0275.github.io/public/gage.html

RMarkdown code found in .Rmd file. 

Data analysis report summarizing OLS regressions and a gage linearity and bias analysis of a small 10 x 3 data set.

## Problem

A co-worker had developed **two portable test machines** (*m1, m2*) meant to measure the same property as an in-house system, the **Reference** (*ref*) system. The absolute values of the measurements between machines were quickly found to differ significantly when testing with the same sample.

Two models, one for each machine, were desired to predict *ref* measurements based on the readings of each of *m1* and *m2*.  Desired accuracy for each model was within 10% of the *ref* value. 