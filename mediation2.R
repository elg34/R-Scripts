rm(list = ls())
library(mediation)
library(multilevel)

# # # Data and analysis taken from:
# http://data.library.virginia.edu/introduction-to-mediation-analysis/
data <- read.csv('http://static.lib.virginia.edu/statlab/materials/data/mediationData.csv')

# # # Is the mediation effect statistically significant?
# Option 1: Sobel test (Sobel, 1982) 
# Option 2 (recently preferred): Bootstrapping (Preacher & Hayes, 2004)
# For bootstrapping at least 500 simulations recommended

# # # OPTION 1 :: Mediation (Sobel!)
sobel(data$X,data$M,data$Y)

# # # OPTION 2 :: Mediation (Bootstrap!)
# Total Effect = a total effect of X on Y (Y~X). 
# The direct effect (ADE) = a direct effect of X on Y after taking into account M (Y~X+M). 
# The mediation effect (ACME) is the total effect minus the direct effect
# Mediation significant = ACME significant!
b <- lm(M ~ X, data=data)     # Linear Regression: Effect of Treatment on Mediator
c <- lm(Y ~ X + M, data=data) # Linear Regression: Effect of Treatment on Outcome
boot_med <- mediate(b, c, sims=500, boot=TRUE, treat="X", mediator="M")
summary(boot_med)
plot(boot_med)

