rm(list = ls())

# Only do this if not already installed
install.packages('ivreg')

library(ivreg)

# OLS regression (biased?)
reg_OLS = lm(log(wage) ~ education + experience + ethnicity + smsa + south, data = SchoolingReturns)
summary(reg_OLS)

# IV regression (unbiased?)
reg_IV = ivreg(log(wage) ~ education + experience + ethnicity + smsa + south |
              nearcollege + age + ethnicity + smsa + south,
            data = SchoolingReturns)
summary(reg_IV)
