# Load data and standardise outcome variables
source("./99_Read_Data.R")

# Load required packages
library(rms)
library(performance)

# Set distributions of survey variables
dd <- datadist(surv_wide)
options(datadist = "dd")


### Ordinal model (ORM)
fit_orm1 <- orm(
  formula = income2 ~ 
    income1 + # Prior income
    nsaoh1DHEALTH + nsaoh2dhealth + # Dental health responses
    nsaoh1SEX + rcs(nsaoh1AGE, 5) + nsaoh1ABTSI + # Demographics
    nsaoh1REMOTECODE + nsaoh1IRSADSCORE2001, # Regional variation
  data = surv_wide,
  x = T, y = T
)
# Interpretation: After accounting for income at baseline, demographics and regional indicators,
#   how do oral health at baseline and over the past 12 months affect income at time point 2?

# Model summary
fit_orm1
# Model interpretation
summary(fit_orm1) |> suppressWarnings()
# Based on the results from the model, moving from (Low) to (High) affects income by (Effect)
# From this model we can see that as self-rated oral health declines, 
#   respondents experience a lower than expected increase in income

AIC(fit_orm1) # AIC useful for checking knots in non-linear spline terms
anova(fit_orm1) # Confirmation that age should be non-linear
suppressWarnings(plot(Predict(fit_orm1))) # Conditional relationships between income2 and X vars
plot(resid(fit_orm1)) # Residuals plot (looks great)
plot(check_collinearity(fit_orm1)) # Collinearity, measured by variance inflation factor

# Replication of above model, using number of teeth instead of self-reported oral health
fit_orm2 <- orm(
  formula = income2 ~ 
    income1 + # Prior income
    nsaoh1NUMTEETH + nsaoh2numteeth + # Dental health responses
    nsaoh1SEX + rcs(nsaoh1AGE, 5) + nsaoh1ABTSI + # Demographics
    nsaoh1REMOTECODE + nsaoh1IRSADSCORE2001, # Regional variation
  data = surv_wide,
  x = T, y = T
)

fit_orm2
summary(fit_orm2)

AIC(fit_orm2)
anova(fit_orm2)
suppressWarnings(plot(Predict(fit_orm2)))
plot(resid(fit_orm2))
plot(check_collinearity(fit_orm2))
