# Load data and standardise outcome variables
load("surveydata.RData")

# Load required packages
library(rms)
library(performance)

# Set distributions of survey variables
dd <- datadist(surv_wide)
options(datadist = "dd")


### Ordinal model (ORM)
fit_orm <- orm(
  formula = income2 ~ 
    d_dhealth +
    income1 + # Prior income
    nsaoh1SEX + rcs(nsaoh1AGE, 5) + # Demographics
    nsaoh1IRSADSCORE2001, # Variation due to deprivation index
  data = surv_wide,
  x = T, y = T
)
# Interpretation: After accounting for income at baseline, demographics and regional indicators,
#   how do oral health at baseline and over the past 12 months affect income at time point 2?

# Model summary
fit_orm
# Model interpretation
summary(fit_orm) |> suppressWarnings()
# Based on the results from the model, moving from (Low) to (High) affects income by (Effect)
# From this model we can see that as self-rated oral health declines, 
#   respondents experience a lower than expected increase in income

AIC(fit_orm) # AIC useful for checking knots in non-linear spline terms
anova(fit_orm) # Checking impact of predictors
suppressWarnings(plot(Predict(fit_orm))) # Conditional relationships between income2 and X vars
plot(resid(fit_orm)) # Residuals plot (looks great)
check_collinearity(fit_orm) # Collinearity, measured by variance inflation factor
