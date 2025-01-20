# Load data and standardise outcome variables
load("surveydata.RData")

# Load required packages
library(rms)
library(performance)

# Set distributions of survey variables
dd <- datadist(surv_wide)
options(datadist = "dd")


### Ordinal model (ORM)
fit_orm_dhealth <- orm(
  formula = income2 ~ 
    d_dhealth +
    income1 + # Prior income
    nsaoh1SEX + rcs(nsaoh1AGE, 5) + # Demographics
    nsaoh1IRSADSCORE2001, # Variation due to deprivation index
  data = subset(surv_wide, nsaoh2age < 65),
  x = T, y = T
)

fit_orm_numteeth <- orm(
  formula = income2 ~ 
    d_numteeth +
    income1 + # Prior income
    nsaoh1SEX + rcs(nsaoh1AGE, 5) + # Demographics
    nsaoh1IRSADSCORE2001, # Variation due to deprivation index
  data = subset(surv_wide, nsaoh2age < 65),
  x = T, y = T
)





# Model summary
anova(fit_orm_numteeth) # Checking impact of predictors
suppressWarnings(plot(Predict(fit_orm_numteeth))) # Conditional relationships between income2 and X vars
plot(resid(fit_orm_numteeth)) # Residuals plot (looks great)
check_collinearity(fit_orm_numteeth) # Collinearity, measured by variance inflation factor
