# Load data and standardise outcome variables
load("surveydata.RData")

# Load required packages
library(rms)
library(performance)

# Set distributions of survey variables
dd <- datadist(surv_wide)
options(datadist = "dd")


### Ordinal model (ORM)
fit_dhealth <- lrm(
  formula = income2 ~ 
    d_dhealth +
    income1 + # Prior income
    nsaoh1SEX + rcs(nsaoh1AGE, 5) + # Demographics
    nsaoh1IRSADSCORE2001, # Variation due to deprivation index
  data = subset(surv_wide, nsaoh2age < 65),
  x = T, y = T
)

fit_numteeth <- lrm(
  formula = income2 ~ 
    d_numteeth +
    income1 + # Prior income
    nsaoh1SEX + rcs(nsaoh1AGE, 5) + # Demographics
    nsaoh1IRSADSCORE2001, # Variation due to deprivation index
  data = subset(surv_wide, nsaoh2age < 65),
  x = T, y = T
)

fit_dhealth
fit_numteeth
summary(fit_numteeth)



# Model summary
anova(fit_numteeth) # Checking impact of predictors
plot(resid(fit_numteeth)) # Residuals plot (looks great)
