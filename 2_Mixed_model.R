# Exploratory modelling
library(lme4)
library(rms)
library(performance)

fit_mm <- lmer(
  income ~ timepoint + dhealth + toothach + appear + costprev + 
    sex + rcs(age, 4) + highqual + remotecode + IRSAD + 
    (1|ID),
  data = surv_long
)

AIC(fit_mm)
check_model(fit_mm)
