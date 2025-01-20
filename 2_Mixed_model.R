# Exploratory modelling
load("surveydata.RData")

library(nlme)
library(rms)
library(performance)

dd_mixed <- datadist(surv_long); options(datadist = "dd_mixed")

fit_mm <- Gls(
  income ~ timepoint * numteeth + appear + costprev + 
    sex + rcs(age, 4) + highqual + IRSAD,
  data = subset(surv_long, age < 65),
  correlation = corCompSymm(form = ~1|ID)
)

AIC(fit_mm)
check_model(fit_mm)
summary(fit_mm) |> suppressWarnings() # check model effect sizes
anova(fit_mm)
plot(Predict(fit_mm)) |> suppressWarnings()
fit_mm
