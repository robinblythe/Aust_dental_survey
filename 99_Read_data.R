options(scipen = 100, digits = 5)

library(tidyverse)

# Import and select columns, create consistent ID
surveydata_raw <- vroom::vroom("C:/Users/Robin/NUS Dropbox/Robin Daniel Blythe/NDCS programs/Australian dental survey/Data/NSAOH_dataset_19DEC2024.csv") |>
  select(
    ### Time 2
    nsaoh2ID, nsaoh2numteeth, nsaoh2dhealth, nsaoh2toothach, nsaoh2appear, nsaoh2avoidfo,
    nsaoh2gumdisea, nsaoh2gumtold, nsaoh2costprev,
    nsaoh2sex, nsaoh2age, nsaoh2abtsi, nsaoh2schyear, nsaoh2highqual,
    nsaoh2employ, nsaoh2unemploy, nsaoh2income, nsaoh2remotecodecurr, nsaoh2IRSADscorecurr2016,
    ### Time 1
    nsaoh1ID, nsaoh1NUMTEETH, nsaoh1DHEALTH, nsaoh1TOOTHACH, nsaoh1APPEAR, nsaoh1AVOIDFO,
    nsaoh1GUMDISEA, nsaoh1GUMTOLD, nsaoh1COSTPREV,
    nsaoh1SEX, nsaoh1AGE, nsaoh1ABTSI, nsaoh1SCHYEAR, nsaoh1HIGHQUAL,
    nsaoh1EMPLOY, nsaoh1UNEMPLOY, nsaoh1INCOME, nsaoh1REMOTECODE, nsaoh1IRSADSCORE2001
  ) |>
  mutate(ID = row_number())

# Single row per person, for linear model
surv_wide <- surveydata_raw |>
  mutate(
    income1 = case_when(
      # Standardise income variable
      nsaoh1INCOME %in% c(1, 2) ~ 1,
      nsaoh1INCOME > 1 ~ nsaoh1INCOME - 1,
      .default = nsaoh1INCOME),
    income2 = case_when(
      nsaoh2income %in% c(4, 5) ~ 4,
      nsaoh2income %in% c(6, 7) ~ 5,
      nsaoh2income %in% c(8, 9) ~ 6,
      nsaoh2income > 9 ~ 7,
      .default = nsaoh2income
    ),
  # Replace negative values with NA
  across(everything(), ~ replace(., . < 0, NA))) |>
  select(ID, income1, income2, 
         nsaoh2numteeth:nsaoh2IRSADscorecurr2016,
         nsaoh1NUMTEETH:nsaoh1IRSADSCORE2001
         )

# Split into separate dataframes by timepoint to recombine later in long form
surv_t1 <- cbind(surveydata_raw, timepoint = 1) |>
  select(ID, timepoint, nsaoh1NUMTEETH:nsaoh1IRSADSCORE2001)
names(surv_t1)[3:20] <- substring(tolower(names(surv_t1)[3:20]), 7)
names(surv_t1)[20] <- "IRSAD"

surv_t2 <- cbind(surveydata_raw, timepoint = 2) |>
  select(ID, timepoint, nsaoh2numteeth:nsaoh2IRSADscorecurr2016)
names(surv_t2)[3:20] <- substring(tolower(names(surv_t2)[3:20]), 7)
names(surv_t2)[19:20] <- c("remotecode", "IRSAD")

# Convert to long form
surv_long <- bind_rows(surv_t1, surv_t2) |>
  mutate(income = case_when(
    # Standardise income variable
    timepoint == 1 & income %in% c(1, 2) ~ 1,
    timepoint == 1 & income > 1 ~ income - 1,
    timepoint == 2 & income %in% c(4, 5) ~ 4,
    timepoint == 2 & income %in% c(6, 7) ~ 5,
    timepoint == 2 & income %in% c(8, 9) ~ 6,
    timepoint == 2 & income > 9 ~ 7,
    .default = income
  ),
  # Replace negative values with NA
  across(everything(), ~ replace(., . < 0, NA))) |>
  # Keep only IDs with income in both timepoints
  group_by(ID) |>
  filter(!is.na(income),
         all(c(1, 2) %in% timepoint)) |>
  ungroup()

remove(surveydata_raw, surv_t1, surv_t2)

