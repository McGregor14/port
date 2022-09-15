# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder flare interim data is stored in
redcap_interim_data_loc <-
  here("data", "interim", "redcap", "step-01")

# Read in dataset
stai_data <-
  read_rds(paste0(redcap_interim_data_loc, "/redcap-stai-data", ".Rds")) %>%
  remove_empty(which = c("rows", "cols")) %>%
  remove_constant(na.rm = T, quiet = F)

# Reverse score items
# Method: take the maximum value for the scale, add 1, and subtract the participant's score.
# Reverse the following items: 1, 3, 6, 7, 10, 13, 14, 16, 19
stai_data <- stai_data %>%
  mutate(across(
    .cols = c(
      stai_baseline_1,
      stai_baseline_3,
      stai_baseline_6,
      stai_baseline_3,
      stai_baseline_7,
      stai_baseline_10,
      stai_baseline_13,
      stai_baseline_14,
      stai_baseline_16,
      stai_baseline_19,
      
      stai_followup_1,
      stai_followup_3,
      stai_followup_6,
      stai_followup_3,
      stai_followup_7,
      stai_followup_10,
      stai_followup_13,
      stai_followup_14,
      stai_followup_16,
      stai_followup_19
    ),
    
    ~ 4 + 1 - .
  ))

# Generate derived variables
stai_data <- stai_data %>%
  mutate(
    # STAI: Sum of all items
    stai_baseline_trait_total = rowSums(select(
      ., contains("stai") & contains("baseline")
    )),
    stai_followup_trait_total = rowSums(select(
      ., contains("stai") & contains("followup")
    ))
  )

# Save data
saveRDS(stai_data,
        here("data", "interim", "redcap", "step-02", "stai.Rds"))
