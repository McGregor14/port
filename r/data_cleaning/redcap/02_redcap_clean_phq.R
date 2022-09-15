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
phq_data <-
  read_rds(paste0(redcap_interim_data_loc, "/redcap-phq-data", ".Rds")) %>%
  remove_empty(which = c("rows", "cols")) %>%
  select(participant_id, matches("\\d$"), everything())

# Clean existing variables
phq_data <- phq_data %>%
  mutate(
    # Set all -99 values (prefer not to answer) to NA
    across(
      .cols = contains("plans") |
        contains("preparations") | contains("intent"),
      ~ na_if(.,-99)
    ),
    
    # Recode 1 to 'yes' and 0 to 'no' for plans and preparations columns
    across(.cols = contains("plans") | contains("preparations"),
           ~ factor(recode(
             .,
             `1` = "yes",
             `0` = "no"
           )))
  )

# Generate derived variables
phq_data <- phq_data %>%
  mutate(
    # PHQ9: Sum of all individual items (1-9)
    phq9_screening_total = rowSums(select(
      .,
      starts_with("phq9") & contains("screening") & matches("\\d$")
    )),
    phq9_baseline_total = rowSums(select(
      ., starts_with("phq9") & contains("baseline") & matches("\\d$")
    )),
    phq9_followup_total = rowSums(select(
      ., starts_with("phq9") & contains("followup") & matches("\\d$")
    )),
    
    # PHQ9 severity: 0-4 minimal, 5-9 mild, 10-14 moderate, 15-19 moderately severe, 20+ severe
    phq9_screening_severity = factor(
      case_when(
        phq9_screening_total < 5 ~ "minimal",
        phq9_screening_total < 10 ~ "mild",
        phq9_screening_total < 15 ~ "moderate",
        phq9_screening_total < 20 ~ "moderately_severe",
        phq9_screening_total >= 20 ~ "severe",
        TRUE ~ NA_character_
      ),
      levels = c("minimal", "mild", "moderate", "moderately_severe", "severe")
    ),
    
    phq9_baseline_severity = factor(
      case_when(
        phq9_baseline_total < 5 ~ "minimal",
        phq9_baseline_total < 10 ~ "mild",
        phq9_baseline_total < 15 ~ "moderate",
        phq9_baseline_total < 20 ~ "moderately_severe",
        phq9_baseline_total >= 20 ~ "severe",
        TRUE ~ NA_character_
      ),
      levels = c("minimal", "mild", "moderate", "moderately_severe", "severe")
    ),
    
    phq9_followup_severity = factor(
      case_when(
        phq9_followup_total < 5 ~ "minimal",
        phq9_followup_total < 10 ~ "mild",
        phq9_followup_total < 15 ~ "moderate",
        phq9_followup_total < 20 ~ "moderately_severe",
        phq9_followup_total >= 20 ~ "severe",
        TRUE ~ NA_character_
      ),
      levels = c("minimal", "mild", "moderate", "moderately_severe", "severe")
    ),
    
    # PHQ9 binary depression: >=10 - depression diagnosis
    phq9_screening_binary_depression =
      case_when(
        phq9_screening_total >= 10 ~ TRUE,
        phq9_screening_total < 10 ~ FALSE,
        TRUE ~ NA
      ),
    
    phq9_baseline_binary_depression =
      case_when(
        phq9_baseline_total >= 10 ~ TRUE,
        phq9_baseline_total < 10 ~ FALSE,
        TRUE ~ NA
      ),
    
    phq9_followup_binary_depression =
      case_when(
        phq9_followup_total >= 10 ~ TRUE,
        phq9_followup_total < 10 ~ FALSE,
        TRUE ~ NA
      )
    
  )

# Save data
saveRDS(phq_data,
        here("data", "interim", "redcap", "step-02", "phq.Rds"))
