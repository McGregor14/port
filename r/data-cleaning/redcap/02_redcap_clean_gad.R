# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning
library(naniar) # missing data

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder that interim data is stored in
redcap_interim_data_loc <-
  here("data", "interim", "redcap", "step-01")

# Read in dataset
gad_data <-
  read_rds(paste0(redcap_interim_data_loc, "/redcap-gad-data", ".Rds")) %>%
  remove_empty(which = c("rows", "cols")) %>%
  remove_constant(na.rm = T, quiet = F)

# Generate derived variables
gad_data <- gad_data %>%
  mutate(
    # GAD7: Sum of all items
    gad7_screening_total = rowSums(select(
      ., starts_with("gad7") & contains("screening")
    )),
    gad7_baseline_total = rowSums(select(
      ., starts_with("gad7") & contains("baseline")
    )),
    gad7_followup_total = rowSums(select(
      ., starts_with("gad7") & contains("followup")
    )),
    
    # GAD7 severity: 0-4 minimal, 5-9 mild, 10-14 moderate, 15+ severe
    gad7_screening_severity = factor(
      case_when(
        gad7_screening_total < 5 ~ "minimal",
        gad7_screening_total < 10 ~ "mild",
        gad7_screening_total < 15 ~ "moderate",
        gad7_screening_total >= 15 ~ "severe",
        TRUE ~ NA_character_
      ),
      levels = c("minimal", "mild", "moderate", "severe")
    ),
    
    gad7_baseline_severity = factor(
      case_when(
        gad7_baseline_total < 5 ~ "minimal",
        gad7_baseline_total < 10 ~ "mild",
        gad7_baseline_total < 15 ~ "moderate",
        gad7_baseline_total >= 15 ~ "severe",
        TRUE ~ NA_character_
      ),
      levels = c("minimal", "mild", "moderate", "severe")
    ),
    
    gad7_followup_severity = factor(
      case_when(
        gad7_followup_total < 5 ~ "minimal",
        gad7_followup_total < 10 ~ "mild",
        gad7_followup_total < 15 ~ "moderate",
        gad7_followup_total >= 15 ~ "severe",
        TRUE ~ NA_character_
      ),
      levels = c("minimal", "mild", "moderate", "severe")
    ),
    
    # GAD7 binary anxiety: >=10 - anxiety diagnosis
    gad7_screening_binary_anxiety =
      case_when(
        gad7_screening_total >= 10 ~ TRUE,
        gad7_screening_total < 10 ~ FALSE,
        TRUE ~ NA
      ),
    
    gad7_baseline_binary_anxiety =
      case_when(
        gad7_baseline_total >= 10 ~ TRUE,
        gad7_baseline_total < 10 ~ FALSE,
        TRUE ~ NA
      ),
    
    gad7_followup_binary_anxiety =
      case_when(
        gad7_followup_total >= 10 ~ TRUE,
        gad7_followup_total < 10 ~ FALSE,
        TRUE ~ NA
      ),
    
    # GAD7 IAPT anxiety: >=8 - IAPT anxiety diagnosis
    gad7_screening_iapt_anxiety =
      case_when(
        gad7_screening_total >= 8 ~ TRUE,
        gad7_screening_total < 8 ~ FALSE,
        TRUE ~ NA
      ),
    
    gad7_baseline_iapt_anxiety =
      case_when(
        gad7_baseline_total >= 8 ~ TRUE,
        gad7_baseline_total < 8 ~ FALSE,
        TRUE ~ NA
      ),
    
    gad7_followup_iapt_anxiety =
      case_when(
        gad7_followup_total >= 8 ~ TRUE,
        gad7_followup_total < 8 ~ FALSE,
        TRUE ~ NA
      )
    
  )

# Save data
saveRDS(gad_data,
        here("data", "interim", "redcap", "step-02", "gad.Rds"))
