# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder flare interim data is stored in
redcap_interim_data_loc <- here("data", "interim", "redcap", "step-01")

# Read in dataset
phq_data <- read_rds(paste0(redcap_interim_data_loc, "/redcap-phq-data", ".Rds")) %>% 
  remove_empty(which = c("rows", "cols"))

# Generate derived variables
phq_data <- phq_data %>% 
  mutate(
    
    # phq: Sum of all items
    phq9_screening_total = rowSums(select(., starts_with("phq9") & contains("screening"))),
    phq9_baseline_total = rowSums(select(., starts_with("phq9") & contains("baseline"))),
    phq9_followup_total = rowSums(select(., starts_with("phq9") & contains("followup"))),
    
    # phq7 severity: 0-4 minimal, 5-9 mild, 10-14 moderate, 15+ severe
    phq9_screening_severity = factor(
      case_when(
        phq9_screening_total < 5 ~ "minimal",
        phq9_screening_total < 10 ~ "mild",
        phq9_screening_total < 15 ~ "moderate",
        phq9_screening_total >= 15 ~ "severe",
        TRUE ~ NA_character_
      ), 
      levels = c("minimal", "mild", "moderate", "severe")),
    
    phq9_baseline_severity = factor(
      case_when(
        phq9_baseline_total < 5 ~ "minimal",
        phq9_baseline_total < 10 ~ "mild",
        phq9_baseline_total < 15 ~ "moderate",
        phq9_baseline_total >= 15 ~ "severe",
        TRUE ~ NA_character_
      ), 
      levels = c("minimal", "mild", "moderate", "severe")),
    
    phq9_followup_severity = factor(
      case_when(
        phq9_followup_total < 5 ~ "minimal",
        phq9_followup_total < 10 ~ "mild",
        phq9_followup_total < 15 ~ "moderate",
        phq9_followup_total >= 15 ~ "severe",
        TRUE ~ NA_character_
      ), 
      levels = c("minimal", "mild", "moderate", "severe"))
    
  )

# Save data
saveRDS(phq_data, here("data", "interim", "redcap", "step-02", "redcap-phq-data.Rds"))
