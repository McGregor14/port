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
gad_raw <- read_rds(paste0(redcap_interim_data_loc, "/redcap-gad-data", ".Rds")) %>% 
  clean_names() %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  remove_constant(na.rm = T, quiet = F) %>% 
  select(-contains("raw_score")) # Remove raw score columns and recalculate later on

# Rename all columns except participant_id

# Replace numeric indicator of collection wave with descriptor
gad_names <- names(gad_raw[2:length(gad_raw)]) %>%
  str_replace(., "1$", "screening") %>% 
  str_replace(., "2$", "baseline") %>% 
  str_replace(., "4$", "followup")

gad_names <- c("participant_id", gad_names) # Add the id column name back to the vector

names(gad_raw) <- gad_names # Reassign the names to the dataset

# Swap name elements so that collection wave comes before item number
gad_raw <- gad_raw %>% 
  rename_with(.fn = ~gsub('gad7_(\\d+)_(.*)', 'gad7_\\2_\\1', .x), .cols = starts_with('gad7'))

# Generate derived variables
gad_data <- gad_raw %>% 
  mutate(
    
    # GAD7: Sum of all items
    gad7_screening_total = rowSums(select(., starts_with("gad7") & contains("screening"))),
    gad7_baseline_total = rowSums(select(., starts_with("gad7") & contains("baseline"))),
    gad7_followup_total = rowSums(select(., starts_with("gad7") & contains("followup"))),
    
    # GAD7 severity: 0-4 minimal, 5-9 mild, 10-14 moderate, 15+ severe
    gad7_screening_severity = factor(
      case_when(
        gad7_screening_total < 5 ~ "minimal",
        gad7_screening_total < 10 ~ "mild",
        gad7_screening_total < 15 ~ "moderate",
        gad7_screening_total >= 15 ~ "severe",
        TRUE ~ NA_character_
      ), 
      levels = c("minimal", "mild", "moderate", "severe")),
    
    gad7_baseline_severity = factor(
      case_when(
        gad7_baseline_total < 5 ~ "minimal",
        gad7_baseline_total < 10 ~ "mild",
        gad7_baseline_total < 15 ~ "moderate",
        gad7_baseline_total >= 15 ~ "severe",
        TRUE ~ NA_character_
      ), 
      levels = c("minimal", "mild", "moderate", "severe")),
    
    gad7_followup_severity = factor(
      case_when(
        gad7_followup_total < 5 ~ "minimal",
        gad7_followup_total < 10 ~ "mild",
        gad7_followup_total < 15 ~ "moderate",
        gad7_followup_total >= 15 ~ "severe",
        TRUE ~ NA_character_
      ), 
      levels = c("minimal", "mild", "moderate", "severe"))
    
  )

# Save data
saveRDS(gad_data, here("data", "interim", "redcap", "step-02", "redcap-gad-data.Rds"))
