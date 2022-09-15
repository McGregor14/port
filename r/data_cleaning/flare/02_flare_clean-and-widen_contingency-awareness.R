# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder that interim data is stored in
flare_interim_data_loc <- here("data", "interim", "flare", "step-01")

# Read in dataset
contingency_awareness_raw <- read_rds(paste0(flare_interim_data_loc, "/contingency-awareness-data", ".Rds")) %>% 
  clean_names() %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  remove_constant(na.rm = T, quiet = F)

# Clean and widen dataset
contingency_awareness <- contingency_awareness_raw %>% 
  rename(
    contingency_awareness_answer = awareness_answer,
    contingency_awareness_stimulus_guess = confirmation_answer,
    contingency_aware = is_aware
  )

# Save data
saveRDS(contingency_awareness, here("data", "interim", "flare", "step-02", "contingency-awareness.Rds"))