# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder flare interim data is stored in
flare_interim_data_loc <- here("data", "interim", "flare", "step-01")

# Read in dataset
contingency_awareness_raw <- read_rds(paste0(flare_interim_data_loc, "/contingency_awareness_data", ".Rds")) %>% 
  clean_names() %>% 
  remove_empty() %>% 
  remove_constant(na.rm = T, quiet = F)

# Clean and widen dataset
contingency_awareness <- contingency_awareness_raw %>% 
  rename(
    contingency_awareness_answer = awareness_answer,
    contingency_awareness_stimulus_guess = confirmation_answer,
    contingency_aware = is_aware
  )

# Save data
saveRDS(contingency_awareness, paste0(here("data", "interim","flare", "step-02"), "/contingency_awareness.Rds"))
