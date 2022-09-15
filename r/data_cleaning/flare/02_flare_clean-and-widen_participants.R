# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning
library(lubridate) # working with dates/times

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder that interim data is stored in
flare_interim_data_loc <- here("data", "interim", "flare", "step-01")

# Read in dataset
participants_raw <- read_rds(paste0(flare_interim_data_loc, "/participants", ".Rds")) %>% 
  clean_names() %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  remove_constant(na.rm = T, quiet = F)

# Clean and widen dataset
participants <- participants_raw %>% 
  select(-voucher) %>% # Remove redundant voucher column
  mutate(across(.cols = c(current_module, reinforced_stimulus), .fns = ~str_to_lower(str_replace_all(., " ", "_")))) %>%
  mutate(across(.cols = c(current_module, reinforced_stimulus), .fns = ~ fct_infreq(as_factor(.)))) %>% 
  rename_with(.fn = ~paste0("flare_", .x), .cols = !participant_id) # Add 'flare' as a prefix to all columns except the participant_id column

# Generate derived variables
participants <- participants %>% 
  mutate(
    flare_created_date = date(flare_created_at),
    flare_created_hour = hour(flare_created_at),
    
    flare_started_date = date(flare_started_at),
    flare_started_hour = hour(flare_started_at),
    
    flare_finished_date = date(flare_finished_at),
    flare_finished_hour = hour(flare_finished_at),
    
    flare_completion_duration_mins = as.double(difftime(flare_finished_at, flare_started_at, units = "mins")),
    
    flare_batch_no = as.integer(factor(flare_created_at)),
    
    flare_started_app = if_else(
      condition = !is.na(flare_started_at),
      true = TRUE,
      false = FALSE
    ),
    
    flare_completed_app = if_else(
      condition = !is.na(flare_completion_duration_mins),
      true = TRUE,
      false = FALSE
    )
  )

# Save data
saveRDS(participants, here("data", "interim", "flare", "step-02", "participants.Rds"))