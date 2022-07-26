# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder flare interim data is stored in
flare_interim_data_loc <- here("data", "interim", "flare", "step-01")

# Read in dataset
participants_raw <- read_rds(paste0(flare_interim_data_loc, "/participants", ".Rds")) %>% 
  clean_names() %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  remove_constant(na.rm = T, quiet = F)

# Clean and widen dataset
participants <- participants_raw %>% 
  select(-voucher) %>% 
  mutate(across(.cols = c(current_module, reinforced_stimulus), .fns = ~str_to_lower(str_replace_all(., " ", "_")))) %>% 
  mutate(across(.cols = c(current_module, reinforced_stimulus), .fns = ~ fct_infreq(as_factor(.))))

# Generate derived variables
participants <- participants %>% 
  mutate(
    created_date = date(created_at),
    created_hour = hour(created_at),
    
    started_date = date(started_at),
    started_hour = hour(started_at),
    
    finished_date = date(finished_at),
    finished_hour = hour(finished_at),
    
    completion_duration_mins = as.double(difftime(finished_at, started_at, units = "mins")),
    
    batch_no = as.integer(factor(created_at)),
    
    started_app = if_else(
      condition = !is.na(started_at),
      true = TRUE,
      false = FALSE
    ),
    
    completed_app = if_else(
      condition = !is.na(completion_duration_mins),
      true = TRUE,
      false = FALSE
    )
  )

# Save data
saveRDS(participants, here("data", "interim", "flare", "step-02", "participants.Rds"))