# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning
library(fedmatch)

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder flare interim data is stored in
flare_interim_data_loc <- here("data", "interim", "flare", "step-01")

# Read in dataset
basic_info_raw <- read_rds(paste0(flare_interim_data_loc, "/basic_info_data", ".Rds")) %>% 
  clean_names() %>% 
  remove_empty() %>% 
  remove_constant(na.rm = T, quiet = F)

# Clean and widen dataset
basic_info <- basic_info_raw %>% 
  mutate(
    across(.cols = c(device_make, device_model), .fns = ~clean_strings(.)),
    os_name = str_to_lower(str_replace_all(os_name, " ", "_")),
    os_name = if_else(str_detect(os_name, "release-keys"), "android", os_name),
    across(.cols = c(headphone_type, device_make, device_model, os_name), .fns = ~ fct_infreq(as_factor(.)))
  )

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
saveRDS(participants, paste0(here("data", "interim","flare", "step-02"), "/participants.Rds"))
