# Clear the environment
rm(list = ls())

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
basic_info_raw <- read_rds(paste0(flare_interim_data_loc, "/basic-info-data", ".Rds")) %>% 
  clean_names() %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  remove_constant(na.rm = T, quiet = F)

# Clean and widen dataset
basic_info <- basic_info_raw %>% 
  mutate(
    across(.cols = c(device_make, device_model), .fns = ~clean_strings(.)),
    across(.cols = c(device_make, device_model, os_name), .fns = ~str_to_lower(str_replace_all(., " ", "_"))),
    os_name = if_else(str_detect(os_name, "release-keys"), "android", os_name),
    across(.cols = c(headphone_type, device_make, device_model, os_name), .fns = ~ fct_infreq(as_factor(.)))
  )

# Save data
saveRDS(basic_info, here("data", "interim", "flare", "step-02", "basic-info.Rds"))