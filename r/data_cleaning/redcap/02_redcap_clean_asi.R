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
asi_raw <- read_rds(paste0(redcap_interim_data_loc, "/redcap-asi-data", ".Rds")) %>% 
  clean_names() %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  remove_constant(na.rm = T, quiet = F)

# Rename all columns except participant_id
asi_names <- names(asi_raw[2:length(asi_raw)]) %>% 
  substr(., 1, nchar(.)-2) %>% # remove the '_2'
  str_replace(., "_", "_baseline_") %>%  # add '_baseline_' to the middle of the name
  str_replace_all(., "-", "_")

asi_names <- c("participant_id", asi_names) # Add the id column name back to the vector

names(asi_raw) <- asi_names # Reassign the names to the dataset

# Generate derived variables
asi_data <- asi_raw %>% 
  mutate(
    asi_baseline_total = rowSums(select(., contains("asi_baseline")))
  )

# Save data
saveRDS(asi_data, here("data", "interim", "redcap", "step-02", "redcap-asi-data.Rds"))
