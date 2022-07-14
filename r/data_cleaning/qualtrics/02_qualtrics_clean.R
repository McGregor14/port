# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder midapp interim data is stored in
mid_app_interim_data_loc <- here("data", "interim", "qualtrics", "step-01")

# Read in dataset
mid_app_data <- read_rds(paste0(mid_app_interim_data_loc, "/mid_app_data", ".Rds")) %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  remove_constant(na.rm = T, quiet = F)

# Rename all columns except port_id
mid_app_names <- names(mid_app_data[1:62]) %>% 
  substr(.,1,nchar(.)-2) %>% # remove the '_3'
  str_replace(., "_", "_midapp_") %>%  # add '_midapp_' to the middle of the name
  str_replace_all(., "-", "_")

mid_app_names <- c(mid_app_names, "participant_id") # Add the id column name back to the vector

names(mid_app_data) <- mid_app_names # Reassign the names to the dataset

mid_app_data <- mid_app_data %>% 
  select(participant_id, everything())

# Adjust column types
mid_app_data <- mid_app_data %>% 
  mutate(across(.cols = -participant_id, ~as.integer(.)))

# Reverse score items


# Generate derived variables
mid_app_data <- mid_app_data %>% 
  
  

# Save data
saveRDS(mid_app_data, paste0(here("data", "interim","qualtrics", "step-01"), "/mid_app_data.Rds"))