# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(fs) # file system operations
library(qualtRics) # interacting with Qualtrics

# Source function script
source(file = here("r", "functions.R"))

# Read in csv file stored in the qualtrics folder
mid_app_raw <- read_csv(here("data", "raw", "qualtrics", "2022-07-05_midapp-survey.csv"), 
                        col_select = 18:80, # First few columns contain redundant meta-data
                        col_names = TRUE, 
                        trim_ws = TRUE) %>% 
  slice(-c(1,2)) # Remove second and third rows which contain metadata about the questions from Qualtrics

# Rename all columns except port_id
mid_app_names <- names(mid_app_raw[1:62]) %>% 
  substr(.,1,nchar(.)-2) %>% # remove the '_3'
  str_replace(., "_", "_midapp_") %>%  # add '_midapp_' to the middle of the name
  str_replace_all(., "-", "_")

mid_app_names <- c(mid_app_names, "port_id") # Add the id column name back to the vector

names(mid_app_raw) <- mid_app_names # Reassign the names to the dataset

mid_app_raw <- mid_app_raw %>% 
  select(port_id, everything())

# Save data
saveRDS(mid_app_raw, paste0(here("data", "interim","qualtrics", "step-01"), "/mid_app_questionnaires.Rds"))
