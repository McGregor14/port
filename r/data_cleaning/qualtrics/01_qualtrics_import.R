# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing

# Source function script
source(file = here("r", "functions.R"))

# Read in midapp csv file stored in the qualtrics folder
mid_app_raw <- read_csv(here("data", "raw", "qualtrics", "2022-07-05_midapp-survey.csv"), 
                        col_select = 18:80, # First few columns contain redundant meta-data
                        col_names = TRUE, 
                        trim_ws = TRUE) %>% 
  slice(-c(1,2)) # Remove second and third rows which contain metadata about the questions from Qualtrics

# Remove test participants (without PORT_R_ prefix) and drop ID prefix (PORT_R_)
mid_app_data <- mid_app_raw %>% 
  filter(str_detect(port_id, 'PORT_R_'))

mid_app_data <- mid_app_data %>% 
  mutate(port_id = substring(port_id, 8)) %>% 
  rename(participant_id = port_id)

# Save data
saveRDS(mid_app_data, paste0(here("data", "interim","qualtrics", "step-01"), "/mid_app_data.Rds"))