# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder that interim data is stored in
redcap_interim_data_loc <-
  here("data", "interim", "glad", "step-01")

# Read in dataset
glad_data <-
  read_rds(paste0(redcap_interim_data_loc, "/glad-data", ".Rds")) %>%
  remove_empty(which = c("rows", "cols")) %>%
  remove_constant(na.rm = T, quiet = F)

# Convert biological sex to a factor
glad_data <- glad_data %>%
  mutate(demographics_biological_sex = factor(demographics_biological_sex))

# Save data
saveRDS(glad_data,
        here("data", "processed", "glad-data.Rds"))
