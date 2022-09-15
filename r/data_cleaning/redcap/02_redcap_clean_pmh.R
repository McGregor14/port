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
  here("data", "interim", "redcap", "step-01")

# Read in dataset
pmh_data <-
  read_rds(paste0(redcap_interim_data_loc, "/redcap-pmh-data", ".Rds")) %>%
  remove_empty(which = c("rows", "cols")) %>%
  remove_constant(na.rm = T, quiet = F)

# Generate derived variables
pmh_data <- pmh_data %>%
  mutate(pmh_baseline_mean = rowMeans(select(., contains("pmh_baseline"))))

# Save data
saveRDS(pmh_data,
        here("data", "interim", "redcap", "step-02", "pmh.Rds"))
