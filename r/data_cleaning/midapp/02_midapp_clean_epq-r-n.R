# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder that interim data is stored in
midapp_interim_data_loc <-
  here("data", "interim", "midapp", "step-01")

# Read in dataset
epq_r_n_data <-
  read_rds(paste0(midapp_interim_data_loc, "/midapp-epq_r_n-data", ".Rds")) %>%
  remove_empty(which = c("rows", "cols")) %>%
  remove_constant(na.rm = T, quiet = F)

# TODO: Find out if any items are reverse scored

# Generate derived variables
epq_r_n_data <- epq_r_n_data %>%
  mutate(# TODO: EPQ-R-N Scale: ?
    epq_r_midapp_nscale = rowSums(select(., contains("epq_r_nscale"))))

# Save data
saveRDS(epq_r_n_data,
        here("data", "interim", "midapp", "step-02", "epq-r-n.Rds"))