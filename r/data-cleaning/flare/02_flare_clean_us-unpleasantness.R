# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder that interim data is stored in
flare_interim_data_loc <-
  here("data", "interim", "flare", "step-01")

# Read in dataset
us_unpleasantness_raw <-
  read_rds(paste0(flare_interim_data_loc, "/us-unpleasantness-data", ".Rds")) %>%
  # clean_names: makes all names unique, all lower case & only consist of _,
  # numbers, and letters
  clean_names() %>%
  # remove_empty: removes empty rows and columns
  remove_empty(which = c("rows", "cols")) %>%
  # remove_constant: removes constant columns
  remove_constant(na.rm = T, quiet = F)

# Clean dataset
us_unpleasantness <- us_unpleasantness_raw %>%
  rename(flare_us_unpleasantness_rating = rating)

# Save data
saveRDS(
  us_unpleasantness,
  here("data", "interim", "flare", "step-02", "us-unpleasantness.Rds")
)
