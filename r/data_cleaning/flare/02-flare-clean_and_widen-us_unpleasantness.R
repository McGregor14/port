# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder flare interim data is stored in
flare_interim_data_loc <- here("data", "interim", "flare", "step-01")

# Read in dataset
us_unpleasantness_raw <- read_rds(paste0(flare_interim_data_loc, "/us_unpleasantness_data", ".Rds")) %>% 
  clean_names() %>% 
  remove_empty() %>% 
  remove_constant(na.rm = T, quiet = F)

# Clean and widen dataset
us_unpleasantness <- us_unpleasantness_raw %>% 
  rename(
    us_unpleasantness_rating = rating
  )

# Save data
saveRDS(us_unpleasantness, paste0(here("data", "interim","flare", "step-02"), "/us_unpleasantness.Rds"))
