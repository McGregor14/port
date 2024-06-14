# Script to generate folder structure within the data folder
# Important because the data folder is hidden on github

# Clear the environment
rm(list = ls())

# Load packages
library(here) # file referencing
library(fs) # file system operations

# Create data folders

# Raw folder only goes to the level of data source
dir_create(here("data", "raw", "flare"))
dir_create(here("data", "raw", "ieso"))
dir_create(here("data", "raw", "midapp"))
dir_create(here("data", "raw", "redcap"))

# Interim folder has data source and two levels
dir_create(here("data", "interim", "flare", "step-01"))
dir_create(here("data", "interim", "flare", "step-02"))
dir_create(here("data", "interim", "ieso", "step-01"))
dir_create(here("data", "interim", "ieso", "step-02"))
dir_create(here("data", "interim", "midapp", "step-01"))
dir_create(here("data", "interim", "midapp", "step-02"))
dir_create(here("data", "interim", "redcap", "step-01"))
dir_create(here("data", "interim", "redcap", "step-02"))

# Processed data all ends up in the same folder
dir_create(here("data", "processed"))