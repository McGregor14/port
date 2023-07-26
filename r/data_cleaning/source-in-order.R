# Script to source all scripts in the correct order

# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse)
library(here)

# Create function for sources all folders of a particular stage in a subfolder
source_cleaning_files <- function(subfolder, pattern_tmp) {
  
  path_tmp <- here("r", "data_cleaning", subfolder)
  
  for (f in list.files(path_tmp, pattern = pattern_tmp)) {
    source(paste0(path_tmp, "/", f))
    
    # Set pattern and path again to avoid clear workspaces
    
    # Set pattern to look for in folders
    pttn_tmp <- pattern_tmp
    
    # Set path
    pth_tmp <- here("r", "data_cleaning", subfolder)
  }
}

# step-01 -----------------------------------------------------------------

# flare
source_cleaning_files(subfolder = "flare", pattern_tmp = "^01_.*\\.R$")

# glad
source_cleaning_files(subfolder = "glad", pattern_tmp = "^01_.*\\.R$")

# ieso
source_cleaning_files(subfolder = "ieso", pattern_tmp = "^01_.*\\.R$")

# midapp
source(file =
         here("r", "data_cleaning", "midapp", "/01_midapp_import-and-rename.R"))

# redcap
source(file =
         here("r", "data_cleaning", "redcap", "/01_redcap_import-and-rename.R"))


# step-02 -----------------------------------------------------------------

# Clear the environment
rm(list = ls())

# Set pattern to look for in folders
pattern_tmp <- "^02_.*\\.R$"

# flare

# Set path
path_tmp <- here("r", "data_cleaning", "flare")

for (f in list.files(path_tmp, pattern = pattern_tmp)) {
  source(paste0(path_tmp, "/", f))
  
  # Set pattern and path again to avoid clear workspaces
  
  # Set pattern to look for in folders
  pattern_tmp <- "^02_.*\\.R$"
  
  # Set path
  path_tmp <- here("r", "data_cleaning", "flare")
}

# flare




