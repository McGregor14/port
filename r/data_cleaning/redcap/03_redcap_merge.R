# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(fs) # file system operations

# Specify the location of data files from step 02 of cleaning
redcap_data_path <- (here("data", "interim", "redcap", "step-02"))

# List all the RDS files stored in the folder
redcap_rds_files <- dir_ls(redcap_data_path, regexp = "\\.Rds$")

# Read in all RDS files stored in the folder
redcap_data_list <- redcap_rds_files %>% 
  map(~read_rds(.))

# Remove the pathname, duplicate prefix, and filetype from the names of the list elements
redcap_data_names <- redcap_data_list %>%
  names() %>% 
  str_remove_all(., paste0(redcap_data_path, "/")) %>%
  str_remove(., ".Rds") %>% 
  str_remove(., "redcap-") %>%
  str_remove(., "-data")

# Set the new names of the list elements
names(redcap_data_list) <- redcap_data_names

# Reorder the list ready for merging
redcap_data_list <- redcap_data_list[c("other",
                                     "asi", 
                                     "cbas", 
                                     "fss", 
                                     "gad", 
                                     "ius", 
                                     "phq",
                                     "pmh",
                                     "pswq",
                                     "stai",
                                     "wsas")]

# Merge all elements of the list
redcap_data <- redcap_data_list %>% 
  reduce(full_join, by = "participant_id")

# Save data
saveRDS(redcap_data, here("data", "processed", "redcap-data.Rds"))
