# Script for storing functions used throughout the port repo
# ----------------------------------------------------------

# find_latest_flare_folder:
# Provides the name of the most recently exported flare folder
find_latest_flare_file <- function(experiment_name, path_name = "") {
  
  dir_ls(path_name) %>% # Lists the folders in the directory
    str_replace(., paste0(path_name, experiment_name, "-"), "") %>% # Removes the part of the string before the timestamp
    as_datetime(.) %>% # Converts the timestamp to a date
    max(.) %>% # Takes the most recent date from the vector of dates created above
    format_ISO8601(.) %>% # Converts most recent date back to the ISO 8601 but contains some special characters 
    str_remove_all(., "-|:") %>% # Removes special characters that appear in the path names
    paste0(experiment_name, "-", ., "Z") # Prepends the experiment name and hyphen and appends a Z to the string

  }



# function 2:
# etc