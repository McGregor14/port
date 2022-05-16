# Script for storing functions used throughout the port repo
# ----------------------------------------------------------

# find_latest_flare_folder:
# Provides the name of the most recently exported flare folder
find_latest_flare_folder <- function(experiment_name, path_name = "") {
  
  fs::dir_ls(path_name) %>% # Lists the folders in the directory
    stringr::str_replace(., paste0(path_name, experiment_name, "-"), "") %>% # Removes the part of the string before the timestamp
    lubridate::as_datetime(.) %>% # Converts the timestamp to a date
    max(.) %>% # Takes the most recent date from the vector of dates created above
    lubridate::format_ISO8601(.) %>% # Converts most recent date back to the ISO 8601 but contains some special characters 
    stringr::str_remove_all(., "-|:") %>% # Removes special characters that appear in the path names
    paste0(experiment_name, "-", ., "Z") # Prepends the experiment name and hyphen and appends a Z to the string

  }



# save_all_objects:
# Saves all the dataframe objects in the global environment as .Rds files to a specified path
save_all_dataframes <- function(path){
  
  obj_names <- ls(.GlobalEnv)
  for(i in 1:length(obj_names)) {
    file <- paste0(path, "/", obj_names[i],".Rds")
    if(is.data.frame(get(obj_names[i]))){
      saveRDS(get(obj_names[i], envir = .GlobalEnv), file)
    }
  }
  
}



# %!in%:
# Opposite of the %in% operator
'%!in%' <- Negate(`%in%`)
