# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder flare interim data is stored in
redcap_interim_data_loc <- here("data", "interim", "redcap", "step-01")

# Read in dataset
ius_raw <- read_rds(paste0(redcap_interim_data_loc, "/redcap-ius-data", ".Rds")) %>% 
  clean_names() %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  remove_constant(na.rm = T, quiet = F)

# Rename all columns except participant_id

# Replace numeric indicator of collection wave with descriptor
ius_names <- names(ius_raw[2:length(ius_raw)]) %>%
  str_replace(., "4$", "followup")

ius_names <- c("participant_id", ius_names) # Add the id column name back to the vector

names(ius_raw) <- ius_names # Reassign the names to the dataset

# Swap name elements so that collection wave comes before item number
ius_raw <- ius_raw %>% 
  rename_with(.fn = ~gsub('ius12_(\\d+)_(.*)', 'ius12_\\2_\\1', .x), .cols = starts_with('ius12'))

# Generate derived variables
ius_data <- ius_raw %>% 
  mutate(
    
    # IUS: Sum of all items
    ius12_followup_total = rowSums(select(., contains("ius12"))),
    
    # IUS Prospective Anxiety subscale: 1, 2, 4, 5, 8, 9, 11
    ius12_followup_prospective_anxiety = rowSums(select(., c(ius12_followup_1, 
                                                             ius12_followup_2,
                                                             ius12_followup_4,
                                                             ius12_followup_5,
                                                             ius12_followup_8,
                                                             ius12_followup_9,
                                                             ius12_followup_11))),
    
    # IUS Inhibitory Anxiety subscale: 3, 6, 7, 10, 12
    ius12_followup_inhibitory_anxiety = rowSums(select(., c(ius12_followup_3, 
                                                            ius12_followup_6,
                                                            ius12_followup_7,
                                                            ius12_followup_10,
                                                            ius12_followup_12)))
    
  )

# Save data
saveRDS(ius_data, here("data", "interim", "redcap", "step-02", "redcap-ius-data.Rds"))
