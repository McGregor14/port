# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Read in redcap csv file stored in the redcap folder
redcap_raw <- read_csv(here("data", "raw", "redcap", "2022-08-24_anonymised-dataset_tm.csv"),
                       col_names = TRUE,
                       trim_ws = TRUE)

# Remove test participants (without PORT_R_ prefix)
redcap_data <- redcap_raw %>% 
  filter(str_detect(port_id, 'PORT_R_'))

# Drop ID prefix (PORT_R_) and rename ID column to match other datasets
redcap_data <- redcap_data %>% 
  mutate(port_id = substring(port_id, 8)) %>% 
  rename(participant_id = port_id)

# Drop unnecessary columns
redcap_data <- redcap_data %>% 
  select(-final_survey_2_questions_timestamp)

# Clean names
redcap_data <- redcap_data %>% 
  clean_names() %>%
  
  # Remove 'scale' from pmh measure to match naming convention of other measures
  rename_with(.fn = ~str_remove(., "_scale"), .cols = starts_with("pmh")) %>%
  
  # For relevant columns, replace number at the end of strings with the correct wave of data collection
  rename_with(.fn = 
                ~str_replace(., "1$", "screening") %>% 
                str_replace(., "2$", "baseline") %>% 
                str_replace(., "4$", "followup"), 
              .cols = 
                starts_with(c('asi',
                              'cbas',
                              'fss',
                              'gad',
                              'ius',
                              'phq',
                              'pmh',
                              'pswq',
                              'stai',
                              'wsas'))) %>% 
  
  # Swap name elements so that collection wave comes before item number
  rename_with(
    .fn = ~gsub(
      pattern = '(asi|cbas|fss|gad7|ius12|phq9|pmh|pswq|stai|wsas)_(\\d+)_(.*)', 
      replacement = '\\1_\\3_\\2', 
      .x
    ), 
    .cols = matches('^(asi|cbas|fss|gad7|ius12|phq9|pmh|pswq|stai|wsas)')
  ) %>% 
 
  # PHQ specific renaming for variables following a different pattern 
  
  # Remove item number from intent question
  rename_with(
    .fn = ~str_remove(., "_10"), 
    .cols = matches('^phq9_intent')
  ) %>% 
  
  # Swap elements of the intent variables
  rename_with(
    .fn = ~gsub(
      pattern = '(phq9)_(.*)_(.*)', 
      replacement = '\\1_\\3_\\2', 
      .x
    ), 
    .cols = matches('^phq9_intent')
  ) %>% 
  
  # Swap elements of the risk variables
  rename_with(
    .fn = ~gsub(
      pattern = '(port)_(.*)_(.*)_(.*)', 
      replacement = '\\1_\\4_\\2_\\3', 
      .x
    ), 
    .cols = matches('^phq9_risk')
  ) %>% 
  
  # Ethnic origin specific renaming
  
  # Fix "ethic" typo
  rename_with(
    .fn = ~str_replace(., "ethic", "ethnic"), 
    .cols = matches('^ethic')
  ) %>% 
  
  # Timestamp specific renaming
  rename_with(
    .fn = ~gsub(
      pattern = '(port2|port3|port5)_(.*)_(survey)_(timestamp)', 
      replacement = '\\2_\\4', 
      .x
    ), 
    .cols = matches('timestamp$')
  )

# Generate different datasets for each measure

# ASI
`redcap-asi-data` <- redcap_data %>% 
  select(participant_id, starts_with("asi"))

# CBAS
`redcap-cbas-data` <- redcap_data %>% 
  select(participant_id, starts_with("cbas"))

# FSS
`redcap-fss-data` <- redcap_data %>% 
  select(participant_id, starts_with("fss"))

# GAD
`redcap-gad-data` <- redcap_data %>% 
  select(participant_id, starts_with("gad7"))

# IUS
`redcap-ius-data` <- redcap_data %>% 
  select(participant_id, starts_with("ius12"))

# PHQ
`redcap-phq-data` <- redcap_data %>% 
  select(participant_id, starts_with("phq9"))

# PMH
`redcap-pmh-data` <- redcap_data %>% 
  select(participant_id, starts_with("pmh"))

# PSWQ
`redcap-pswq-data` <- redcap_data %>% 
  select(participant_id, starts_with("pswq"))

# STAI
`redcap-stai-data` <- redcap_data %>% 
  select(participant_id, starts_with("stai"))

# WSAS
`redcap-wsas-data` <- redcap_data %>% 
  select(participant_id, starts_with("wsas"))

# All other columns
`redcap-other-data` <- redcap_data %>% 
  select(-starts_with(c("asi",
                        "cbas",
                        "fss",
                        "gad7",
                        "ius12",
                        "phq9",
                        "pmh",
                        "pswq",
                        "stai",
                        "wsas")))

# Remove datasets that aren't going to be saved
rm(redcap_raw)
rm(redcap_data)

# Save all dataframe objects in the global environment
save_all_dataframes(path = here("data", "interim", "redcap", "step-01"))