# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Read in redcap csv file stored in the redcap folder
redcap_raw <- read_csv(here("data", "raw", "redcap", "2022-07-26_anonymised-dataset_tm.csv"),
                         col_names = TRUE,
                         trim_ws = TRUE)

# Remove test participants (without PORT_R_ prefix)
redcap_data <- redcap_raw %>% 
  filter(str_detect(port_id, 'PORT_R_'))

# Drop ID prefix (PORT_R_) and rename ID column to match other datasets
redcap_data <- redcap_data %>% 
  mutate(port_id = substring(port_id, 8)) %>% 
  rename(participant_id = port_id)

# Clean names
redcap_data <- redcap_data %>% 
  clean_names() %>%
  
  # Remove 'scale' from pmh measure to match naming convention of other measures
  rename_with(.fn = ~str_remove(., "_scale"), .cols = starts_with("pmh")) %>%
  
  # For relevant columns, replace number at the end of strings with the correct wave of data collection
  
  # Screening
  rename_with(.fn = ~str_replace(., "1$", "screening"), .cols = starts_with(c('asi',
                                                                              'cbas',
                                                                              'fss',
                                                                              'gad',
                                                                              'ius',
                                                                              'phq',
                                                                              'pmh',
                                                                              'pswq',
                                                                              'stai',
                                                                              'wsas'))) %>% 
  
  # Baseline
  rename_with(.fn = ~str_replace(., "2$", "baseline"), .cols = starts_with(c('asi',
                                                                              'cbas',
                                                                              'fss',
                                                                              'gad',
                                                                              'ius',
                                                                              'phq',
                                                                              'pmh',
                                                                              'pswq',
                                                                              'stai',
                                                                              'wsas'))) %>% 
  
  # Follow-up
  rename_with(.fn = ~str_replace(., "4$", "followup"), .cols = starts_with(c('asi',
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
  rename_with(.fn = ~gsub('asi_(\\d+)_(.*)', 'asi_\\2_\\1', .x), .cols = starts_with('asi')) %>% 
  rename_with(.fn = ~gsub('cbas_(\\d+)_(.*)', 'cbas_\\2_\\1', .x), .cols = starts_with('cbas')) %>% 
  rename_with(.fn = ~gsub('fss_(\\d+)_(.*)', 'fss_\\2_\\1', .x), .cols = starts_with('fss')) %>% 
  rename_with(.fn = ~gsub('gad7_(\\d+)_(.*)', 'gad7_\\2_\\1', .x), .cols = starts_with('gad7')) %>% 
  rename_with(.fn = ~gsub('ius12_(\\d+)_(.*)', 'ius12_\\2_\\1', .x), .cols = starts_with('ius12')) %>% 
  rename_with(.fn = ~gsub('phq9_(\\d+)_(.*)', 'phq9_\\2_\\1', .x), .cols = starts_with('phq9')) %>% 
  rename_with(.fn = ~gsub('pmh_(\\d+)_(.*)', 'pmh_\\2_\\1', .x), .cols = starts_with('pmh')) %>% 
  rename_with(.fn = ~gsub('pswq_(\\d+)_(.*)', 'pswq_\\2_\\1', .x), .cols = starts_with('pswq')) %>% 
  rename_with(.fn = ~gsub('stai_(\\d+)_(.*)', 'stai_\\2_\\1', .x), .cols = starts_with('stai')) %>% 
  rename_with(.fn = ~gsub('wsas_(\\d+)_(.*)', 'wsas_\\2_\\1', .x), .cols = starts_with('wsas')) 
  
  

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