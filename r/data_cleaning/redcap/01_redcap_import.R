# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing

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