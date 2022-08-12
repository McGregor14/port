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
pswq_data <- read_rds(paste0(redcap_interim_data_loc, "/redcap-pswq-data", ".Rds")) %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  remove_constant(na.rm = T, quiet = F)

# Reverse score items
# Method: take the maximum value for the scale, add 1, and subtract the participant's score.
# Reverse the following items: 1, 3, 8, 10, 11
pswq_data <- pswq_data %>% 
  mutate(across(.cols = c(
    
    pswq_baseline_1,
    pswq_baseline_3,
    pswq_baseline_8,
    pswq_baseline_10,
    pswq_baseline_11,
    
    pswq_followup_1,
    pswq_followup_3,
    pswq_followup_8,
    pswq_followup_10,
    pswq_followup_11,),
    
    ~ 5 + 1 - .))

# Generate derived variables
pswq_data <- pswq_data %>% 
  mutate(
    pswq_baseline_total = rowSums(select(., contains("pswq_baseline"))),
    pswq_followup_total = rowSums(select(., contains("pswq_followup")))
  )

# Save data
saveRDS(pswq_data, here("data", "interim", "redcap", "step-02", "redcap-pswq-data.Rds"))
