# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder flare interim data is stored in
redcap_interim_data_loc <-
  here("data", "interim", "redcap", "step-01")

# Read in dataset
ius_data <-
  read_rds(paste0(redcap_interim_data_loc, "/redcap-ius-data", ".Rds")) %>%
  remove_empty(which = c("rows", "cols")) %>%
  remove_constant(na.rm = T, quiet = F)

# Generate derived variables
ius_data <- ius_data %>%
  mutate(
    # IUS: Sum of all items
    ius12_followup_total = rowSums(select(., contains("ius12"))),
    
    # IUS Prospective Anxiety subscale: 1, 2, 4, 5, 8, 9, 11
    ius12_followup_prospective_anxiety = rowSums(select(
      .,
      c(
        ius12_followup_1,
        ius12_followup_2,
        ius12_followup_4,
        ius12_followup_5,
        ius12_followup_8,
        ius12_followup_9,
        ius12_followup_11
      )
    )),
    
    # IUS Inhibitory Anxiety subscale: 3, 6, 7, 10, 12
    ius12_followup_inhibitory_anxiety = rowSums(select(
      .,
      c(
        ius12_followup_3,
        ius12_followup_6,
        ius12_followup_7,
        ius12_followup_10,
        ius12_followup_12
      )
    ))
    
  )

# Save data
saveRDS(ius_data,
        here("data", "interim", "redcap", "step-02", "ius.Rds"))
