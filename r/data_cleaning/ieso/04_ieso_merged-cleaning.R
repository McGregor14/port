# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Read in dataset
ieso_data <-
  read_rds(here("data", "interim", "ieso", "step-03", "ieso-data.Rds"))



# Create ieso exclusion variable for PORT --------------------------------
ieso_data <- 
  ieso_data %>%
  mutate(
    ieso_exclusion_port = case_when(
      ieso_treatment_completed_total < 2 |
        ieso_discharge_reason_primary == "not_suitable_for_service"
      ~ TRUE,
      TRUE ~ FALSE
    ),
    .after = ieso_last_treatment_date
  )

# Print the number of exclusions
ieso_data %>%
  count(ieso_exclusion_port)



# Create treatment change scores ------------------------------------------
ieso_data <- 
  ieso_data %>%
  mutate(gad7_treatment_change = 
           gad7_treatment_total_final - gad7_treatment_total_1,
         gad7_treatment_change_observed =
           gad7_treatment_total_final_observed - gad7_treatment_total_1)



# Save data ---------------------------------------------------------------
saveRDS(ieso_data, here("data", "processed", "ieso-data.Rds"))
