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
           gad7_treatment_total_final_observed - gad7_treatment_total_1,
         
         phq9_treatment_change = 
           phq9_treatment_total_final - phq9_treatment_total_1,
         phq9_treatment_change_observed =
           phq9_treatment_total_final_observed - phq9_treatment_total_1)



# Create IAPT change scores -----------------------------------------------
ieso_data1 <- 
  ieso_data %>%
  transmute(
    gad7_treatment_reliable_improvement =
      if_else(gad7_treatment_change <= -4,
              TRUE,
              FALSE),
    gad7_treatment_reliable_improvement_observed = 
      if_else(gad7_treatment_change_observed <= -4,
              TRUE,
              FALSE)
  )


# Reliable improvement - improved on at least one of the measures (GAD/PHQ) *check
# Recovery - above to below clinical cut-off on both measures (can be above on one of the measures at the start)
# Reliable and clinically significant improvement (RCSI)

# Caseness: 
# PHQ >= 10 
# GAD >= 8

# Reliable change index:
# PHQ = 6
# GAD = 4

# Save data ---------------------------------------------------------------
saveRDS(ieso_data, here("data", "processed", "ieso-data.Rds"))
