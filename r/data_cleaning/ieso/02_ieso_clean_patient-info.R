# Setup -------------------------------------------------------------------

# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning
library(lubridate) # working with dates/times
library(fedmatch) # cleaning strings

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder that interim data is stored in
ieso_interim_data_loc <-
  here("data", "interim", "ieso", "step-01")

# Read in dataset
patient_data <-
  read_rds(paste0(ieso_interim_data_loc, "/ieso-patient-data", ".Rds")) %>%
  # remove_empty: removes empty rows and columns
  remove_empty(which = c("rows", "cols")) %>%
  # remove_constant: removes columns where all observations are the same
  remove_constant(na.rm = T, quiet = F) %>%
  select(-starts_with(c("sum", "count")))


# Clean variables & generate derived variables ----------------------------


## ieso_length_of_treatment_days ------------------------------------------

# Difference between referral date and end date in days
patient_data <-
  patient_data %>%
  mutate(
    ieso_length_of_treatment_days =
      ieso_referral_date %--% ieso_end_date / days(1),
    .after = ieso_end_date
  )


## ieso_therapist_id ------------------------------------------------------

# Therapist ID converted to factor and ordered by the numeric value of the ID
patient_data <-
  patient_data %>%
  mutate(ieso_therapist_id = fct_inseq(factor(ieso_therapist_id)))


## ieso_pathway -----------------------------------------------------------
patient_data <-
  patient_data %>%
  
  mutate(
    # Tidy the strings (remove special characters and replace spaces with 
    # underscores)
    ieso_pathway =
      str_replace_all(ieso_pathway, "\\+", " plus") %>%
      clean_strings() %>%
      str_replace_all(., " ", "_"),
    
    # Convert to a factor and set order of levels
    ieso_pathway =
      factor(
        ieso_pathway,
        levels = c("assessment", "step_2", "step_3", "step_3_plus")
      )
  )


## ieso_discharge_reason (primary/secondary) ------------------------------
patient_data <-
  patient_data %>%
  
  # Separate discharge reason into primary and secondary reasons
  separate(
    ieso_discharge_reason,
    into = c(
      "ieso_discharge_reason_primary",
      "ieso_discharge_reason_secondary"
    ),
    sep = ": "
  ) %>%
  
  # Replace forward slashes with 'or', spaces with underscores, and remove
  # brackets
  mutate(across(
    .cols = c(
      "ieso_discharge_reason_primary",
      "ieso_discharge_reason_secondary"
    ),
    ~ str_replace_all(., c("\\/" = " or ", " " = "_", "\\(" = "", "\\)" = ""))
  )) %>%
  
  # Convert strings to lower case
  mutate(across(
    .cols = c(
      "ieso_discharge_reason_primary",
      "ieso_discharge_reason_secondary"
    ),
    ~ str_to_lower(.)
  )) %>%
  
  # Convert to factors ordered by number of observations
  mutate(across(
    .cols = c(
      "ieso_discharge_reason_primary",
      "ieso_discharge_reason_secondary"
    ),
    ~ fct_infreq(factor(.))
  ))


## ieso_diagnosis_name, ieso_protocol_token ------------------------------
patient_data <-
  patient_data %>%
  
  # Clean character strings (lower case and remove special characters)
  mutate(across(
    .cols = c("ieso_diagnosis_name",
              "ieso_protocol_token"),
    ~ clean_strings(.)
  )) %>%
  
  # Replace spaces with underscores
  mutate(across(
    .cols = c(
      "ieso_diagnosis_name",
      "ieso_protocol_token"
    ),
    ~ str_replace_all(., " ", "_")
  )) %>%
  
  # Convert to factors ordered by number of observations
  mutate(across(
    .cols = c("ieso_diagnosis_name",
              "ieso_protocol_token"),
    ~ fct_infreq(factor(.))
  ))


# Save data ---------------------------------------------------------------
saveRDS(patient_data,
        here("data", "interim", "ieso", "step-02", "patient_info.Rds"))
