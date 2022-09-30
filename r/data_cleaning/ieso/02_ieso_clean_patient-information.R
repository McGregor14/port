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
session_data <-
  read_rds(paste0(ieso_interim_data_loc, "/ieso-session-data", ".Rds")) %>%
  # remove_empty: removes empty rows and columns
  # remove_constant: removes constant columns
  remove_empty(which = c("rows", "cols")) %>%
  remove_constant(na.rm = T, quiet = F) %>% 
  select(-starts_with(c("sum", "count")))

# Clean variables & generate derived variables

# session_date
session_data <-
  session_data %>%
  mutate(
    # Convert strings to dates
    date = as_datetime(date, format = "%d/%m/%Y %H:%M")
    ) %>%
  rename(session_date_time = date)

#
session_data <-
  session_data %>%
  mutate(across(
    .cols = c("appointment_type", 
              "status"),
    
    # Convert all variables to factor 
    # Order by number of observations in each level
    ~clean_strings(.) 
  ))


# ieso_therapist_id
patient_data <-
  patient_data %>%
  mutate(
    # Convert to factor ordered by the numeric value of the ID
    ieso_therapist_id = fct_inseq(factor(ieso_therapist_id))
  )


# ieso_pathway
patient_data <-
  patient_data %>%
  mutate(
    # Tidy the strings
    ieso_pathway =
      str_replace_all(ieso_pathway, "\\+", " plus") %>%
      clean_strings() %>%
      str_replace_all(., " ", "_"),
    
    # Convert to a factor
    ieso_pathway =
      factor(ieso_pathway, 
             levels = c("assessment", "step_2", "step_3", "step_3_plus"))
  )

# ieso_diagnosis_name, ieso_protocol_token, ieso_discharge_reason
patient_data <-
  patient_data %>%
  mutate(across(
    .cols = c("ieso_diagnosis_name", 
              "ieso_protocol_token", 
              "ieso_discharge_reason"),
    
    # Convert all variables to factor 
    # Order by number of observations in each level
    ~fct_infreq(factor(.))
  ))

# Save data
saveRDS(session_data,
        here("data", "interim", "ieso", "step-02", "session_info.Rds"))
