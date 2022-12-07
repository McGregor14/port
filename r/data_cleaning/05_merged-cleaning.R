# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Read in dataset
port_data <-
  read_rds(here("data", "processed", "full-processed-data.Rds"))


# Create PORT exclusion variables -----------------------------------------

port_data <- port_data %>%
  mutate(
    port_flare_exclusion = case_when(
      # Self-reporting that they did not follow the instructions properly for the
      # fear conditioning task
      flare_did_follow_instructions == FALSE |
        flare_did_remove_headphones == TRUE |
        flare_did_pay_attention == FALSE |
        flare_task_environment == "public" |
        flare_was_quiet == FALSE |
        flare_not_alone == TRUE |
        flare_was_interrupted == TRUE |
        
        # Self-reporting that they did not find the aversive stimulus unpleasant
        us_unpleasantness_rating <= 5 |
        
        # Missing six or more trial ratings during acquisition or extinction
        missing_exp_acquisition >= 6 |
        missing_exp_extinction >= 6 |
        
        # Having an average device volume of 50% or lower during acquisition
        volume_average_acquisition <= .5 |
        
        # Disconnecting headphones or exiting the app
        headphone_disconnects_total > 0 |
        exits_total > 0 ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  select(participant_id, fc_exclusion) %>%
  count(fc_exclusion)


# Steps

# Make a record of participants that aren't excluded from fear conditioning




# processed_data %>%
#   rowwise() %>%
#   mutate(
#     na_count = sum(is.na(c_across(
#       matches("asi_baseline_\\d+$")
#     )),
#     na.rm = T),
#     mean = if_else(
#       is.na(asi_baseline_total) & na_count < 2,
#       mean(c_across(matches(
#         "asi_baseline_\\d+$"
#       )), na.rm = T),
#       NA_real_
#     )
#   ) %>%
#   ungroup() %>%
#   select(starts_with("asi_baseline"), na_count) %>%
#   filter(na_count > 2) %>%
#   view()
