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


# FLARe ---------------------------------------------------------------------

# Create long version of the FLARe trial data
fc_long <-
  port_data %>%
  # Selects expectancy rating columns (i.e. those starting with "exp" and
  # ending with digits)
  select(participant_id, matches("^exp.*\\d$")) %>%
  # Make data long with columns for phase, stimulus, and rating
  pivot_longer(
    -participant_id,
    names_to = c("phase", "stimulus", "trial"),
    names_pattern = "exp_([A-Za-z]+)_(cs_[A-Za-z]+)_(\\d+$)",
    values_to = "rating"
  )

# Count number of missed trials per participant, phase, and stimulus
fc_missing <-
  fc_long %>%
  # Count the number of missed trials for each participant by phase and stimulus
  group_by(participant_id, phase, stimulus) %>%
  summarise(exp_trials_missed = sum(is.na(rating))) %>%
  ungroup() %>%
  # Widen the data (columns for each phase & stimulus combo)
  pivot_wider(
    names_from = c("phase", "stimulus"),
    names_prefix =  "exp_trials_missed_",
    values_from = exp_trials_missed
  ) %>%
  # Create columns for total missed trials per phase and over the whole task
  mutate(
    exp_trials_missed_acquisition_total =
      rowSums(
        select(
          .,
          exp_trials_missed_acquisition_cs_plus,
          exp_trials_missed_acquisition_cs_minus
        )
      ),
    
    exp_trials_missed_extinction_total =
      rowSums(
        select(
          .,
          exp_trials_missed_extinction_cs_plus,
          exp_trials_missed_extinction_cs_minus
        )
      ),
    
    exp_trials_missed_total =
      rowSums(
        select(
          .,
          exp_trials_missed_acquisition_cs_plus,
          exp_trials_missed_acquisition_cs_minus,
          exp_trials_missed_extinction_cs_plus,
          exp_trials_missed_extinction_cs_minus
        )
      )
  )

port_data %>%
  mutate(
    fc_exclusion = case_when(
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
