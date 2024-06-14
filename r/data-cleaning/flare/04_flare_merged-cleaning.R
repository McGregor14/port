# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Read in dataset
flare_data <-
  read_rds(here("data", "interim", "flare", "step-03", "flare-data.Rds"))



# Fix flare complete variable ---------------------------------------------

# Due to an error on the portal, participants that reach the final module have
# missing values for their start/end times for flare. Since they have completed
# the fear conditioning task, they are reclassified as having completed the app
flare_data <- flare_data %>%
  mutate(flare_completed_app = case_when(
    flare_current_module == "post_experiment_questions" ~ TRUE,
    .default = flare_completed_app
  ))



# Create FLARe exclusion variable for PORT --------------------------------
flare_data <- flare_data %>%
  mutate(
    flare_exclusion_port = case_when(
      # Self-reporting that they did not follow the instructions properly for
      # the fear conditioning task
      flare_did_follow_instructions == FALSE |
        is.na(flare_did_follow_instructions) |
        
        flare_did_remove_headphones == TRUE |
        is.na(flare_did_remove_headphones) |
        
        flare_did_pay_attention == FALSE |
        is.na(flare_did_pay_attention) |
        
        flare_task_environment == "public" |
        is.na(flare_task_environment) |

        flare_was_quiet == FALSE |
        is.na(flare_was_quiet) |
        
        flare_not_alone == TRUE |
        is.na(flare_not_alone) |
        
        flare_was_interrupted == TRUE |
        is.na(flare_was_interrupted) |
        
        
        # Self-reporting that they did not find the aversive stimulus unpleasant
        flare_us_unpleasantness_rating <= 5 |
        is.na(flare_us_unpleasantness_rating) |
        
        # Missing six or more trial ratings during acquisition or extinction
        flare_missing_exp_acquisition >= 6 |
        is.na(flare_missing_exp_acquisition) |
        
        flare_missing_exp_extinction >= 6 |
        is.na(flare_missing_exp_extinction) |
        
        # Having an average device volume of 50% or lower during acquisition
        flare_volume_exclusion_50pct_acquisition == TRUE |
        is.na(flare_volume_exclusion_50pct_acquisition) |
        
        # Disconnecting headphones or exiting the app
        flare_headphone_disconnects_total > 0 |
        is.na(flare_headphone_disconnects_total) |
        
        flare_exits_total > 0 |
        is.na(flare_exits_total) ~ TRUE,
      
      TRUE ~ FALSE
    )
  )


# Print the number of exclusions
flare_data %>%
  # Drop participants that didn't do FLARe
  filter(!is.na(flare_started_at_dttm)) %>%
  count(flare_exclusion_port)



# Save data ---------------------------------------------------------------
saveRDS(flare_data, here("data", "processed", "flare-data.Rds"))

