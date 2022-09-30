# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder that interim data is stored in
midapp_interim_data_loc <-
  here("data", "interim", "midapp", "step-01")

# Read in dataset
bis_bas_data <-
  read_rds(paste0(midapp_interim_data_loc, "/midapp-bis_bas-data", ".Rds")) %>%
  # remove_empty: removes empty rows and columns
  # remove_constant: removes constant columns
  remove_empty(which = c("rows", "cols")) %>%
  remove_constant(na.rm = T, quiet = F)

# Reverse score items
# Method: take the maximum value for the scale, add 1, and subtract the participant's score.

# BIS/BAS:
# Items 1, 3-21, 23-24 are reverse-scored.
# Items 2 and 22 are scored normally.
# Items 1, 6, 11, 17 are filler items and do not correspond to any subscale.

bis_bas_data <- bis_bas_data %>%
  mutate(across(
    .cols = c(
      bis_bas_midapp_1,
      bis_bas_midapp_3,
      bis_bas_midapp_4,
      bis_bas_midapp_5,
      bis_bas_midapp_6,
      bis_bas_midapp_7,
      bis_bas_midapp_8,
      bis_bas_midapp_9,
      bis_bas_midapp_10,
      bis_bas_midapp_11,
      bis_bas_midapp_12,
      bis_bas_midapp_13,
      bis_bas_midapp_14,
      bis_bas_midapp_15,
      bis_bas_midapp_16,
      bis_bas_midapp_17,
      bis_bas_midapp_18,
      bis_bas_midapp_19,
      bis_bas_midapp_20,
      bis_bas_midapp_21,
      bis_bas_midapp_23,
      bis_bas_midapp_24
    ),
    
    ~ 4 + 1 - .
  ))

# Generate derived variables
bis_bas_data <- bis_bas_data %>%
  mutate(
    # BIS: 2, 8, 13, 16, 19, 22, 24
    bis_midapp_total = rowSums(select(
      .,
      c(
        bis_bas_midapp_2,
        bis_bas_midapp_8,
        bis_bas_midapp_13,
        bis_bas_midapp_16,
        bis_bas_midapp_19,
        bis_bas_midapp_22,
        bis_bas_midapp_24
      )
    )),
    
    # BAS Drive: 3, 9, 12, 21
    bas_midapp_drive = rowSums(select(
      .,
      c(
        bis_bas_midapp_3,
        bis_bas_midapp_9,
        bis_bas_midapp_12,
        bis_bas_midapp_21
      )
    )),
    
    # BAS Fun Seeking: 5, 10, 15, 20
    bas_midapp_fun_seeking = rowSums(select(
      .,
      c(
        bis_bas_midapp_5,
        bis_bas_midapp_10,
        bis_bas_midapp_15,
        bis_bas_midapp_20
      )
    )),
    
    # BAS Reward Responsiveness: 4, 7, 14, 18, 23
    bas_midapp_reward_responsiveness = rowSums(select(
      .,
      c(
        bis_bas_midapp_4,
        bis_bas_midapp_7,
        bis_bas_midapp_14,
        bis_bas_midapp_18,
        bis_bas_midapp_23
      )
    ))
  )

# Save data
saveRDS(bis_bas_data,
        here("data", "interim", "midapp", "step-02", "bis-bas.Rds"))