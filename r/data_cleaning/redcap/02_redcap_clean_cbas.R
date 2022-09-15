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
cbas_data <-
  read_rds(paste0(redcap_interim_data_loc, "/redcap-cbas-data", ".Rds")) %>%
  remove_empty(which = c("rows", "cols")) %>%
  remove_constant(na.rm = T, quiet = F)

# Generate derived variables
cbas_data <- cbas_data %>%
  mutate(
    # CBAS behavioural social subscale: 1, 8, 14, 15, 17, 21, 23, 24
    cbas_baseline_behavioural_social = rowSums(
      select(
        .,
        cbas_baseline_1,
        cbas_baseline_8,
        cbas_baseline_14,
        cbas_baseline_15,
        cbas_baseline_17,
        cbas_baseline_21,
        cbas_baseline_23,
        cbas_baseline_24
      )
    ),
    
    # CBAS behavioural non-social subscale: 3, 6, 9, 10, 11, 13, 28
    cbas_baseline_behavioural_nonsocial = rowSums(
      select(
        .,
        cbas_baseline_3,
        cbas_baseline_6,
        cbas_baseline_9,
        cbas_baseline_11,
        cbas_baseline_13,
        cbas_baseline_28
      )
    ),
    
    # CBAS cognitive social subscale: 10, 12, 16, 20, 22, 26, 30
    cbas_baseline_cognitive_social = rowSums(
      select(
        .,
        cbas_baseline_10,
        cbas_baseline_12,
        cbas_baseline_16,
        cbas_baseline_20,
        cbas_baseline_22,
        cbas_baseline_26,
        cbas_baseline_30
      )
    ),
    
    # CBAS cognitive non-social subscale: 2, 4, 5, 7, 18, 19, 25, 27, 29, 31
    cbas_baseline_cognitive_nonsocial = rowSums(
      select(
        .,
        cbas_baseline_2,
        cbas_baseline_4,
        cbas_baseline_5,
        cbas_baseline_7,
        cbas_baseline_18,
        cbas_baseline_19,
        cbas_baseline_25,
        cbas_baseline_27,
        cbas_baseline_29,
        cbas_baseline_31
      )
    )
    
  )

# Save data
saveRDS(cbas_data,
        here("data", "interim", "redcap", "step-02", "cbas.Rds"))
