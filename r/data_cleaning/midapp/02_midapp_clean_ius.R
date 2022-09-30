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
ius_data <-
  read_rds(paste0(midapp_interim_data_loc, "/midapp-ius-data", ".Rds")) %>%
  # remove_empty: removes empty rows and columns
  # remove_constant: removes constant columns
  remove_empty(which = c("rows", "cols")) %>%
  remove_constant(na.rm = T, quiet = F)

# No reverse scored items

# Generate derived variables
ius_data <- ius_data %>%
  mutate(
    # IUS: Sum of all items
    ius12_midapp_total = rowSums(select(., contains("ius12"))),
    
    # IUS Prospective Anxiety subscale: 1, 2, 4, 5, 8, 9, 11
    ius12_midapp_prospective_anxiety = rowSums(select(
      .,
      c(
        ius12_midapp_1,
        ius12_midapp_2,
        ius12_midapp_4,
        ius12_midapp_5,
        ius12_midapp_8,
        ius12_midapp_9,
        ius12_midapp_11
      )
    )),
    
    # IUS Inhibitory Anxiety subscale: 3, 6, 7, 10, 12
    ius12_midapp_inhibitory_anxiety = rowSums(select(
      .,
      c(
        ius12_midapp_3,
        ius12_midapp_6,
        ius12_midapp_7,
        ius12_midapp_10,
        ius12_midapp_12
      )
    ))
  )

# Save data
saveRDS(ius_data,
        here("data", "interim", "midapp", "step-02", "ius.Rds"))