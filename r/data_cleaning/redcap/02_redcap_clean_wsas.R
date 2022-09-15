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
wsas_data <-
  read_rds(paste0(redcap_interim_data_loc, "/redcap-wsas-data", ".Rds")) %>%
  remove_empty(which = c("rows", "cols")) %>%
  remove_constant(na.rm = T, quiet = F)

# Generate derived variables
wsas_data <- wsas_data %>%
  mutate(
    # WSAS sum of all individual scores (1-5)
    wsas_screening_total = rowSums(select(., contains("wsas_screening"))),
    wsas_baseline_total = rowSums(select(., contains("wsas_baseline"))),
    wsas_followup_total = rowSums(select(., contains("wsas_followup"))),
    
    # WSAS impairment: 0-9 low, 10-19 moderate, 20-40 severe
    wsas_screening_impairment = factor(
      case_when(
        wsas_screening_total < 10 ~ "low",
        wsas_screening_total < 20 ~ "moderate",
        wsas_screening_total >= 20 ~ "severe",
        TRUE ~ NA_character_
      ),
      levels = c("low", "moderate", "severe")
    ),
    
    wsas_baseline_impairment = factor(
      case_when(
        wsas_baseline_total < 10 ~ "low",
        wsas_baseline_total < 20 ~ "moderate",
        wsas_baseline_total >= 20 ~ "severe",
        TRUE ~ NA_character_
      ),
      levels = c("low", "moderate", "severe")
    ),
    
    wsas_followup_impairment = factor(
      case_when(
        wsas_followup_total < 10 ~ "low",
        wsas_followup_total < 20 ~ "moderate",
        wsas_followup_total >= 20 ~ "severe",
        TRUE ~ NA_character_
      ),
      levels = c("low", "moderate", "severe")
    )
  )

# Save data
saveRDS(wsas_data,
        here("data", "interim", "redcap", "step-02", "wsas.Rds"))
