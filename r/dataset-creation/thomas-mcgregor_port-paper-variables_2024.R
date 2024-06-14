# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(fs) # file system operations

# Read in dataset
port <-
  read_rds(here("data", "processed", "merged", "06_clean-processed-data.Rds"))

# Filter port data for relevant observations
port <-
  port %>%
  filter(port_exclusion == FALSE)

# Select relevant variables
port <-
  port %>%
  select(
    # General
    port_exclusion,
    demographics_age_at_screening_years,
    demographics_biological_sex,
    demographics_ethnic_origin,
    
    # Treatment
    ieso_protocol_token,
    ieso_diagnosis_name,
    ieso_treatment_completed_total,
    
    # GAD
    gad7_baseline_total,
    gad7_assessment_total,
    gad7_treatment_total_1,
    gad7_treatment_total_final,
    gad7_followup_total,
    gad7_treatment_change_observed,
    
    # PHQ
    phq9_baseline_total,
    phq9_assessment_total,
    phq9_treatment_total_1,
    phq9_treatment_total_final,
    phq9_followup_total,
    phq9_treatment_change_observed,
    
    # FC
    exp_acquisition_cs_plus_mean,
    exp_acquisition_cs_minus_mean,
    exp_acquisition_differential_mean,
    exp_extinction_cs_plus_mean,
    exp_extinction_cs_minus_mean,
    exp_extinction_differential_mean,
    aff_baseline_cs_plus_mean,
    aff_baseline_cs_minus_mean,
    aff_baseline_differential,
    aff_break_cs_plus_mean,
    aff_break_cs_minus_mean,
    aff_break_differential,
    aff_post_cs_plus_mean,
    aff_post_cs_minus_mean,
    aff_post_differential,
    
    # Other
    fss_baseline_total,
    fss_followup_total,
    asi_baseline_total,
    cbas_baseline_total,
    eacs_midapp_total
    
  ) %>%
  mutate(id = row_number(), .before = everything())

# Write data
write_csv(port,
          here(
            "open-data",
            "thomas-mcgregor_port-paper-variables_2024.csv"
          ))