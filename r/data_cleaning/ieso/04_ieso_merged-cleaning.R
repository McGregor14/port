# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Read in dataset
ieso_data <-
  read_rds(here("data", "interim", "ieso", "step-03", "ieso-data.Rds"))



# Create ieso exclusion variable for PORT --------------------------------
ieso_data <-
  ieso_data %>%
  mutate(
    ieso_exclusion_port = case_when(
      ieso_treatment_completed_total < 2 |
        ieso_discharge_reason_primary == "not_suitable_for_service"
      ~ TRUE,
      TRUE ~ FALSE
    ),
    .after = ieso_last_treatment_date
  )

# Print the number of exclusions
ieso_data %>%
  count(ieso_exclusion_port)



# Create treatment change scores ------------------------------------------

# Change in scores from assessment to the last treatment session
ieso_data <-
  ieso_data %>%
  mutate(
    gad7_treatment_change =
      gad7_treatment_total_final - gad7_treatment_total_1,
    gad7_treatment_change_observed =
      gad7_treatment_total_final_observed - gad7_treatment_total_1,
    
    phq9_treatment_change =
      phq9_treatment_total_final - phq9_treatment_total_1,
    phq9_treatment_change_observed =
      phq9_treatment_total_final_observed - phq9_treatment_total_1
  )



# Create IAPT reliable improvement ----------------------------------------

# Reliable improvement - improved on at least one of the measures (GAD/PHQ) and
# did not reliably deteriorate on the other

# Reliable change index:
# PHQ = 6
# GAD = 4

ieso_data <-
  ieso_data %>%
  mutate(
    # reliable_change_improvement:
    # check whether the measure changed reliably (positively)
    gad7_iapt_reliable_change_improvement =
      if_else(gad7_treatment_change <= -4,
              TRUE,
              FALSE),
    gad7_iapt_reliable_change_improvement_observed =
      if_else(gad7_treatment_change_observed <= -4,
              TRUE,
              FALSE),
    
    phq9_iapt_reliable_change_improvement =
      if_else(phq9_treatment_change <= -6,
              TRUE,
              FALSE),
    phq9_iapt_reliable_change_improvement_observed =
      if_else(phq9_treatment_change_observed <= -6,
              TRUE,
              FALSE),
    
    # reliable_change_deterioration:
    # check whether the measure changed reliably (negatively)
    gad7_iapt_reliable_change_deterioration =
      if_else(gad7_treatment_change >= 4,
              TRUE,
              FALSE),
    gad7_iapt_reliable_change_deterioration_observed =
      if_else(gad7_treatment_change_observed >= 4,
              TRUE,
              FALSE),
    
    phq9_iapt_reliable_change_deterioration =
      if_else(phq9_treatment_change >= 6,
              TRUE,
              FALSE),
    phq9_iapt_reliable_change_deterioration_observed =
      if_else(phq9_treatment_change_observed >= 6,
              TRUE,
              FALSE),
    
    # iapt_improvement/deterioration:
    # check whether participants improved or deteriorated reliably on either
    # measure
    iapt_improvement =
      case_when(
        gad7_iapt_reliable_change_improvement == TRUE |
          phq9_iapt_reliable_change_improvement == TRUE ~ TRUE,
        gad7_iapt_reliable_change_improvement == FALSE &
          phq9_iapt_reliable_change_improvement == FALSE ~ FALSE,
        TRUE ~ NA
      ),
    
    iapt_deterioration =
      case_when(
        gad7_iapt_reliable_change_deterioration == TRUE |
          phq9_iapt_reliable_change_deterioration == TRUE ~ TRUE,
        gad7_iapt_reliable_change_deterioration == FALSE &
          phq9_iapt_reliable_change_deterioration == FALSE ~ FALSE,
        TRUE ~ NA
      ),
    
    iapt_improvement_observed =
      case_when(
        gad7_iapt_reliable_change_improvement_observed == TRUE |
          phq9_iapt_reliable_change_improvement_observed == TRUE ~ TRUE,
        gad7_iapt_reliable_change_improvement_observed == FALSE &
          phq9_iapt_reliable_change_improvement_observed == FALSE ~ FALSE,
        TRUE ~ NA
      ),
    
    iapt_deterioration_observed =
      case_when(
        gad7_iapt_reliable_change_deterioration_observed == TRUE |
          phq9_iapt_reliable_change_deterioration_observed == TRUE ~ TRUE,
        gad7_iapt_reliable_change_deterioration_observed == FALSE &
          phq9_iapt_reliable_change_deterioration_observed == FALSE ~ FALSE,
        TRUE ~ NA
      ),
    
    # iapt_reliable_improvement:
    # check whether participants improved reliably on either measure without
    # deteriorating on the other
    iapt_reliable_improvement =
      case_when(
        iapt_improvement == TRUE & iapt_deterioration == FALSE ~ TRUE,
        iapt_improvement == FALSE |
          iapt_deterioration == TRUE ~ FALSE,
        TRUE ~ NA
      ),
    
    iapt_reliable_improvement_observed =
      case_when(
        iapt_improvement_observed == TRUE &
          iapt_deterioration_observed == FALSE ~ TRUE,
        iapt_improvement_observed == FALSE |
          iapt_deterioration_observed == TRUE ~ FALSE,
        TRUE ~ NA
      )
  ) %>%
  
  # Remove unnecessary columns
  select(
    -c(
      gad7_iapt_reliable_change_improvement,
      gad7_iapt_reliable_change_improvement_observed,
      phq9_iapt_reliable_change_improvement,
      phq9_iapt_reliable_change_improvement_observed,
      
      gad7_iapt_reliable_change_deterioration,
      gad7_iapt_reliable_change_deterioration_observed,
      phq9_iapt_reliable_change_deterioration,
      phq9_iapt_reliable_change_deterioration_observed,
      
      iapt_improvement,
      iapt_deterioration,
      iapt_improvement_observed,
      iapt_deterioration_observed
    )
  )

# Create IAPT recovery ----------------------------------------------------

# Recovery - above to below clinical cut-off on both measures (can be above on
# one of the measures at the start)

# Caseness:
# PHQ >= 10
# GAD >= 8

ieso_data <-
  ieso_data %>%
  mutate(
    case_start = if_else(
      gad7_treatment_iapt_anxiety_1 == TRUE |
        phq9_treatment_binary_depression_1 == TRUE,
      TRUE,
      FALSE
    ),
    
    case_end = if_else(
      gad7_treatment_iapt_anxiety_final == TRUE |
        phq9_treatment_binary_depression_final == TRUE,
      TRUE,
      FALSE
    ),
    
    case_end_observed = if_else(
      gad7_treatment_iapt_anxiety_final_observed == TRUE |
        phq9_treatment_binary_depression_final_observed == TRUE,
      TRUE,
      FALSE
    ),
    
    iapt_recovery = case_when(
      case_start == TRUE & case_end == FALSE ~ TRUE,
      case_start == TRUE & case_end == TRUE ~ FALSE,
      case_start == FALSE ~ NA,
      TRUE ~ NA
    ),
    
    iapt_recovery_observed = case_when(
      case_start == TRUE & case_end_observed == FALSE ~ TRUE,
      case_start == TRUE & case_end_observed == TRUE ~ FALSE,
      case_start == FALSE ~ NA,
      TRUE ~ NA
    )
  ) %>%
  
  select(-c(case_start, case_end, case_end_observed))



# Reliable and clinically significant improvement (RCSI)

ieso_data <-
  ieso_data %>%
  mutate(
    iapt_rcsi = case_when(
      is.na(iapt_reliable_improvement) |
        is.na(iapt_recovery) ~ NA,
      iapt_reliable_improvement == TRUE &
        iapt_recovery == TRUE ~ TRUE,
      iapt_reliable_improvement == FALSE |
        iapt_recovery == FALSE ~ FALSE,
      TRUE ~ NA
    ),
    
    iapt_rcsi_observed = case_when(
      is.na(iapt_reliable_improvement_observed) |
        is.na(iapt_recovery_observed) ~ NA,
      iapt_reliable_improvement_observed == TRUE &
        iapt_recovery_observed == TRUE ~ TRUE,
      iapt_reliable_improvement_observed == FALSE |
        iapt_recovery_observed == FALSE ~ FALSE,
      TRUE ~ NA
    )
  )


# Save data ---------------------------------------------------------------
saveRDS(ieso_data, here("data", "processed", "ieso-data.Rds"))
