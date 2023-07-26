# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning
library(lubridate) # working with dates/times

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder that interim data is stored in
redcap_interim_data_loc <-
  here("data", "interim", "redcap", "step-01")

# Read in dataset
other_data <-
  read_rds(paste0(redcap_interim_data_loc, "/redcap-other-data", ".Rds")) %>%
  remove_empty(which = c("rows", "cols")) %>%
  select(!starts_with("ethnic"), everything())

# Clean data

# Dates and ages
other_data <- other_data %>%
  mutate(
    # Date that treatment finished (rename old variable)
    treatment_finished_date = date_finished_treatment,
    
    # Replace 'not completed' with NA for timestamp variables
    # Note: screening has no character observations so is automatically read-in
    # as a dttm variable, this could break if a character observation is
    # recorded
    consentmain_timestamp = na_if(consentmain_timestamp, "[not completed]"),
    final_timestamp = na_if(final_timestamp, "[not completed]"),
    
    # Convert timestamps to dates and rename
    screening_date = as_date(screening_timestamp),
    baseline_date = as_date(consentmain_timestamp),
    followup_date = as_date(final_timestamp),
    
    # Age at screening
    demographics_age_at_screening_years =
      as.integer(floor(
        date_of_birth %--% screening_date / years(1)
      )),
    
    # Followup time since therapy
    # Time between last therapy session and the followup survey
    treatment_followup_time_since_therapy_days =
      treatment_finished_date %--% followup_date / days(1),
    treatment_followup_time_since_therapy_days =
      replace(
        treatment_followup_time_since_therapy_days,
        which(treatment_followup_time_since_therapy_days < 0),
        NA
      )
  )

# Screening & withdrawal
other_data <- other_data %>%
  mutate(across(
    .cols = c(phone_eligibility_gad8,
              phone_screen_outcome),
    as.logical
  )) %>% 
  rename(
    port_phone_eligibility = phone_eligibility_gad8,
    port_screening_eligibility = phone_screen_outcome
  )


# Gender:
# 0 - Male
# 1 - Female
# 2 - Non-binary
# 3 - Prefer to self-define
# -88 - Don't know
# -99 - Prefer not to answer

other_data <- other_data %>%
  mutate(
    demographics_gender =
      fct_infreq(factor(
        recode(
          gender,
          `0` = "male",
          `1` = "female",
          `2` = "non_binary",
          `3` = "self_define",
          `-88` = "dont_know",
          `-99` = "prefer_not_to_answer"
        ),
        
        levels = c(
          "male",
          "female",
          "non_binary",
          "self_define",
          "dont_know",
          "prefer_not_to_answer"
        )
      )),
    
    # Lump together levels with less than 5 observations to avoid identifiable
    # data
    demographics_gender =
      fct_lump_min(demographics_gender,
                   min = 5,
                   other_level = "small_n_identifiable")
  )

# Employment:
# 1 - Full-time employed
# 2 - Part-time employed
# 3 - Self-employed
# 4 - Unemployed
# 5 - Zero hours contract
# 6 - Student
# 7 - Stay-at-home parent/carer
# 8 - Retired
# 9 - Other

other_data <- other_data %>%
  mutate(
    tmp_employment_other =
      case_when(
        str_detect(employment_other, "Disab|disab|Health|health")
        ~ "unemployed_health_disability",
        str_detect(employment_other, "Full time")
        ~ "ft_employed",
        TRUE ~ "other"
      ),
    
    demographics_employment =
      fct_infreq(factor(
        case_when(
          employment == 1 ~ "ft_employed",
          employment == 2 ~ "pt_employed",
          employment == 3 ~ "self_employed",
          employment == 4 ~ "unemployed",
          employment == 5 ~ "zero_hours_contract",
          employment == 6 ~ "student",
          employment == 7 ~ "parent_carer",
          employment == 8 ~ "retired",
          employment == 9 ~ tmp_employment_other,
          TRUE ~ NA_character_
        ),
        
        levels = c(
          "ft_employed",
          "pt_employed",
          "self_employed",
          "unemployed",
          "zero_hours_contract",
          "student",
          "parent_carer",
          "retired",
          "unemployed_health_disability",
          "other"
        )
      )),
    
    # Lump together levels with less than 5 observations to avoid identifiable
    # data
    demographics_employment = fct_lump_min(
      demographics_employment,
      min = 5,
      other_level = "small_n_identifiable"
    )
  )

# Ethnic origin:

# 1 - White
# 2 - Mixed
# 3 - Asian or Asian British
# 4 - Black or Black British
# 5 - Arab
# 6 - Other
# 88 - Don't know
# 99 - Prefer not to answer

other_data <- other_data %>%
  mutate(
    demographics_ethnic_origin =
      fct_infreq(factor(
        case_when(
          # Specify DK and PNTA first so that responses aren't recorded for
          # those who don't wish them to be
          ethnic_origin_88 == 1 ~ "dont_know",
          ethnic_origin_99 == 1 ~ "prefer_not_to_answer",
          
          # Specify mixed and other second so that ethnicity is not
          # oversimplified
          ethnic_origin_2 == 1 ~ "mixed",
          ethnic_origin_6 == 1 ~ "other",
          
          ethnic_origin_1 == 1 ~ "white",
          ethnic_origin_3 == 1 ~ "asian_or_asian_british",
          ethnic_origin_4 == 1 ~ "black_or_black_british",
          ethnic_origin_5 == 1 ~ "arab",
          
          TRUE ~ NA_character_
        ),
        
        levels = c(
          "white",
          "mixed",
          "asian_or_asian_british",
          "black_or_black_british",
          "arab",
          "other",
          "dont_know",
          "prefer_not_to_answer"
        )
      )),
    
    # Lump together levels with less than 5 observations to avoid identifiable
    # data
    demographics_ethnic_origin = fct_lump_min(
      demographics_ethnic_origin,
      min = 5,
      other_level = "small_n_identifiable"
    )
  ) %>%
  
  # Remove all remaining ethnicity variables
  # As they are too fine grained (i.e. potentially identifiable to use)
  select(!starts_with("ethnic"))


# Treatment
other_data <- other_data %>%
  
  mutate(
    # "Did you find your sessions with Ieso helpful?"
    # Question added after the start date of the study.
    # As a result, some participants were asked this question after they had
    # already completed the followup survey. Responses for those participants
    # are recorded in the sessionhelpful_4_added variable. Use these responses
    # when participants are missing a response for the main variable
    # iesohelpful_4. This will prioritise their initial response if they
    # happened to be asked the same question twice.
    treatment_followup_treatment_helpful =
      if_else(
        condition = is.na(iesohelpful_4),
        true = sessionhelpful_4_added,
        false = iesohelpful_4,
        missing = NA_real_
      ),
    
    treatment_followup_treatment_helpful =
      factor(
        recode(
          treatment_followup_treatment_helpful,
          `0` = "no",
          `1` = "yes"
        ),
        
        levels = c("yes", "no")
      ),
    
    # "How much did your symptoms and day-to-day functioning improve after your sessions with Ieso?"
    # Question added after the start date of the study
    # As a result, some participants were asked this question after they had already completed the followup survey
    # Responses for those participants are recorded in the symptomsimprove_4_added variables
    # Use these responses when participants are missing a response for the main variable symptomsimprove_4
    # This will prioritise their initial response if they happened to be asked the same question twice
    
    # Create single variable for incorrectly dummy coded checkbox responses (should have been radio)
    symptomsimprove_4_added =
      case_when(
        symptomsimprove_4_added_0 == 1 ~ 0,
        symptomsimprove_4_added_1 == 1 ~ 1,
        symptomsimprove_4_added_2 == 1 ~ 2,
        symptomsimprove_4_added_3 == 1 ~ 3,
        symptomsimprove_4_added_4 == 1 ~ 4,
        TRUE ~ NA_real_
      ),
    
    # Use added variable when main variable is missing (see above explanation)
    treatment_followup_symptoms_improve =
      if_else(
        condition = is.na(symptomsimprove_4),
        true = symptomsimprove_4_added,
        false = symptomsimprove_4,
        missing = NA_real_
      ),
    
    # Recode to factor with correct labels
    treatment_followup_symptoms_improve =
      factor(
        recode(
          treatment_followup_symptoms_improve,
          `0` = "much_worse",
          `1` = "little_worse",
          `2` = "no_change",
          `3` = "little_better",
          `4` = "much_better"
        ),
        
        levels = c(
          "much_worse",
          "little_worse",
          "no_change",
          "little_better",
          "much_better"
        )
      )
  )

# Remove redundant variables
other_data <- other_data %>%
  select(
    participant_id,
    screening_date,
    baseline_date,
    followup_date,
    starts_with("port"),
    starts_with("demographics"),
    starts_with("treatment")
  )

# Save data
saveRDS(other_data,
        here("data",
             "interim",
             "redcap",
             "step-02",
             "other.Rds"))
