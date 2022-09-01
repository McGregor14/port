# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning
library(lubridate) # working with dates/times

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder flare interim data is stored in
redcap_interim_data_loc <-
  here("data", "interim", "redcap", "step-01")

# Read in dataset
other_data <-
  read_rds(paste0(redcap_interim_data_loc, "/redcap-other-data", ".Rds")) %>%
  remove_empty(which = c("rows", "cols")) %>%
  select(!starts_with("ethnic"), everything())

# Clean data

# Age
other_data <- other_data %>%
  mutate(demographics_age_screening = date_of_birth %--% sc_date / years(1))

# Gender:
# 0 - Male
# 1 - Female
# 2 - Non-binary
# 3 - Prefer to self-define
# -88 - Don't know
# -99 - Prefer not to answer

other_data <- other_data %>%
  mutate(demographics_gender =
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
           )))

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
        str_detect(employment_other, "Disab|disab|Health|health") ~ "unemployed_health_disability",
        str_detect(employment_other, "Full time") ~ "ft_employed",
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
      ))
  )

# Ethnic origin:

origin <- other_data %>%
  select(starts_with("ethnic")) %>%
  names()

# 1 - White
# 2 - Mixed
# 3 - Asian or Asian British
# 4 - Black or Black British
# 5 - Arab
# 6 - Other
# 88 - Don't know
# 99 - Prefer not to answer

other_data <- other_data %>%
  mutate(demographics_ethnic_origin =
           fct_infreq(factor(
             case_when(
               # Specify DK and PNTA first so that responses aren't recorded for those who don't wish them to be
               ethnic_origin_88 == 1 ~ "dont_know",
               ethnic_origin_99 == 1 ~ "prefer_not_to_answer",
               
               # Specify other and mixed second so that ethnicity is not oversimplified
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
           )))

# "ethnic_origin_black_1" Caribbean
# "ethnic_origin_black_2" South African
# "ethnic_origin_black_3" Kenyan
# "ethnic_origin_black_4" Nigerian
# "ethnic_origin_black_5" Ghanaian
# "ethnic_origin_black_6" Ugandan
# "ethnic_origin_black_7" Any other black background
# "ethnic_origin_black_99" Prefer not to say

# "ethnic_origin_asian_1" Indian
# "ethnic_origin_asian_2" Pakistani
# "ethnic_origin_asian_3" Chinese
# "ethnic_origin_asian_4" Bangladeshi
# "ethnic_origin_asian_5" Sri Lankan
# "ethnic_origin_asian_6" Iranian
# "ethnic_origin_asian_7" Other Asian backgrounds
# "ethnic_origin_asian_99" Prefer not to answer

# "ethnic_origin_mixed_1" White
# "ethnic_origin_mixed_2" South Asian
# "ethnic_origin_mixed_3" East Asian
# "ethnic_origin_mixed_4" Black African
# "ethnic_origin_mixed_5" Black Caribbean
# "ethnic_origin_mixed_6" South African
# "ethnic_origin_mixed_7" Arab
# "ethnic_origin_mixed_8" Any other mixed background
# "ethnic_origin_mixed_99" Prefer not to say

# "ethnic_orgin_white_1" British (Scottish, English or Welsh)
# "ethnic_orgin_white_2" Irish
# "ethnic_orgin_white_3" Roma or Irish Traveler
# "ethnic_orgin_white_4" Spanish
# "ethnic_orgin_white_5" Polish
# "ethnic_orgin_white_6" German
# "ethnic_orgin_white_7" French
# "ethnic_orgin_white_8" Italian
# "ethnic_orgin_white_9" Other European
# "ethnic_orgin_white_10" Australian
# "ethnic_orgin_white_11" American
# "ethnic_orgin_white_12" Other
# "ethnic_orgin_white_99" Prefer not to say

# "ethnicity_other"

# Treatment
other_data <- other_data %>%
  
  mutate(
    # Date that treatment finished (rename old variable)
    treatment_date_finished = date_finished_treatment,
    
    # "Did you find your sessions with Ieso helpful?"
    # Question added after the start date of the study
    # As a result, some participants were asked this question after they had already completed the followup survey
    # Responses for those participants are recorded in the sessionhelpful_4_added variable
    # Use these responses when participants are missing a response for the main variable iesohelpful_4
    # This will prioritise their initial response if they happened to be asked the same question twice
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

# Followup time since therapy
# Time between last therapy session and the followup survey
other_data <- other_data %>%
  mutate(
    final_timestamp = na_if(final_timestamp, "[not completed]"),
    final_timestamp = as_date(final_timestamp),
    followup_time_since_therapy = date_finished_treatment %--% final_timestamp / days(1),
    followup_time_since_therapy = replace(
      followup_time_since_therapy,
      which(followup_time_since_therapy < 0),
      NA
    )
  )

# Save data
saveRDS(other_data,
        here(
          "data",
          "interim",
          "redcap",
          "step-02",
          "redcap-other-data.Rds"
        ))
