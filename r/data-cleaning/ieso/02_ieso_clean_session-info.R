# Setup -------------------------------------------------------------------

# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning
library(lubridate) # working with dates/times
library(fedmatch) # cleaning strings

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder that interim data is stored in
ieso_interim_data_loc <-
  here("data", "interim", "ieso", "step-01")

# Read in dataset
session_data <-
  read_rds(paste0(ieso_interim_data_loc, "/ieso-session-data", ".Rds")) %>%
  # remove_empty: removes empty rows and columns
  remove_empty(which = c("rows", "cols")) %>%
  # remove_constant: removes columns where all observations are the same
  remove_constant(na.rm = T, quiet = F) %>%
  # select variables of interest
  select(
    participant_id,
    ieso_date,
    ieso_appointment_type,
    ieso_status,
    ieso_contact_duration,
    ieso_phq9score,
    ieso_gad7score,
    ieso_wsas_score,
    ieso_ocd_score,
    ieso_social_phobia_inventory_score,
    ieso_hain_score,
    ieso_pcl5score,
    ieso_gad_imaginal_exposure_utterances,
    ieso_gad_imaginal_exposure_words,
    ieso_gad_in_vivo_exposure_utterances,
    ieso_gad_in_vivo_exposure_words,
    ieso_gad_learning_from_exposure_utterances,
    ieso_gad_learning_from_exposure_words
  ) %>%
  # Remove 'gad' from exposure variable names
  rename_with(.fn = ~ str_remove(.x, "gad_"),
              .cols = contains(c("words", "utterances")))


# Clean variables & generate derived variables ----------------------------


## Date -------------------------------------------------------------------

# Convert date variable to datetime (currently character)
session_data <-
  session_data %>%
  mutate(ieso_dttm = as_datetime(ieso_date, format = "%d/%m/%Y %H:%M"),
         .after = participant_id) %>%
  select(-ieso_date)


## Appointment type and status --------------------------------------------
session_data <-
  session_data %>%
  
  # Clean character strings (lower case and remove special characters)
  mutate(across(
    .cols = c("ieso_appointment_type",
              "ieso_status"),
    ~ clean_strings(.)
  )) %>%
  
  # Add underscores between words where necessary
  mutate(
    ieso_status = recode(
      ieso_status,
      completedlate = "completed_late",
      cancelledlate = "cancelled_late",
      agentcancelled = "agent_cancelled"
    )
  ) %>%
  
  # Convert to factors ordered by number of observations
  mutate(across(
    .cols = c("ieso_appointment_type",
              "ieso_status"),
    ~ fct_infreq(factor(.))
  ))

# Create object for long data
session_data_long <- session_data


## Session counts ---------------------------------------------------------

# Create an object containing the number of sessions for each type of
# appointment and whether they attended etc, to be merged with other wide
# datasets at a later point.

session_counts <-
  session_data %>%
  
  # For each participant, appointment type, and status, count number of
  # sessions (observations)
  group_by(participant_id, ieso_appointment_type, ieso_status) %>%
  count() %>%
  ungroup() %>%
  arrange(participant_id, ieso_appointment_type, ieso_status) %>%
  
  # Widen the dataset so that each column represents a type of appointment
  # and whether or not the participant attended, dna'd etc
  pivot_wider(
    names_from = c(ieso_appointment_type, ieso_status),
    values_from = n
  ) %>%
  
  # Replace NAs with 0 for all new columns
  mutate(across(.cols = where(is.numeric),
                ~ replace_na(., 0))) %>%
  
  # Sort the order of variables
  select(
    participant_id,
    
    assessment_completed,
    assessment_completed_late,
    assessment_aborted,
    assessment_dna,
    
    treatment_completed,
    treatment_completed_late,
    treatment_aborted,
    treatment_dna,
    treatment_cancelled_late,
    treatment_agent_cancelled
  ) %>%
  
  # Add 'ieso' prefix and 'n' suffix to column names
  rename_with(.fn = ~ paste0("ieso_", .x, "_n"),
              .cols = -participant_id)

# Add completed/incomplete totals for session counts
session_counts <-
  session_counts %>%
  mutate(
    ieso_assessment_completed_total =
      ieso_assessment_completed_n + ieso_assessment_completed_late_n,
    ieso_assessment_incomplete_total =
      ieso_assessment_aborted_n + ieso_assessment_dna_n,
    ieso_treatment_completed_total =
      ieso_treatment_completed_n + ieso_treatment_completed_late_n,
    ieso_treatment_incomplete_total =
      ieso_treatment_aborted_n + ieso_treatment_dna_n +
      ieso_treatment_cancelled_late_n + ieso_treatment_agent_cancelled_n
  )


## Assessment data ---------------------------------------------------------

# Create object containing assessment observations
session_data_assessments <-
  session_data %>%
  
  # Filter out sessions that aren't assessments
  filter(ieso_appointment_type == "assessment") %>%
  
  # Sort the dataset by participant and date
  group_by(participant_id) %>%
  arrange(participant_id, ieso_dttm, by_group = TRUE) %>%
  
  # Take the most recent assessment session for each participant
  slice(which.max(ieso_dttm)) %>%
  ungroup()


### ieso_assessment_date --------------------------------------------------

# Create an object containing the dates for each participant's assessment, to
# be merged with other wide datasets at a later point

session_assessment_date <-
  session_data_assessments %>%
  transmute(participant_id,
            ieso_assessment_date = as_date(ieso_dttm))

### Assessment scores -----------------------------------------------------

# Create an object containing assessment scores for each participant, to be
# merged with other wide datasets at a later point

session_assessment_scores <-
  session_data_assessments %>%
  
  # Choose relevant columns
  select(-c(
    ieso_dttm,
    ieso_appointment_type,
    ieso_status,
    ieso_contact_duration
  )) %>%
  
  # Rename all columns except participant_id
  rename_with(
    # Remove 'score' and 'ieso_'
    .fn = ~ str_remove_all(., "score|ieso_") %>%
      # Remove trailing underscores
      str_remove_all(., "_$") %>%
      # Add '_assessment_total' as a suffix
      paste0(., "_assessment_total"),
    .cols = -participant_id
  )


#### GAD 7 ----------------------------------------------------------------

# Add GAD summary scores
session_assessment_scores <-
  session_assessment_scores %>%
  mutate(
    # GAD 7 severity: 0-4 minimal, 5-9 mild, 10-14 moderate, 15+ severe
    gad7_assessment_severity = factor(
      case_when(
        gad7_assessment_total < 5 ~ "minimal",
        gad7_assessment_total < 10 ~ "mild",
        gad7_assessment_total < 15 ~ "moderate",
        gad7_assessment_total >= 15 ~ "severe",
        TRUE ~ NA_character_
      ),
      levels = c("minimal", "mild", "moderate", "severe")
    ),
    
    # GAD7 binary anxiety: >=10 - anxiety diagnosis
    gad7_assessment_binary_anxiety =
      case_when(
        gad7_assessment_total >= 10 ~ TRUE,
        gad7_assessment_total < 10 ~ FALSE,
        TRUE ~ NA
      ),
    
    # GAD7 IAPT anxiety: >=8 - IAPT anxiety diagnosis
    gad7_assessment_iapt_anxiety =
      case_when(
        gad7_assessment_total >= 8 ~ TRUE,
        gad7_assessment_total < 8 ~ FALSE,
        TRUE ~ NA
      )
  )


#### PHQ 9 ----------------------------------------------------------------

# Add PHQ summary scores
session_assessment_scores <-
  session_assessment_scores %>%
  mutate(
    # PHQ9 severity:
    # 0-4 minimal, 5-9 mild, 10-14 moderate, 15-19 moderately severe, 20+ severe
    phq9_assessment_severity = factor(
      case_when(
        phq9_assessment_total < 5 ~ "minimal",
        phq9_assessment_total < 10 ~ "mild",
        phq9_assessment_total < 15 ~ "moderate",
        phq9_assessment_total < 20 ~ "moderately_severe",
        phq9_assessment_total >= 20 ~ "severe",
        TRUE ~ NA_character_
      ),
      levels = c("minimal", "mild", "moderate", "moderately_severe", "severe")
    ),
    
    # PHQ9 binary depression: >=10 - depression diagnosis
    phq9_assessment_binary_depression =
      case_when(
        phq9_assessment_total >= 10 ~ TRUE,
        phq9_assessment_total < 10 ~ FALSE,
        TRUE ~ NA
      )
  )


#### WSAS -----------------------------------------------------------------

# Add WSAS summary scores
session_assessment_scores <-
  session_assessment_scores %>%
  mutate(# WSAS impairment: 0-9 low, 10-19 moderate, 20-40 severe
    wsas_assessment_impairment = factor(
      case_when(
        wsas_assessment_total < 10 ~ "low",
        wsas_assessment_total < 20 ~ "moderate",
        wsas_assessment_total >= 20 ~ "severe",
        TRUE ~ NA_character_
      ),
      levels = c("low", "moderate", "severe")
    ))

#### OCD, Social Phobia Inventory, HAIN, PCL5 -----------------------------

# TO-DO: CHECKING WITH IESO ABOUT HOW THE SCORES ARE OBTAINED (ADDED AS ISSUE)


#### Exposure -------------------------------------------------------------

# Create variables indicating whether each type of exposure was used in
# assessment
session_assessment_scores <-
  session_assessment_scores %>%
  
  # imaginal
  mutate(
    imaginal_exposure_assessment = case_when(
      is.na(imaginal_exposure_utterances_assessment_total) &
        is.na(imaginal_exposure_words_assessment_total) ~ NA,
      rowSums(select(
        .,
        c(
          imaginal_exposure_utterances_assessment_total,
          imaginal_exposure_words_assessment_total
        )
      ), na.rm = TRUE) >= 1 ~ TRUE,
      rowSums(select(
        .,
        c(
          imaginal_exposure_utterances_assessment_total,
          imaginal_exposure_words_assessment_total
        )
      ), na.rm = TRUE) < 1 ~ FALSE,
    ),
    .after = imaginal_exposure_words_assessment_total
  ) %>%
  
  # in vivo
  mutate(
    in_vivo_exposure_assessment = case_when(
      is.na(in_vivo_exposure_utterances_assessment_total) &
        is.na(in_vivo_exposure_words_assessment_total) ~ NA,
      rowSums(select(
        .,
        c(
          in_vivo_exposure_utterances_assessment_total,
          in_vivo_exposure_words_assessment_total
        )
      ), na.rm = TRUE) >= 1 ~ TRUE,
      rowSums(select(
        .,
        c(
          in_vivo_exposure_utterances_assessment_total,
          in_vivo_exposure_words_assessment_total
        )
      ), na.rm = TRUE) < 1 ~ FALSE,
    ),
    .after = in_vivo_exposure_words_assessment_total
  ) %>%
  
  # learning from
  mutate(
    learning_from_exposure_assessment = case_when(
      is.na(learning_from_exposure_utterances_assessment_total) &
        is.na(learning_from_exposure_words_assessment_total) ~ NA,
      rowSums(select(
        .,
        c(
          learning_from_exposure_utterances_assessment_total,
          learning_from_exposure_words_assessment_total
        )
      ), na.rm = TRUE) >= 1 ~ TRUE,
      rowSums(select(
        .,
        c(
          learning_from_exposure_utterances_assessment_total,
          learning_from_exposure_words_assessment_total
        )
      ), na.rm = TRUE) < 1 ~ FALSE,
    ),
    .after = learning_from_exposure_words_assessment_total
  )

# Create variables indicating total number of exposure words and utterances used
# in assessment

session_assessment_scores <-
  session_assessment_scores %>%
  
  # exposure utterances total
  mutate(
    exposure_utterances_assessment_total = if_else(
      is.na(learning_from_exposure_utterances_assessment_total) &
        is.na(in_vivo_exposure_utterances_assessment_total) &
        is.na(learning_from_exposure_utterances_assessment_total),
      NA,
      rowSums(select(
        .,
        c(
          learning_from_exposure_utterances_assessment_total,
          in_vivo_exposure_utterances_assessment_total,
          learning_from_exposure_utterances_assessment_total
        )
      ))
    ),
    .after = learning_from_exposure_assessment
  ) %>%
  
  # exposure words total
  mutate(
    exposure_words_assessment_total = if_else(
      is.na(learning_from_exposure_words_assessment_total) &
        is.na(in_vivo_exposure_words_assessment_total) &
        is.na(learning_from_exposure_words_assessment_total),
      NA,
      rowSums(select(
        .,
        c(
          learning_from_exposure_words_assessment_total,
          in_vivo_exposure_words_assessment_total,
          learning_from_exposure_words_assessment_total
        )
      ))
    ),
    .after = exposure_utterances_assessment_total
  )

# Create variable indicating whether exposure was used in assessment

session_assessment_scores <-
  session_assessment_scores %>%
  
  # exposure assessment
  mutate(
    exposure_assessment = case_when(
      is.na(exposure_utterances_assessment_total) &
        is.na(exposure_words_assessment_total) ~ NA,
      rowSums(select(
        .,
        c(
          exposure_utterances_assessment_total,
          exposure_words_assessment_total
        )
      ), na.rm = TRUE) >= 1 ~ TRUE,
      rowSums(select(
        .,
        c(
          exposure_utterances_assessment_total,
          exposure_words_assessment_total
        )
      ), na.rm = TRUE) < 1 ~ FALSE,
    ),
    .after = exposure_words_assessment_total
  )

#### Reorder variables ----------------------------------------------------
session_assessment_scores <-
  session_assessment_scores %>%
  select(
    participant_id,
    starts_with("gad7"),
    starts_with("phq9"),
    starts_with("wsas"),
    contains("exposure"),
    everything()
  )


## Treatment data ---------------------------------------------------------

# Create object containing completed treatment observations
session_data_treatments <-
  session_data %>%
  
  # Filter out sessions that aren't treatment or completed sessions
  filter(
    ieso_appointment_type == "treatment",
    ieso_status == "completed" |
      ieso_status == "completed_late"
  ) %>%
  
  # Sort the dataset by participant and date
  group_by(participant_id) %>%
  arrange(participant_id, ieso_dttm, by_group = TRUE) %>%
  
  # Add session number
  mutate(treatment_number = row_number()) %>%
  ungroup() %>%
  
  # Reorder variables
  select(participant_id,
         treatment_number,
         ieso_dttm,
         everything())


### ieso_treatment_dates ---------------------------------------------

# Create an object containing treatment dates for each participant, to be
# merged with other wide datasets at a later point
session_treatment_dates <-
  session_data_treatments %>%
  
  # For each participant, get the first and last treatment
  group_by(participant_id) %>%
  filter(treatment_number == 1 |
           treatment_number == max(treatment_number)) %>%
  
  # Order the dataset so that for each participant their first and last
  # treatment are listed and remove redundant columns
  arrange(treatment_number, .by_group = TRUE) %>%
  select(participant_id,
         treatment_number,
         ieso_dttm) %>%
  
  # Create a new column that contains eventual variable names beside first and
  # last session
  mutate(
    treatment = case_when(
      treatment_number == 1 ~ "ieso_first_treatment_date",
      treatment_number == max(treatment_number) ~ "ieso_last_treatment_date"
    )
  ) %>%
  
  # Remove treatment number and spread dates so that they appear below first/
  # last variables (i.e. widen dataset)
  select(-treatment_number) %>%
  pivot_wider(names_from = "treatment", values_from = "ieso_dttm") %>%
  
  # Convert dttm to dates
  mutate(
    ieso_first_treatment_date = as_date(ieso_first_treatment_date),
    ieso_last_treatment_date = as_date(ieso_last_treatment_date)
  )


### Treatment scores ------------------------------------------------------

# Select relevant columns and rename variables
session_treatments_scores <-
  session_data_treatments %>%
  
  select(-c(
    ieso_dttm,
    ieso_appointment_type,
    ieso_status,
    ieso_contact_duration
  )) %>%
  
  rename_with(
    # Remove "score" and "ieso" from variable names
    .fn = ~ str_remove_all(., "score|ieso_") %>%
      # Remove trailing underscores
      str_remove_all(., "_$") %>%
      # Add "treatment_total" to the end of scores
      paste0(., "_treatment_total"),
    .cols = contains("score") | contains("exposure")
  )


#### GAD 7 ----------------------------------------------------------------

# Add GAD summary scores
session_treatments_scores <-
  session_treatments_scores %>%
  
  group_by(participant_id, treatment_number) %>%
  mutate(
    # GAD 7 severity: 0-4 minimal, 5-9 mild, 10-14 moderate, 15+ severe
    gad7_treatment_severity = factor(
      case_when(
        gad7_treatment_total < 5 ~ "minimal",
        gad7_treatment_total < 10 ~ "mild",
        gad7_treatment_total < 15 ~ "moderate",
        gad7_treatment_total >= 15 ~ "severe",
        TRUE ~ NA_character_
      ),
      levels = c("minimal", "mild", "moderate", "severe")
    ),
    
    # GAD7 binary anxiety: >=10 - anxiety diagnosis
    gad7_treatment_binary_anxiety =
      case_when(
        gad7_treatment_total >= 10 ~ TRUE,
        gad7_treatment_total < 10 ~ FALSE,
        TRUE ~ NA
      ),
    
    # GAD7 IAPT anxiety: >=8 - IAPT anxiety diagnosis
    gad7_treatment_iapt_anxiety =
      case_when(
        gad7_treatment_total >= 8 ~ TRUE,
        gad7_treatment_total < 8 ~ FALSE,
        TRUE ~ NA
      )
  ) %>%
  ungroup()


#### PHQ 9 ----------------------------------------------------------------

# Add PHQ summary scores
session_treatments_scores <-
  session_treatments_scores %>%
  
  # Group so that scores are calculated per participant and treatment session
  group_by(participant_id, treatment_number) %>%
  mutate(
    # PHQ9 severity:
    # 0-4 minimal, 5-9 mild, 10-14 moderate, 15-19 moderately severe, 20+ severe
    phq9_treatment_severity = factor(
      case_when(
        phq9_treatment_total < 5 ~ "minimal",
        phq9_treatment_total < 10 ~ "mild",
        phq9_treatment_total < 15 ~ "moderate",
        phq9_treatment_total < 20 ~ "moderately_severe",
        phq9_treatment_total >= 20 ~ "severe",
        TRUE ~ NA_character_
      ),
      levels = c("minimal", "mild", "moderate", "moderately_severe", "severe")
    ),
    
    # PHQ9 binary depression: >=10 - depression diagnosis
    phq9_treatment_binary_depression =
      case_when(
        phq9_treatment_total >= 10 ~ TRUE,
        phq9_treatment_total < 10 ~ FALSE,
        TRUE ~ NA
      )
  ) %>%
  ungroup()


#### WSAS -----------------------------------------------------------------

# Add WSAS summary scores
session_treatments_scores <-
  session_treatments_scores %>%
  
  # Group so that scores are calculated per participant and treatment session
  group_by(participant_id, treatment_number) %>%
  mutate(# WSAS impairment: 0-9 low, 10-19 moderate, 20-40 severe
    wsas_treatment_impairment = factor(
      case_when(
        wsas_treatment_total < 10 ~ "low",
        wsas_treatment_total < 20 ~ "moderate",
        wsas_treatment_total >= 20 ~ "severe",
        TRUE ~ NA_character_
      ),
      levels = c("low", "moderate", "severe")
    )) %>%
  ungroup()


#### Exposure -------------------------------------------------------------

# Create variables indicating whether each type of exposure was used in
# each treatment session
session_treatments_scores <-
  session_treatments_scores %>%
  
  # imaginal
  mutate(
    imaginal_exposure_treatment = case_when(
      is.na(imaginal_exposure_utterances_treatment_total) &
        is.na(imaginal_exposure_words_treatment_total) ~ NA,
      rowSums(select(
        .,
        c(
          imaginal_exposure_utterances_treatment_total,
          imaginal_exposure_words_treatment_total
        )
      ), na.rm = TRUE) >= 1 ~ TRUE,
      rowSums(select(
        .,
        c(
          imaginal_exposure_utterances_treatment_total,
          imaginal_exposure_words_treatment_total
        )
      ), na.rm = TRUE) < 1 ~ FALSE,
    ),
    .after = imaginal_exposure_words_treatment_total
  ) %>%
  
  # in vivo
  mutate(
    in_vivo_exposure_treatment = case_when(
      is.na(in_vivo_exposure_utterances_treatment_total) &
        is.na(in_vivo_exposure_words_treatment_total) ~ NA,
      rowSums(select(
        .,
        c(
          in_vivo_exposure_utterances_treatment_total,
          in_vivo_exposure_words_treatment_total
        )
      ), na.rm = TRUE) >= 1 ~ TRUE,
      rowSums(select(
        .,
        c(
          in_vivo_exposure_utterances_treatment_total,
          in_vivo_exposure_words_treatment_total
        )
      ), na.rm = TRUE) < 1 ~ FALSE,
    ),
    .after = in_vivo_exposure_words_treatment_total
  ) %>%
  
  # learning from
  mutate(
    learning_from_exposure_treatment = case_when(
      is.na(learning_from_exposure_utterances_treatment_total) &
        is.na(learning_from_exposure_words_treatment_total) ~ NA,
      rowSums(select(
        .,
        c(
          learning_from_exposure_utterances_treatment_total,
          learning_from_exposure_words_treatment_total
        )
      ), na.rm = TRUE) >= 1 ~ TRUE,
      rowSums(select(
        .,
        c(
          learning_from_exposure_utterances_treatment_total,
          learning_from_exposure_words_treatment_total
        )
      ), na.rm = TRUE) < 1 ~ FALSE,
    ),
    .after = learning_from_exposure_words_treatment_total
  )

# Create variables indicating total number of exposure words and utterances used
# in each treatment session

session_treatments_scores <-
  session_treatments_scores %>%
  
  # exposure utterances total
  mutate(
    exposure_utterances_treatment_total = if_else(
      is.na(learning_from_exposure_utterances_treatment_total) &
        is.na(in_vivo_exposure_utterances_treatment_total) &
        is.na(learning_from_exposure_utterances_treatment_total),
      NA,
      rowSums(select(
        .,
        c(
          learning_from_exposure_utterances_treatment_total,
          in_vivo_exposure_utterances_treatment_total,
          learning_from_exposure_utterances_treatment_total
        )
      ))
    ),
    .after = learning_from_exposure_treatment
  ) %>%
  
  # exposure words total
  mutate(
    exposure_words_treatment_total = if_else(
      is.na(learning_from_exposure_words_treatment_total) &
        is.na(in_vivo_exposure_words_treatment_total) &
        is.na(learning_from_exposure_words_treatment_total),
      NA,
      rowSums(select(
        .,
        c(
          learning_from_exposure_words_treatment_total,
          in_vivo_exposure_words_treatment_total,
          learning_from_exposure_words_treatment_total
        )
      ))
    ),
    .after = exposure_utterances_treatment_total
  )

# Create variable indicating whether exposure was used in each treatment session

session_treatments_scores <-
  session_treatments_scores %>%
  
  # exposure treatment
  mutate(
    exposure_treatment = case_when(
      is.na(exposure_utterances_treatment_total) &
        is.na(exposure_words_treatment_total) ~ NA,
      rowSums(select(
        .,
        c(
          exposure_utterances_treatment_total,
          exposure_words_treatment_total
        )
      ), na.rm = TRUE) >= 1 ~ TRUE,
      rowSums(select(
        .,
        c(
          exposure_utterances_treatment_total,
          exposure_words_treatment_total
        )
      ), na.rm = TRUE) < 1 ~ FALSE,
    ),
    .after = exposure_words_treatment_total
  )

# Create dataframe summarising exposure used across all treatment sessions

# totals
all_sessions_exposure_totals <-
  session_treatments_scores %>%
  select(participant_id,
         treatment_number,
         contains("exposure") & contains("total")) %>%
  group_by(participant_id) %>%
  summarise(across(
    .cols = contains("total"),
    .fns = ~ if_else(all(is.na(.x)), NA, sum(.x, na.rm = TRUE)),
    .names = "{col}_all_sessions"
  ),
  .groups = "keep") %>%
  ungroup()

# included
all_sessions_exposure_included <-
  session_treatments_scores %>%
  select(participant_id,
         treatment_number,
         contains("exposure") & !contains("total")) %>%
  group_by(participant_id) %>%
  summarise(across(
    .cols = contains("exposure"),
    .fns = ~ if_else(all(is.na(.x)), NA, any(.x == TRUE)),
    .names = "{col}_all_sessions"
  ),
  .groups = "keep") %>%
  ungroup()

# proportion
all_sessions_exposure_proportion <-
  session_treatments_scores %>%
  select(participant_id,
         treatment_number,
         contains("exposure") & !contains("total")) %>%
  group_by(participant_id) %>%
  summarise(across(
    .cols = contains("exposure"),
    .fns = ~ if_else(all(is.na(.x)), NA, mean(.x, na.rm = TRUE)),
    .names = "{col}_prop_all_sessions"
  ),
  .groups = "keep") %>%
  ungroup()



### Final treatment scores ------------------------------------------------
final_treatment_scores <-
  session_treatments_scores %>%
  select(-contains("exposure")) %>% 
  group_by(participant_id) %>%
  filter(treatment_number == max(treatment_number)) %>%
  select(-treatment_number) %>%
  rename_with(.fn = ~ paste0(., "_final"),
              .cols = -participant_id)


### Final observed treatment scores ---------------------------------------

# Last observed (non-NA value) score bought forward
final_observed_treatment_scores <-
  
  # Starting with long data
  session_treatments_scores %>%
  
  # For each participant
  group_by(participant_id) %>%
  mutate(across(
    # And across the following measures
    .cols = c(
      phq9_treatment_total,
      gad7_treatment_total,
      wsas_treatment_total,
      ocd_treatment_total,
      social_phobia_inventory_treatment_total,
      hain_treatment_total,
      pcl5_treatment_total,
      gad7_treatment_severity,
      gad7_treatment_binary_anxiety,
      gad7_treatment_iapt_anxiety,
      phq9_treatment_severity,
      phq9_treatment_binary_depression,
      wsas_treatment_impairment
    ),
    
    .fns = ~ if_else(
      condition =
        # If the last treatment session
        treatment_number == max(treatment_number, na.rm = TRUE) &
        # Is missing
        is.na(.x),
      
      # Return the last non-missing value
      true = last(na.omit(.x)),
      
      # Otherwise, return the existing score
      false = .x
    ),
    
    # Create new columns with "_final_observed" appended to the existing column
    # name
    .names = "{col}_final_observed"
  )) %>%
  
  # Only keep rows from a participant's final treatment session and newly
  # created columns
  filter(treatment_number == max(treatment_number, na.rm = TRUE)) %>%
  select(participant_id, ends_with("final_observed")) %>%
  ungroup()


### Reorder variables -----------------------------------------------------

session_treatments_scores <-
  session_treatments_scores %>%
  select(
    participant_id,
    treatment_number,
    starts_with("gad7"),
    starts_with("phq9"),
    starts_with("wsas"),
    everything()
  )

final_treatment_scores <-
  final_treatment_scores %>%
  select(
    participant_id,
    starts_with("gad7"),
    starts_with("phq9"),
    starts_with("wsas"),
    everything()
  )

final_observed_treatment_scores <-
  final_observed_treatment_scores %>%
  select(
    participant_id,
    starts_with("gad7"),
    starts_with("phq9"),
    starts_with("wsas"),
    everything()
  )


### Widen treatment scores ------------------------------------------------

# Widen treatment scores for each participant, to be merged with other wide
# datasets at a later point
session_treatments_scores <-
  session_treatments_scores %>%
  pivot_wider(names_from = treatment_number,
              values_from = -c(participant_id, treatment_number)) %>% 
  # Remove exposure variables summarising totals
  select(-(contains("exposure") & contains("total")))

# Merge wide datasets -----------------------------------------------------

# Create a list of datasets to merge
session_data_list <- list(
  session_counts,
  session_assessment_date,
  session_treatment_dates,
  session_assessment_scores,
  session_treatments_scores,
  final_treatment_scores,
  final_observed_treatment_scores,
  all_sessions_exposure_totals,
  all_sessions_exposure_included,
  all_sessions_exposure_proportion
)

# Merge all elements of the list
session_data <- session_data_list %>%
  reduce(full_join, by = "participant_id")


# Save data ---------------------------------------------------------------

# Save wide dataset
saveRDS(session_data,
        here("data", "interim", "ieso", "step-02", "session-info.Rds"))

# Save long dataset
saveRDS(session_data_long,
        here("data", "processed", "treatment-data-long.Rds"))