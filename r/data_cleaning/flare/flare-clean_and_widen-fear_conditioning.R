# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder flare interim data is stored in
flare_interim_data_loc <- here("data", "interim", "flare", "step-01")

# Read in dataset
fear_conditioning_raw <- read_rds(paste0(flare_interim_data_loc, "/fear_conditioning_data", ".Rds")) %>% 
  clean_names() %>% 
  remove_empty() %>% 
  select(-c(module_type, module_id))

# Clean dataset values
fear_conditioning <- fear_conditioning_raw %>% 
  mutate(normalised_stimulus = recode(normalised_stimulus, `cs+` = "cs_plus", `cs-` = "cs_minus"))



# EXPECTANCY RATINGS

# Save the preferred order of expectancy variables
expectancy_order <- c(
  "exp_acquisition_cs_minus_1",
  "exp_acquisition_cs_minus_2",
  "exp_acquisition_cs_minus_3",
  "exp_acquisition_cs_minus_4",
  "exp_acquisition_cs_minus_5",
  "exp_acquisition_cs_minus_6",
  "exp_acquisition_cs_minus_7",
  "exp_acquisition_cs_minus_8",
  "exp_acquisition_cs_minus_9",
  "exp_acquisition_cs_minus_10",
  "exp_acquisition_cs_minus_11",
  "exp_acquisition_cs_minus_12",

  "exp_acquisition_cs_plus_1",
  "exp_acquisition_cs_plus_2",
  "exp_acquisition_cs_plus_3",
  "exp_acquisition_cs_plus_4",
  "exp_acquisition_cs_plus_5",
  "exp_acquisition_cs_plus_6",
  "exp_acquisition_cs_plus_7",
  "exp_acquisition_cs_plus_8",
  "exp_acquisition_cs_plus_9",
  "exp_acquisition_cs_plus_10",
  "exp_acquisition_cs_plus_11",
  "exp_acquisition_cs_plus_12",
  
  "exp_extinction_cs_minus_1",
  "exp_extinction_cs_minus_2",
  "exp_extinction_cs_minus_3",
  "exp_extinction_cs_minus_4",
  "exp_extinction_cs_minus_5",
  "exp_extinction_cs_minus_6",
  "exp_extinction_cs_minus_7",
  "exp_extinction_cs_minus_8",
  "exp_extinction_cs_minus_9",
  "exp_extinction_cs_minus_10",
  "exp_extinction_cs_minus_11",
  "exp_extinction_cs_minus_12",
  "exp_extinction_cs_minus_13",
  "exp_extinction_cs_minus_14",
  "exp_extinction_cs_minus_15",
  "exp_extinction_cs_minus_16",
  "exp_extinction_cs_minus_17",
  "exp_extinction_cs_minus_18",
  
  "exp_extinction_cs_plus_1",
  "exp_extinction_cs_plus_2",
  "exp_extinction_cs_plus_3",
  "exp_extinction_cs_plus_4",
  "exp_extinction_cs_plus_5",
  "exp_extinction_cs_plus_6",
  "exp_extinction_cs_plus_7",
  "exp_extinction_cs_plus_8",
  "exp_extinction_cs_plus_9",
  "exp_extinction_cs_plus_10",
  "exp_extinction_cs_plus_11",
  "exp_extinction_cs_plus_12",
  "exp_extinction_cs_plus_13",
  "exp_extinction_cs_plus_14",
  "exp_extinction_cs_plus_15",
  "exp_extinction_cs_plus_16",
  "exp_extinction_cs_plus_17",
  "exp_extinction_cs_plus_18"
)

# Create a wide version of the expectancy ratings (one column per rating)
expectancy_ratings_wide <- fear_conditioning %>% 
  select(participant_id, phase, normalised_stimulus, trial_by_stimulus, rating) %>% 
  unite("variable", phase, normalised_stimulus, trial_by_stimulus, sep = "_", na.rm = T) %>% 
  mutate(
    variable = paste0("exp_", variable),
    variable = factor(variable, levels = expectancy_order)
    ) %>% 
  arrange(participant_id, variable) %>% 
  pivot_wider(names_from = variable, values_from = rating)

# Generate derived variables for wide expectancy ratings dataset

# Create dataframe with missed trials info
number_missing_ratings <- expectancy_ratings_wide %>%
  rowwise() %>% 
  mutate(
    # Number of missed ratings per phase
    exp_acquisition_missing = rowSums(is.na(across(contains("acquisition"))), na.rm = T),
    exp_extinction_missing = rowSums(is.na(across(contains("extinction"))), na.rm = T)
    ) %>% 
  ungroup() %>% 
  select(participant_id, exp_acquisition_missing, exp_extinction_missing)

# Generate derived variables using long dataset

# Create dataframe with volume info
volume_info <- fear_conditioning %>% 
  group_by(participant_id, phase) %>% 
  mutate(average_phase_volume = mean(volume_level, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(participant_id, phase, average_phase_volume) %>% 
  distinct() %>% 
  mutate(average_phase_volume = if_else(average_phase_volume > 1, NA_real_, average_phase_volume)) %>% 
  pivot_wider(names_from = phase, values_from = average_phase_volume, names_prefix = "average_volume_") %>% 
  mutate(volume_exclusion_50pct_acquisition = if_else(average_volume_acquisition <= .5, TRUE, FALSE))

headphone_info <- fear_conditioning %>% 
  group_by(participant_id, phase) %>%
  mutate(total_headphone_disconnects = sum(headphones == FALSE, na.rm = T)) %>% 
  ungroup() %>% 
  select(participant_id, phase, total_headphone_disconnects) %>% 
  distinct() %>% 
  pivot_wider(names_from = phase, values_from = total_headphone_disconnects, names_prefix = "total_headphone_disconnects_") %>% 
  mutate(across(.cols = c(total_headphone_disconnects_acquisition, total_headphone_disconnects_extinction), .fns = ~replace_na(., 0))) %>% 
  mutate(total_headphone_disconnects = rowSums(select(., c(total_headphone_disconnects_acquisition, total_headphone_disconnects_extinction)), na.rm = T)) 

app_exits_info <- fear_conditioning %>% 
  group_by(participant_id, phase) %>%
  mutate(
    total_iti_exits = sum(did_leave_iti == FALSE, na.rm = T),
    total_trial_exits = sum(did_leave_trial == FALSE, na.rm = T)
  ) %>% 
  ungroup() %>% 
  select(participant_id, phase, total_iti_exits, total_trial_exits) %>% 
  distinct() %>% 
  pivot_wider(names_from = phase, values_from = total_headphone_disconnects, names_prefix = "total_headphone_disconnects_") %>% 
  mutate(across(.cols = c(total_headphone_disconnects_acquisition, total_headphone_disconnects_extinction), .fns = ~replace_na(., 0))) %>% 
  mutate(total_headphone_disconnects = rowSums(select(., c(total_headphone_disconnects_acquisition, total_headphone_disconnects_extinction)), na.rm = T)) 

fear_conditioning %>% 
  filter(participant_id == "1JECmREpmmRUB2p") %>% 
  view()

# Use long expectancy ratings data to impute missing ratings for participants with < 6 missed trials per phase
# Expectancy ratings will be imputed for participants missing data up to five out of 24 (acquisition) or 36 (extinction) trials per phase, as follows: 
# If a participant misses the first trial of a fear conditioning phase, the sample average of that trial will be imputed. 
# If a participant misses the final trial of a fear conditioning phase, their last rating will be carried forward for the trial. 
# All other fear conditioning trials where the participant misses an expectancy rating will be imputed using the mean of the participant’s last rating made before, and first rating made after, the trial.
# expectancy_ratings_long <- fear_conditioning %>%
#   select(participant_id, phase, normalised_stimulus, trial, trial_by_stimulus, rating) %>%
#   arrange(participant_id, phase, trial) %>% 
#   group_by(participant_id, phase) %>% 
#   mutate(no_missed_ratings = sum(is.na(rating))) %>% 
#   ungroup() %>% 
#   group_by(phase) %>% 
#   mutate(
#     rating = ifelse(trial == 1 & no_missed_ratings > 5, NA, rating),
#     rating = ifelse(trial == 1 & no_missed_ratings < 6 & is.na(rating), mean(rating, na.rm = T), rating)
#   )



# Save data
# saveRDS(fear_conditioning, paste0(here("data", "interim","flare", "step-02"), "/fear_conditioning.Rds"))
