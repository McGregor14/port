# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder flare interim data is stored in
flare_interim_data_loc <- here("data", "interim", "flare", "step-01")

# Read in dataset and remove empty/redundant columns
fear_conditioning_raw <- read_rds(paste0(flare_interim_data_loc, "/fear-conditioning-data", ".Rds")) %>% 
  clean_names() %>% 
  remove_empty(which = c("rows", "cols")) %>% 
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

# Generate derived variables

# Create dataframe with missed trials info
number_missing_ratings <- expectancy_ratings_wide %>%
  mutate(
    # Number of missed ratings per phase
    missing_exp_acquisition = rowSums(is.na(select(., contains("acquisition"))), na.rm = T),
    missing_exp_extinction = rowSums(is.na(select(., contains("extinction"))), na.rm = T)
  ) %>% 
  select(participant_id, missing_exp_acquisition, missing_exp_extinction)


# Create dataframe with response delay info (i.e. the time it took to respond after CS is presented, should be between 3 and 8 seconds)
rating_delay_info <- fear_conditioning %>% 
  mutate(rating_delay_secs = as.numeric(difftime(response_recorded_at, trial_started_at, units = 'secs'))) %>% 
  group_by(participant_id) %>% 
  summarise(
    rating_delay_min_secs = min(rating_delay_secs, na.rm = T),
    rating_delay_max_secs = max(rating_delay_secs, na.rm = T)
    ) %>% 
  mutate(
    rating_delay_min_flag = if_else(rating_delay_min_secs < 3, TRUE, FALSE),
    rating_delay_max_flag = if_else(rating_delay_max_secs > 8, TRUE, FALSE)
    ) %>% 
  ungroup()


# Create dataframe with time between each trial
# Maximum time between trials should be ~11 seconds, the sum of:
# Trial length = 8 seconds
# Max ITI length = 3 seconds
# Processing lag of a few seconds
# Create a flag if max trial delay is greater than the length of two trials
trial_delay_info <- fear_conditioning %>%
  select(participant_id, phase, trial, trial_started_at) %>% 
  group_by(participant_id, phase, trial) %>% 
  arrange() %>% 
  group_by(participant_id, phase) %>% 
  mutate(prev_trial_started_at = lag(trial_started_at),
         trial_delay_secs = as.numeric(difftime(trial_started_at, prev_trial_started_at, units = 'secs'))) %>% 
  group_by(participant_id) %>% 
  summarise(trial_delay_max_secs = max(trial_delay_secs, na.rm = T)) %>% 
  mutate(trial_delay_flag = if_else(trial_delay_max_secs > 22, TRUE, FALSE)) %>% 
  ungroup()

# Create dataframe with length of break
break_length_info <- fear_conditioning %>%
  group_by(participant_id, phase) %>% 
  mutate(last_trial_per_phase = max(trial, na.rm = T),
         first_trial_per_phase = min(trial, na.rm = T)) %>% 
  filter((phase == "acquisition" & trial == last_trial_per_phase) | (phase == "extinction" & trial == first_trial_per_phase)) %>% 
  ungroup() %>% 
  arrange(participant_id, phase) %>% 
  group_by(participant_id) %>% 
  mutate(prev_trial_started_at = lag(trial_started_at),
         break_length_mins = as.numeric(difftime(trial_started_at, prev_trial_started_at, units = 'mins'))) %>% 
  select(participant_id, break_length_mins) %>% 
  ungroup() %>% 
  drop_na()

# Create dataframe with timing information
timing_info <- fear_conditioning %>% 
  group_by(participant_id) %>% 
  summarise(fc_start_time = min(trial_started_at, na.rm = T),
            fc_end_time = max(trial_started_at, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(fc_duration_mins = difftime(fc_end_time, fc_start_time, units = 'mins'))

# Create dataframe with volume info
volume_info <- fear_conditioning %>% 
  group_by(participant_id, phase) %>% 
  mutate(average_phase_volume = mean(volume_level, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(participant_id, phase, average_phase_volume) %>% 
  distinct() %>% 
  mutate(average_phase_volume = if_else(average_phase_volume > 1, NA_real_, average_phase_volume)) %>% 
  pivot_wider(names_from = phase, values_from = average_phase_volume, names_prefix = "volume_average_") %>% 
  mutate(volume_exclusion_50pct_acquisition = if_else(volume_average_acquisition <= .5, TRUE, FALSE))

# Create dataframe with info about headphone removals
headphone_info <- fear_conditioning %>% 
  group_by(participant_id, phase) %>%
  mutate(headphone_disconnects = sum(headphones == FALSE, na.rm = T)) %>% 
  ungroup() %>% 
  select(participant_id, phase, headphone_disconnects) %>% 
  distinct() %>% 
  pivot_wider(names_from = phase, values_from = headphone_disconnects, names_prefix = "headphone_disconnects_") %>% 
  mutate(across(.cols = c(headphone_disconnects_acquisition, headphone_disconnects_extinction), .fns = ~replace_na(., 0))) %>% 
  mutate(headphone_disconnects_total = rowSums(select(., c(headphone_disconnects_acquisition, headphone_disconnects_extinction)), na.rm = T)) 

# Create dataframe with info about number of exits
app_exits_info <- fear_conditioning %>% 
  group_by(participant_id, phase) %>%
  mutate(
    iti_exits = sum(did_leave_iti == TRUE, na.rm = T),
    trial_exits = sum(did_leave_trial == TRUE, na.rm = T)
  ) %>% 
  ungroup() %>% 
  select(participant_id, phase, iti_exits, trial_exits) %>% 
  distinct() %>% 
  pivot_wider(names_from = phase, values_from = c(iti_exits, trial_exits)) %>% 
  mutate(across(.cols = -participant_id, .fns = ~replace_na(., 0))) %>% 
  mutate(iti_exits_total = rowSums(select(., c(iti_exits_acquisition, iti_exits_extinction)), na.rm = T),
         trial_exits_total = rowSums(select(., c(trial_exits_acquisition, trial_exits_extinction)), na.rm = T)) %>% 
  mutate(exits_total = rowSums(select(., c(iti_exits_total, trial_exits_total)), na.rm = T))

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

# Merge dataframes together
fear_conditioning <- list(expectancy_ratings_wide,
                          number_missing_ratings,
                          rating_delay_info,
                          trial_delay_info,
                          break_length_info,
                          timing_info,
                          volume_info,
                          headphone_info,
                          app_exits_info) %>% 
  reduce(full_join, by = "participant_id")

# Save data
saveRDS(fear_conditioning, here("data", "interim", "flare", "step-02", "fear-conditioning.Rds"))