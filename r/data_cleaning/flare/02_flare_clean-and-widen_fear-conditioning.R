# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder that interim data is stored in
flare_interim_data_loc <-
  here("data", "interim", "flare", "step-01")

# Read in dataset and remove empty/redundant columns
fear_conditioning_raw <-
  read_rds(paste0(flare_interim_data_loc, "/fear-conditioning-data", ".Rds")) %>%
  # clean_names: makes all names unique, all lower case & only consist of _,
  # numbers, and letters
  clean_names() %>%
  # remove_empty: removes empty rows and columns
  remove_empty(which = c("rows", "cols")) %>%
  # remove_constant: removes constant columns
  select(-c(module_type, module_id))

# Clean dataset values
fear_conditioning <- fear_conditioning_raw %>%
  mutate(normalised_stimulus =
           recode(normalised_stimulus, `cs+` = "cs_plus", `cs-` = "cs_minus"))


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
  select(participant_id,
         phase,
         normalised_stimulus,
         trial_by_stimulus,
         rating) %>%
  unite(
    "variable",
    phase,
    normalised_stimulus,
    trial_by_stimulus,
    sep = "_",
    na.rm = T
  ) %>%
  mutate(
    variable = paste0("exp_", variable),
    variable = factor(variable, levels = expectancy_order)
  ) %>%
  arrange(participant_id, variable) %>%
  pivot_wider(names_from = variable, values_from = rating)

# Generate derived variables
expectancy_means_differentials <-
  expectancy_ratings_wide %>%
  mutate(
    # Means
    exp_acquisition_cs_plus_mean =
      rowMeans(select(., contains(
        c("acquisition", "plus")
      ))),
    exp_acquisition_cs_minus_mean =
      rowMeans(select(., contains(
        c("acquisition", "minus")
      ))),
    exp_extinction_cs_plus_mean =
      rowMeans(select(., contains(
        c("extinction", "plus")
      ))),
    exp_extinction_cs_minus_mean =
      rowMeans(select(., contains(
        c("extinction", "minus")
      )))
  ) %>%
  
  mutate(
    # Difference in means
    exp_acquisition_differential_mean =
      exp_acquisition_cs_plus_mean - exp_acquisition_cs_minus_mean,
    exp_extinction_differential_mean =
      exp_extinction_cs_plus_mean - exp_extinction_cs_minus_mean
  ) %>%
  select(
    participant_id,
    exp_acquisition_cs_plus_mean,
    exp_acquisition_cs_minus_mean,
    exp_acquisition_differential_mean,
    exp_extinction_cs_plus_mean,
    exp_extinction_cs_minus_mean,
    exp_extinction_differential_mean
  )

# Create dataframe with missed trials info
number_missing_ratings <- expectancy_ratings_wide %>%
  mutate(
    # Number of missed ratings per phase
    flare_missing_exp_acquisition = rowSums(is.na(select(
      ., contains("acquisition")
    )), na.rm = T),
    flare_missing_exp_extinction = rowSums(is.na(select(
      ., contains("extinction")
    )), na.rm = T)
  ) %>%
  select(participant_id,
         flare_missing_exp_acquisition,
         flare_missing_exp_extinction)


# Create dataframe with response delay info (i.e. the time it took to respond
# after CS is presented, should be between 3 and 8 seconds)
rating_delay_info <- fear_conditioning %>%
  mutate(rating_delay_secs = as.numeric(
    difftime(response_recorded_at, trial_started_at, units = 'secs')
  )) %>%
  group_by(participant_id) %>%
  summarise(
    flare_rating_delay_min_secs = min(rating_delay_secs, na.rm = T),
    flare_rating_delay_max_secs = max(rating_delay_secs, na.rm = T)
  ) %>%
  mutate(
    flare_rating_delay_min_flag = if_else(flare_rating_delay_min_secs < 3,
                                          TRUE,
                                          FALSE),
    flare_rating_delay_max_flag = if_else(flare_rating_delay_max_secs > 8,
                                          TRUE,
                                          FALSE)
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
  mutate(
    prev_trial_started_at = lag(trial_started_at),
    trial_delay_secs = as.numeric(
      difftime(trial_started_at, prev_trial_started_at, units = 'secs')
    )
  ) %>%
  group_by(participant_id) %>%
  summarise(flare_trial_delay_max_secs = max(trial_delay_secs, na.rm = T)) %>%
  mutate(flare_trial_delay_flag = if_else(flare_trial_delay_max_secs > 22,
                                          TRUE,
                                          FALSE)) %>%
  ungroup()

# Create dataframe with length of break
break_length_info <- fear_conditioning %>%
  group_by(participant_id, phase) %>%
  mutate(
    last_trial_per_phase = max(trial, na.rm = T),
    first_trial_per_phase = min(trial, na.rm = T)
  ) %>%
  filter((phase == "acquisition" &
            trial == last_trial_per_phase) |
           (phase == "extinction" & trial == first_trial_per_phase)
  ) %>%
  ungroup() %>%
  arrange(participant_id, phase) %>%
  group_by(participant_id) %>%
  mutate(
    prev_trial_started_at = lag(trial_started_at),
    flare_break_length_mins = as.numeric(
      difftime(trial_started_at, prev_trial_started_at, units = 'mins')
    )
  ) %>%
  select(participant_id, flare_break_length_mins) %>%
  ungroup() %>%
  drop_na()

# Create dataframe with timing information
timing_info <- fear_conditioning %>%
  group_by(participant_id) %>%
  summarise(
    flare_fc_start_dttm = min(trial_started_at, na.rm = T),
    flare_fc_end_dttm = max(trial_started_at, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(flare_fc_duration_mins =
           as.numeric(
             difftime(flare_fc_end_dttm, flare_fc_start_dttm, units = 'mins')
           ))

# Create dataframe with volume info
volume_info <- fear_conditioning %>%
  group_by(participant_id, phase) %>%
  mutate(average_phase_volume = mean(volume_level, na.rm = TRUE)) %>%
  ungroup() %>%
  select(participant_id, phase, average_phase_volume) %>%
  distinct() %>%
  mutate(average_phase_volume = if_else(average_phase_volume > 1,
                                        NA_real_,
                                        average_phase_volume)) %>%
  pivot_wider(names_from = phase,
              values_from = average_phase_volume,
              names_prefix = "flare_volume_average_") %>%
  mutate(
    flare_volume_exclusion_50pct_acquisition =
      if_else(flare_volume_average_acquisition <= .5,
              TRUE,
              FALSE)
  )

# Create dataframe with info about headphone removals
headphone_info <- fear_conditioning %>%
  group_by(participant_id, phase) %>%
  mutate(headphone_disconnects = sum(headphones == FALSE, na.rm = T)) %>%
  ungroup() %>%
  select(participant_id, phase, headphone_disconnects) %>%
  distinct() %>%
  pivot_wider(names_from = phase,
              values_from = headphone_disconnects,
              names_prefix = "flare_headphone_disconnects_") %>%
  mutate(across(
    .cols = c(
      flare_headphone_disconnects_acquisition,
      flare_headphone_disconnects_extinction
    ),
    .fns = ~ replace_na(., 0)
  )) %>%
  mutate(flare_headphone_disconnects_total = rowSums(select(
    .,
    c(
      flare_headphone_disconnects_acquisition,
      flare_headphone_disconnects_extinction
    )
  ), na.rm = T))

# Create dataframe with info about number of exits
app_exits_info <- fear_conditioning %>%
  group_by(participant_id, phase) %>%
  mutate(
    flare_iti_exits = sum(did_leave_iti == TRUE, na.rm = T),
    flare_trial_exits = sum(did_leave_trial == TRUE, na.rm = T)
  ) %>%
  ungroup() %>%
  select(participant_id, phase, flare_iti_exits, flare_trial_exits) %>%
  distinct() %>%
  pivot_wider(
    names_from = phase,
    values_from = c(flare_iti_exits, flare_trial_exits)
  ) %>%
  mutate(across(
    .cols = -participant_id,
    .fns = ~ replace_na(., 0)
  )) %>%
  mutate(
    flare_iti_exits_total = rowSums(select(
      .,
      c(flare_iti_exits_acquisition, flare_iti_exits_extinction)
    ), na.rm = T),
    flare_trial_exits_total = rowSums(select(
      .,
      c(flare_trial_exits_acquisition, flare_trial_exits_extinction)
    ), na.rm = T)
  ) %>%
  mutate(flare_exits_total = rowSums(select(
    ., c(flare_iti_exits_total, flare_trial_exits_total)
  ), na.rm = T))

# Merge dataframes together
fear_conditioning <- list(
  expectancy_ratings_wide,
  expectancy_means_differentials,
  number_missing_ratings,
  rating_delay_info,
  trial_delay_info,
  break_length_info,
  timing_info,
  volume_info,
  headphone_info,
  app_exits_info
) %>%
  reduce(full_join, by = "participant_id")

# Save data
saveRDS(
  fear_conditioning,
  here("data", "interim", "flare", "step-02", "fear-conditioning.Rds")
)