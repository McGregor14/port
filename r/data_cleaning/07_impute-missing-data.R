# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Read in dataset
port_data <-
  read_rds(here("data", "processed", "merged", "06_clean-processed-data.Rds"))


# Imputation --------------------------------------------------------------


## Expectancy ratings -----------------------------------------------------

# Set the order of expectancy ratings to be used later
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

# Filter out FLARe exclusions
flare_participants <-
  port_data %>%
  filter(port_exclusion == FALSE)

# Create long version of the FLARe trial data
expectancy_long <-
  flare_participants %>%
  # Selects expectancy rating columns (i.e. those starting with "exp_" and
  # ending with digits and aren't differential variables)
  select(-contains("differential")) %>%
  select(participant_id, matches("^exp_.*\\d$")) %>%
  # Make data long with columns for phase, stimulus, and rating
  pivot_longer(
    -participant_id,
    names_to = c("phase", "normalised_stimulus", "trial_by_stimulus"),
    names_pattern = "exp_([A-Za-z]+)_(cs_[A-Za-z]+)_(\\d+$)",
    values_to = "rating"
  ) %>%
  # Make trial and rating numeric (remain as factors from previous variable)
  mutate(across(
    .cols = c(trial_by_stimulus, rating),
    .fns = ~ as.numeric(.x)
  )) %>%
  # Arrange data in order
  arrange(participant_id, phase, normalised_stimulus, trial_by_stimulus)

# 1. If the first trial is missed, the average for the first trial for the same
# stimulus type (CS+ or CS-) for the whole sample will be used as the missing
# value

expectancy_imputed_long <-
  expectancy_long %>%
  group_by(phase, normalised_stimulus, trial_by_stimulus) %>%
  mutate(trial_mean = mean(rating, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(rating =
           if_else(
             condition = trial_by_stimulus == 1 & is.na(rating),
             true = trial_mean,
             false = rating
           )) %>%
  select(-trial_mean)

# 2. If the last trial is missed, the preceding value for the same stimulus type
# will be carried forward to replace the missing value

expectancy_imputed_long <-
  expectancy_imputed_long %>%
  group_by(participant_id, phase, normalised_stimulus) %>%
  mutate(rating =
           if_else(
             condition =
               trial_by_stimulus == max(trial_by_stimulus, na.rm = TRUE) &
               is.na(rating),
             true = last(na.omit(rating)),
             false = rating
           )) %>%
  ungroup()

# 3. If any other trial is missed, the average of the preceding and following
# value will be used for that stimulus
expectancy_imputed_long <-
  expectancy_imputed_long %>%
  group_by(participant_id, phase, normalised_stimulus) %>%
  mutate(last_rating = rating,
         next_rating = rating) %>%
  fill(last_rating, .direction = "down") %>%
  fill(next_rating, .direction = "up") %>%
  rowwise() %>%
  mutate(avg_rating_either_side = mean(c(last_rating, next_rating))) %>%
  ungroup() %>%
  mutate(rating = if_else(
    condition = is.na(rating),
    true = avg_rating_either_side,
    false = rating
  )) %>%
  select(-c(last_rating, next_rating, avg_rating_either_side))


# Create a wide version of the expectancy ratings (one column per rating)
expectancy_imputed_wide <- expectancy_imputed_long %>%
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
expectancy_imputed_means_differentials <-
  expectancy_imputed_wide %>%
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
    # Differentials
    exp_acquisition_differential_1 = exp_acquisition_cs_plus_1 - exp_acquisition_cs_minus_1,
    exp_acquisition_differential_2 = exp_acquisition_cs_plus_2 - exp_acquisition_cs_minus_2,
    exp_acquisition_differential_3 = exp_acquisition_cs_plus_3 - exp_acquisition_cs_minus_3,
    exp_acquisition_differential_4 = exp_acquisition_cs_plus_4 - exp_acquisition_cs_minus_4,
    exp_acquisition_differential_5 = exp_acquisition_cs_plus_5 - exp_acquisition_cs_minus_5,
    exp_acquisition_differential_6 = exp_acquisition_cs_plus_6 - exp_acquisition_cs_minus_6,
    exp_acquisition_differential_7 = exp_acquisition_cs_plus_7 - exp_acquisition_cs_minus_7,
    exp_acquisition_differential_8 = exp_acquisition_cs_plus_8 - exp_acquisition_cs_minus_8,
    exp_acquisition_differential_9 = exp_acquisition_cs_plus_9 - exp_acquisition_cs_minus_9,
    exp_acquisition_differential_10 = exp_acquisition_cs_plus_10 - exp_acquisition_cs_minus_10,
    exp_acquisition_differential_11 = exp_acquisition_cs_plus_11 - exp_acquisition_cs_minus_11,
    exp_acquisition_differential_12 = exp_acquisition_cs_plus_12 - exp_acquisition_cs_minus_12,
    
    exp_extinction_differential_1 = exp_extinction_cs_plus_1 - exp_extinction_cs_minus_1,
    exp_extinction_differential_2 = exp_extinction_cs_plus_2 - exp_extinction_cs_minus_2,
    exp_extinction_differential_3 = exp_extinction_cs_plus_3 - exp_extinction_cs_minus_3,
    exp_extinction_differential_4 = exp_extinction_cs_plus_4 - exp_extinction_cs_minus_4,
    exp_extinction_differential_5 = exp_extinction_cs_plus_5 - exp_extinction_cs_minus_5,
    exp_extinction_differential_6 = exp_extinction_cs_plus_6 - exp_extinction_cs_minus_6,
    exp_extinction_differential_7 = exp_extinction_cs_plus_7 - exp_extinction_cs_minus_7,
    exp_extinction_differential_8 = exp_extinction_cs_plus_8 - exp_extinction_cs_minus_8,
    exp_extinction_differential_9 = exp_extinction_cs_plus_9 - exp_extinction_cs_minus_9,
    exp_extinction_differential_10 = exp_extinction_cs_plus_10 - exp_extinction_cs_minus_10,
    exp_extinction_differential_11 = exp_extinction_cs_plus_11 - exp_extinction_cs_minus_11,
    exp_extinction_differential_12 = exp_extinction_cs_plus_12 - exp_extinction_cs_minus_12,
    exp_extinction_differential_13 = exp_extinction_cs_plus_13 - exp_extinction_cs_minus_13,
    exp_extinction_differential_14 = exp_extinction_cs_plus_14 - exp_extinction_cs_minus_14,
    exp_extinction_differential_15 = exp_extinction_cs_plus_15 - exp_extinction_cs_minus_15,
    exp_extinction_differential_16 = exp_extinction_cs_plus_16 - exp_extinction_cs_minus_16,
    exp_extinction_differential_17 = exp_extinction_cs_plus_17 - exp_extinction_cs_minus_17,
    exp_extinction_differential_18 = exp_extinction_cs_plus_18 - exp_extinction_cs_minus_18
  ) %>%
  mutate(
    # Differential means
    exp_acquisition_differential_mean = rowMeans(select(
      .,
      c(
        exp_acquisition_differential_1,
        exp_acquisition_differential_2,
        exp_acquisition_differential_3,
        exp_acquisition_differential_4,
        exp_acquisition_differential_5,
        exp_acquisition_differential_6,
        exp_acquisition_differential_7,
        exp_acquisition_differential_8,
        exp_acquisition_differential_9,
        exp_acquisition_differential_10,
        exp_acquisition_differential_11,
        exp_acquisition_differential_12
      )
    )),
    exp_extinction_differential_mean = rowMeans(select(
      .,
      c(
        exp_extinction_differential_1,
        exp_extinction_differential_2,
        exp_extinction_differential_3,
        exp_extinction_differential_4,
        exp_extinction_differential_5,
        exp_extinction_differential_6,
        exp_extinction_differential_7,
        exp_extinction_differential_8,
        exp_extinction_differential_9,
        exp_extinction_differential_10,
        exp_extinction_differential_11,
        exp_extinction_differential_12,
        exp_extinction_differential_13,
        exp_extinction_differential_14,
        exp_extinction_differential_15,
        exp_extinction_differential_16,
        exp_extinction_differential_17,
        exp_extinction_differential_18
      )
    ))
  ) %>%
  
  mutate(
    # Difference in means
    exp_acquisition_difference_in_means =
      exp_acquisition_cs_plus_mean - exp_acquisition_cs_minus_mean,
    exp_extinction_difference_in_means =
      exp_extinction_cs_plus_mean - exp_extinction_cs_minus_mean
  )

# Round all values to 2 decimal places
expectancy_imputed_means_differentials <- 
  expectancy_imputed_means_differentials %>% 
  mutate(across(.cols = where(is.numeric),
                .fns = ~round(.x, digits = 2)))


# Fix port_data using imputed dataset
port_data_imputed <- port_data %>%
  rows_patch(expectancy_imputed_means_differentials, by = "participant_id")

janitor::compare_df_cols(port_data_imputed, port_data, return = "mismatch")

summary(arsenal::comparedf(port_data_imputed, port_data))


# Non-FC imputation -------------------------------------------------------

# processed_data %>%
#   rowwise() %>%
#   mutate(
#     na_count = sum(is.na(c_across(
#       matches("asi_baseline_\\d+$")
#     )),
#     na.rm = T),
#     mean = if_else(
#       is.na(asi_baseline_total) & na_count < 2,
#       mean(c_across(matches(
#         "asi_baseline_\\d+$"
#       )), na.rm = T),
#       NA_real_
#     )
#   ) %>%
#   ungroup() %>%
#   select(starts_with("asi_baseline"), na_count) %>%
#   filter(na_count > 2) %>%
#   view()

# Save data
saveRDS(
  port_data_imputed,
  here(
    "data",
    "processed",
    "merged",
    "07_imputed-processed-data.Rds"
  )
)