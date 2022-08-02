# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning
library(fedmatch) # for cleaning strings

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder flare interim data is stored in
flare_interim_data_loc <- here("data", "interim", "flare", "step-01")

# Read in dataset
post_experiment_questions_raw <- read_rds(paste0(flare_interim_data_loc, "/post-experiment-questions-data", ".Rds")) %>% 
  clean_names() %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  select(-c(module_type, module_id, experiment_id, experiment_code))

# Clean and widen dataset
post_experiment_questions <- post_experiment_questions_raw %>% 
  mutate(
    did_remove_headphones = if_else(!is.na(headphones_removal_point), TRUE, did_remove_headphones),
    headphones_removal_point = clean_strings(headphones_removal_point),
    headphones_removal_point = str_replace_all(headphones_removal_point, "hadn t", "hadnt"),
    across(.cols = c(headphones_removal_point, task_environment), .fns = ~ fct_infreq(as_factor(.)))
  ) %>% 
  rename(
    experiment_unpleasantness_rating = experiment_unpleasant_rating,
    not_alone = was_alone
  ) %>%
  rename_with(.fn = ~paste0("flare_", .x), .cols = !participant_id) # Add 'flare' as a prefix to all columns except the participant_id column

# Save data
saveRDS(post_experiment_questions, here("data", "interim", "flare", "step-02", "post-experiment-questions.Rds"))