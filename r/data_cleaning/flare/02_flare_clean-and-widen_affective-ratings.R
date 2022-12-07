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

# Read in dataset
affective_ratings_raw <-
  read_rds(paste0(flare_interim_data_loc, "/affective-rating-data", ".Rds")) %>%
  # clean_names: makes all names unique, all lower case & only consist of _,
  # numbers, and letters
  clean_names() %>%
  # remove_empty: removes empty rows and columns
  remove_empty(which = c("rows", "cols")) %>%
  # remove_constant: removes constant columns
  remove_constant(na.rm = T, quiet = F)

# Clean and widen dataset
affective_ratings <- affective_ratings_raw %>%
  select(-c(module_id, stimulus)) %>%
  mutate(
    module_label = str_to_lower(str_replace(module_label, " ", "_")),
    normalised_stimulus = recode(normalised_stimulus, `cs+` = "cs_plus", `cs-` = "cs_minus")
  ) %>%
  unite("variable",
        module_label,
        normalised_stimulus,
        sep = "_",
        na.rm = T) %>%
  pivot_wider(names_from = variable, values_from = rating) %>%
  rename_with(.fn = ~ paste0("aff_", .x),
              .cols = -participant_id)

# Generate derived variables
affective_ratings <- affective_ratings %>%
  mutate(
    aff_baseline_cs_plus_mean = rowMeans(select(., intersect(
      starts_with('aff_baseline') , ends_with('plus')
    )), na.rm = T),
    aff_baseline_cs_minus_mean = rowMeans(select(., intersect(
      starts_with('aff_baseline') , ends_with('minus')
    )), na.rm = T),
    
    aff_break_cs_plus_mean = rowMeans(select(., intersect(
      starts_with('aff_break') , ends_with('plus')
    )), na.rm = T),
    aff_break_cs_minus_mean = rowMeans(select(., intersect(
      starts_with('aff_break') , ends_with('minus')
    )), na.rm = T),
    
    aff_post_cs_plus_mean = rowMeans(select(., intersect(
      starts_with('aff_post') , ends_with('plus')
    )), na.rm = T),
    aff_post_cs_minus_mean = rowMeans(select(., intersect(
      starts_with('aff_post') , ends_with('minus')
    )), na.rm = T)
  ) %>%
  mutate(across(.cols = -participant_id, ~ replace(., is.nan(.), NA)))

affective_ratings <- affective_ratings %>%
  mutate(
    aff_baseline_differential =
      aff_baseline_cs_plus_mean - aff_baseline_cs_minus_mean,
    aff_break_differential =
      aff_break_cs_plus_mean - aff_break_cs_minus_mean,
    aff_post_differential =
      aff_post_cs_plus_mean - aff_post_cs_minus_mean
  )

# Save data
saveRDS(
  affective_ratings,
  here("data", "interim", "flare", "step-02", "affective-ratings.Rds")
)