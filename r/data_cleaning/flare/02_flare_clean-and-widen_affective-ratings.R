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

# Read in dataset
affective_ratings_raw <- read_rds(paste0(flare_interim_data_loc, "/affective-rating-data", ".Rds")) %>% 
  clean_names() %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  remove_constant(na.rm = T, quiet = F)

# Clean and widen dataset
affective_ratings <- affective_ratings_raw %>% 
  select(-c(module_id, stimulus)) %>% 
  mutate(module_label = str_to_lower(str_replace(module_label, " ", "_")),
         normalised_stimulus = recode(normalised_stimulus, `cs+` = "cs_plus", `cs-` = "cs_minus")) %>% 
  unite("variable", module_label, normalised_stimulus, sep = "_", na.rm = T) %>% 
  pivot_wider(names_from = variable, values_from = rating)

# Generate derived variables
affective_ratings <- affective_ratings %>% 
  mutate(
    affective_rating_baseline_cs_plus = rowMeans(select(., intersect(starts_with('baseline') , ends_with('plus'))), na.rm = T),
    affective_rating_baseline_cs_minus = rowMeans(select(., intersect(starts_with('baseline') , ends_with('minus'))), na.rm = T),
    
    affective_rating_break_cs_plus = rowMeans(select(., intersect(starts_with('break') , ends_with('plus'))), na.rm = T),
    affective_rating_break_cs_minus = rowMeans(select(., intersect(starts_with('break') , ends_with('minus'))), na.rm = T),
    
    affective_rating_post_cs_plus = rowMeans(select(., intersect(starts_with('post') , ends_with('plus'))), na.rm = T),
    affective_rating_post_cs_minus = rowMeans(select(., intersect(starts_with('post') , ends_with('minus'))), na.rm = T)
  ) %>% 
  mutate(across(.cols = everything(.), ~replace(., is.nan(.), NA)))

# Save data
saveRDS(affective_ratings, here("data", "interim", "flare", "step-02", "affective-ratings.Rds"))