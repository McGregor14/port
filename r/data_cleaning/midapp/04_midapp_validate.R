# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning
library(validate) # validating new variables
library(visdat) # visualise missing data

# Read in dataset
mid_app_data <- read_rds(here("data", "interim", "midapp", "step-01", "mid-app-data.Rds")) %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  remove_constant(na.rm = T, quiet = F)


# Assess missingness (pt 1)
vis_miss(mid_app_data)

mid_app_missing_count_1 <- mid_app_data %>% 
  summarise(across(.cols = everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "n_missing")

mid_app_missing_count_1

mid_app_data %>% 
  filter(if_any(everything(), is.na)) %>% 
  pull(participant_id)


# Validate new scales
# Check all values are within correct ranges
rules <- validator(eacs_range = in_range(eacs_midapp_total, min=14, max=56),
                   ius12_range = in_range(ius12_midapp_total, min=12, max=60),
                   ius12_midapp_prospective_anxiety_range = in_range(ius12_midapp_prospective_anxiety, min=7, max=35),
                   ius12_midapp_inhibitory_anxiety_range = in_range(ius12_midapp_inhibitory_anxiety, min=5, max=25),
                   bis_midapp_total_range = in_range(bis_midapp_total, min=7, max=28),
                   bas_midapp_drive_range = in_range(bas_midapp_drive, min=4, max=16),
                   bas_midapp_fun_seeking_range = in_range(bas_midapp_fun_seeking, min=4, max=16),
                   bas_midapp_reward_responsiveness_range = in_range(bas_midapp_reward_responsiveness, min=5, max=20),
                   epq_r_midapp_nscale_range = in_range(epq_r_midapp_nscale, min=0, max=12))

validation <- confront(mid_app_data, rules)

summary(validation)

plot(validation)

# Assess missingness (pt 2)
vis_miss(mid_app_data)

mid_app_missing_count_2 <- mid_app_data %>% 
  summarise(across(.cols = everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "n_missing")

mid_app_missing_count_2

mid_app_data %>% 
  filter(if_any(everything(), is.na)) %>% 
  pull(participant_id)

# Save data
saveRDS(mid_app_data, here("data", "interim", "midapp", "step-02", "mid-app-data.Rds"))