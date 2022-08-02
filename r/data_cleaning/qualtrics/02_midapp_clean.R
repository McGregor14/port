# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning
library(validate) # validating new variables
library(visdat) # visualise missing data

# Read in dataset
mid_app_data <- read_rds(here("data", "interim", "qualtrics", "step-01", "mid-app-data.Rds")) %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  remove_constant(na.rm = T, quiet = F)

# Rename all columns except participant_id
mid_app_names <- names(mid_app_data[1:62]) %>% 
  substr(.,1,nchar(.)-2) %>% # remove the '_3'
  str_replace(., "_", "_midapp_") %>%  # add '_midapp_' to the middle of the name
  str_replace_all(., "-", "_")

mid_app_names <- c(mid_app_names, "participant_id") # Add the id column name back to the vector

names(mid_app_data) <- mid_app_names # Reassign the names to the dataset

mid_app_data <- mid_app_data %>% 
  select(participant_id, everything())

# Adjust column types
mid_app_data <- mid_app_data %>% 
  mutate(across(.cols = -participant_id, ~as.integer(.))) # All columns except participant ID

# Assess missingness (pt 1)
vis_miss(mid_app_data)

mid_app_missing_count_1 <- mid_app_data %>% 
  summarise(across(.cols = everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "n_missing")

mid_app_missing_count_1

mid_app_data %>% 
  filter(if_any(everything(), is.na)) %>% 
  pull(participant_id)

# Reverse score items
# Method: take the maximum value for the scale, add 1, and subtract the participant's score.

# eACS:
# To use the eACS as a one factor measure, first reverse the following items: 1, 2, 3, 4, 5, 7, 8, 13, 14

mid_app_data <- mid_app_data %>% 
  mutate(across(.cols = c(eacs_midapp_1,
                          eacs_midapp_2,
                          eacs_midapp_3,
                          eacs_midapp_4,
                          eacs_midapp_5,
                          eacs_midapp_7,
                          eacs_midapp_8,
                          eacs_midapp_13,
                          eacs_midapp_14), 
                ~ 4 + 1 - .))

# BIS/BAS:
# Items 1, 3-21, 23-24 are reverse-scored.
# Items 2 and 22 are scored normally.
# Items 1, 6, 11, 17 are filler items and do not correspond to any subscale.

mid_app_data <- mid_app_data %>% 
  mutate(across(.cols = c(bis_bas_midapp_1,
                          bis_bas_midapp_3,
                          bis_bas_midapp_4,
                          bis_bas_midapp_5,
                          bis_bas_midapp_6,
                          bis_bas_midapp_7,
                          bis_bas_midapp_8,
                          bis_bas_midapp_9,
                          bis_bas_midapp_10,
                          bis_bas_midapp_11,
                          bis_bas_midapp_12,
                          bis_bas_midapp_13,
                          bis_bas_midapp_14,
                          bis_bas_midapp_15,
                          bis_bas_midapp_16,
                          bis_bas_midapp_17,
                          bis_bas_midapp_18,
                          bis_bas_midapp_19,
                          bis_bas_midapp_20,
                          bis_bas_midapp_21,
                          bis_bas_midapp_23,
                          bis_bas_midapp_24), 
                ~ 4 + 1 - .))

# TODO: epq?

# Generate derived variables
mid_app_data <- mid_app_data %>% 
  mutate(
    
    # eACS: Sum of all items
    eacs_midapp_total = rowSums(select(., contains("eacs"))),
    
    # IUS: Sum of all items
    ius12_midapp_total = rowSums(select(., contains("ius12"))),
    
    # IUS Prospective Anxiety subscale: 1, 2, 4, 5, 8, 9, 11
    ius12_midapp_prospective_anxiety = rowSums(select(., c(ius12_midapp_1, 
                                                                 ius12_midapp_2,
                                                                 ius12_midapp_4,
                                                                 ius12_midapp_5,
                                                                 ius12_midapp_8,
                                                                 ius12_midapp_9,
                                                                 ius12_midapp_11))),
    
    # IUS Inhibitory Anxiety subscale: 3, 6, 7, 10, 12
    ius12_midapp_inhibitory_anxiety = rowSums(select(., c(ius12_midapp_3, 
                                                                ius12_midapp_6,
                                                                ius12_midapp_7,
                                                                ius12_midapp_10,
                                                                ius12_midapp_12))),
    
    # BIS: 2, 8, 13, 16, 19, 22, 24
    bis_midapp_total = rowSums(select(., c(bis_bas_midapp_2, 
                                           bis_bas_midapp_8,
                                           bis_bas_midapp_13,
                                           bis_bas_midapp_16,
                                           bis_bas_midapp_19,
                                           bis_bas_midapp_22,
                                           bis_bas_midapp_24))),
    
    # BAS Drive: 3, 9, 12, 21
    bas_midapp_drive = rowSums(select(., c(bis_bas_midapp_3, 
                                                 bis_bas_midapp_9,
                                                 bis_bas_midapp_12,
                                                 bis_bas_midapp_21))),
    
    # BAS Fun Seeking: 5, 10, 15, 20 
    bas_midapp_fun_seeking = rowSums(select(., c(bis_bas_midapp_5, 
                                                       bis_bas_midapp_10,
                                                       bis_bas_midapp_15,
                                                       bis_bas_midapp_20))),
    
    # BAS Reward Responsiveness: 4, 7, 14, 18, 23
    bas_midapp_reward_responsiveness = rowSums(select(., c(bis_bas_midapp_4, 
                                                                 bis_bas_midapp_7,
                                                                 bis_bas_midapp_14,
                                                                 bis_bas_midapp_18,
                                                                 bis_bas_midapp_23))),
    
    # TODO: EPQ-R-N Scale: ?
    epq_r_midapp_nscale = rowSums(select(., contains("epq_r_nscale")))
    
  )

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
saveRDS(mid_app_data, here("data", "interim", "qualtrics", "step-02", "mid-app-data.Rds"))