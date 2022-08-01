# Clear the environment
rm(list = ls())

# Load packages
library(tidyverse) # data manipulation
library(here) # file referencing
library(janitor) # data cleaning

# Source function script
source(file = here("r", "functions.R"))

# Specify the subfolder flare interim data is stored in
redcap_interim_data_loc <- here("data", "interim", "redcap", "step-01")

# Read in dataset
fss_raw <- read_rds(paste0(redcap_interim_data_loc, "/redcap-fss-data", ".Rds")) %>% 
  clean_names() %>% 
  remove_empty(which = c("rows", "cols")) %>% 
  remove_constant(na.rm = T, quiet = F)

# Rename all columns except participant_id

# Replace numeric indicator of collection wave with descriptor
fss_names <- names(fss_raw[2:length(fss_raw)]) %>%
  str_replace(., "2$", "baseline") %>% 
  str_replace(., "4$", "followup")

fss_names <- c("participant_id", fss_names) # Add the id column name back to the vector

names(fss_raw) <- fss_names # Reassign the names to the dataset

# Swap name elements so that collection wave comes before item number
fss_raw <- fss_raw %>% 
  rename_with(.fn = ~gsub('fss_(\\d+)_(.*)', 'fss_\\2_\\1', .x), .cols = starts_with('fss'))

# Generate derived variables

# The FSS yields 5 sub scales and one total score
# Scale scores are calculated as the sum of respective items
# No items are reverse scored
fss_data <- fss_raw %>% 
  mutate(
    
    # FSS: Sum of all items
    fss_baseline_total = rowSums(select(., starts_with("fss") & contains("baseline"))),
    fss_followup_total = rowSums(select(., starts_with("fss") & contains("followup"))),
    
    # Social Fears (SOC) subscale: 5, 8, 9, 10, 15, 20, 27, 33, 41, 44, 47, 51, 52
    fss_baseline_soc = rowSums(select(., c(fss_baseline_5,
                                           fss_followup_8,
                                           fss_followup_9,
                                           fss_followup_10,
                                           fss_followup_15,
                                           fss_followup_20,
                                           fss_followup_27,
                                           fss_followup_33,
                                           fss_followup_41,
                                           fss_followup_44,
                                           fss_followup_47,
                                           fss_followup_51,
                                           fss_followup_52))),
    
    fss_followup_soc = rowSums(select(., c(fss_followup_5,
                                           fss_followup_8,
                                           fss_followup_9,
                                           fss_followup_10,
                                           fss_followup_15,
                                           fss_followup_20,
                                           fss_followup_27,
                                           fss_followup_33,
                                           fss_followup_41,
                                           fss_followup_44,
                                           fss_followup_47,
                                           fss_followup_51,
                                           fss_followup_52))),
    
    # Agoraphobia Fears (AGO) subscale: 2, 3, 6, 7, 11, 17, 18, 19, 23, 24, 35, 40, 42
    fss_baseline_ago = rowSums(select(., c(fss_baseline_2,
                                           fss_baseline_3,
                                           fss_baseline_6,
                                           fss_baseline_7,
                                           fss_baseline_11,
                                           fss_baseline_17,
                                           fss_baseline_18,
                                           fss_baseline_19,
                                           fss_baseline_23,
                                           fss_baseline_24,
                                           fss_baseline_35,
                                           fss_baseline_40,
                                           fss_baseline_42))),
    
    fss_followup_ago = rowSums(select(., c(fss_followup_2,
                                           fss_followup_3,
                                           fss_followup_6,
                                           fss_followup_7,
                                           fss_followup_11,
                                           fss_followup_17,
                                           fss_followup_18,
                                           fss_followup_19,
                                           fss_followup_23,
                                           fss_followup_24,
                                           fss_followup_35,
                                           fss_followup_40,
                                           fss_followup_42))),
    # Injury Fears (INJ) subscale: 1, 4, 12, 14, 22, 32, 36, 38, 39, 43, 46, 50
    fss_baseline_inj = rowSums(select(., c(fss_baseline_1,
                                           fss_baseline_4,
                                           fss_baseline_12,
                                           fss_baseline_14,
                                           fss_baseline_22,
                                           fss_baseline_32,
                                           fss_baseline_36,
                                           fss_baseline_38,
                                           fss_baseline_39,
                                           fss_baseline_43,
                                           fss_baseline_46,
                                           fss_baseline_50))),
    
    fss_followup_inj = rowSums(select(., c(fss_followup_1,
                                           fss_followup_4,
                                           fss_followup_12,
                                           fss_followup_14,
                                           fss_followup_22,
                                           fss_followup_32,
                                           fss_followup_36,
                                           fss_followup_38,
                                           fss_followup_39,
                                           fss_followup_43,
                                           fss_followup_46,
                                           fss_followup_50))),
    
    # Sex Aggression Fears (SEX) subscale: 25, 26, 28, 30, 31, 34, 48, 49
    fss_baseline_sex = rowSums(select(., c(fss_baseline_25,
                                           fss_baseline_26,
                                           fss_baseline_28,
                                           fss_baseline_30,
                                           fss_baseline_31,
                                           fss_baseline_34,
                                           fss_baseline_48,
                                           fss_baseline_49))),
    
    fss_followup_sex = rowSums(select(., c(fss_followup_25,
                                           fss_followup_26,
                                           fss_followup_28,
                                           fss_followup_30,
                                           fss_followup_31,
                                           fss_followup_34,
                                           fss_followup_48,
                                           fss_followup_49))),
    
    # Fear of Harmless Animals (ANI) subscale: 13, 16, 21, 29, 37, 45
    fss_baseline_ani = rowSums(select(., c(fss_baseline_13,
                                           fss_baseline_16,
                                           fss_baseline_21,
                                           fss_baseline_29,
                                           fss_baseline_37,
                                           fss_baseline_45))),
    
    fss_followup_ani = rowSums(select(., c(fss_followup_13,
                                           fss_followup_16,
                                           fss_followup_21,
                                           fss_followup_29,
                                           fss_followup_37,
                                           fss_followup_45)))
    
  )

# Save data
saveRDS(fss_data, here("data", "interim", "redcap", "step-02", "redcap-fss-data.Rds"))
