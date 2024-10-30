

# Preprocessing Alternating Serial Reaction Time (ASRT) task

# Operationalising ASRT results. Consider the following examples.
# 
# Epoch 1 for Participant A and Participant B
# Low probability = 1,000 ms
# High probability = 800 ms
# Low-high effect = 1000 - 800 = 200 ms
# 
# Epoch 5 for Participant A
# Low probability = 900 ms
# High probability = 600 ms
# Low-high effect = 900 - 600 = 300 ms
# Learning effect (Epoch 5 - Epoch 1) = 300 - 200 = 100 ms
# 
# Epoch 5 for Participant B
# Low probability = 900 ms
# High probability = 700 ms
# Low-high effect = 900 - 700 = 200 ms
# Learning effect (Epoch 5 - Epoch 1) = 200 - 200 = 0 ms
# 
# Participant A gained 100 ms across epochs, demonstrating a learning effect, 
# whereas Participant B did not present any learning effect.


# Load the necessary libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)


# Define the path
path_to_ASRT <- "Raw data/Executive functions/Session 1"

# Read the files
ASRT_1 <- read_csv(file.path(path_to_ASRT, "ASRT_DGS.csv"))
ASRT_2 <- read_csv(file.path(path_to_ASRT, "ASRT_Stroop.csv"))
ASRT_3 <- read_csv(file.path(path_to_ASRT, "ASRT_ASRT.csv"))

# Load the extra CSV files
ASRT_1_extra <- read_csv(file.path(path_to_ASRT, "qdvg4_ASRT_DGS.csv"))
ASRT_2_extra_1 <- read_csv(file.path(path_to_ASRT, "wbij5_ASRT_Stroop.csv"))
ASRT_2_extra_2 <- read_csv(file.path(path_to_ASRT, "xqls8_ASRT_Stroop.csv"))

# Select the relevant columns
columns_to_select <- c('Participant Public ID', 'time_elapsed', 'rt', 'correct', 
                       'triplet_type', 'p_or_r', 'block', 'sequence', 'is_practice', 
                       'first_response', 'trial_number', 'correct_pos', 
                       'correct_resp_button', 'resp_button', 'cumulative_RT', 
                       'actual_triplet')

# Combine the data frames
ASRT_1 <- bind_rows(ASRT_1, ASRT_1_extra)
ASRT_2 <- bind_rows(ASRT_2, ASRT_2_extra_1, ASRT_2_extra_2)

# Select relevant columns from each dataframe
ASRT_1 <- ASRT_1 %>% select(all_of(columns_to_select))
ASRT_2 <- ASRT_2 %>% select(all_of(columns_to_select))
ASRT_3 <- ASRT_3 %>% select(all_of(columns_to_select))

# Combine all the dataframes
merged_ASRT <- bind_rows(ASRT_1, ASRT_2, ASRT_3)

# Convert string values to numeric where appropriate
merged_ASRT <- merged_ASRT %>%
  mutate(across(c(cumulative_RT, trial_number), as.numeric)) %>%
  replace_na(list(cumulative_RT = 0))

# Create a new 'epoch' column based on block ranges
merged_ASRT <- merged_ASRT %>%
  mutate(epoch = case_when(
    block <= 5 ~ 1,
    block >= 6 & block <= 10 ~ 2,
    block >= 11 & block <= 15 ~ 3,
    block >= 16 & block <= 20 ~ 4,
    block >= 21 & block <= 25 ~ 5,
    TRUE ~ NA_real_
  ))

# Calculate median reaction time (RT) for each combination of participant, group, trial, and triplet type
TL_RT_wide <- merged_ASRT %>%
  group_by(`Participant Public ID`, p_or_r, epoch, triplet_type) %>%
  summarize(median_RT = median(cumulative_RT, na.rm = TRUE)) %>%
  pivot_wider(names_from = c(epoch, triplet_type), values_from = median_RT) %>%
  ungroup()

# Rename the columns to reflect epochs and triplet types
names(TL_RT_wide)[-c(1, 2)] <- paste0("e", names(TL_RT_wide)[-c(1, 2)])

# Handle missing values and create new calculated columns
TL_RT_wide_1 <- TL_RT_wide %>%
  drop_na(p_or_r) %>%  # Ensure 'p_or_r' column doesn't have missing values
  mutate(across(everything(), ~replace_na(., 0))) %>%  # Replace NA with 0
  mutate(
    e1_TL = e1_L - e1_H,
    e2_TL = e2_L - e2_H,
    e3_TL = e3_L - e3_H,
    e4_TL = e4_L - e4_H,
    e5_TL = e5_L - e5_H,
    ASRT_learning_effect = e5_TL - e1_TL
  )

# Save the result to a CSV file
write_csv(TL_RT_wide_1, "TL_RT_wide_1.csv")


