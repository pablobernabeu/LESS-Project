

# Preprocessing Alternating Serial Reaction Time (ASRT) task

# The following hypothetical examples illustrate how the ASRT 
# learning effect is operationalised.
# 
# Epoch 1 for Participant A and Participant B, with identical results
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
# Participant A gained 100 ms across epochs, revealing a learning effect, 
# whereas Participant B did not present any learning effect.


# Load the necessary libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)


# Path to files
path_to_ASRT <- "data/raw data/Executive functions/Session 1"

# Read in and combine the files
ASRT = rbind( 
  read_csv(file.path(path_to_ASRT, "serial reaction time 1.csv")),
  read_csv(file.path(path_to_ASRT, "serial reaction time 2.csv")),
  read_csv(file.path(path_to_ASRT, "serial reaction time 3.csv")),
  read_csv(file.path(path_to_ASRT, "qdvg4 serial reaction time.csv")),
  read_csv(file.path(path_to_ASRT, "wbij5 serial reaction time.csv")),
  read_csv(file.path(path_to_ASRT, "xqls8 serial reaction time.csv")) 
)

# Convert string values to numeric where appropriate
ASRT <- ASRT %>%
  mutate(across(c(cumulative_RT, trial_number), as.numeric)) %>%
  replace_na(list(cumulative_RT = 0))

# Create a new 'epoch' column based on block ranges
ASRT <- ASRT %>%
  mutate(epoch = case_when(
    block <= 5 ~ 1,
    block >= 6 & block <= 10 ~ 2,
    block >= 11 & block <= 15 ~ 3,
    block >= 16 & block <= 20 ~ 4,
    block >= 21 & block <= 25 ~ 5,
    TRUE ~ NA_real_
  ))

# Calculate median reaction time (RT) for each combination of participant, 
# group, trial, and triplet type.
ASRT <- ASRT %>%
  group_by(`Participant Public ID`, p_or_r, epoch, triplet_type) %>%
  summarize(median_RT = median(cumulative_RT, na.rm = TRUE)) %>%
  pivot_wider(names_from = c(epoch, triplet_type), values_from = median_RT) %>%
  ungroup()

# Rename the columns to reflect epochs and triplet types
names(ASRT)[-c(1, 2)] <- paste0("e", names(ASRT)[-c(1, 2)])

# Handle missing values and create new calculated columns
ASRT_1 <- ASRT %>%
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

