

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


library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)


# Path to files
path = "data/raw data/executive functions/Session 1"

# Read in and combine the files
ASRT = rbind( 
  read_csv(file.path(path, "serial reaction time 1.csv")),
  read_csv(file.path(path, "serial reaction time 2.csv")),
  read_csv(file.path(path, "serial reaction time 3.csv")),
  read_csv(file.path(path, "qdvg4 serial reaction time.csv")),
  read_csv(file.path(path, "wbij5 serial reaction time.csv")),
  read_csv(file.path(path, "xqls8 serial reaction time.csv")) 
) %>%
  
  # Rename columns
  rename(participant_home_ID = `Participant Public ID`,
         pattern_or_random = p_or_r) %>%
  
  # Convert string values to numeric where appropriate
  mutate(across(c(cumulative_RT, trial_number), as.numeric)) %>%
  replace_na(list(cumulative_RT = 0)) %>%
  
  # Create a new 'epoch' column based on block ranges
  mutate(epoch = case_when(
    block <= 5 ~ 1,
    block >= 6 & block <= 10 ~ 2,
    block >= 11 & block <= 15 ~ 3,
    block >= 16 & block <= 20 ~ 4,
    block >= 21 & block <= 25 ~ 5,
    TRUE ~ NA_real_
  )) %>%
  
  # Calculate median reaction time (RT) for each combination of participant, 
  # group, trial and triplet type.
  
  group_by(participant_home_ID, pattern_or_random, epoch, triplet_type) %>%
  summarize(median_RT = median(cumulative_RT, na.rm = TRUE)) %>%
  pivot_wider(names_from = c(epoch, triplet_type), values_from = median_RT) %>%
  ungroup()

# Rename the columns to reflect epochs and triplet types
names(ASRT)[-c('participant_home_ID', 'pattern_or_random')] = 
  paste0("epoch", names(ASRT)[-c(1, 2)])

# Handle missing values and create new calculated columns
ASRT = ASRT %>%
  drop_na(pattern_or_random) %>%  # Ensure 'pattern_or_random' column doesn't have missing values
  mutate(across(everything(), ~replace_na(., 0))) %>%  # Replace NA with 0
  mutate(
    epoch1_TL = epoch1_L - epoch1_H,
    epoch2_TL = epoch2_L - epoch2_H,
    epoch3_TL = epoch3_L - epoch3_H,
    epoch4_TL = epoch4_L - epoch4_H,
    epoch5_TL = epoch5_L - epoch5_H,
    ASRT_learning_effect = epoch5_TL - epoch1_TL
  )

# Get descriptives and plot distribution of ASRT_learning_effect 
# split by pattern and random condition.

ASRT %>% 
  summarise(ASRT_learning_effect = median(ASRT_learning_effect), 
            .by = pattern_or_random)

ASRT %>%
  ggplot(aes(x = factor(1), y = ASRT_learning_effect)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = pattern_or_random, shape = pattern_or_random), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(x = NULL)

# Select the relevant columns
ASRT = ASRT %>%
  select(participant_home_ID, ASRT_learning_effect)

# Whereas pattern trials are sensitive to implicit learning, random trials are 
# more related to motor and attentional processes. To obtain an accurate 
# measure of implicit learning, the motor and attentional processes should be 
# controlled for in the statistical analysis. 

