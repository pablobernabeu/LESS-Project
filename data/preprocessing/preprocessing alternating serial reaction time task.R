

# Preprocessing alternating serial reaction time (ASRT) task from Session 1

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
  
  # Rename conditions
  mutate(pattern_or_random = case_match(pattern_or_random, 
                                        'P' ~ 'pattern_Stroop', 
                                        'R' ~ 'random_Stroop')) %>%
  
  # Convert string values to numeric where appropriate
  mutate(across(c(cumulative_RT, trial_number), as.numeric)) %>%
  
  # Leave only high and low probability trials
  filter(triplet_type %in% c('H', 'L')) %>%
  
  # Remove NAs 
  drop_na(participant_home_ID, pattern_or_random, block, 
          triplet_type, cumulative_RT)

# Inspect number of data points per patter/random type and per triplet type
ASRT %>% group_by(pattern_or_random, triplet_type) %>% summarise(n())

# Trim RTs. First, remove any reaction times (RTs) smaller than 50 ms or larger 
# than 5,000 ms. Next, remove any RTs that lie more than 3 standard deviations 
# (SD) away from the mean. The latter calculation is performed within 
# participants, within pattern/random condition, within blocks, and within 
# triplet types. Three SDs is a typical cut-off (e.g., 
# https://doi.org/10.3758/s13428-012-0304-z). At the end, the percentage of 
# data trimmed out is presented. 

# Create empty dataframe using column 
# names from the original data set.
trimmed_ASRT = ASRT[0,]

trimmed_ASRT = ASRT %>%
  
  # Apply minimum and maximum cut-offs
  filter(!cumulative_RT < 50, !cumulative_RT > 5000) %>%
  
  # Apply 3 SD cut-off within the nests
  group_by(participant_home_ID, pattern_or_random, block, triplet_type) %>%
  group_modify(~ {
    mean_rt = mean(.x$cumulative_RT, na.rm = TRUE)
    sd_rt = sd(.x$cumulative_RT, na.rm = TRUE)
    
    # Filter rows within 3 SDs from the mean
    .x %>% filter(cumulative_RT > (mean_rt - 3 * sd_rt) & 
                    cumulative_RT < (mean_rt + 3 * sd_rt))
  }) %>%
  ungroup()

# View percentage of trials trimmed
((nrow(ASRT) - nrow(trimmed_ASRT)) / nrow(ASRT)) * 100

# Apply change
ASRT = trimmed_ASRT

# Create a new 'epoch' column based on block ranges
ASRT = ASRT %>%
  mutate(
    epoch = case_when(
      block <= 5 ~ 1,
      block >= 6 & block <= 10 ~ 2,
      block >= 11 & block <= 15 ~ 3,
      block >= 16 & block <= 20 ~ 4,
      block >= 21 & block <= 25 ~ 5,
      TRUE ~ NA_real_
    ),
    epoch = paste0('epoch', epoch)
  )

gc() # free up memory

# Calculate mean reaction time (RT) for each combination of participant, 
# group, trial and triplet type.

ASRT = ASRT %>% 
  group_by(participant_home_ID, pattern_or_random, epoch, triplet_type) %>%
  summarize(mean_RT = mean(cumulative_RT, na.rm = TRUE)) %>%
  ungroup()

# Pivot categorical values of some variables into independent columns
ASRT = ASRT %>%
  pivot_wider(names_from = c(epoch, triplet_type), 
              values_from = mean_RT)

gc() # free up memory

# Subtract RTs for low probability trials from RTs for high-probability trials,
# and remove NAs.

ASRT = ASRT %>% 
  mutate(
    epoch1_diff = epoch1_L - epoch1_H,
    epoch2_diff = epoch2_L - epoch2_H,
    epoch3_diff = epoch3_L - epoch3_H,
    epoch4_diff = epoch4_L - epoch4_H,
    epoch5_diff = epoch5_L - epoch5_H,
    ASRT_learning_effect = epoch5_diff - epoch1_diff
  ) %>%
  drop_na(ASRT_learning_effect)

gc() # free up memory

# Get descriptives and plot distribution of ASRT_learning_effect split 
# by pattern/random condition.

ASRT %>% group_by(pattern_or_random) %>%
  summarise(pattern_Stroop = mean(ASRT_learning_effect))

ASRT %>%
  ggplot(aes(x = factor(1), y = ASRT_learning_effect)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = pattern_or_random, shape = pattern_or_random), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(x = NULL)

# Check how many participants have data for pattern trials and 
# for random trials.
ASRT %>% group_by(pattern_or_random) %>% summarise(participants = n())

# Split apart pattern and random trials
ASRT = ASRT %>%
  pivot_wider(names_from = pattern_or_random, 
              values_from = ASRT_learning_effect)

# Aggregate data by participant
ASRT = ASRT %>%
  group_by(participant_home_ID) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  ungroup()

# Select the relevant columns
Session1_ASRT = ASRT %>% 
  select(participant_home_ID, pattern_Stroop, random_Stroop)

# Whereas pattern trials are sensitive to implicit learning, random trials are 
# more related to motor and attentional processes. To obtain an accurate 
# measure of implicit learning, the motor and attentional processes should be 
# controlled for in the statistical analysis. 

