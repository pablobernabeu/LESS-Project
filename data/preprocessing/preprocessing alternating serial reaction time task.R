

# Preprocessing alternating serial reaction time (ASRT) task from Session 1


library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)

# Path to files
path = 'data/raw data/executive functions/Session 1'

# Read in and combine the files
ASRT = rbind( 
  read_csv(file.path(path, 'serial reaction time 1.csv')),
  read_csv(file.path(path, 'serial reaction time 2.csv')),
  read_csv(file.path(path, 'serial reaction time 3.csv')),
  read_csv(file.path(path, 'qdvg4 serial reaction time.csv')),
  read_csv(file.path(path, 'wbij5 serial reaction time.csv')),
  read_csv(file.path(path, 'xqls8 serial reaction time.csv')) 
) %>%
  
  # Rename columns
  rename(participant_home_ID = `Participant Public ID`,
         pattern_or_random = p_or_r) %>%
  
  # Rename conditions
  mutate(pattern_or_random = case_match(pattern_or_random, 
                                        'P' ~ 'pattern_ASRT', 
                                        'R' ~ 'random_ASRT')) %>%
  
  # Convert string values to numeric where appropriate
  mutate(across(c(cumulative_RT, trial_number), as.numeric)) %>%
  
  # Leave only high and low probability trials
  filter(triplet_type %in% c('H', 'L')) %>%
  
  # Remove NAs 
  drop_na(participant_home_ID, pattern_or_random, block, 
          triplet_type, cumulative_RT)


# Inspect number of data points per participant, per pattern/random type 
# and per triplet type.

ASRT %>% 
  filter(pattern_or_random == 'pattern_ASRT') %>%  # pattern only
  group_by(participant_home_ID, triplet_type) %>% 
  summarise(count = n()) %>% 
  arrange(participant_home_ID, count)

ASRT %>% 
  filter(pattern_or_random == 'random_ASRT') %>%   # random only
  group_by(participant_home_ID, triplet_type) %>% 
  summarise(count = n()) %>% 
  arrange(participant_home_ID, count)

# Count participants with and without low-probability pattern trials
ASRT %>%
  filter(pattern_or_random == 'pattern_ASRT') %>%
  group_by(participant_home_ID) %>%
  summarize(has_Low = any(triplet_type == 'L')) %>%  
  group_by(has_Low) %>%
  summarize(count = n())


# Trim RTs. First, remove any reaction times (RTs) smaller than 50 ms or larger 
# than 5,000 ms. Next, remove any RTs that lie more than 3 standard deviations 
# (SD) away from the mean within participants, within pattern/random condition, 
# within blocks and within triplet types. Three SDs is a typical cut-off (e.g., 
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


# Inspect number of data points in epoch 1 per participant, 
# per pattern/random type and per triplet type.

ASRT %>% 
  filter(epoch == 'epoch1', pattern_or_random == 'pattern_ASRT') %>%  # pattern only
  group_by(participant_home_ID, triplet_type) %>% 
  summarise(count = n()) %>% 
  arrange(participant_home_ID, count)

ASRT %>% 
  filter(epoch == 'epoch1', pattern_or_random == 'random_ASRT') %>%  # random only
  group_by(participant_home_ID, triplet_type) %>% 
  summarise(count = n()) %>% 
  arrange(participant_home_ID, count)

# Inspect number of data points in epoch 5 per participant, 
# per pattern/random type and per triplet type.

ASRT %>% 
  filter(epoch == 'epoch5', pattern_or_random == 'pattern_ASRT') %>%  # pattern only
  group_by(participant_home_ID, triplet_type) %>% 
  summarise(count = n()) %>% 
  arrange(participant_home_ID, count)

ASRT %>% 
  filter(epoch == 'epoch5', pattern_or_random == 'random_ASRT') %>%  # random only
  group_by(participant_home_ID, triplet_type) %>% 
  summarise(count = n()) %>% 
  arrange(participant_home_ID, count)

# Count participants with and without low-probability pattern trials
ASRT %>%
  filter(pattern_or_random == 'pattern_ASRT') %>%
  group_by(participant_home_ID) %>%
  summarize(has_Low = any(triplet_type == 'L')) %>%  
  group_by(has_Low) %>%
  summarize(count = n())


# **********************
# 
# Conclusion: Most participants are missing data for low-probability trials, 
# making the comparison between epoch 1 and epoch 5 impossible. Therefore, 
# the results are reanalysed below without the epoch distinction. 
# 
# **********************


# Get descriptives and plot distribution of pattern_ASRT_H and random_ASRT_H

ASRT %>% group_by(pattern_or_random, triplet_type) %>%
  summarise(M = mean(cumulative_RT), SD = sd(cumulative_RT))

ASRT_stats <- ASRT %>%
  group_by(pattern_or_random, triplet_type) %>%
  summarise(mean_RT = mean(cumulative_RT, na.rm = TRUE), 
            count = n(),
            n_participants = n_distinct(participant_home_ID)) %>%
  ungroup()

ASRT %>%
  ggplot(aes(x = factor(1), y = cumulative_RT, color = triplet_type, 
             fill = triplet_type, group = triplet_type)) +
  facet_wrap(~ pattern_or_random) + 
  geom_violin(width = 0.4) + 
  geom_jitter(width = 0.1, size = 0.0005, alpha = .08) +
  geom_label(
    data = ASRT_stats, 
    aes(x = factor(1), y = mean_RT, label = paste('M =', round(mean_RT, 2)), 
        group = triplet_type),
    position = position_dodge(width = 0.4), size = 4, color = 'white'
  ) +
  geom_text(
    data = ASRT_stats, 
    aes(x = factor(1), y = -70, label = paste(count, 'obs.'), group = triplet_type),
    position = position_dodge(width = 0.4), size = 4, 
  ) +
  geom_text(
    data = ASRT_stats, 
    aes(x = factor(1), y = -270, label = paste(n_participants, 'partcps.'), 
        group = triplet_type),
    position = position_dodge(width = 0.4), size = 4, 
  ) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(limits = c(-300, 5000), expand = c(0.01, 0)) +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        strip.text = element_text(size = 12), axis.title.x = element_blank(), 
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.title = element_text(size = 12), legend.text = element_text(size = 11))


######################

# TBC 13 Nov 2024

######################


# Calculate mean reaction time (RT) for each combination of participant, 
# pattern/random type and triplet type.
# Pivot categorical values of triplet_type into independent columns.

ASRT = ASRT %>% 
  group_by(participant_home_ID, pattern_or_random, triplet_type) %>%
  summarize(mean_RT = mean(cumulative_RT, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = c(pattern_or_random, triplet_type), 
              values_from = mean_RT) %>%
  
  # Subtract high-probability pattern trials from 
  # high-probability random trials.
  mutate(ASRT = random_ASRT_H - pattern_ASRT_H)

gc() # free up memory

ASRT %>%
  ggplot(aes(x = factor(1), y = ASRT_learning_effect)) +
  geom_boxplot(width = 0.4, fill = 'white') +
  geom_jitter(aes(color = pattern_or_random, shape = pattern_or_random), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c('#00AFBB', '#E7B800')) + 
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
  select(participant_home_ID, pattern_ASRT, random_ASRT)

# Whereas pattern trials are sensitive to implicit learning, random trials are 
# more related to motor and attentional processes. To obtain an accurate 
# measure of implicit learning, the motor and attentional processes should be 
# controlled for in the statistical analysis. 

