

# Preprocess alternating serial reaction time (session1_ASRT) task from Session 1

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)

# Path to files
path <- 'data/raw data/executive functions/Session 1'

# Read in and combine the files
session1_ASRT <- bind_rows( 
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

session1_ASRT %>% 
  filter(pattern_or_random == 'pattern_ASRT') %>%  # pattern only
  group_by(participant_home_ID, triplet_type) %>% 
  summarise(count = n()) %>% 
  arrange(participant_home_ID, count)

session1_ASRT %>% 
  filter(pattern_or_random == 'random_ASRT') %>%   # random only
  group_by(participant_home_ID, triplet_type) %>% 
  summarise(count = n()) %>% 
  arrange(participant_home_ID, count)

# Count participants with and without low-probability pattern trials
session1_ASRT %>%
  filter(pattern_or_random == 'pattern_ASRT') %>%
  group_by(participant_home_ID) %>%
  summarize(has_Low = any(triplet_type == 'L')) %>%  
  group_by(has_Low) %>%
  summarize(count = n())


# Trim data. First, remove any reaction times (RTs) smaller than 50 ms or larger 
# than 5,000 ms. Second, remove any participants that have less than 100 
# observations. Third, remove any RTs that lie more than 3 standard deviations 
# (SD) away from the mean within participants, within pattern/random condition, 
# within blocks, within triplet types, and within correct/incorrect responses. 
# Three SDs is a typical cut-off (e.g., https://doi.org/10.3758/s13428-012-0304-z). 
# At the end, the percentage of data trimmed out is presented. Based on 
# preliminary data, this percentage is expected to be around 5%. 

# Create empty dataframe using column 
# names from the original data set.
trimmed_ASRT <- session1_ASRT[0,]

trimmed_ASRT <- session1_ASRT %>%
  
  # Apply minimum and maximum cut-offs to RTs
  filter(!cumulative_RT < 50, !cumulative_RT > 5000) %>%
  
  # Remove any participants that have less than 100 observations
  group_by(participant_home_ID) %>%
  filter(n() >= 100) %>%
  ungroup() %>%
  
  # Apply 3 SD cut-off within the grouping factors
  group_by(participant_home_ID, pattern_or_random, block, triplet_type) %>%
  group_modify(~ {
    mean_rt <- mean(.x$cumulative_RT, na.rm = TRUE)
    sd_rt <- sd(.x$cumulative_RT, na.rm = TRUE)
    
    # Filter rows within 3 SDs from the mean
    .x %>% filter(cumulative_RT > (mean_rt - 3 * sd_rt) & 
                    cumulative_RT < (mean_rt + 3 * sd_rt))
  }) %>%
  ungroup()

# View percentage of trials trimmed
((nrow(session1_ASRT) - nrow(trimmed_ASRT)) / nrow(session1_ASRT)) * 100

# Apply change
session1_ASRT <- trimmed_ASRT


# Create a new 'epoch' column based on block ranges
session1_ASRT <- session1_ASRT %>%
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

gc() # Free unused memory


# Inspect number of data points in epoch 1 per participant, 
# per pattern/random type and per triplet type.

session1_ASRT %>% 
  filter(epoch == 'epoch1', pattern_or_random == 'pattern_ASRT') %>%  # pattern only
  group_by(participant_home_ID, triplet_type) %>% 
  summarise(count = n()) %>% 
  arrange(participant_home_ID, count)

session1_ASRT %>% 
  filter(epoch == 'epoch1', pattern_or_random == 'random_ASRT') %>%  # random only
  group_by(participant_home_ID, triplet_type) %>% 
  summarise(count = n()) %>% 
  arrange(participant_home_ID, count)

# Inspect number of data points in epoch 5 per participant, 
# per pattern/random type and per triplet type.

session1_ASRT %>% 
  filter(epoch == 'epoch5', pattern_or_random == 'pattern_ASRT') %>%  # pattern only
  group_by(participant_home_ID, triplet_type) %>% 
  summarise(count = n()) %>% 
  arrange(participant_home_ID, count)

session1_ASRT %>% 
  filter(epoch == 'epoch5', pattern_or_random == 'random_ASRT') %>%  # random only
  group_by(participant_home_ID, triplet_type) %>% 
  summarise(count = n()) %>% 
  arrange(participant_home_ID, count)

# Count participants with and without low-probability pattern trials
session1_ASRT %>%
  filter(pattern_or_random == 'pattern_ASRT') %>%
  group_by(participant_home_ID) %>%
  summarize(has_Low = any(triplet_type == 'L')) %>%  
  group_by(has_Low) %>%
  summarize(count = n())

gc() # Free unused memory

# Get descriptives and plot distributions

session1_ASRT %>% group_by(pattern_or_random, triplet_type) %>%
  summarise(M = mean(cumulative_RT), SD = sd(cumulative_RT))

session1_ASRT_stats <- session1_ASRT %>%
  group_by(pattern_or_random, triplet_type) %>%
  summarise(mean_RT = mean(cumulative_RT, na.rm = TRUE), 
            count = n(),
            n_participants = n_distinct(participant_home_ID)) %>%
  ungroup()

session1_ASRT %>%
  ggplot(aes(x = factor(1), y = cumulative_RT, color = triplet_type, 
             fill = triplet_type, group = triplet_type)) +
  facet_wrap(~ pattern_or_random) + 
  geom_violin(width = 0.4) + 
  geom_jitter(width = 0.1, size = 0.0005, alpha = .08) +
  geom_label(
    data = session1_ASRT_stats, 
    aes(x = factor(1), y = mean_RT, label = paste('M =', round(mean_RT, 2)), 
        group = triplet_type),
    position = position_dodge(width = 0.4), size = 4, color = 'white', 
    show.legend = FALSE
  ) +
  geom_text(
    data = session1_ASRT_stats, 
    aes(x = factor(1), y = -70, label = paste(count, 'obs.'), group = triplet_type),
    position = position_dodge(width = 0.4), size = 4, show.legend = FALSE
  ) +
  geom_text(
    data = session1_ASRT_stats, 
    aes(x = factor(1), y = -270, label = paste(n_participants, 'partcps.'), 
        group = triplet_type),
    position = position_dodge(width = 0.4), size = 4, show.legend = FALSE
  ) +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(limits = c(-300, 5000), expand = c(0.01, 0)) +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 12, margin = margin(r = 10, unit = 'pt')),
        strip.text = element_text(size = 12), axis.title.x = element_blank(), 
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        legend.title = element_text(size = 12), legend.text = element_text(size = 11))


# Calculate high-low difference per participant, irrespective of pattern/random type
session1_ASRT <- session1_ASRT %>% 
  group_by(participant_home_ID, triplet_type) %>%
  summarize(mean_RT = mean(cumulative_RT, na.rm = TRUE)) %>%
  ungroup() %>%
  
  # Pivot categorical values of triplet_type into independent columns
  pivot_wider(names_from = triplet_type, values_from = mean_RT) %>%
  
  # Compute session1_ASRT effect by subtracting high-probability from 
  # low-probability trials.
  mutate(session1_ASRT = L - H)

gc() # Free unused memory

# Finally, check how many participants have data for the session1_ASRT effect
session1_ASRT %>% filter(complete.cases(session1_ASRT)) %>% 
  summarise(n_distinct(participant_home_ID)) %>% pull

# Select the relevant columns
session1_ASRT <- session1_ASRT %>% 
  select(participant_home_ID, session1_ASRT)

