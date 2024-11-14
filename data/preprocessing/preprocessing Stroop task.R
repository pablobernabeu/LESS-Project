

# Preprocessing Stroop task from Session 1

library(dplyr)
library(tidyr)
library(readr)
library(janitor)

# Path to files
path = 'data/raw data/executive functions/Session 1'

# Read in and combine the files
session1_Stroop = rbind( 
  read_csv(file.path(path, 'Stroop 1.csv')),
  read_csv(file.path(path, 'Stroop 2.csv')),
  read_csv(file.path(path, 'Stroop 3.csv')),
  read_csv(file.path(path, 'qdvg4 Stroop.csv')),
  read_csv(file.path(path, 'wbij5 Stroop.csv')),
  read_csv(file.path(path, 'xqls8 Stroop.csv')) 
) %>%
  
  # Rename columns
  rename(participant_home_ID = `Participant Public ID`,
         rt = `Reaction Time`, 
         trial_number = `Trial Number`) %>%
  
  # Convert string values to numeric where appropriate
  mutate(across(c(rt, trial_number), as.numeric))

# Clean column names
colnames(session1_Stroop) = make.names(colnames(session1_Stroop))
print(colnames(session1_Stroop))


# Trim data. First, remove any reaction times (RTs) smaller than 50 ms or larger 
# than 5,000 ms. Second, remove any RTs that lie more than 3 standard deviations 
# (SD) away from the mean within participants and within correct/incorrect 
# responses. Three SDs is a typical cut-off (e.g., 
# https://doi.org/10.3758/s13428-012-0304-z). At the end, the percentage of 
# data trimmed out is presented. Based on preliminary data, this percentage is
# expected to be around 5%. 

# Create empty dataframe using column 
# names from the original data set.
trimmed_session1_Stroop = session1_Stroop[0,]

trimmed_session1_Stroop = session1_Stroop %>%
  
  # Apply minimum and maximum cut-offs to RTs
  filter(!rt < 50, !rt > 5000) %>%
  
  # Remove any participants that have less than 100 observations
  group_by(participant_home_ID) %>%
  filter(n() >= 100) %>%
  ungroup() %>%
  
  # Apply 3 SD cut-off within the nests
  group_by(participant_home_ID, pattern_or_random, block, triplet_type) %>%
  group_modify(~ {
    mean_rt = mean(.x$rt, na.rm = TRUE)
    sd_rt = sd(.x$rt, na.rm = TRUE)
    
    # Filter rows within 3 SDs from the mean
    .x %>% filter(rt > (mean_rt - 3 * sd_rt) & 
                    rt < (mean_rt + 3 * sd_rt))
  }) %>%
  ungroup()

# View percentage of trials trimmed
((nrow(session1_Stroop) - nrow(trimmed_session1_Stroop)) / nrow(session1_Stroop)) * 100

# Apply change
session1_Stroop = trimmed_session1_Stroop


# Select relevant columns
Stroop = Stroop %>%
  select(rt, participant_home_ID, Correct, Incorrect, congruency, Zone.Type) %>%
  
  # Filter to remove rows where Zone.Type is not 'response'
  filter(Zone.Type == 'response_keyboard') %>%
  
  # Filter and clean the data by removing NAs and ensuring congruency is a factor
  drop_na(rt, congruency) %>%
  mutate(congruency = factor(congruency, levels = c(0, 1), 
                             labels = c('incongruent', 'congruent')))

# Calculate the mean reaction time according to congruence for each participant
Stroop = Stroop %>%
  group_by(participant_home_ID, congruency) %>%
  summarize(mean_reaction_time_Stroop = mean(rt, na.rm = TRUE)) %>%
  ungroup()

# Calculate the difference between the reaction times of each congruence 
# condition per participant.

session1_Stroop = Stroop %>%
  group_by(participant_home_ID) %>%
  pivot_wider(names_from = congruency, values_from = mean_reaction_time_Stroop) %>%
  mutate(session1_Stroop = incongruent - congruent) %>%
  select(participant_home_ID, Stroop) %>%
  ungroup()

