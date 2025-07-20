

# Descriptive information on trial-by-trial EEG data

library(dplyr)
library(knitr)

source('data/R_functions/merge_trialbytrial_EEG_data.R')

trialbytrial_EEG_data <- 
  merge_trialbytrial_EEG_data(
    EEG_file_pattern = '^\\d+_trialbytrial_S[123]_S10[123]\\.',
    min_time = -100, max_time = 1100,
    include_baseline = TRUE,
    aggregate_electrodes = FALSE,
    aggregate_time_points = FALSE,
    selected_macroregion = NULL
  )

# Count trials across participants, sessions and experimental conditions
EEG_trial_count <- 
  trialbytrial_EEG_data %>%
  group_by(mini_language, participant_lab_ID, session, grammatical_property, grammaticality) %>%
  summarise(n_trials = n_distinct(sentence_marker), .groups = "drop")

# Save trial count data to disk (and read it back in)
saveRDS('data/EEG_trial_count.rds', EEG_trial_count)
# EEG_trial_count <- readRDS('data/EEG_trial_count.rds')

# Order factor levels
EEG_trial_count$grammaticality <- 
  factor(EEG_trial_count$grammaticality, 
         levels = c('Grammatical', 'Ungrammatical', 'Number agreement violation',
                    'Article location violation', ''))

# Count trials across experimental conditions
EEG_trial_count %>%
  group_by(mini_language, session, grammatical_property, grammaticality) %>%
  summarise(min_trials = min(n_trials), max_trials = max(n_trials),
            mean_trials = mean(n_trials)) %>%
  kable()

