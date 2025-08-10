

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
  group_by(mini_language, participant_lab_ID, session, grammatical_property, 
           grammaticality) %>%
  summarise(n_trials = n_distinct(sentence_marker), .groups = "drop")

# Save trial count data to disk (and read it back in)
saveRDS(EEG_trial_count, 'data/EEG_trial_count.rds')
# EEG_trial_count <- readRDS('data/EEG_trial_count.rds')

# Order factor levels

EEG_trial_count$mini_language <- 
  factor(EEG_trial_count$mini_language, 
         levels = c('Mini-Norwegian', 'Mini-English'))

EEG_trial_count$grammatical_property <- 
  factor(EEG_trial_count$grammatical_property, 
         levels = c('Gender agreement', 'Differential object marking', 
                    'Verb-object number agreement'))

EEG_trial_count$grammaticality <- 
  factor(EEG_trial_count$grammaticality, 
         levels = c('Grammatical', 'Ungrammatical', 
                    'Number agreement violation',
                    'Article location violation'))

# Count trials and calculate percentage of discarded trials, considering that  
# the experiment contained 48 trials per condition.

EEG_trial_count %>%
  summarise(discarded_trials = paste0(round(100 - (mean(n_trials) / 48 * 100), 2), '%'),
            kept_trials_mean = round(mean(n_trials), 2),
            kept_trials_SD = round(sd(n_trials), 2))

# Count trials across sessions and experimental conditions
EEG_trial_count %>%
  group_by(mini_language, session, grammatical_property, grammaticality) %>%
  reframe(discarded_trials = paste0(round(100 - (mean(n_trials) / 48 * 100), 2), '%'),
          kept_trials_mean = round(mean(n_trials), 2),
          kept_trials_SD = round(sd(n_trials), 2)) %>%
  rename('Mini-language' = mini_language,
         'Session' = session,
         'Grammatical property' = grammatical_property,
         'Grammaticality' = grammaticality,
         'Discarded trials' = discarded_trials,
         'Mean retained trials' = kept_trials_mean,
         'SD retained trials' = kept_trials_SD) %>%
  kable()

# Save to CSV file

write.csv(EEG_trial_count, 'data/EEG_trial_count_per_condition.csv', 
          row.names = FALSE, fileEncoding = 'UTF-8')

