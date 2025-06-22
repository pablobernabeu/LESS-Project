

# Descriptive information on trial-by-trial EEG data

source('data/R_functions/import_trialbytrial_EEG_data.R')

trialbytrial_EEG_data <- 
  import_trialbytrial_EEG_data(
    EEG_file_pattern = '^\\d+_trialbytrial_S[123]_S10[123]\\.',
    min_time = -100, max_time = 1100,
    include_baseline = TRUE,
    aggregate_electrodes = FALSE,
    aggregate_time_points = FALSE,
    selected_macroregion = NULL
  )

# Count trials across data sections

trialbytrial_EEG_data_temp %>%
  group_by(session, participant_lab_ID, grammatical_property, grammaticality) %>%
  summarise(trials = n_distinct(sentence_marker)) %>%
  summarise(min_trials = min(trials), max_trials = max(trials),
            mean_trials = mean(trials)) %>%
  saveRDS('data/EEG_trial_count.rds')

