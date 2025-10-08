# Function to merge trial-by-trial EEG data with other data (e.g., session progress,
# cognitive tasks, language history questionnaire) and compute relative session times.

merge_trialbytrial_EEG_data <-
  # Default file pattern catches all data
  function(EEG_file_pattern = "^\\d+_trialbytrial_S[123]_S10[123]\\.",
           min_time = -100, max_time = 1100,
           include_baseline = TRUE,
           aggregate_electrodes = FALSE,
           aggregate_time_points = FALSE,
           selected_macroregion = NULL) {
    require(dplyr)
    require(readxl)

    IDs_session_progress <-
      read.csv("data/Participant IDs and session progress.csv") %>%
      rename(mini_language = language) %>% # Rename for greater clarity
      # Remove session details to reduce data size
      select(-contains(c("inspector", "conductor", "supervision"))) %>%
      mutate(participant_lab_ID = as.factor(participant_lab_ID))

    source("data/importation and preprocessing/preprocess digit span task.R")
    source("data/importation and preprocessing/preprocess Stroop task.R")
    source("data/importation and preprocessing/preprocess alternating serial reaction time task.R")

    source("data/importation and preprocessing/preprocess Language History Questionnaire.R")

    source("data/R_functions/import_trialbytrial_EEG_data.R")
    trialbytrial_EEG_data <-
      import_trialbytrial_EEG_data(
        EEG_file_pattern = EEG_file_pattern,
        min_time = min_time,
        max_time = max_time,
        aggregate_electrodes = aggregate_electrodes,
        aggregate_time_points = aggregate_time_points,
        include_baseline = include_baseline,
        selected_macroregion = selected_macroregion
      )

    # Free unused memory
    gc()

    # Set factor levels
    trialbytrial_EEG_data <- trialbytrial_EEG_data %>%
      mutate(
        grammatical_property = factor(
          grammatical_property,
          levels = c(
            "Gender agreement", "Differential object marking",
            "Verb-object number agreement"
          )
        ),
        brain_region = factor(
          brain_region,
          levels = c(
            "left anterior", "midline anterior", "right anterior",
            "left medial", "midline medial", "right medial",
            "left posterior", "midline posterior", "right posterior"
          )
        ),
        session = factor(session, levels = c(2, 3, 4, 6))
      ) %>%
      # Combine the data frames based on participants' IDs

      mutate(participant_lab_ID = as.factor(participant_lab_ID)) %>%
      left_join(IDs_session_progress, by = "participant_lab_ID") %>%
      left_join(session1_digit_span, by = "participant_home_ID", relationship = "many-to-many") %>%
      left_join(session1_Stroop, by = "participant_home_ID", relationship = "many-to-many") %>%
      left_join(session1_ASRT, by = "participant_home_ID", relationship = "many-to-many") %>%
      left_join(LHQ3_aggregate_scores, by = "participant_LHQ3_ID", relationship = "many-to-many") %>%
      # Compute relative time in days for each session relative to Session 2.
      # Parse dates/times, handling both 'YYYY-MM-DD HH:MM' and 'YYYY-MM-DD' formats.
      # When times are available, compute days with up to 1 decimal place precision.

      mutate(
        # Parse Session 2 datetime (baseline)
        Session2_datetime_parsed = as.POSIXct(Session2_date_time, format = "%Y-%m-%d %H:%M", tz = "UTC"),

        # Parse Session 1 date (no time available)
        Session1_datetime_parsed = as.POSIXct(Session1_date, format = "%Y-%m-%d", tz = "UTC"),

        # Parse Session 3 datetime
        Session3_datetime_parsed = as.POSIXct(Session3_date_time, format = "%Y-%m-%d %H:%M", tz = "UTC"),

        # Parse Session 4 datetime
        Session4_datetime_parsed = as.POSIXct(Session4_date_time, format = "%Y-%m-%d %H:%M", tz = "UTC"),

        # Parse Session 5 date (no time available)
        Session5_datetime_parsed = as.POSIXct(Session5_date, format = "%Y-%m-%d", tz = "UTC"),

        # Parse Session 6 datetime
        Session6_datetime_parsed = as.POSIXct(Session6_date_time, format = "%Y-%m-%d %H:%M", tz = "UTC"),

        # Calculate time differences in days relative to Session 2 for all sessions
        # These are temporary variables used to compute the final days_from_session2
        Session2_days_temp = 0, # Session 2 is the baseline (0 days)

        Session3_days_temp = round(
          as.numeric(difftime(Session3_datetime_parsed, Session2_datetime_parsed, units = "days")),
          1
        ),
        Session4_days_temp = round(
          as.numeric(difftime(Session4_datetime_parsed, Session2_datetime_parsed, units = "days")),
          1
        ),
        Session6_days_temp = round(
          as.numeric(difftime(Session6_datetime_parsed, Session2_datetime_parsed, units = "days")),
          1
        ),

        # Create single variable: days_from_session2
        # Map each row's session number to its corresponding time difference
        days_from_session2 = case_when(
          session == 2 ~ Session2_days_temp,
          session == 3 ~ Session3_days_temp,
          session == 4 ~ Session4_days_temp,
          session == 6 ~ Session6_days_temp,
          TRUE ~ NA_real_
        )
      ) %>%
      # Remove temporary datetime parsing and session-specific day columns
      select(-ends_with("_datetime_parsed"), -ends_with("_days_temp")) %>%
      # Translate markers into labels and create time windows, brain regions, etc.
      mutate(

        # Recode dichotomous predictor (Brauer & Curtin, 2018; https://doi.org/10.1037/met0000159)
        recoded_mini_language = as.numeric(case_when(
          mini_language == "Mini-Norwegian" ~ 0.5,
          mini_language == "Mini-English" ~ -0.5,
          TRUE ~ NA_real_
        )),

        # Z-score continuous between-participants predictors (Brauer & Curtin, 2018;
        # https://doi.org/10.1037/met0000159).

        z_session1_digit_span = ifelse(is.na(session1_digit_span) | session1_digit_span == "", NA,
          as.numeric(scale(session1_digit_span))
        ),
        z_session1_Stroop = ifelse(is.na(session1_Stroop) | session1_Stroop == "", NA,
          as.numeric(scale(session1_Stroop))
        ),
        z_session1_ASRT = ifelse(is.na(session1_ASRT) | session1_ASRT == "", NA,
          as.numeric(scale(session1_ASRT))
        ),
        z_multilingual_language_diversity = ifelse(is.na(multilingual_language_diversity) |
          multilingual_language_diversity == "", NA,
        as.numeric(scale(multilingual_language_diversity))
        ),

        # Z-score relative session time variable (continuous between-participants predictor)
        # Standardized globally across all sessions and participants
        z_days_from_session2 = ifelse(is.na(days_from_session2), NA,
          as.numeric(scale(days_from_session2))
        ),

        # Convert character variables to factors
        across(where(is.character), as.factor)
      ) %>%
      # Order variables
      select(
        amplitude, participant_lab_ID, session, grammatical_property,
        grammaticality, sentence_marker, brain_region, macroregion,
        hemisphere, caudality, time_window, everything()
      ) %>%
      # Replace empty cells with NAs and remove rows that have NAs in all columns
      mutate(
        across(where(is.factor), as.character),
        across(where(is.character), ~ na_if(.x, "")),
        # Convert character variables to factors
        across(where(is.character), as.factor)
      ) %>%
      filter(if_any(everything(), ~ !is.na(.)))

    # Free unused memory
    gc()

    return(trialbytrial_EEG_data)
  }
