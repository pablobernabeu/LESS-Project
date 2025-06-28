# Import and preprocess behavioural data from lab sessions, namely, from the test,
# the experiment and the final control tasks. This data does not include the data
# from the home sessions, which are preprocessed instead in other scripts in the
# current directory.

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(data.table)

# List all relevant .csv files in the directory
files <- list.files(
  pattern = "^subject-\\d+\\.csv$",
  path = "data/raw data/behavioural data from lab sessions",
  full.names = TRUE, recursive = TRUE
)

# Extract metadata from file paths and names
metadata <- tibble(
  file = files,
  participant_lab_ID = str_extract(basename(files), "(?<=subject-)[0-9]+"),
  session = str_extract(files, "(?<=Session )[0-9]+")
)

# Define a function to process a single file
process_file <- function(file, metadata_row) {
  raw_data <- read_csv(file,
    na = c("", "NA"), skip_empty_rows = TRUE,
    show_col_types = FALSE
  ) %>%
    mutate(
      participant_lab_ID = as.integer(metadata_row$participant_lab_ID),
      session = metadata_row$session,
      across(everything(), ~ as.character(.))
    )
  return(raw_data)
}

# Define colour-coding per grammaticality to facilitate plotting later
grammaticality_colours <- c(
  "Grammatical" = "forestgreen",
  "Ungrammatical" = "firebrick1",
  "Number\nviolation" = "grey40",
  "Article\nmisplacement" = "steelblue4"
)

# Process all files and combine into a single data frame
behavioural_lab_data <- bind_rows(
  lapply(seq_len(nrow(metadata)), function(i) {
    process_file(file = metadata$file[i], metadata_row = metadata[i, ])
  })
) %>%
  # Rename to avoid confusion
  rename(mini_language = language) %>%
  mutate(
    # In the logfiles from the test task, the grammatical property field was
    # unspecified. There is no uncertainty, however, as the tests for the
    # three grammatical properties were sequentially administered across
    # sessions, as detailed below.
    grammatical_property = case_when(
      is.na(grammatical_property) & session == "2" ~ "Gender agreement",
      is.na(grammatical_property) & session == "3" ~ "Differential object marking",
      is.na(grammatical_property) & session == "4" ~ "Verb-object agreement",
      TRUE ~ grammatical_property
    ),
    grammatical_property = str_to_sentence(grammatical_property),
    participant_lab_ID = as.numeric(subject_nr),
    grammaticality = recode(
      grammaticality,
      "article location violation" = "Article\nMisplacement",
      "number violation" = "Number\nViolation"
    )
  ) %>%
  mutate(
    correct = as.numeric(as.character(correct)),
    across(c(mini_language, session, grammaticality), as.factor),
    grammaticality = case_when(
      grammaticality == "grammatical" ~ "Grammatical",
      grammaticality %in% c(
        "DOM violation", "VOA violation",
        "gender violation"
      ) ~ "Ungrammatical",
      TRUE ~ grammaticality
    )
  ) %>%
  # Add an accuracy column to create the accuracy data frame
  group_by(
    participant_lab_ID, session, session_part,
    grammaticality, grammatical_property
  ) %>%
  mutate(accuracy = mean(correct, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(as.numeric(as.character(accuracy))) %>%
  # Set limit to reaction times to create the RT data frame
  filter(response_time > 200 & response_time < 4000) %>%
  mutate(response_time = as.numeric(as.character(response_time)))

# Remove duplicate trials per participant when trial != 0. This
# is necessary because trial numbers are sometimes repeated in
# the logfiles from OpenSesame.
behavioural_lab_data <- behavioural_lab_data %>%
  group_by(participant_lab_ID) %>%
  group_modify(~ bind_rows(
    filter(.x, trial == 0),
    filter(.x, trial != 0) %>% distinct(trial, .keep_all = TRUE)
  )) %>%
  ungroup()
