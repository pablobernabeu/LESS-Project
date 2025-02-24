
# Import and preprocess behavioural data from lab sessions

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(data.table)

# List all relevant .txt files in the directory
files <- list.files(pattern = "^subject-\\d+\\.csv$", 
                    full.names = TRUE, recursive = TRUE, 
                    path = 'data/raw data/behavioural data from lab sessions')

# Extract metadata from file paths and names
metadata <- tibble(
  file = files,
  participant_lab_ID = str_extract(basename(files), '(?<=subject-)[0-9]+'),
  session = str_extract(files, '(?<=Session )[0-9]+')
)

# Define a function to process a single file
process_file <- function(file, metadata_row) {
  raw_data <- read_csv(file, na = c("", "NA"), skip_empty_rows = TRUE, 
                       show_col_types = FALSE) %>%
    mutate(
      participant_lab_ID = as.integer(metadata_row$participant_lab_ID),
      session = metadata_row$session,
      # Turn every column into character to facilitate binding of files
      across(everything(), ~ as.character(.))
    )
  return(raw_data)
}

# Process all files and combine into a single data frame
behavioural_lab_data <- bind_rows(
  lapply( seq_len(nrow(metadata)), function(i) {
    process_file(file = metadata$file[i], metadata_row = metadata[i, ])
  } )
)

# Process variables
behavioural_lab_data <- behavioural_lab_data %>%
  mutate(
    grammatical_property = case_when(
      is.na(grammatical_property) & session == "2" ~ "Gender agreement",
      is.na(grammatical_property) & session == "3" ~ "Differential object marking",
      is.na(grammatical_property) & session == "4" ~ "Verb-object agreement",
      TRUE ~ grammatical_property  # Keep existing values unchanged
    ),
    grammatical_property = str_to_title(grammatical_property),  # Capitalize each word
    subject_id = as.numeric(subject_nr),
    grammaticality = recode(
      grammaticality,
      "article location violation" = "Article\nMisplacement",
      "number violation" = "Number\nViolation"
    )
  ) %>%
  distinct(trial, .keep_all = TRUE) %>%  # Removing duplicate trials
  mutate(across(everything(), as.character))  # Convert all columns to character for binding


# Standardise the values                               

behavioural_lab_data <- behavioural_lab_data %>%
  mutate(
    correct = as.numeric(as.character(correct)),
    across(c(language, session, grammaticality), as.factor)
  )

# Sort out the names
behavioural_lab_data <- behavioural_lab_data %>%
  mutate(
    grammaticality = case_when(
      grammaticality == "grammatical" ~ "Grammatical",
      grammaticality %in% c("DOM violation", "VOA violation", "gender violation") ~ "Ungrammatical",
      TRUE ~ grammaticality
    )
  )

# Add an accuracy column to create the accuracy data frame
behavioural_lab_data_accuracy <- behavioural_lab_data %>%
  group_by(subject_id, grammaticality, grammatical_property, session_part) %>%
  mutate(accuracy = mean(correct, na.rm = TRUE)) %>%  
  ungroup()
behavioural_lab_data$accuracy <- as.numeric(as.character(behavioural_lab_data$accuracy))

# Set limit to reaction times to create the RT data frame
behavioural_lab_data_RT <- behavioural_lab_data %>%
  filter(response_time > 200 & response_time < 4000) %>%
  mutate(response_time = as.numeric(as.character(response_time)))

# Define colour-coding per grammaticality to facilitate plotting later
grammaticality_colours <- c(
  "Grammatical" = "forestgreen",
  "Ungrammatical" = "firebrick1",
  "Number\nviolation" = "grey40",
  "Article\nmisplacement" = "steelblue4"
)
