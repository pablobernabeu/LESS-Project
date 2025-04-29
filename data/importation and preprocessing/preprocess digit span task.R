
# Preprocess digit span forward and backward from Session 1

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)

# Path to files
path <- 'data/raw data/executive functions/Session 1'

# Function for coercing response column to character class
coerce_response_character <- function(file) {
  df <- readr::read_csv(file)
  if ('Response' %in% names(df)) {
    df$Response <- as.character(df$Response)
  }
  return(df)
}

# Read in all digit span files and combine them
session1_digit_span <- bind_rows(
  lapply(list.files(path = path, pattern = 'digit span', full.names = TRUE), 
         coerce_response_character)
) %>%
  
  # Rename columns
  rename(participant_home_ID = 'Participant Public ID',
         Zone.Type = 'Zone Type') %>%
  
  # Filter to rows containing the responses
  filter(Zone.Type == 'response_text_entry')

# Clean column names
colnames(session1_digit_span) <- make.names(colnames(session1_digit_span))

# Summarise results by participant
session1_digit_span <- session1_digit_span %>%
  group_by(participant_home_ID) %>%
  summarize(session1_digit_span = sum(Correct, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(participant_home_ID)) # Remove rows without participant ID
