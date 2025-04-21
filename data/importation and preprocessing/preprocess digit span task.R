
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

# Read in and combine the files
session1_digit_span <- bind_rows( 
  coerce_response_character(file.path(path, 'digit span 1.csv')),
  coerce_response_character(file.path(path, 'digit span 2.csv')),
  coerce_response_character(file.path(path, 'digit span 3.csv')),
  coerce_response_character(file.path(path, 'qdvg4 digit span.csv')),
  coerce_response_character(file.path(path, 'wbij5 digit span.csv')),
  coerce_response_character(file.path(path, 'xqls8 digit span.csv'))
) %>%
  
  # Rename columns
  rename(participant_home_ID = `Participant Public ID`)

# Clear column names
colnames(session1_digit_span) <- make.names(colnames(session1_digit_span))

session1_digit_span <- session1_digit_span %>%
  mutate(display = as.factor(display)) %>%
  group_by(participant_home_ID) %>%
  summarize(session1_digit_span = sum(Correct)) %>%
  ungroup()
