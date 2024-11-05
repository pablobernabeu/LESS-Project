

# Preprocessing digit span forward and backward

# 


library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)


# Path to files
path = "data/raw data/executive functions/Session 1"

# Read in and combine the files
digit_span = rbind( 
  read_csv(file.path(path, "digit span 1.csv")),
  read_csv(file.path(path, "digit span 2.csv")),
  read_csv(file.path(path, "digit span 3.csv")),
  read_csv(file.path(path, "qdvg4 digit span.csv")),
  read_csv(file.path(path, "wbij5 digit span.csv")),
  read_csv(file.path(path, "xqls8 digit span.csv")) 
) %>%
  
  # Rename columns
  rename(participant_home_ID = `Participant Public ID`) %>%
  
  # Convert string values to numeric where appropriate
  mutate(across(c(cumulative_RT, trial_number), as.numeric)) %>%
  replace_na(list(cumulative_RT = 0))



