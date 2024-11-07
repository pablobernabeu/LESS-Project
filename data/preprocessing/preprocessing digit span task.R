

# Preprocessing digit span forward and backward


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
  rename(participant_home_ID = `Participant Public ID`)

# Clear column names
colnames(digit_span) = make.names(colnames(digit_span))

digit_span = digit_span %>%
  mutate(display = as.factor(display)) %>%
  group_by(participant_home_ID) %>%
  summarize(DGS_total = sum(Correct))

