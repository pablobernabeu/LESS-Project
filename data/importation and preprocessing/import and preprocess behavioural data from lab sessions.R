

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
  raw_data <- read_csv(file, na = c("", "NA"), skip_empty_rows = TRUE) %>%
    mutate(
      participant_lab_ID = as.integer(metadata_row$participant_lab_ID),
      session = metadata_row$session,
      # Turn every column into character to facilitate binding of files
      across(everything(), ~ as.character(.))
    )
  return(raw_data)
}

# Process all files and combine into a single data frame
behav_lab_data <- bind_rows(
  lapply( seq_len(nrow(metadata)), function(i) {
    process_file(file = metadata$file[i], metadata_row = metadata[i, ])
  } )
)

# Process variables
behav_lab_data <- behav_lab_data %>% mutate(
  
  grammatical_property = case_when(
    session_part == 'Test' & session == '2' ~ 'Gender agreement',
    session_part == 'Test' & session == '3' ~ 'Differential object marking',
    session_part == 'Test' & session == '4' ~ 'Verb-object number agreement',
    .default = grammatical_property
  ),
  
  # Turn every column into character to facilitate binding of files
  across(everything(), ~ as.character(.))
)


