

# Importing averaged EEG data

library(dplyr)
library(tidyr)
library(stringr)
library(stringr)
library(data.table)

# List all relevant .txt files in the directory
files <- list.files(pattern = "^\\d+_(S[123])_S10[123]\\.txt$", 
                    full.names = TRUE, recursive = TRUE, 
                    path = 'data/raw data/EEG')

# Step 1: Extract metadata from file paths and names
metadata <- tibble(
  file = files,
  participant_lab_ID = str_extract(basename(files), '^[0-9]+'),
  session = str_extract(files, '(?<=Session )[0-9]+'),
  grammatical_property = str_extract(basename(files), 'S[1-9]'),
  grammaticality = str_extract(basename(files), 'S[0-9]{3}')
)

# Step 2: Define a function to process a single file
process_file <- function(file, metadata_row) {
  
  # Read raw EEG data
  raw_data <- fread(file, sep = ' ', header = FALSE)
  
  # Extract electrode names (first column of the file)
  electrodes <- raw_data[[1]]
  
  # Remove the electrode column from raw data
  raw_data <- raw_data[, -1]
  
  # Get the number of columns once
  num_columns <- ncol(raw_data)
  
  # Generate the sequence of time points (integers only, no decimal points)
  time <- seq(-100, 1098, by = 2)
  time <- as.integer(time)
  
  # If the sequence length doesn't match the number of columns, adjust it
  if (length(time) != num_columns) {
    # Repeat the sequence until it matches the number of columns
    time <- rep(time, length.out = num_columns)
  }
  
  # Convert time to character to assign as column names
  colnames(raw_data) <- as.character(time)
  
  # Convert data to long format
  long_data <- as.data.frame(raw_data) %>%
    mutate(electrode = electrodes) %>%
    pivot_longer(cols = -electrode, 
                 names_to = 'time', 
                 values_to = 'amplitude') %>%
    
    mutate(
      participant_lab_ID = as.integer(metadata_row$participant_lab_ID),
      session = metadata_row$session,
      grammatical_property = metadata_row$grammatical_property,
      grammaticality = metadata_row$grammaticality,
      time = as.integer(time),
      
      # Convert character variables to factors
      across(where(is.character), as.factor)
    )
  
  return(long_data)
}

# Step 3: Process all files and combine into a single data frame
averaged_EEG_data <- bind_rows(
  lapply(seq_len(nrow(metadata)), function(i) {
    process_file(file = metadata$file[i], 
                 metadata_row = metadata[i, ])
  })
)

cat('Summary of the processed EEG data:\n')
print(summary(averaged_EEG_data))

# Free unused memory
gc()

