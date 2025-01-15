

# Importing averaged EEG data

library(dplyr)
library(tidyr)
library(stringr)
library(data.table)


# List all relevant .txt files in the directory
txt_files = list.files(pattern = "^\\d+_(S[123])_S10[123]\\.txt$", full.names = TRUE, 
                       recursive = TRUE, path = 'data/raw data/EEG')

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
  time <- as.integer(time)  # Ensure the sequence is integer-based from the start
  
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
    pivot_longer(cols = -electrode, names_to = 'time', values_to = 'amplitude') %>%
    
    mutate(
      participant_lab_ID = as.integer(metadata_row$participant_lab_ID),
      session = metadata_row$session,
      grammatical_property = metadata_row$grammatical_property,
      grammaticality = metadata_row$grammaticality,
      time = as.integer(time),  # Ensure time is integer
      time_window = case_when(
        time >= 200 & time <= 498 ~ '200_500',
        time >= 300 & time <= 598 ~ '300_600',
        time >= 400 & time <= 898 ~ '400_900',
        TRUE ~ NA_character_
      ),
      region = case_when(
        electrode %in% c('T7', 'C3', 'CP5') ~ 'left medial',
        electrode %in% c('T8', 'C4', 'CP6') ~ 'right medial',
        electrode %in% c('Fp1', 'F3', 'F7', 'FT9', 'FC5') ~ 'left anterior',
        electrode %in% c('Fp2', 'F4', 'F8', 'FT10', 'FC6') ~ 'right anterior',
        electrode %in% c('P7', 'P3', 'O1') ~ 'left posterior',
        electrode %in% c('P8', 'P4', 'O2') ~ 'right posterior',
        electrode %in% c('Fz', 'FC1', 'FC2') ~ 'midline anterior',
        electrode %in% c('Cz', 'CP1', 'CP2') ~ 'midline medial',
        electrode %in% c('Pz', 'Oz') ~ 'midline posterior',
        TRUE ~ NA_character_
      ),
      
      # Translate triggers to linguistic labels (see https://osf.io/974k8)
      
      grammatical_property = 
        case_when(grammatical_property == 'S1' ~ 'Gender agreement', 
                  grammatical_property == 'S2' ~ 'Differential object marking', 
                  grammatical_property == 'S3' ~ 'Verb-object agreement'),
      
      grammaticality = 
        case_when(grammaticality == 'S101' ~ 'Grammatical', 
                  grammaticality == 'S102' ~ 'Ungrammatical', 
                  grammaticality == 'S103' ~ 'Control violation')
    )
  
  return(long_data)
}

# Step 3: Process all files and combine into a single data frame
ERP_data <- bind_rows(
  lapply(seq_len(nrow(metadata)), function(i) {
    process_file(metadata$file[i], metadata[i, ])
  })
)

cat('Summary of the processed ERP data:\n')
print(summary(ERP_data))

# Free up memory
gc()

