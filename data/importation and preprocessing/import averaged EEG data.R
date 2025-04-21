

# Importing averaged EEG data

library(dplyr)
library(tidyr)
library(stringr)
library(data.table)

# List all relevant .txt files in the directory
files <- list.files(pattern = "^\\d+_(S[123])_S10[123]\\.txt$", 
                    full.names = TRUE, recursive = TRUE, 
                    path = 'data/raw data/EEG')

# Extract metadata from file paths and names
metadata <- tibble(
  file = files,
  participant_lab_ID = str_extract(basename(files), '^[0-9]+'),
  session = str_extract(files, '(?<=Session )[0-9]+'),
  grammatical_property = str_extract(basename(files), 'S[1-9]'),
  grammaticality = str_extract(basename(files), 'S[0-9]{3}')
)

# Define a function to process a single file
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

# Process all files and combine into a single data frame
averaged_EEG_data <- bind_rows(
  lapply(seq_len(nrow(metadata)), function(i) {
    process_file(file = metadata$file[i], 
                 metadata_row = metadata[i, ])
  })
)

# Translate markers into labels and create time windows, brain regions, etc.
averaged_EEG_data <- averaged_EEG_data %>%
  mutate(
    
    grammatical_property = case_when(
      grammatical_property == 'S1' ~ 'Gender agreement', 
      grammatical_property == 'S2' ~ 'Differential object marking', 
      grammatical_property == 'S3' ~ 'Verb-object number agreement',
      .default = grammatical_property),
    
    grammaticality = as.character(case_when(
      grammaticality == 'S101' ~ 'Grammatical', 
      grammaticality == 'S102' ~ 'Ungrammatical', 
      
      # Ancillary violation conditions varying by grammatical property
      
      grammatical_property == 'Gender agreement' & 
        grammaticality == 'S103' ~ 'Number agreement violation',
      
      !grammatical_property == 'Gender agreement' & 
        grammaticality == 'S103' ~ 'Article location violation',
      
      TRUE ~ NA_character_
    )),
    
    time_window = case_when(
      time >= 200 & time <= 498 ~ '200_500',
      time >= 300 & time <= 598 ~ '300_600',
      time >= 400 & time <= 898 ~ '400_900',
      TRUE ~ NA_character_
    ),
    brain_region = case_when(
      electrode %in% c('Fp1', 'F3', 'F7', 'FT9', 'FC5') ~ 'left anterior',
      electrode %in% c('Fp2', 'F4', 'F8', 'FT10', 'FC6') ~ 'right anterior',
      electrode %in% c('T7', 'C3', 'CP5') ~ 'left medial',
      electrode %in% c('T8', 'C4', 'CP6') ~ 'right medial',
      electrode %in% c('P7', 'P3', 'O1') ~ 'left posterior',
      electrode %in% c('P8', 'P4', 'O2') ~ 'right posterior',
      electrode %in% c('Fz', 'FC1', 'FC2') ~ 'midline anterior',
      electrode %in% c('Cz', 'CP1', 'CP2') ~ 'midline medial',
      electrode %in% c('Pz', 'Oz') ~ 'midline posterior',
      TRUE ~ NA_character_
    ),
    hemisphere = case_when(
      str_detect(brain_region, 'left') ~ 'left',
      str_detect(brain_region, 'right') ~ 'right',
      TRUE ~ NA_character_
    ),
    caudality = case_when(
      str_detect(brain_region, 'anterior') ~ 'anterior',
      str_detect(brain_region, 'medial') ~ 'medial',
      str_detect(brain_region, 'posterior') ~ 'posterior',
      TRUE ~ NA_character_
    ),
    
    # Recode dichotomous predictors (Brauer & Curtin, 2018; https://doi.org/10.1037/met0000159).
    # Session is specifically recoded following Michael Clark's recommendation at 
    # https://m-clark.github.io/sem/growth-curves.html#numbering-the-time-points.
    
    recoded_grammaticality = case_when(
      grammaticality == 'Grammatical' ~ 0.5,
      grammaticality == 'Number agreement violation' ~ 0,
      grammaticality == 'Article location violation' ~ 0,
      grammaticality == 'Ungrammatical' ~ -0.5,
      TRUE ~ NA_real_
    ),
    recoded_session = case_when(
      session == 2 ~ 0,
      session == 3 ~ 1,
      session == 4 ~ 2,
      session == 6 ~ 3,
      TRUE ~ NA_real_
    ),
    recoded_hemisphere = case_when(
      hemisphere == 'left' ~ -0.5,
      hemisphere == 'right' ~ 0.5,
      TRUE ~ NA_real_
    ),
    recoded_caudality = case_when(
      caudality == 'anterior' ~ -0.5,
      caudality == 'medial' ~ 0,
      caudality == 'posterior' ~ 0.5,
      TRUE ~ NA_real_
    ),
    
    # Set factor levels
    session = factor(session, levels = c(2, 3, 4, 6)),
    grammatical_property = factor(
      grammatical_property, 
      levels = c('Gender agreement', 'Differential object marking', 
                 'Verb-object number agreement')
    ),
    brain_region = factor(
      brain_region, 
      levels = c('left anterior', 'midline anterior', 'right anterior', 
                 'left medial', 'midline medial', 'right medial', 
                 'left posterior', 'midline posterior', 'right posterior')
    )
  )

cat('Summary of the processed EEG data:\n')
print(summary(averaged_EEG_data))

# Free unused memory
gc()

