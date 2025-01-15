

# Importing trial-by-trial EEG data

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(data.table)

# List all relevant .txt files in the directory
txt_files = list.files(pattern = "^\\d+_trialbytrial_(S[123])_S10[123]\\.txt$", full.names = TRUE, 
                       recursive = TRUE, path = 'data/raw data/EEG')

# Extract metadata from .txt files
txt_metadata <- tibble(
  file = txt_files,
  participant_lab_ID = str_extract(basename(txt_files), '^[0-9]+'),
  session = str_extract(txt_files, '(?<=Session )[0-9]+'),
  grammatical_property = str_extract(basename(txt_files), 'S[1-9]'),
  grammaticality = str_extract(basename(txt_files), 'S[0-9]{3}')
)

# List all relevant .vmrk files in the directory
vmrk_files = list.files(pattern = '^\\d.*.vmrk', full.names = TRUE, 
                        recursive = TRUE, path = 'data/raw data/EEG')

# Extract metadata from .vmrk files
vmrk_metadata <- tibble(
  file = vmrk_files,
  participant_lab_ID = str_extract(basename(vmrk_files), '^[0-9]+'),
  session = str_extract(vmrk_files, '(?<=Session )[0-9]+')
)

# Identify related .vmrk files
truncated_files <- vmrk_metadata %>%
  group_by(participant_lab_ID, session) %>%
  filter(n() > 1) %>%
  arrange(nchar(file)) %>%
  summarise(
    first_part = file[1],
    second_part = file[2],
    .groups = "drop"
  )

# Match .txt files with their corresponding .vmrk files.
# Match on `participant_lab_ID` and `session`
txt_metadata <- txt_metadata %>%
  left_join(vmrk_metadata, by = c("participant_lab_ID", "session"), 
            suffix = c(".txt", ".vmrk"), relationship = 'many-to-many')

# Ensure every .txt file has a match
if (any(is.na(txt_metadata$file.vmrk))) {
  stop("Some .txt files could not be matched with a .vmrk file!")
}

# Define a function to process matched .txt and .vmrk files

process_file <- function(txt_file, vmrk_file, metadata_row) {
  
  # Check if the vmrk_file is part of truncated_files
  is_truncated <- vmrk_file %in% c(truncated_files$first_part, truncated_files$second_part)
  
  if (is_truncated) {
    # Find the corresponding pair
    truncated_pair <- truncated_files %>%
      filter(first_part == vmrk_file | second_part == vmrk_file) %>%
      slice(1)
    
    # Read both parts of the truncated file
    first_content <- readLines(truncated_pair$first_part)
    second_content <- readLines(truncated_pair$second_part)
    
    # Combine the two parts
    markers <- c(first_content, second_content)
  } else {
    # Read the standard file
    markers <- readLines(vmrk_file)
  }
  
  # Extract markers from the combined lines
  marker_infos <- markers[grep("\\[Marker Infos\\]", markers):length(markers)]
  matches <- grep("S ?(1[1-9][0-9]|2[0-4][0-9]|25[0-3])", marker_infos, value = TRUE)
  extracted_strings <- gsub(".*(S ?(1[1-9][0-9]|2[0-4][0-9]|25[0-3])).*", "\\1", matches)
  sentence_markers <- gsub(" ", "", extracted_strings)
  
  # Read raw EEG data from the .txt file
  raw_data <- fread(txt_file, sep = ' ', header = FALSE)
  electrodes <- raw_data[[1]]
  raw_data <- raw_data[, -1]
  time <- seq(-100, 1098, by = 2)
  
  if (ncol(raw_data) != length(time)) {
    time <- rep(time, length.out = ncol(raw_data))
  }
  
  colnames(raw_data) <- as.character(time)
  
  # Determine the number of trials and electrodes
  num_trials <- length(sentence_markers)  # Number of trials matches sentence markers
  num_time_points <- length(seq(-100, 1098, by = 2))  # Time points per electrode
  num_electrodes <- length(electrodes)
  
  # Calculate total rows and assign trial and electrode sequences
  total_rows <- num_trials * num_electrodes * num_time_points
  trial_sequence <- rep(rep(1:num_trials, each = num_electrodes * num_time_points), times = 1)
  electrode_sequence <- rep(rep(1:num_electrodes, each = num_time_points), times = num_trials)
  
  # Map trial numbers to trial names
  trial_names <- sentence_markers[trial_sequence]
  
  # Convert data to long format
  long_data <- as.data.frame(raw_data) %>%
    mutate(electrode = electrodes) %>%
    pivot_longer(cols = -electrode, names_to = "time", values_to = "amplitude") %>%
    mutate(
      trial = trial_sequence[1:nrow(.)],  # Assign trial sequence
      trial_name = trial_names[1:nrow(.)],  # Assign trial names
      electrode = electrode_sequence[1:nrow(.)],  # Assign electrode sequence
      participant_lab_ID = metadata_row$participant_lab_ID,
      session = metadata_row$session,
      grammatical_property = metadata_row$grammatical_property,
      grammaticality = metadata_row$grammaticality,
      time = as.integer(time),
      time_window = case_when(
        time >= 200 & time <= 500 ~ '200_500',
        time >= 300 & time <= 600 ~ '300_600',
        time >= 400 & time <= 900 ~ '400_900',
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
      )
    )
  
  return(long_data)
}

# Process all matched files
ERP_data <- bind_rows(
  lapply(seq_len(nrow(txt_metadata)), function(i) {
    process_file( txt_metadata$file.txt[i], 
                  txt_metadata$file.vmrk[i], 
                  txt_metadata[i, ] )
  })
)

# Save and summarize the data
cat("Summary of the processed ERP data:\n")
print(summary(ERP_data))

