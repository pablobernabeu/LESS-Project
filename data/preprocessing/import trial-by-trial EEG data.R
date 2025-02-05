

# Import trial-by-trial EEG data

library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(purrr)


# Define file paths
general_file_pattern <- "^\\d+_trialbytrial_(S[123])_S10[123]\\."

# List all relevant .txt files in the directory
txt_files <- list.files(
  pattern = paste0(general_file_pattern, "txt$"), 
  full.names = TRUE, recursive = TRUE, 
  path = "data/raw data/EEG"
)

# List all relevant .vmrk files in the directory
vmrk_files <- list.files(
  pattern = paste0(general_file_pattern, "vmrk$"), 
  full.names = TRUE, recursive = TRUE, 
  path = "data/raw data/EEG"
)

# Pair txt and vmrk files by matching names (excluding extensions)
paired_files <- tibble(
  txt_file = txt_files,
  base_path = sub("\\.txt$", "", txt_files) # path without file ext
) %>%
  inner_join(
    tibble(
      vmrk_file = vmrk_files,
      base_path = sub("\\.vmrk$", "", vmrk_files) # path without file ext
    ),
    by = "base_path", 
  ) %>%
  mutate(
    participant_lab_ID = sub(".*Export\\/([0-9]+)_.*", "\\1", base_path),
    session = sub(".*Session (\\d).*", "\\1", base_path),
    grammatical_property = sub(".*_(S[1-3])_.*", "\\1", base_path),
    grammaticality = sub(".*_(S10[1-3])$", "\\1", base_path)
  )

# Initialize the output data frame
trialbytrial_EEG_data <- NULL

# Process each pair
for (i in seq_len(nrow(paired_files))) {
  
  txt_file <- paired_files$txt_file[i]
  vmrk_file <- paired_files$vmrk_file[i]
  
  # Extract markers from the .vmrk file
  markers <- readLines(vmrk_file)
  marker_start <- grep("\\[Marker Infos\\]", markers)
  
  if (length(marker_start) == 0) {
    stop("The [Marker Infos] section is missing in the .vmrk file.")
  }
  
  marker_infos <- markers[marker_start:length(markers)]
  sentence_markers <- marker_infos[grep("S1[1-9][0-9]|S2[0-4][0-9]|S25[0-3]", marker_infos)]
  
  if (length(sentence_markers) == 0) {
    stop("No valid sentence markers (S110–S253) found in the .vmrk file.")
  }
  
  sentence_markers <- gsub(".*S(\\d{3}).*", "\\1", sentence_markers)
  
  # Define hard-coded time points
  time_points <- seq(-100, 1098, by = 2)
  
  # Read the raw .txt file line by line
  raw_file_content <- readLines(txt_file)
  
  # Extract electrode names and data
  parsed_lines <- lapply(raw_file_content, function(line) strsplit(line, "\\s+")[[1]])
  electrode_names <- sapply(parsed_lines, `[[`, 1)  # First word of each line
  data_values <- do.call(rbind, lapply(parsed_lines, function(x) x[-1]))  # Remaining numbers
  
  # Check for parsing issues
  if (any(is.na(data_values))) {
    stop("NAs detected in data values. Check the .txt file for non-numeric content.")
  }
  
  # Ensure electrode names and data are aligned
  if (length(electrode_names) != nrow(data_values)) {
    stop("Mismatch between electrode names and data rows.")
  }
  
  # Validate the data structure
  expected_columns <- length(time_points) * length(sentence_markers)
  if (ncol(data_values) != expected_columns) {
    stop("Data structure mismatch: The number of columns does not align with trials and time points.")
  }
  
  # Reshape the data into a tidy format
  reshaped_data <- as.data.frame(data_values) %>%
    mutate(electrode = electrode_names) %>%
    pivot_longer(
      cols = -electrode,
      names_to = "placeholder_time_index", # removed below
      values_to = "amplitude"
    ) %>%
    select(-placeholder_time_index) %>%
    
    mutate(
      
      # Convert commas to points in amplitude values
      amplitude = as.numeric(gsub(",", ".", amplitude)),
      
      participant_lab_ID = as.factor(paired_files$participant_lab_ID[i]), 
      session = as.factor(paired_files$session[i]), 
      grammatical_property = as.factor(paired_files$grammatical_property[i]), 
      grammaticality = as.factor(paired_files$grammaticality[i]),
      time = rep(time_points, times = length(sentence_markers) * length(electrode_names)),
      sentence_marker = as.factor(rep(rep(sentence_markers, each = length(time_points)), 
                                      times = length(electrode_names))),
      
      # Convert character variables to factors
      across(where(is.character), as.factor)
    ) %>%
    
    # Order variables
    select(amplitude, participant_lab_ID, session, grammatical_property, 
           grammaticality, sentence_marker, electrode, time)
  
  # Incrementally append the reshaped data to the main output data frame
  trialbytrial_EEG_data <- bind_rows(trialbytrial_EEG_data, reshaped_data)
  
  # Print progress
  cat("Processed file:", txt_file, "\n")
}

cat('Summary of the processed EEG data:\n')
print(summary(trialbytrial_EEG_data))

# Free unused memory
gc()

