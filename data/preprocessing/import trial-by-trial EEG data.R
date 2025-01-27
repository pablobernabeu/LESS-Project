
# Load required libraries
library(dplyr)
library(tidyr)
library(data.table)
library(purrr)

# Define file paths
general_file_pattern <- "^\\d+_trialbytrial_(S[123])_S10[123]\\."

# List all relevant .txt files in the directory
txt_files <- list.files(
  pattern = paste0(general_file_pattern, "txt$"),
  full.names = TRUE,
  recursive = TRUE,
  path = "data/raw data/EEG"
)

# List all relevant .vmrk files in the directory
vmrk_files <- list.files(
  pattern = paste0(general_file_pattern, "vmrk$"),
  full.names = TRUE,
  recursive = TRUE,
  path = "data/raw data/EEG"
)

# Pair files by matching names (excluding extensions)
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
    grammatical_property = sub(".*_(S[1-3])_.*", "\\1", base_path),
    grammaticality = sub(".*_(S10[1-3])$", "\\1", base_path),
    
    # Translate markers to linguistic labels (see https://osf.io/974k8)
    
    grammatical_property = case_when(
      grammatical_property == 'S1' ~ 'Gender agreement', 
      grammatical_property == 'S2' ~ 'Differential object marking', 
      grammatical_property == 'S3' ~ 'Verb-object number agreement',
      .default = grammatical_property),
    
    grammaticality = case_when(
      grammaticality == 'S101' ~ 'Grammatical', 
      grammaticality == 'S102' ~ 'Ungrammatical', 
      grammaticality == 'S103' ~ 'Ancillary violation',
      .default = grammaticality)
  )

# Initialize output object to store results
output_data <- list()

# Initialize the output data frame
trialbytrial_EEG_data <- NULL

# Process each pair
for (i in seq_len(nrow(paired_files))) {
  
  txt_file <- paired_files$txt_file[i]
  vmrk_file <- paired_files$vmrk_file[i]
  grammatical_property <- paired_files$grammatical_property[i]
  grammaticality <- paired_files$grammaticality[i]
  
  # Extract markers from the .vmrk file
  markers <- readLines(vmrk_file)
  marker_start <- grep("\\[Marker Infos\\]", markers)
  
  if (length(marker_start) == 0) {
    stop("The [Marker Infos] section is missing in the .vmrk file.")
  }
  
  marker_infos <- markers[marker_start:length(markers)]
  sentence_marker <- marker_infos[grep("S1[1-9][0-9]|S2[0-4][0-9]|S25[0-3]", marker_infos)]
  
  if (length(sentence_marker) == 0) {
    stop("No valid sentence markers (S110–S253) found in the .vmrk file.")
  }
  
  sentence_marker <- gsub(".*S(\\d{3}).*", "\\1", sentence_marker)
  
  # Define hard-coded time points
  time_points <- seq(-100, 1098, by = 2)
  if (length(time_points) != 600) stop("Time points must include exactly 600 values.")
  
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
  expected_columns <- 600 * length(sentence_marker)
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
      amplitude = as.numeric(amplitude),
      grammatical_property = as.factor(grammatical_property),
      grammaticality = as.factor(grammaticality),
      sentence_marker = as.factor(rep(rep(sentence_marker, each = 600), 
                                      times = length(electrode_names))),
      electrode = as.factor(electrode),
      time = as.factor(
        rep(time_points, 
            times = length(sentence_marker) * length(electrode_names))
      ),
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
      )
    ) %>%
    # Order variables
    select(amplitude, grammatical_property, grammaticality, sentence_marker, 
           electrode, region, time, time_window)
  
  # Incrementally append the reshaped data to the main output data frame
  trialbytrial_EEG_data <- bind_rows(trialbytrial_EEG_data, reshaped_data)
  
  # Print progress
  cat("Processed file:", txt_file, "\n")
}

# The resulting data is stored in trialbytrial_EEG_data

cat('Summary of the processed EEG data:\n')
print(summary(trialbytrial_EEG_data))

# Free unused memory
gc()

