

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
  base_name = tools::file_path_sans_ext(basename(txt_files))
) %>%
  inner_join(
    tibble(
      vmrk_file = vmrk_files,
      base_name = tools::file_path_sans_ext(basename(vmrk_files))
    ),
    by = "base_name"
  )

# Initialize output object to store results
output_data <- list()

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
  sentence_markers <- marker_infos[grep("S1(10|[1-9][1-9]|2[0-5][0-3])", marker_infos)]
  
  if (length(sentence_markers) == 0) {
    stop("No valid sentence markers (S110–S253) found in the .vmrk file.")
  }
  
  trial_names <- gsub(".*S(\\d{3}).*", "S\\1", sentence_markers)
  
  # Define hard-coded time points
  time_points <- seq(-100, 1098, by = 2)
  if (length(time_points) != 600) stop("Time points must include exactly 600 values.")
  
  # Read the raw .txt file line by line
  raw_file_content <- readLines(txt_file)
  
  # Extract electrode names and data
  parsed_lines <- lapply(raw_file_content, function(line) strsplit(line, "\\s+")[[1]])
  electrode_names <- sapply(parsed_lines, `[[`, 1)  # First word of each line
  data_values <- do.call(rbind, lapply(parsed_lines, function(parts) as.numeric(parts[-1])))  # Remaining numbers
  
  # Check for parsing issues
  if (any(is.na(data_values))) {
    stop("NAs detected in data values. Check the .txt file for non-numeric content.")
  }
  
  # Ensure electrode names and data are aligned
  if (length(electrode_names) != nrow(data_values)) {
    stop("Mismatch between electrode names and data rows.")
  }
  
  # Validate the data structure
  expected_columns <- 600 * length(trial_names)
  if (ncol(data_values) != expected_columns) {
    stop("Data structure mismatch: The number of columns does not align with trials and time points.")
  }
  
  # Reshape the data into a tidy format
  reshaped_data <- as.data.frame(data_values) %>%
    mutate(electrode = electrode_names) %>%
    pivot_longer(
      cols = -electrode,
      names_to = "time_index",
      values_to = "amplitude"
    ) %>%
    mutate(
      time_point = rep(time_points, times = length(trial_names) * length(electrode_names)),
      trial = rep(rep(trial_names, each = 600), times = length(electrode_names))
    )
  
  # Append to output data
  output_data[[i]] <- reshaped_data
  cat("Processed file:", txt_file, "\n")
}

# Combine all processed data into a single data frame
final_data <- bind_rows(output_data)

# Return the final data
gc()  # Trigger garbage collection to free up memory
final_data
