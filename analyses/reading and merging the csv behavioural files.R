# Function to extract the number that corresponds to each participant after "subject-"
extract_number <- function(filename) {
  match <- gsub(".*subject-([0-9]+).*", "\\1", basename(filename))  # Extract number
  if (match == basename(filename)) return(NA)  # If no match, return NA
  return(as.numeric(match))  # Convert to numeric
}


#Session 2
# List all CSV files in the directory
Session2_file_list <- list.files(path = "data/raw data/EEG/Session 2/Raw", pattern = "*.csv", full.names = TRUE)

# Loop through each file and fix the first line
for (file in Session2_file_list) {
  file_content <- readLines(file)  # Read the file as text
  
  file_content[1] <- gsub("\\.", ",", file_content[1])  # Fix only the header row
  
  writeLines(file_content, file)  # Overwrite the file with corrected header
}


# Check extracted subject numbers (for debugging)
Session2_subject_ids <- sapply(Session2_file_list, extract_number)
print(Session2_subject_ids)  # Print extracted IDs to check correctness

# Read all files into a list and add the extracted number as a column
Session2_df_list <- lapply(seq_along(Session2_file_list), function(i) {
  df <- read.csv(Session2_file_list[i], stringsAsFactors = FALSE)
  df$subject_id <- Session2_subject_ids[i]  # Add extracted number as a new column
  return(df)
})

# Find all unique column names across files
all_columns <- unique(unlist(lapply(Session2_df_list, colnames)))

# Ensure all data frames have the same columns
Session2_df_list <- lapply(Session2_df_list, function(df) {
  missing_cols <- setdiff(all_columns, colnames(df))
  df[missing_cols] <- NA  # Add missing columns with NA
  df <- df[all_columns]   # Reorder columns to maintain consistency
  return(df)
})

# Combine all data frames
Session2_final_df <- do.call(rbind, Session2_df_list)


View(Session2_final_df)
colnames(Session2_final_df)

#Session 3

Session3_file_list <- list.files(path = "data/raw data/EEG/Session 3/Raw", pattern = "*.csv", full.names = TRUE)

for (file in Session3_file_list) {
  file_content <- readLines(file)  # Read the file as text
  
  file_content[1] <- gsub("\\.", ",", file_content[1])  # Fix only the header row
  
  writeLines(file_content, file)  # Overwrite the file with corrected header
}

Session3_subject_ids <- sapply(Session3_file_list, extract_number)
print(Session3_subject_ids)  # Print extracted IDs to check correctness

Session3_df_list <- lapply(seq_along(Session3_file_list), function(i) {
  Session3_df <- read.csv(Session3_file_list[i], stringsAsFactors = FALSE)
  Session3_df$subject_id <- Session3_subject_ids[i]  # Add extracted number as a new column
  return(Session3_df)
})

all_columns <- unique(unlist(lapply(Session3_df_list, colnames)))

Session3_df_list <- lapply(Session3_df_list, function(Session3_df) {
  Session3_missing_cols <- setdiff(all_columns, colnames(Session3_df))
  Session3_df[Session3_missing_cols] <- NA  # Add missing columns with NA
  Session3_df <- Session3_df[all_columns]   # Reorder columns to maintain consistency
  return(Session3_df)
})

Session3_final_df <- do.call(rbind, Session3_df_list)


colnames(Session3_final_df)










# Session 4

Session4_file_list <- list.files(path = "data/raw data/EEG/Session 4/Raw", pattern = "*.csv", full.names = TRUE)


for (file in Session4_file_list) {
  file_content <- readLines(file)  # Read the file as text
  
  file_content[1] <- gsub("\\.", ",", file_content[1])  # Fix only the header row
  
  writeLines(file_content, file)  # Overwrite the file with corrected header
}

Session4_subject_ids <- sapply(Session4_file_list, extract_number)
print(Session4_subject_ids)  # Print extracted IDs to check correctness

Session4_df_list <- lapply(seq_along(Session4_file_list), function(i) {
  Session4_df <- read.csv(Session4_file_list[i], stringsAsFactors = FALSE)
  Session4_df$subject_id <- Session4_subject_ids[i]  # Add extracted number as a new column
  return(Session4_df)
})

all_columns <- unique(unlist(lapply(Session4_df_list, colnames)))

Session4_df_list <- lapply(Session4_df_list, function(Session4_df) {
  Session4_missing_cols <- setdiff(all_columns, colnames(Session4_df))
  Session4_df[Session4_missing_cols] <- NA  # Add missing columns with NA
  Session4_df <- Session4_df[all_columns]   # Reorder columns to maintain consistency
  return(Session4_df)
})

Session4_final_df <- do.call(rbind, Session4_df_list)
colnames(Session4_final_df)



# Session 6

Session6_file_list <- list.files(path = "data/raw data/EEG/Session 6/Raw", 
                                 pattern = "*.csv", full.names = TRUE)

for (file in Session6_file_list) {
  file_content <- readLines(file)  # Read the file as text
  
  file_content[1] <- gsub("\\.", ",", file_content[1])  # Fix only the header row
  
  writeLines(file_content, file)  # Overwrite the file with corrected header
}

Session6_subject_ids <- sapply(Session6_file_list, extract_number)
print(Session6_subject_ids)  # Print extracted IDs to check correctness
Session6_df_list <- lapply(seq_along(Session6_file_list), function(i) {
  Session6_df <- read.csv(Session6_file_list[i], stringsAsFactors = FALSE)
  Session6_df$subject_id <- Session6_subject_ids[i] 
  return(Session6_df)
})

all_columns <- unique(unlist(lapply(Session6_df_list, colnames)))

Session6_df_list <- lapply(Session6_df_list, function(Session6_df) {
  Session6_missing_cols <- setdiff(all_columns, colnames(Session6_df))
  Session6_df[Session6_missing_cols] <- NA  # Add missing columns with NA
  Session6_df <- Session6_df[all_columns]   # Reorder columns to maintain consistency
  return(Session6_df)
})
Session6_final_df <- do.call(rbind, Session6_df_list)

colnames(Session6_final_df)

View(Session6_final_df)





library(dplyr)
library(tidyr)

# Function to convert all columns to character
convert_to_character <- function(df) {
  df %>% mutate(across(everything(), as.character))
}

# Apply the function to each data frame
Session2_final_df <- convert_to_character(Session2_final_df)
Session3_final_df <- convert_to_character(Session3_final_df)
Session4_final_df <- convert_to_character(Session4_final_df)
Session6_final_df <- convert_to_character(Session6_final_df)

# Bind rows after conversion
combined_df <- bind_rows(
  Session2_final_df, 
  Session3_final_df, 
  Session4_final_df, 
  Session6_final_df, 
  .id = "session"
)
View(combined_df)