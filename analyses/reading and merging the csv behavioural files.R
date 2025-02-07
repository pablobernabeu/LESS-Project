library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)


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
Session2_final_df$Session <- "1st session"


#Session 3

Session3_file_list <- list.files(path = "data/raw data/EEG/Session 3/Raw", pattern = "*.csv", full.names = TRUE)

for (file in Session3_file_list) {
  file_content <- readLines(file)  # Read the file as text
  
  file_content[1] <- gsub("\\.", ",", file_content[1])  # Fix only the header row
  
  writeLines(file_content, file)  # Overwrite the file with corrected header
}

Session3_subject_ids <- sapply(Session3_file_list, extract_number)

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

Session3_final_df$Session <- "2nd Session"


# Session 4

Session4_file_list <- list.files(path = "data/raw data/EEG/Session 4/Raw", pattern = "*.csv", full.names = TRUE)


for (file in Session4_file_list) {
  file_content <- readLines(file)  # Read the file as text
  
  file_content[1] <- gsub("\\.", ",", file_content[1])  # Fix only the header row
  
  writeLines(file_content, file)  # Overwrite the file with corrected header
}

Session4_subject_ids <- sapply(Session4_file_list, extract_number)

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
Session4_final_df$Session <- "3rd Session"


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
Session6_final_df$Session <- "4th Session"

# combining the different session to make a joint data frame

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
print(combined_df)

clean_combined_df <- combined_df[, c("grammaticality", "grammatical_property",  
             "session_part", "trial", "sentence_1", "sentence_2", "correct", "subject_id", "Session")]


#splitting the participants into mini-language groups

clean_combined_df <- clean_combined_df %>%
  mutate(
    subject_id = as.numeric(subject_id),  # Ensure subject_id is numeric
    `Mini language` = ifelse(subject_id %% 2 == 1, "Mini-English", "Mini-Norwegian")
  )

#removing duplicate trials
clean_combined_df <- clean_combined_df %>%
  filter(trial != lag(trial, default = first(trial)))


#View(clean_combined_df)

# Create a new data frame with only "gender agreement" in grammatical_property
Gender_agreement_df <- clean_combined_df %>%
  filter(grammatical_property == "gender agreement", session_part == "Experiment")

#grouping the two ungrammatical conditions
Gender_agreement_df$grammaticality[Gender_agreement_df$grammaticality %in% c("gender violation", "number violation")] <- "ungrammatical"
#View (Gender_agreement_df)


Verb_object_agreement_df <- clean_combined_df %>%
  filter(grammatical_property == "verb-object agreement")
#View (Verb_object_agreement_df)

Differential_object_marking_df <- clean_combined_df %>%
  filter(grammatical_property == "differential object marking")
#View (Verb_object_agreement_df)

# Convert 'correct' column to numeric if it's not already
Gender_agreement_df$correct <- as.numeric(as.character(Gender_agreement_df$correct))
Gender_agreement_df$"Mini language" <- as.factor(Gender_agreement_df$"Mini language")
Gender_agreement_df$Session <- as.factor(Gender_agreement_df$Session)
Gender_agreement_df$grammaticality <- as.factor(Gender_agreement_df$grammaticality)


##############################################################

# separate norwegian group data frame
Norwegian_df <- Gender_agreement_df %>%
  filter(`Mini language` == "Mini-Norwegian")
# Ensure that Session is a factor
Norwegian_df$Session <- factor(Norwegian_df$Session, levels = c("1st session", "2nd Session", "3rd Session", "4th Session"))

#View(Norwegian_df)


##################
library(ggplot2)
library(scales)

# Assuming "correct" is numeric and in the range 0 (incorrect) and 1 (correct)

ggplot(Norwegian_df, aes(x = Session, y = correct, fill = grammaticality)) +
  geom_violin(trim = FALSE, scale = "area") +  # Violin plot for distribution
  stat_summary(fun = "mean", geom = "point", size = 2, color = "black") +  # Add mean points for each group
  scale_y_continuous(labels = label_percent(scale = 1)) +  # Transform y-axis to percentages (0 to 100%)
  labs(
    title = "Violin Plot of Correct Responses by Session and Grammaticality",
    x = "Session",
    y = "Percentage of Correct Responses"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"))

# Convert 'correct' to a factor with levels "incorrect" (0) and "correct" (1)
Norwegian_df$correct <- factor(Norwegian_df$correct, levels = c(0, 1), labels = c("incorrect", "correct"))

# Plot with the new factor labels for 'correct'
ggplot(Norwegian_df, aes(x = Session, y = correct, fill = grammaticality)) +
  geom_violin(trim = FALSE, scale = "area") +  # Violin plot for distribution of 1s
  stat_summary(fun = "mean", geom = "point", size = 2, color = "black") +  # Add mean points for each group
  scale_y_discrete(    # Use scale_y_discrete since 'correct' is now a factor
    labels = c("incorrect", "correct")
  ) +
  labs(
    title = "Violin Plot of Correct Responses by Session and Grammaticality",
    x = "Session",
    y = "Correct Responses"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"))

# Reverse the order of 'correct' to have "correct" at the top and "incorrect" at the bottom
Norwegian_df$correct <- factor(Norwegian_df$correct, levels = c(1, 0), labels = c("Correct responses", "Incorrect responses"))

# Plot with reversed y-axis labels
ggplot(Norwegian_df, aes(x = Session, y = correct, fill = grammaticality)) +
  geom_violin(trim = FALSE, scale = "area") +  # Violin plot for distribution of 1s
  stat_summary(fun = "mean", geom = "point", size = 2, color = "black") +  # Add mean points for each group
  scale_y_discrete(    # Use scale_y_discrete since 'correct' is now a factor
    labels = c("Incorrect responses", "Correct responses")  # Custom labels for y-axis
  ) +
  labs(
    title = "Violin Plot of Correct Responses by Session and Grammaticality",
    x = "Session",
    y = "Response Type"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"))

