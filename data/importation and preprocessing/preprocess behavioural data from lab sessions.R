library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(introdataviz) 
library(patchwork)

# how to install the introdataviz package to get split and half violin plots
#devtools::install_github("psyteachr/introdataviz")


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
Session2_final_df$Session <- "session 2"


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

Session3_final_df$Session <- "Session 3"


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
Session4_final_df$Session <- "Session 4"


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
Session6_final_df$Session <- "Session 6"

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

clean_combined_df <- combined_df[, c("grammaticality", "grammatical_property",  
                                     "session_part", "trial", "sentence_1", "sentence_2", "correct", "subject_id", "Session")]


#View(clean_combined_df)
#splitting the participants into mini-language groups

clean_combined_df <- clean_combined_df %>%
  mutate(
    subject_id = as.numeric(subject_id),  # Ensure subject_id is numeric
    `Mini language` = ifelse(subject_id %% 2 == 1, "Mini-English", "Mini-Norwegian")
  )

#removing duplicate trials
clean_combined_df <- clean_combined_df %>%
  filter(trial != lag(trial, default = first(trial)))


#Plotting for gender agreement

# Create a new data frame with only "gender agreement" in grammatical_property
Gender_agreement_df <- clean_combined_df %>%
  filter(grammatical_property == "gender agreement", session_part == "Experiment")

#grouping the two ungrammatical conditions
Gender_agreement_df$grammaticality[Gender_agreement_df$grammaticality %in% c("gender violation")] <- "Ungrammatical"
Gender_agreement_df$grammaticality[Gender_agreement_df$grammaticality == "grammatical"] <- "Grammatical"

Gender_agreement_df$correct <- as.numeric(as.character(Gender_agreement_df$correct))
Gender_agreement_df$"Mini language" <- as.factor(Gender_agreement_df$"Mini language")
Gender_agreement_df$Session <- as.factor(Gender_agreement_df$Session)
Gender_agreement_df$grammaticality <- as.factor(Gender_agreement_df$grammaticality)

# Add accuracy as a new column in Gender_agreement_df
Gender_agreement_df <- Gender_agreement_df %>%
 group_by(subject_id, grammaticality) %>%
mutate(accuracy = mean(correct)) %>%
ungroup()

Gender_agreement_df_violin <- Gender_agreement_df %>%
  filter(grammaticality %in% c("Grammatical", "Ungrammatical"))


Norwegian_plot_gender <- Gender_agreement_df_violin %>%
  filter(`Mini language` == "Mini-Norwegian") 
  ggplot(aes(x = Session, y = accuracy * 100, fill = grammaticality)) +  
  introdataviz::geom_split_violin(alpha = 0.4, trim = TRUE) +
  geom_boxplot(width = .2, alpha = 1, fatten = NULL, show.legend = FALSE, outlier.shape = NA) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, position = position_dodge(.095), colour = "white") +
  scale_x_discrete(name = "", labels = c("Session 2", "Session 3", "Session 4", "Session 6")) +
  scale_y_continuous(
    name = "Accuracy", 
    breaks = seq(0, 100, by = 20), 
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +  
  scale_fill_manual(values = c("Grammatical" = "forestgreen", "Ungrammatical" = "firebrick1"), name = "Grammaticality") +
  ggtitle("Mini-Norwegian") +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.y = element_text(size = 11, face = "bold") 
  )

print(Norwegian_plot_gender)


English_plot_gender <- Gender_agreement_df_violin %>%
  filter(`Mini language` == "Mini-English") %>% 
  ggplot(aes(x = Session, y = accuracy * 100, fill = grammaticality)) +  
  introdataviz::geom_split_violin(alpha = 0.4, trim = TRUE) +
  geom_boxplot(width = .2, alpha = 1, fatten = NULL, show.legend = FALSE, outlier.shape = NA) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, position = position_dodge(.095), colour = "white") +
  scale_x_discrete(name = "", labels = c("Session 2", "Session 3", "Session 4", "Session 6")) +
  scale_y_continuous(
    name = "Accuracy", 
    breaks = seq(0, 100, by = 20), 
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +  
  scale_fill_manual(values = c("Grammatical" = "forestgreen", "Ungrammatical" = "firebrick1"), name = "Grammaticality") +
  ggtitle("Mini-English") +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.y = element_text(size = 11, face = "bold") +
      facet_wrap(~`Mini language`, ncol = 1)
  )


# Display the plot
print(English_plot_gender)

facet_wrap(~'mini language', ncol = 1)

# Combine the plots and add a title
Gender_agreement_plot <- Norwegian_plot_gender / English_plot_gender + 
  plot_annotation(title = "Accuracy on gender agreement in the experiment"+
                    facet_wrap(~`Mini language`, ncol = 1)
  )

# Display the combined plot
print(Gender_agreement_plot)

#ggsave(filename = paste0(Gender_agreement_plot, '.png'), path = 'analyses/plots/Nhavioural data', 
#      width = 10, height = 8, dpi = 300, units = 'in')

######################################

###### plotting for Differential object marking

Differential_object_marking_df <- clean_combined_df %>%
  filter(grammatical_property == "differential object marking")


# Convert 'correct' column to numeric if it's not already
Differential_object_marking_df$correct <- as.numeric(as.character(Differential_object_marking_df$correct))
Differential_object_marking_df$"Mini language" <- as.factor(Differential_object_marking_df$"Mini language")
Differential_object_marking_df$Session <- as.factor(Differential_object_marking_df$Session)
Differential_object_marking_df$grammaticality <- as.factor(Differential_object_marking_df$grammaticality)

# Separate Norwegian group data frame
#Norwegian_DOM_df <- Differential_object_marking_df %>%
 # filter(`Mini language` == "Mini-Norwegian", grammaticality %in% c("Grammatical", "Ungrammatical"))

# Add accuracy as a new column in Differential_object_marking_df_violin
Differential_object_marking_df_violin <- Differential_object_marking_df %>%
  group_by(subject_id, grammaticality) %>%
  mutate(accuracy = mean(correct)) %>%
  ungroup()
# Add "Grammatical" as a valid factor level if it's missing
Differential_object_marking_df_violin$grammaticality <- factor(Differential_object_marking_df_violin$grammaticality, 
                                                               levels = c("Ungrammatical", "grammatical", "Grammatical"))

#grouping the two ungrammatical conditions
Differential_object_marking_df_violin$grammaticality[Differential_object_marking_df$grammaticality %in% c("DOM violation")] <- "Ungrammatical"

Differential_object_marking_df_violin$grammaticality[Differential_object_marking_df_violin$grammaticality == "grammatical"] <- "Grammatical"



# separate English group data frame
#English_DOM_df <- Differential_object_marking_df %>%
 # filter(`Mini language` == "Mini-Norwegian", grammaticality %in% c("Grammatical", "Ungrammatical"))

# Add accuracy as a new column in English_DOM_df
#English_DOM_df <- English_DOM_df %>%
 # group_by(subject_id, grammaticality) %>%
#  mutate(accuracy = mean(correct)) %>%
 # ungroup()
########

# Ensure that the Session variable is a factor with all the necessary levels
Differential_object_marking_df_violin$Session <- factor(Differential_object_marking_df_violin$Session, levels = c("Session 3", "Session 4", "Session 6"))

# Now create the plot
Norwegian_plot_DOM <- Differential_object_marking_df_violin %>%
  filter(`Mini language` == "Mini-English") %>% 
  ggplot (aes(x = Session, y = accuracy * 100, fill = grammaticality)) + 
  ggplot(Differential_object_marking_df_violin, aes(x = Session, y = accuracy * 100, fill = grammaticality)) +  
  introdataviz::geom_split_violin(alpha = 0.4, trim = TRUE) +
  geom_boxplot(width = .2, alpha = 1, fatten = NULL, show.legend = FALSE, outlier.shape = NA) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, position = position_dodge(.175)) +
  scale_x_discrete(name = "", labels = c("Session 3", "Session 4", "Session 6")) +
  scale_y_continuous(
    name = "Accuracy", 
    breaks = seq(0, 100, by = 20), 
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +  
  scale_fill_manual(values = c("Grammatical" = "forestgreen", "Ungrammatical" = "firebrick1"), name = "Grammaticality") +
  ggtitle("Mini-Norwegian") +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.y = element_text(size = 11, face = "bold") 
  )

print(Norwegian_plot_DOM)

English_plot_DOM <- ggplot(Differential_object_marking_df, aes(x = Session, y = accuracy * 100, fill = grammaticality)) +  
  introdataviz::geom_split_violin(alpha = 0.4, trim = TRUE) +
  geom_boxplot(width = .2, alpha = 1, fatten = NULL, show.legend = FALSE, outlier.shape = NA) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, position = position_dodge(.175)) +
  scale_x_discrete(name = "", labels = c("Session 3", "Session 4", "Session 6")) +
  scale_y_continuous(
    name = "Accuracy", 
    breaks = seq(0, 100, by = 20), 
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +  
  scale_fill_manual(values = c("Grammatical" = "forestgreen", "Ungrammatical" = "firebrick1"), name = "Grammaticality") +
  ggtitle("Mini-English") +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.y = element_text(size = 11, face = "bold")  
  )

# Print the plot
#print(English_plot_DOM)


# Combine the plots and add a title
DOM_plot <- Norwegian_plot_DOM / English_plot_DOM + 
  plot_annotation(title = "Accuracy on differential object marking in the experiment")

# Display the combined plot
print(DOM_plot)

#ggsave(filename = paste0(VOA_plot, '.png'), path = 'analyses/plots/Nhavioural data', 
#      width = 10, height = 8, dpi = 300, units = 'in')

###########################
#Plotting for Verb object agreement

Verb_object_agreement_df <- clean_combined_df %>%
  filter(grammatical_property == "verb-object agreement")
#View (Verb_object_agreement_df)

#grouping the two ungrammatical conditions
Verb_object_agreement_df$grammaticality[Verb_object_agreement_df$grammaticality %in% c("VOA violation")] <- "Ungrammatical"
#View (Verb_object_agreement_df)

# Add "Grammatical" as a valid factor level if it's missing
Verb_object_agreement_df$grammaticality <- factor(Verb_object_agreement_df$grammaticality, 
                                                  levels = c("Ungrammatical", "grammatical", "Grammatical"))

# Replace "grammatical" with "Grammatical"
Verb_object_agreement_df$grammaticality[Verb_object_agreement_df$grammaticality == "grammatical"] <- "Grammatical"

# Convert 'correct' column to numeric if it's not already
Verb_object_agreement_df$correct <- as.numeric(as.character(Verb_object_agreement_df$correct))
Verb_object_agreement_df$"Mini language" <- as.factor(Verb_object_agreement_df$"Mini language")
Verb_object_agreement_df$Session <- as.factor(Verb_object_agreement_df$Session)
Verb_object_agreement_df$grammaticality <- as.factor(Verb_object_agreement_df$grammaticality)

# Separate Norwegian group data frame
Norwegian_VOA_df <- Verb_object_agreement_df %>%
  filter(`Mini language` == "Mini-Norwegian", grammaticality %in% c("Grammatical", "Ungrammatical"))

# Ensure that Session is a factor
Norwegian_VOA_df$Session <- factor(Norwegian_VOA_df$Session)


# Add accuracy as a new column in Norwegian_VOA_df
Norwegian_VOA_df <- Norwegian_VOA_df %>%
  group_by(subject_id, grammaticality) %>%
  mutate(accuracy = mean(correct)) %>%
  ungroup()

# separate English group data frame
English_VOA_df <- Verb_object_agreement_df %>%
  filter(`Mini language` == "Mini-Norwegian", grammaticality %in% c("Grammatical", "Ungrammatical"))

# Ensure that Session is a factor
English_VOA_df$Session <- factor(English_VOA_df$Session)


# Add accuracy as a new column in English_VOA_df
English_VOA_df <- English_VOA_df %>%
  group_by(subject_id, grammaticality) %>%
  mutate(accuracy = mean(correct)) %>%
  ungroup()
########
Norwegian_plot_VOA <- ggplot(Norwegian_VOA_df, aes(x = Session, y = accuracy * 100, fill = grammaticality)) +  
  introdataviz::geom_split_violin(alpha = 0.4, trim = TRUE) +
  geom_boxplot(width = .2, alpha = 1, fatten = NULL, show.legend = FALSE, outlier.shape = NA) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, position = position_dodge(.175)) +
  scale_x_discrete(name = "", labels = c("Session 4", "Session 6")) +
  scale_y_continuous(
    name = "Accuracy", 
    breaks = seq(0, 100, by = 20), 
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +  
  scale_fill_manual(values = c("Grammatical" = "forestgreen", "Ungrammatical" = "firebrick1"), name = "Grammaticality") +
  ggtitle("Mini-Norwegian") +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.y = element_text(size = 11, face = "bold")   # Title customization
  )

#print(Norwegian_plot_VOA)

English_plot_VOA <- ggplot(English_VOA_df, aes(x = Session, y = accuracy * 100, fill = grammaticality)) +  
  introdataviz::geom_split_violin(alpha = 0.4, trim = TRUE) +
  geom_boxplot(width = .1, alpha = 1, fatten = NULL, show.legend = FALSE,outlier.shape = NA) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, position = position_dodge(.095), colour = "white") +
  scale_x_discrete(name = "", labels = c("Session 4", "Session 6")) +
  scale_y_continuous(
    name = "Accuracy", 
    breaks = seq(0, 100, by = 20), 
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")  ) +  
  scale_fill_manual(values = c("Grammatical" = "forestgreen", "Ungrammatical" = "firebrick1"), name = "Grammaticality") +
  ggtitle("Mini-English") +  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title.y = element_text(size = 11, face = "bold")  # Title customization
  )


# Display the plot
print(English_plot_VOA)

# Combine the plots and add a title
VOA_plot <- Norwegian_plot_VOA / English_plot_VOA + 
  plot_annotation(title = "Accuracy on Verb Object agreement in the experiment")

# Save as PNG
#ggsave(plot, filename = paste0(plot_name, '.png'), path = 'analyses/plots/waveforms/', 
#      create.dir = TRUE, width = 9, height = 8, dpi = 300, units = 'in')

# Save as PDF
#ggsave(plot, filename = paste0(plot_name, '.pdf'), path = 'analyses/plots/waveforms/', 
#      create.dir = TRUE, width = 9, height = 8, dpi = 300, units = 'in')
# Display the combined plot
print(VOA_plot)

#ggsave(filename = paste0(VOA_plot, '.png'), path = 'analyses/plots/Nhavioural data', 
#      width = 10, height = 8, dpi = 300, units = 'in')