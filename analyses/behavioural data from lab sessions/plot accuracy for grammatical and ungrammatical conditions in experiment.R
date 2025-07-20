

# Plot accuracy for grammatical and ungrammatical conditions in the experiment

library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
# install.packages("devtools")
# devtools::install_github("psyteachr/introdataviz")
library(introdataviz)
library(patchwork)
library(ggtext)

# Function to extract the number that corresponds to each participant after "subject-"
extract_number <- function(filename) {
  match <- gsub(".*subject-([0-9]+).*", "\\1", basename(filename))  # Extract number
  if (match == basename(filename)) return(NA)  # If no match, return NA
  return(as.numeric(match))  # Convert to numeric
}

######loading data from all sessions
#Session 2
# List all CSV files in the directory
Session2_file_list <- list.files(
  path = "data/raw data/behavioural data from lab sessions/Session 2", 
  pattern = "*.csv", full.names = TRUE
)

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
Session2_final_df <- Session2_final_df %>%
  mutate(grammatical_property = ifelse(session_part == "Test", "gender agreement", 
                                       grammatical_property))


#Session 3

Session3_file_list <- list.files(
  path = "data/raw data/behavioural data from lab sessions/Session 3", 
  pattern = "*.csv", full.names = TRUE
)

for (file in Session3_file_list) {
  file_content <- readLines(file)  # Read the file as text
  
  file_content[1] <- gsub("\\.", ",", file_content[1])  
  
  writeLines(file_content, file)  
}

Session3_subject_ids <- sapply(Session3_file_list, extract_number)

Session3_df_list <- lapply(seq_along(Session3_file_list), function(i) {
  Session3_df <- read.csv(Session3_file_list[i], stringsAsFactors = FALSE)
  Session3_df$subject_id <- Session3_subject_ids[i] 
  return(Session3_df)
})

all_columns <- unique(unlist(lapply(Session3_df_list, colnames)))

Session3_df_list <- lapply(Session3_df_list, function(Session3_df) {
  Session3_missing_cols <- setdiff(all_columns, colnames(Session3_df))
  Session3_df[Session3_missing_cols] <- NA  
  Session3_df <- Session3_df[all_columns]   
  return(Session3_df)
})

Session3_final_df <- do.call(rbind, Session3_df_list)
Session3_final_df$Session <- "Session 3"
Session3_final_df <- Session3_final_df %>%
  mutate(grammatical_property = ifelse(session_part == "Test", 
                                       "differential object marking", 
                                       grammatical_property))

# Session 4

Session4_file_list <- list.files(
  path = "data/raw data/behavioural data from lab sessions/Session 4", 
  pattern = "*.csv", full.names = TRUE
)


for (file in Session4_file_list) {
  file_content <- readLines(file) 
  
  file_content[1] <- gsub("\\.", ",", file_content[1])
  
  writeLines(file_content, file) 
}

Session4_subject_ids <- sapply(Session4_file_list, extract_number)

Session4_df_list <- lapply(seq_along(Session4_file_list), function(i) {
  Session4_df <- read.csv(Session4_file_list[i], stringsAsFactors = FALSE)
  Session4_df$subject_id <- Session4_subject_ids[i]
  return(Session4_df)
})

all_columns <- unique(unlist(lapply(Session4_df_list, colnames)))

Session4_df_list <- lapply(Session4_df_list, function(Session4_df) {
  Session4_missing_cols <- setdiff(all_columns, colnames(Session4_df))
  Session4_df[Session4_missing_cols] <- NA  
  Session4_df <- Session4_df[all_columns]  
  return(Session4_df)
})

Session4_final_df <- do.call(rbind, Session4_df_list)
Session4_final_df$Session <- "Session 4"
Session4_final_df <- Session4_final_df %>%
  mutate(grammatical_property = ifelse(session_part == "Test", 
                                       "verb-object agreement", grammatical_property))

# Session 6

Session6_file_list <- list.files(
  path = "data/raw data/behavioural data from lab sessions/Session 6", 
  pattern = "*.csv", full.names = TRUE
)

for (file in Session6_file_list) {
  file_content <- readLines(file)  
  
  file_content[1] <- gsub("\\.", ",", file_content[1]) 
  
  writeLines(file_content, file)  
}

Session6_subject_ids <- sapply(Session6_file_list, extract_number)
Session6_df_list <- lapply(seq_along(Session6_file_list), function(i) {
  Session6_df <- read.csv(Session6_file_list[i], stringsAsFactors = FALSE)
  Session6_df$subject_id <- Session6_subject_ids[i] 
  return(Session6_df)
})

all_columns <- unique(unlist(lapply(Session6_df_list, colnames)))

Session6_df_list <- lapply(Session6_df_list, function(Session6_df) {
  Session6_missing_cols <- setdiff(all_columns, colnames(Session6_df))
  Session6_df[Session6_missing_cols] <- NA 
  Session6_df <- Session6_df[all_columns] 
  return(Session6_df)
})
Session6_final_df <- do.call(rbind, Session6_df_list)
Session6_final_df$Session <- "Session 6"

# combining the different sessions to make a joint data frame

# Function to convert all columns to character
convert_to_character <- function(df) {
  df %>% mutate(across(everything(), as.character))
}

Session2_final_df <- convert_to_character(Session2_final_df)
Session3_final_df <- convert_to_character(Session3_final_df)
Session4_final_df <- convert_to_character(Session4_final_df)
Session6_final_df <- convert_to_character(Session6_final_df)

combined_df <- bind_rows(
  Session2_final_df, 
  Session3_final_df, 
  Session4_final_df, 
  Session6_final_df, 
  .id = "session"
)

clean_combined_df <- combined_df[, c("grammaticality", "grammatical_property",  
                                     "session_part", "trial", "response_time", 
                                     "correct", "subject_id", "Session")]


#View(clean_combined_df)
#splitting the participants into mini-language groups

clean_combined_df <- clean_combined_df %>%
  mutate(
    subject_id = as.numeric(subject_id), 
    `Mini language` = ifelse(subject_id %% 2 == 1, "Mini-English", "Mini-Norwegian")
  )

#removing duplicate trials
clean_combined_df <- clean_combined_df %>%
  filter(trial != lag(trial, default = first(trial)))

clean_combined_df <- clean_combined_df %>%
  mutate(
    grammaticality = recode(
      grammaticality,
      "article location violation" = "Article\nmisplacement",
      "number violation" = "Number\nviolation"
    )
  )

clean_combined_df <- clean_combined_df %>%
  rename(mini_language = `Mini language`)

# colour-coding the grammatical properties
grammaticality_colours <- c(
  'Grammatical' = 'forestgreen',
  'Ungrammatical' = 'firebrick1',
  'Number\nviolation' = 'grey40',
  'Article\nmisplacement' = 'steelblue4'
)


###########################################################

# Plot experiment part of gender agreement across sessions

Gender_agreement_df <- clean_combined_df %>%
  filter(grammatical_property == "gender agreement", session_part == "Experiment")

Gender_agreement_df$grammaticality[Gender_agreement_df$grammaticality %in% 
                                     c("gender violation")] <- "Ungrammatical"

Gender_agreement_df <- Gender_agreement_df %>%
  mutate(grammaticality = case_when(
    grammaticality == "grammatical" ~ "Grammatical",
    TRUE ~ grammaticality
  ))

Gender_agreement_df$correct <- as.numeric(as.character(Gender_agreement_df$correct))
Gender_agreement_df$mini_language <- as.factor(Gender_agreement_df$mini_language)
Gender_agreement_df$Session <- as.factor(Gender_agreement_df$Session)
Gender_agreement_df$grammaticality <- as.factor(Gender_agreement_df$grammaticality)

# Add accuracy as a new column in Gender_agreement_df
Gender_agreement_df <- Gender_agreement_df %>%
  group_by(subject_id, grammaticality) %>%
  mutate(accuracy = mean(correct)) %>%
  ungroup()

Gender_agreement_df_violin <- Gender_agreement_df %>%
  filter(grammaticality %in% c("Grammatical", "Ungrammatical"))


# Create the facet_group variable in the dataset
Gender_agreement_df_violin$facet_group <- Gender_agreement_df_violin$mini_language

# 'facet_group' factor is ordered with "Mini-Norwegian" first
Gender_agreement_df_violin$facet_group <- factor(
  Gender_agreement_df_violin$facet_group,
  levels = c("Mini-Norwegian", 
             setdiff(unique(Gender_agreement_df_violin$facet_group), "Mini-Norwegian"))
)

Gender_agreement_plot <- ggplot(Gender_agreement_df_violin,
                                aes(x = Session, y = accuracy * 100, 
                                    fill = grammaticality, 
                                    color = grammaticality)) +  
  introdataviz::geom_split_violin(alpha = 0.4, trim = TRUE,
                                  # Adjust width to change space between levels on the x-axis
                                  position = position_dodge(width = 0.1)  
  ) +
  geom_boxplot(width = .1, alpha = 1, fatten = NULL, show.legend = FALSE, 
               outlier.shape = NA) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = FALSE, 
               position = position_dodge(.095), colour = "white") +
  scale_x_discrete(
    name = "", 
    labels = c("Session 2", "Session 3", "Session 4", "Session 6"),
    expand = expansion(mult = c(0.1, 0.1))  # Adjust space on both sides of the axis
  ) +
  scale_y_continuous(
    name = "Accuracy", 
    breaks = seq(0, 100, by = 20), 
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +  
  scale_color_manual(values = grammaticality_colours) +
  scale_fill_manual(values = grammaticality_colours) +
  ggtitle("Accuracy on gender agreement in the experiment") +  
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    legend.position = 'top',  # Move legend to the top
    legend.justification = 'center',  # Center legend
    legend.title = element_blank(),  
    legend.text = element_text(
      size = 14, margin = margin(r = 10, l = 3, unit = 'pt')  
    ),
    legend.key.width = unit(1.2, 'cm'),  # Adjust width of legend keys
    legend.key.height = unit(0.5, 'cm'),  # Adjust height of legend keys
    plot.title = element_text(
      size = 16, 
      hjust = 0.5,  
      margin = margin(t = 12, b = 1, unit = 'pt')  # Add margin to title
    ),
    panel.border = element_blank(),  # Remove panel border
    strip.background = element_rect(
      fill = 'gray90', 
      colour = 'gray70', 
      linewidth = 0.5  # Facet background color and border
    ),
    strip.text = element_text(size = 14, face = 'bold'),  # Facet label style
    panel.spacing = unit(0.5, 'cm'), 
    panel.spacing.x = unit(1111, 'cm'),# Add spacing between panels
    legend.key = element_rect(fill = "gray90", color = NA),  # Background for legend keys
    legend.box.spacing = unit(1, "cm"),  # Space between legend items
    legend.margin = margin(20, 5, 0.001, 5),
    plot.margin = margin(t = 1, b = 1, l = 10, r = 10) # Reduced margin around the legend 
  ) +
  facet_wrap(~facet_group, ncol = 1)  # Facet by mini_language without sample size

# ggsave("gender_agreement_experiment_plot.png", plot = Gender_agreement_plot, 
#        width = 7, height = 10, dpi = 300)


######################################
###### Differential object marking 

Differential_object_marking_df <- clean_combined_df %>%
  filter(grammatical_property == "differential object marking", 
         session_part == "Experiment")



Differential_object_marking_df$grammaticality[
  Differential_object_marking_df$grammaticality %in% 
    c("DOM violation")] <- "Ungrammatical"

Differential_object_marking_df <- Differential_object_marking_df %>%
  mutate(grammaticality = case_when(
    grammaticality == "grammatical" ~ "Grammatical",
    TRUE ~ grammaticality
  ))

Differential_object_marking_df$correct <- 
  as.numeric(as.character(Differential_object_marking_df$correct))

Differential_object_marking_df$"Mini language" <- 
  as.factor(Differential_object_marking_df$"mini_language")

Differential_object_marking_df$Session <- 
  as.factor(Differential_object_marking_df$Session)

Differential_object_marking_df$grammaticality <- 
  as.factor(Differential_object_marking_df$grammaticality)

Differential_object_marking_df <- Differential_object_marking_df %>%
  group_by(subject_id, grammaticality) %>%
  mutate(accuracy = mean(correct)) %>%
  ungroup()

Differential_object_marking_df_violin <- Differential_object_marking_df %>%
  filter(grammaticality %in% c("Grammatical", "Ungrammatical"))


# Use mini_language for faceting
Differential_object_marking_df_violin$facet_group <- 
  Differential_object_marking_df_violin$mini_language  

Differential_object_marking_df_violin$facet_group <- factor(
  Differential_object_marking_df_violin$facet_group,
  levels = c("Mini-Norwegian", 
             setdiff(unique(Differential_object_marking_df_violin$facet_group), 
                     "Mini-Norwegian"))
)

DOM_experiment_plot <- ggplot(Differential_object_marking_df_violin, 
                              aes(x = Session, y = accuracy * 100, 
                                  fill = grammaticality, color = grammaticality)) +  
  introdataviz::geom_split_violin(alpha = 0.4, trim = TRUE,
                                  position = position_dodge(width = 0.1)
  ) +
  geom_boxplot(width = .1, alpha = 1, fatten = NULL, show.legend = FALSE, 
               outlier.shape = NA) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = FALSE, 
               position = position_dodge(.095), colour = "white") +
  scale_x_discrete(
    name = "", 
    labels = c("Session 3", "Session 4", "Session 6"),
    expand = expansion(mult = c(0.1, 0.1)) 
  ) +
  scale_y_continuous(
    name = "Accuracy", 
    breaks = seq(0, 100, by = 20), 
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +  
  scale_color_manual(values = grammaticality_colours) +
  scale_fill_manual(values = grammaticality_colours) +
  ggtitle("Accuracy on differential object marking in the experiment") +  
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    legend.position = 'top',  # Move legend to the top
    legend.justification = 'center',  # Center legend
    legend.title = element_blank(),  
    legend.text = element_text(
      size = 14, 
      margin = margin(r = 10, l = 3, unit = 'pt')  
    ),
    legend.key.width = unit(1.2, 'cm'), 
    legend.key.height = unit(0.5, 'cm'),  
    plot.title = element_text(
      size = 16, hjust = 0.5,
      margin = margin(t = 12, b = 1, unit = 'pt') 
    ),
    panel.border = element_blank(), 
    strip.background = element_rect(
      fill = 'gray90', colour = 'gray70', linewidth = 0.5  
    ),
    strip.text = element_text(size = 14, face = 'bold'), 
    panel.spacing = unit(0.5, 'cm'), 
    panel.spacing.x = unit(1111, 'cm'),
    legend.key = element_rect(fill = "gray90", color = NA),  
    legend.box.spacing = unit(1, "cm"),  
    legend.margin = margin(20, 5, 0.001, 5),
    plot.margin = margin(t = 1, b = 1, l = 10, r = 10) 
  ) +
  facet_wrap(~facet_group, ncol = 1)  

#ggsave("DOM_experiment_plot.png", plot = DOM_experiment_plot, 
#     width = 7, height = 10, dpi = 300)


###########################################
#### verb object agreement

Verb_object_agreement_df <- clean_combined_df %>%
  filter(grammatical_property == "verb-object agreement", session_part == "Experiment")


Verb_object_agreement_df$grammaticality[Verb_object_agreement_df$grammaticality %in% 
                                          c("VOA violation")] <- "Ungrammatical"

Verb_object_agreement_df <- Verb_object_agreement_df %>%
  mutate(grammaticality = case_when(
    grammaticality == "grammatical" ~ "Grammatical",
    TRUE ~ grammaticality
  ))

Verb_object_agreement_df$correct <- as.numeric(as.character(Verb_object_agreement_df$correct))
Verb_object_agreement_df$mini_language <- as.factor(Verb_object_agreement_df$mini_language)
Verb_object_agreement_df$Session <- as.factor(Verb_object_agreement_df$Session)
Verb_object_agreement_df$grammaticality <- as.factor(Verb_object_agreement_df$grammaticality)

# Add accuracy as a new column in Verb_object_agreement_df
Verb_object_agreement_df <- Verb_object_agreement_df %>%
  group_by(subject_id, grammaticality) %>%
  mutate(accuracy = mean(correct)) %>%
  ungroup()

Verb_object_agreement_df_violin <- Verb_object_agreement_df %>%
  filter(grammaticality %in% c("Grammatical", "Ungrammatical"))


# Create the facet_group variable in the dataset
Verb_object_agreement_df_violin$facet_group <- Verb_object_agreement_df_violin$mini_language  

# Ensure the 'facet_group' factor is ordered with "Mini-Norwegian" first
Verb_object_agreement_df_violin$facet_group <- factor(
  Verb_object_agreement_df_violin$facet_group,
  levels = c("Mini-Norwegian", setdiff(unique(Verb_object_agreement_df_violin$facet_group),
                                       "Mini-Norwegian"))
)

# Now plot the violin plot, faceting by `facet_group` (mini_language)
VOA_plot <- ggplot(Verb_object_agreement_df_violin, 
                   aes(x = Session, y = accuracy * 100, 
                       fill = grammaticality, color = grammaticality)) +  
  introdataviz::geom_split_violin(alpha = 0.4, trim = TRUE,
                                  # Adjust width to change space between levels on the x-axis
                                  position = position_dodge(width = 0.1)  
  ) +
  geom_boxplot(width = .1, alpha = 1, fatten = NULL, show.legend = FALSE, 
               outlier.shape = NA) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = FALSE, 
               position = position_dodge(.095), colour = "white") +
  scale_x_discrete(
    name = "", 
    labels = c("Session 4", "Session 6"),
    expand = expansion(mult = c(0.1, 0.1))  # Adjust space on both sides of the axis
  ) +
  scale_y_continuous(
    name = "Accuracy", 
    breaks = seq(0, 100, by = 20), 
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +  
  scale_color_manual(values = grammaticality_colours) +
  scale_fill_manual(values = grammaticality_colours) +
  ggtitle("Accuracy on verb-object number agreement in the experiment") +  
  
  # Customize the plot with your specified theme
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    legend.position = 'top',  # Move legend to the top
    legend.justification = 'center',  # Center legend
    legend.title = element_blank(),  
    legend.text = element_text(
      size = 14, 
      margin = margin(r = 10, l = 3, unit = 'pt')  
    ),
    legend.key.width = unit(1.2, 'cm'),  # Adjust width of legend keys
    legend.key.height = unit(0.5, 'cm'),  # Adjust height of legend keys
    plot.title = element_text(
      size = 16, 
      hjust = 0.5,  
      margin = margin(t = 12, b = 1, unit = 'pt')  # Add margin to title
    ),
    panel.border = element_blank(),  # Remove panel border
    strip.background = element_rect(
      fill = 'gray90', 
      colour = 'gray70', 
      linewidth = 0.5  # Facet background color and border
    ),
    strip.text = element_text(size = 14, face = 'bold'),  # Facet label style
    panel.spacing = unit(0.5, 'cm'), 
    panel.spacing.x = unit(1111, 'cm'),# Add spacing between panels
    legend.key = element_rect(fill = "gray90", color = NA),  # Background for legend keys
    legend.box.spacing = unit(1, "cm"),  # Space between legend items
    legend.margin = margin(20, 5, 0.001, 5),
    # Reduced margin around the legend (adjust to your preference)
    plot.margin = margin(t = 1, b = 1, l = 10, r = 10) 
  ) +
  facet_wrap(~facet_group, ncol = 1)  # Facet by mini_language without sample size

#ggsave("VOA_plot.png", plot = VOA_plot, width = 7, height = 10, dpi = 300)

