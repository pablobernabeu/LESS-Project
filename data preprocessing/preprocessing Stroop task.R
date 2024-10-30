

# Preprocessing Stroop task


library(dplyr)
library(readr)
library(tidyverse)
library(janitor)
library(readxl)


# Define the path
path_to_Stroop <- "Raw data/Executive functions/Session 1"

# Read the files
Stroop_1 <- read_csv(file.path(path_to_Stroop, "Stroop_DGS.csv"))
Stroop_2 <- read_csv(file.path(path_to_Stroop, "Stroop_Stroop.csv"))
Stroop_3 <- read_csv(file.path(path_to_Stroop, "Stroop_ASRT.csv"))

# Upload the additional CSV files
Stroop_1_extra <- read_csv(file.path(path_to_Stroop, "qdvg4_Stroop_DGS.csv"))
Stroop_2_extra_1 <- read_csv(file.path(path_to_Stroop, "wbij5_Stroop_Stroop.csv"))
Stroop_2_extra_2 <- read_csv(file.path(path_to_Stroop, "xqls8_Stroop_Stroop.csv"))

# Convert the "Reaction Time" column in both data frames to numeric type (double)
Stroop_1$`Reaction Time` <- as.numeric(Stroop_1$`Reaction Time`)
# Warning message:NAs introduced by coercion 
Stroop_1_extra$`Reaction Time` <- as.numeric(Stroop_1_extra$`Reaction Time`)

# Combine the data frames
Stroop_1 <- bind_rows(Stroop_1, Stroop_1_extra)
Stroop_2 <- bind_rows(Stroop_2, Stroop_2_extra_1, Stroop_2_extra_2)

# Clean column names
colnames(Stroop_1) <- make.names(colnames(Stroop_1))
colnames(Stroop_2) <- make.names(colnames(Stroop_2))
colnames(Stroop_3) <- make.names(colnames(Stroop_3))

print(colnames(Stroop_1))

# Select relevant columns
Stroop_1 <- Stroop_1 %>%
  select(Reaction.Time, Participant.Public.ID, Correct, Incorrect, Congruency, Zone.Type)

Stroop_2 <- Stroop_2 %>%
  select(Reaction.Time, Participant.Public.ID, Correct, Incorrect, Congruency,Zone.Type)

Stroop_3 <- Stroop_3 %>%
  select(Reaction.Time, Participant.Public.ID, Correct, Incorrect, Congruency, Zone.Type)

# Combine the dataframes
merged_Stroop_2 <- bind_rows(Stroop_1, Stroop_2, Stroop_3)


# Filter to remove rows where Zone.Type is not "response"
merged_Stroop_2 <- merged_Stroop_2 %>%
  filter(Zone.Type == "response_keyboard")


# Filter and clean the data by removing NA and ensuring Congruency is a factor
cleaned_data_Stroop <- merged_Stroop_2%>%
  drop_na(Reaction.Time, Congruency) %>%
  mutate(Congruency = factor(Congruency, levels = c(0, 1), labels = c("incongruent", "congruent")))


# Calculate the average reaction time according to congruence for each participant
mean_reaction_time_stroop <- cleaned_data_Stroop %>%
  group_by(Participant.Public.ID, Congruency) %>%
  summarize(mean_reaction_time_stroop = mean(Reaction.Time, na.rm = TRUE))

# See the result
# View(mean_reaction_time_stroop)

# Calculate the difference between the reaction times of each congruence for 
# each participant
# difference_reaction_time <- mean_reaction_time_stroop %>%
# group_by(Participant.Public.ID) %>%
# pivot_wider(names_from = Congruency, values_from = mean_reaction_time) %>%
# mutate(difference_reaction_time = congruent - incongruent)




# Step 1: Split the data by participant
split_data <- split(mean_reaction_time_stroop, mean_reaction_time_stroop$Participant.Public.ID)

# Step 2: Calculate the difference for each participant
difference_list <- lapply(split_data, function(participant_data) {
  congruent_time <- participant_data$mean_reaction_time_stroop[participant_data$Congruency == "congruent"]
  incongruent_time <- participant_data$mean_reaction_time_stroop[participant_data$Congruency == "incongruent"]
  
  # Calculate the difference
  difference <- congruent_time - incongruent_time
  
  # Add a new row to the data frame with the difference
  participant_data$difference_reaction_time <- ifelse(participant_data$Congruency == "congruent", difference, NA)
  
  return(participant_data)
})

# Step 3: Recombine the list back into a single data frame
difference_reaction_time <- do.call(rbind, difference_list)

# View the result
# View(difference_reaction_time)


# Save the result in a CSV file
write_csv(difference_reaction_time, "difference_reaction_time_stroop.csv")

