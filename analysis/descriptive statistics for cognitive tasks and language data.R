# Background data analysis

library(dplyr)

# loading DGS

# we don;t  have a number for how many times a 3- or 
# 4-number item was presented but we have a number of corretc responses received.


# total.correct.x refers to the total number of correct responses per participant
# across lists
# total.correct.y refers to the total number of correct responses per participant
# for each specific list

# Mean: Average number of correct responses.
# Median: Middle value of correct responses.
# Standard Deviation: Variability in correct responses.
# Minimum and Maximum: Range of correct responses.

DGS <- read.csv("Raw data/Executive functions/analysis_table_DGS.csv")
# View(DGS)

DGS_summary_statistics <-DGS %>%
  summarize(
    mean_correct = mean(total_correct.x, na.rm = TRUE),
    median_correct = median(total_correct.x, na.rm = TRUE),
    sd_correct = sd(total_correct.x, na.rm = TRUE),
    min_correct = min(total_correct.x, na.rm = TRUE),
    max_correct = max(total_correct.x, na.rm = TRUE),
  )

# View(DGS_summary_statistics)

# DGS summary statistics per participant

DGS_summary_statistics_per_participant <- DGS %>%
  filter(!is.na(Participant.Public.ID)) %>%  
  group_by(Participant.Public.ID) %>% 
  summarize(
    mean_correct = mean(total_correct.x, na.rm = TRUE),
    median_correct = median(total_correct.x, na.rm = TRUE),
    sd_correct = sd(total_correct.x, na.rm = TRUE),
    min_correct = min(total_correct.x, na.rm = TRUE),
    max_correct = max(total_correct.x, na.rm = TRUE),
    omissions = sum(omissions, na.rm = TRUE),    
    intrusions = sum(intrusions, na.rm = TRUE),  
    repetitions = sum(repetitions, na.rm = TRUE) 
  )

# View(DGS_summary_statistics_per_participant)

# Examine how participants perform based on different task conditions, such as
# list length. This helps to understand if task difficulty (list length) affects 
# performance.
DGS_performance_by_list_length <- DGS %>%
  group_by(listLength) %>%
  summarize(
    mean_correct = mean(total_correct.y, na.rm = TRUE),
    median_correct = median(total_correct.y, na.rm = TRUE),
    sd_correct = sd(total_correct.y, na.rm = TRUE),
    min_correct = min(total_correct.y, na.rm = TRUE),
    max_correct = max(total_correct.y, na.rm = TRUE)
  )

# View(DGS_performance_by_list_length)


DGS_summary_statistics_per_participant_by_list_length <- DGS %>%
  filter(!is.na(Participant.Public.ID)) %>%  
  group_by(Participant.Public.ID) %>% 
  summarize(
    mean_correct = mean(total_correct.x, na.rm = TRUE),
    median_correct = median(total_correct.x, na.rm = TRUE),
    sd_correct = sd(total_correct.x, na.rm = TRUE),
    min_correct = min(total_correct.x, na.rm = TRUE),
    max_correct = max(total_correct.x, na.rm = TRUE),
    omissions = sum(omissions, na.rm = TRUE),    
    intrusions = sum(intrusions, na.rm = TRUE),  
    repetitions = sum(repetitions, na.rm = TRUE) 
  )

# View(DGS_summary_statistics_per_participant_by_list_length)



# loading the Stroop task

Stroop <- read.csv("Raw data/Executive functions/difference_reaction_time_stroop.csv")

# View(Stroop)
# Separate the data into congruent and incongruent conditions
congruent_data <- Stroop %>% filter(Congruency == "congruent")
incongruent_data <- Stroop %>% filter(Congruency == "incongruent")

# Merge congruent and incongruent data based on Participant ID
Stroop <- Stroop %>%
  inner_join(incongruent_data, by = "Participant.Public.ID", suffix = c("_congruent", "_incongruent"))

# Calculate the Stroop effect (difference in reaction times)
Stroop <- Stroop %>%
  mutate(stroop_effect = mean_reaction_time_stroop_incongruent - mean_reaction_time_stroop_congruent)


# Summary statistics for congruent, incongruent, and Stroop effect
Stroop_summary_statistics <- Stroop %>%
  summarize(
    mean_congruent = mean(mean_reaction_time_stroop_congruent, na.rm = TRUE),
    sd_congruent = sd(mean_reaction_time_stroop_congruent, na.rm = TRUE),
    min_congruent = min(mean_reaction_time_stroop_congruent, na.rm = TRUE),
    max_congruent = max(mean_reaction_time_stroop_congruent, na.rm = TRUE),
    
    mean_incongruent = mean(mean_reaction_time_stroop_incongruent, na.rm = TRUE),
    sd_incongruent = sd(mean_reaction_time_stroop_incongruent, na.rm = TRUE),
    min_incongruent = min(mean_reaction_time_stroop_incongruent, na.rm = TRUE),
    max_incongruent = max(mean_reaction_time_stroop_incongruent, na.rm = TRUE),
    
    mean_stroop_effect = mean(stroop_effect, na.rm = TRUE),
    sd_stroop_effect = sd(stroop_effect, na.rm = TRUE),
    min_stroop_effect = min(stroop_effect, na.rm = TRUE),
    max_stroop_effect = max(stroop_effect, na.rm = TRUE)
  )
# Display the summary statistics
# View(Stroop_summary_statistics)


# Group by Participant and calculate summary statistics per participant
Stroop_summary_statistics_per_participant <- Stroop %>%
  group_by(Participant.Public.ID) %>%
  summarize(
    mean_congruent = mean(mean_reaction_time_stroop_congruent, na.rm = TRUE),
    sd_congruent = sd(mean_reaction_time_stroop_congruent, na.rm = TRUE),
    
    mean_incongruent = mean(mean_reaction_time_stroop_incongruent, na.rm = TRUE),
    sd_incongruent = sd(mean_reaction_time_stroop_incongruent, na.rm = TRUE),
    
    mean_stroop_effect = mean(stroop_effect, na.rm = TRUE),
    sd_stroop_effect = sd(stroop_effect, na.rm = TRUE)
  )



# View(Stroop_summary_statistics_per_participant)



# correlation with stroop, DGS, 3rd
correlation_test <- cor.test(DGS$total_correct.x, Stroop$difference_reaction_time, use = "complete.obs")
# print(correlation_test)




