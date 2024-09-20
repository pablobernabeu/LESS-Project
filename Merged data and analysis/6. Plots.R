

# ANOVAs for Session 2

# Adapt column names to behavioural files (csv)
# start importing that, only use Accuracy,  NOT reaction times
# go for anovas



# If you've made time a categorical variable and you want to analyze the 
# differences across levels of time (e.g., different time points), you typically 
# don't need to average the data beforehand. Instead, you can use the raw data 
# and include time as a factor in your ANOVA model.
# Averaging data is more common in cases where you want to reduce variability and 
# focus on the mean responses. For example, if you are interested in the average 
# response of each electrode across multiple time points, you might average the 
# data first. However, for a repeated measures ANOVA or mixed-design ANOVA where
# you want to account for within-subject variability across time points, using 
# the raw data is more appropriate

# # # # # # # # # # # # # # # # # # # # # # 

# averaging all rows and columns so that only one value per region
# Calculating column-wise means excluding NA
# Calculating the overall mean of column means


# anovas on the entire dataset to see the progress of activation
# anova_result <- aov(Activation ~ Region, data = Session2_N200_data_frame)
# but maybe do on the entire data frame?

# Load necessary library
library(dplyr)
library(tidyverse)
library(car)
library(ggplot2)
library(forcats)

# ANOVAs on aggregated data to see the overall activation differences between 
# brain regions in the N200, irrespective of time specifics


# For Session 2, time window N200

Session2_N200_data_frame <- read.csv("Raw data/EEG/data/Session 2/Session2_N200_data_frame.csv", header = TRUE)
# View(Session2_N200_data_frame)

# removing participant rqed8 due to incomplete file
Session2_N200_data_frame_filtered <- Session2_N200_data_frame %>%
  filter(Participant_ID != "rqed8")

# aggregating the data to run Anovas
# testing to see if there's an overall effect of region on activation
S2_N200_RegionxActivation <- aov(Activation ~ Region, data = Session2_N200_data_frame_filtered)
head(S2_N200_RegionxActivation)

# seeing where the differences lie between regions
TukeyHSD(S2_N200_RegionxActivation)

# calculating mean and standard error values for plotting and visual interpretation
# The mean is already calculated, using the original data frame to calculate the SD
# Re-run the code using dplyr's functions
# Calculate the mean Activation per Region
mean_activation_S2_N200 <- aggregate(Activation ~ Region, data = Session2_N200_data_frame_filtered, FUN = mean)

# Calculate the standard error (SE) per Region using tapply
se_activation_S2_N200 <- aggregate(Activation ~ Region, data = Session2_N200_data_frame_filtered, 
                          FUN = function(x) sd(x) / sqrt(length(x)))


# Combine the mean and SE into one data frame
S2_N200_Activation_per_Region_plot <- data.frame(
  Region = mean_activation_S2_N200$Region,
  Mean = mean_activation_S2_N200$Activation,
  SE = se_activation_S2_N200$Activation
)

# Print the result
head(S2_N200_Activation_per_Region_plot)

# Reorder regions based on mean activation
S2_N200_Activation_per_Region_plot <- S2_N200_Activation_per_Region_plot %>%
  mutate(Region = fct_reorder(Region, Mean))

# Create a bar plot with error bars using ggplot2
ggplot(S2_N200_Activation_per_Region_plot, aes(x = Region, y = Mean)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  labs(title = "Mean Activation by Brain Region S2_N200", x = "Brain Region", y = "Mean Activation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Calculating the Activation per Region, per Grammaticality

# Calculate mean activation by Region and Grammaticality
mean_activation_S2_N200 <- aggregate(Activation ~ Region + Grammaticality, 
                                     data = Session2_N200_data_frame_filtered, FUN = mean)

# Calculate standard error (SE) by Region and Grammaticality
se_activation_S2_N200 <- aggregate(Activation ~ Region + Grammaticality, 
                                   data = Session2_N200_data_frame_filtered, 
                                   FUN = function(x) sd(x) / sqrt(length(x)))

# Combine the mean and SE into one data frame
S2_N200_Activation_per_Region_plot <- data.frame(
  Region = mean_activation_S2_N200$Region,
  Grammaticality = mean_activation_S2_N200$Grammaticality,
  Mean = mean_activation_S2_N200$Activation,
  SE = se_activation_S2_N200$Activation
)

# Reorder regions based on mean activation
S2_N200_Activation_per_Region_plot <- S2_N200_Activation_per_Region_plot %>%
  mutate(Region = fct_reorder(Region, Mean))

# Create a bar plot with error bars using ggplot2
ggplot(S2_N200_Activation_per_Region_plot, aes(x = Region, y = Mean, fill = Grammaticality)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bars grouped by Grammaticality
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 0.2, position = position_dodge(0.9)) +  # Error bars aligned with bars
  labs(title = "Mean Activation by Brain Region and Grammaticality S2_N200",
       x = "Brain Region", y = "Mean Activation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# visualising the interaction between Activation, Grammaticality, and mini_language

# Calculate mean activation by Region, Grammaticality, and mini_language
mean_activation_S2_N200 <- aggregate(Activation ~ Region + Grammaticality + mini_language, 
                                     data = Session2_N200_data_frame_filtered, FUN = mean)

# Calculate standard error (SE) by Region, Grammaticality, and mini_language
se_activation_S2_N200 <- aggregate(Activation ~ Region + Grammaticality + mini_language, 
                                   data = Session2_N200_data_frame_filtered, 
                                   FUN = function(x) sd(x) / sqrt(length(x)))

# Combine the mean and SE into one data frame
S2_N200_Activation_per_Region_plot <- data.frame(
  Region = mean_activation_S2_N200$Region,
  Grammaticality = mean_activation_S2_N200$Grammaticality,
  mini_language = mean_activation_S2_N200$mini_language,
  Mean = mean_activation_S2_N200$Activation,
  SE = se_activation_S2_N200$Activation
)

# Reorder regions based on mean activation
S2_N200_Activation_per_Region_plot <- S2_N200_Activation_per_Region_plot %>%
  mutate(Region = fct_reorder(Region, Mean))

# Create a bar plot with error bars, showing the interaction between Activation,
# Grammaticality, and mini_language
ggplot(S2_N200_Activation_per_Region_plot, aes(x = Region, y = Mean, fill = Grammaticality)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bars grouped by Grammaticality
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 0.2, position = position_dodge(0.9)) +  
  labs(title = "Mean Activation by Region, Grammaticality, and mini_language S2_N200",
       x = "Brain Region", y = "Mean Activation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ mini_language)  # Create separate subplots for each mini_language








# Calculating the mean activation per region for each participant separately
mean_activation_per_participant <- aggregate(Activation ~ Region + Participant_ID, 
                                             data = Session2_N200_data_frame_filtered, 
                                             FUN = mean)

# View the result
print(mean_activation_per_participant)


ggplot(mean_activation_per_participant, aes(x = Participant_ID, y = Activation, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Activation Per Region for Each Participant",
       x = "Participant",
       y = "Mean Activation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 















# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# For Session 2, time window P300

Session2_P300_data_frame <- read.csv("Raw data/EEG/data/Session 2/Session2_P300_data_frame.csv", header = TRUE)
# View(Session2_P300_data_frame)

# removing participant rqed8 due to incomplete file
Session2_P300_data_frame_filtered <- Session2_P300_data_frame %>%
  filter(Participant_ID != "rqed8")

# S2_P300_aggregated_data <- aggregate(Activation ~ Participant_ID + Region, 
#                                     data = Session2_P300_data_frame, FUN = mean)
# head(S2_P300_aggregated_data)

# aggregating the data to run Anovas
# testing to see if there's an overall effect of region on activation
S2_P300_RegionxActivation <- aov(Activation ~ Region, data = Session2_P300_data_frame_filtered)
head(S2_P300_RegionxActivation)

# seeing where the differences lie between regions
TukeyHSD(S2_P300_RegionxActivation)

# calculating mean and standard error values for plotting and visual interpretation
# The mean is already calculated, using the original data frame to calculate the SD
# Re-run the code using dplyr's functions
# Calculate the mean Activation per Region
mean_activation <- aggregate(Activation ~ Region, data = Session2_P300_data_frame_filtered, FUN = mean)

# Calculate the standard error (SE) per Region using tapply
se_activation <- aggregate(Activation ~ Region, data = Session2_P300_data_frame_filtered, 
                           FUN = function(x) sd(x) / sqrt(length(x)))

# Combine the mean and SE into one data frame
S2_P300_Activation_per_Region_plot <- data.frame(
  Region = mean_activation$Region,
  Mean = mean_activation$Activation,
  SE = se_activation$Activation
)

# Print the result
head(S2_P300_Activation_per_Region_plot)

# Reorder regions based on mean activation
S2_P300_Activation_per_Region_plot <- S2_P300_Activation_per_Region_plot %>%
  mutate(Region = fct_reorder(Region, Mean))

# Create a bar plot with error bars using ggplot2
ggplot(S2_P300_Activation_per_Region_plot, aes(x = Region, y = Mean)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  labs(title = "Mean Activation by Brain Region S2_P300", x = "Brain Region", y = "Mean Activation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Calculating the Activation per Region, per Grammaticality

# Calculate mean activation by Region and Grammaticality
mean_activation_S2_P300 <- aggregate(Activation ~ Region + Grammaticality, 
                                     data = Session2_P300_data_frame_filtered, FUN = mean)

# Calculate standard error (SE) by Region and Grammaticality
se_activation_S2_P300 <- aggregate(Activation ~ Region + Grammaticality, 
                                   data = Session2_P300_data_frame_filtered, 
                                   FUN = function(x) sd(x) / sqrt(length(x)))

# Combine the mean and SE into one data frame
S2_P300_Activation_per_Region_plot <- data.frame(
  Region = mean_activation_S2_P300$Region,
  Grammaticality = mean_activation_S2_P300$Grammaticality,
  Mean = mean_activation_S2_P300$Activation,
  SE = se_activation_S2_P300$Activation
)

# Reorder regions based on mean activation
S2_P300_Activation_per_Region_plot <- S2_P300_Activation_per_Region_plot %>%
  mutate(Region = fct_reorder(Region, Mean))

# Create a bar plot with error bars using ggplot2
ggplot(S2_P300_Activation_per_Region_plot, aes(x = Region, y = Mean, fill = Grammaticality)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bars grouped by Grammaticality
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 0.2, position = position_dodge(0.9)) +  # Error bars aligned with bars
  labs(title = "Mean Activation by Brain Region and Grammaticality S2_P300",
       x = "Brain Region", y = "Mean Activation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# visualising the interaction between Activation, Grammaticality, and mini_language

# Calculate mean activation by Region, Grammaticality, and mini_language
mean_activation_S2_P300 <- aggregate(Activation ~ Region + Grammaticality + mini_language, 
                                     data = Session2_P300_data_frame_filtered, FUN = mean)

# Calculate standard error (SE) by Region, Grammaticality, and mini_language
se_activation_S2_P300 <- aggregate(Activation ~ Region + Grammaticality + mini_language, 
                                   data = Session2_P300_data_frame_filtered, 
                                   FUN = function(x) sd(x) / sqrt(length(x)))

# Combine the mean and SE into one data frame
S2_P300_Activation_per_Region_plot <- data.frame(
  Region = mean_activation_S2_P300$Region,
  Grammaticality = mean_activation_S2_P300$Grammaticality,
  mini_language = mean_activation_S2_P300$mini_language,
  Mean = mean_activation_S2_P300$Activation,
  SE = se_activation_S2_P300$Activation
)

# Reorder regions based on mean activation
S2_P300_Activation_per_Region_plot <- S2_P300_Activation_per_Region_plot %>%
  mutate(Region = fct_reorder(Region, Mean))

# Create a bar plot with error bars, showing the interaction between Activation, 
# Grammaticality, and mini_language
ggplot(S2_P300_Activation_per_Region_plot, aes(x = Region, y = Mean, fill = Grammaticality)) +
  geom_bar(stat = "identity", position = "dodge") +  
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 0.2, position = position_dodge(0.9)) + 
  labs(title = "Mean Activation by Region, Grammaticality, and mini_language, S2_P300",
       x = "Brain Region", y = "Mean Activation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ mini_language)  # Create separate subplots for each mini_language




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# For Session 2, time window P600

Session2_P600_data_frame <- read.csv("Raw data/EEG/data/Session 2/Session2_P600_data_frame.csv", header = TRUE)
# View(Session2_P600_data_frame)

# removing participant rqed8 due to incomplete file
Session2_P600_data_frame_filtered <- Session2_P600_data_frame %>%
  filter(Participant_ID != "rqed8")

# aggregating the data to run Anovas
# testing to see if there's an overall effect of region on activation
S2_P600_RegionxActivation <- aov(Activation ~ Region, data = Session2_P600_data_frame_filtered)
head(S2_P600_RegionxActivation)

# seeing where the differences lie between regions
TukeyHSD(S2_P600_RegionxActivation)

# calculating mean and standard error values for plotting and visual interpretation
# The mean is already calculated, using the original data frame to calculate the SD
# Re-run the code using dplyr's functions
# Calculate the mean Activation per Region
mean_activation <- aggregate(Activation ~ Region, data = Session2_P600_data_frame_filtered, FUN = mean)

# Calculate the standard error (SE) per Region using tapply
se_activation <- aggregate(Activation ~ Region, data = Session2_P600_data_frame_filtered, 
                           FUN = function(x) sd(x) / sqrt(length(x)))

# Combine the mean and SE into one data frame
S2_P600_Activation_per_Region_plot <- data.frame(
  Region = mean_activation$Region,
  Mean = mean_activation$Activation,
  SE = se_activation$Activation
)

# Print the result
head(S2_P600_Activation_per_Region_plot)

# Reorder regions based on mean activation
S2_P600_Activation_per_Region_plot <- S2_P600_Activation_per_Region_plot %>%
  mutate(Region = fct_reorder(Region, Mean))

# Create a bar plot with error bars using ggplot2
ggplot(S2_P600_Activation_per_Region_plot, aes(x = Region, y = Mean)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  labs(title = "Mean Activation by Brain Region S2_P600", x = "Brain Region", y = "Mean Activation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Calculating the Activation per Region, per Grammaticality

# Calculate mean activation by Region and Grammaticality
mean_activation_S2_P600 <- aggregate(Activation ~ Region + Grammaticality, 
                                     data = Session2_P600_data_frame_filtered, FUN = mean)

# Calculate standard error (SE) by Region and Grammaticality
se_activation_S2_P600 <- aggregate(Activation ~ Region + Grammaticality, 
                                   data = Session2_P600_data_frame_filtered, 
                                   FUN = function(x) sd(x) / sqrt(length(x)))

# Combine the mean and SE into one data frame
S2_P600_Activation_per_Region_plot <- data.frame(
  Region = mean_activation_S2_P600$Region,
  Grammaticality = mean_activation_S2_P600$Grammaticality,
  Mean = mean_activation_S2_P600$Activation,
  SE = se_activation_S2_P600$Activation
)

# Reorder regions based on mean activation
S2_P600_Activation_per_Region_plot <- S2_P600_Activation_per_Region_plot %>%
  mutate(Region = fct_reorder(Region, Mean))

# Create a bar plot with error bars using ggplot2
ggplot(S2_P600_Activation_per_Region_plot, aes(x = Region, y = Mean, fill = Grammaticality)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bars grouped by Grammaticality
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 0.2, position = position_dodge(0.9)) +  # Error bars aligned with bars
  labs(title = "Mean Activation by Brain Region and Grammaticality S2_P600",
       x = "Brain Region", y = "Mean Activation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Visualising the activation between Region, Grammaticality, and mini_language

# Calculate mean activation by Region, Grammaticality, and mini_language
mean_activation_S2_P600 <- aggregate(Activation ~ Region + Grammaticality + mini_language, 
                                     data = Session2_P600_data_frame_filtered, FUN = mean)

# Calculate standard error (SE) by Region, Grammaticality, and mini_language
se_activation_S2_P600 <- aggregate(Activation ~ Region + Grammaticality + mini_language, 
                                   data = Session2_P600_data_frame_filtered, 
                                   FUN = function(x) sd(x) / sqrt(length(x)))

# Combine the mean and SE into one data frame
S2_P600_Activation_per_Region_plot <- data.frame(
  Region = mean_activation_S2_P600$Region,
  Grammaticality = mean_activation_S2_P600$Grammaticality,
  mini_language = mean_activation_S2_P600$mini_language,
  Mean = mean_activation_S2_P600$Activation,
  SE = se_activation_S2_P600$Activation
)

# Reorder regions based on mean activation
S2_P600_Activation_per_Region_plot <- S2_P600_Activation_per_Region_plot %>%
  mutate(Region = fct_reorder(Region, Mean))

# Create a bar plot with error bars, showing the interaction between Activation,
# Grammaticality, and mini_language
ggplot(S2_P600_Activation_per_Region_plot, aes(x = Region, y = Mean, fill = Grammaticality)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bars grouped by Grammaticality
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 0.2, position = position_dodge(0.9)) +  
  labs(title = "Mean Activation by Region, Grammaticality, and mini_language S2_P600",
       x = "Brain Region", y = "Mean Activation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ mini_language)  # Create separate subplots for each mini_language





# in session3, exclude participant 3
# in session 4, exclude participants 11 and 7

