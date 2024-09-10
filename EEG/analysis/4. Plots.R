#ANOVAs for Session 2
Session2_data_frame <- read.csv("EEG/data/Session 2/Session2_data_frame.csv", header = TRUE)
Session2_N200_data_frame <- read.csv("EEG/data/Session 2/Session2_N200_data_frame.csv", header = TRUE)
Session2_P300_data_frame <- read.csv("EEG/data/Session 2/Session2_P300_data_frame.csv", header = TRUE)
Session2_P600_data_frame <- read.csv("EEG/data/Session 2/Session2_P600_data_frame.csv", header = TRUE)

View(Session2_N200_data_frame)


Session2_N200_data_frame$Region <- as.factor(Session2_N200_data_frame$Region)
levels(Session2_N200_data_frame$Region)

Session2_N200_data_frame$Grammaticality <- as.factor(Session2_N200_data_frame$Grammaticality)
levels(Session2_N200_data_frame$Grammaticality)

Session2_N200_data_frame$Participant_number <- as.factor(Session2_N200_data_frame$Participant_number)
levels(Session2_N200_data_frame$Participant_number)

#Adapt column names to behavioural files (csv)
#start importing that, only use Accuracy,  NOT reaction times
#go for anovas

# Convert Activation to numeric (if necessary)
Session2_N200_data_frame$Activation <- as.numeric(Session2_N200_data_frame$Activation)

#If you've made time a categorical variable and you want to analyze the 
#differences across levels of time (e.g., different time points), you typically 
#don't need to average the data beforehand. Instead, you can use the raw data 
#and include time as a factor in your ANOVA model.
#Averaging data is more common in cases where you want to reduce variability and 
#focus on the mean responses. For example, if you are interested in the average 
#response of each electrode across multiple time points, you might average the 
#data first. However, for a repeated measures ANOVA or mixed-design ANOVA where
#you want to account for within-subject variability across time points, using 
#the raw data is more appropriate

######################

# averaging all rows and columns so that only one value per region
# Calculating column-wise means excluding NA
# Calculating the overall mean of column means

# Load necessary library
library(dplyr)

# Aggregate data to get mean Activation per Region per Participant
aggregated_data <- Session2_N200_data_frame %>%
  group_by(Participant_ID, Region) %>%
  summarize(Mean_Activation = mean(Activation, na.rm = TRUE))

# View the aggregated data
head(aggregated_data)


cleaned_data <- Session2_N200_data_frame %>%
  filter(!is.na(Activation) & !is.nan(Activation))

# Check if the issue is resolved
aggregated_data <- cleaned_data %>%
  group_by(Participant_ID, Region) %>%
  summarize(Mean_Activation = mean(Activation, na.rm = TRUE))
View(aggregated_data)


# Remove rows with missing values in Activation
Session2_N200_data_frame <- na.omit(Session2_N200_data_frame)

# Ensure Activation is numeric
Session2_N200_data_frame$Activation <- as.numeric(Session2_N200_data_frame$Activation)

# Ensure Electrode is a factor
Session2_N200_data_frame$Region <- as.factor(Session2_N200_data_frame$Electrode)

# Perform one-way ANOVA with Electrode as the factor
anova_result <- aov(Activation ~ Region, data = Session2_N200_data_frame)

# Summary of the ANOVA result
summary(anova_result)
View(Session2_N200_data_frame)

#in session3, exclude participant 3
#in session 4, exclude participants 11 and 7

