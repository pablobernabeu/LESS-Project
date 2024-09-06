#ANOVAs for Session 2
Session2_data_frame <- read.csv("EEG/data/Session 2/Session2_data_frame.csv", header = TRUE)
Session2_N200_data_frame <- read.csv("EEG/data/Session 2/Session2_N200_data_frame.csv", header = TRUE)
Session2_P300_data_frame <- read.csv("EEG/data/Session 2/Session2_P300_data_frame.csv", header = TRUE)
Session2_P600_data_frame <- read.csv("EEG/data/Session 2/Session2_P600_data_frame.csv", header = TRUE)

View(Session2_N200_data_frame
     )





















#setting the columns Time, Region, Grammaticality and Participant_number as factors 
#in order to run ANOVAs
#Session3_melted_data$Time <- as.factor(Session4_melted_data_S1$Time)
Session4_melted_data$Region <- as.factor(Session4_melted_data$Region)
Session4_melted_data$Grammaticality <- as.factor(Session4_melted_data$Grammaticality)
Session4_melted_data$Participant_number <- as.factor(Session4_melted_data$Participant_number)

#Adapt column names to behavioural files (csv)
#start importing that, oonly use Accuracy,  NOT reaction times
#go for anovas


#If you've made time a categorical variable and you want to analyze the 
#differences across levels of time (e.g., different time points), you typically 
#don't need to average the data beforehand. Instead, you can use the raw data 
#and include time as a factor in your ANOVA model.
#Averaging data is more common in cases where you want to reduce variability and 
#focus on the mean responses. For example, if you are interested in the average 
#response of each electrode across multiple time points, you might average the 
#data first. However, for a repeated measures ANOVA or mixed-design ANOVA where
#you want to account for within-subject variability across time points, using 
#the raw data is more appropriate.


# Ensure the Time column is a factor
S3_N200_df$Time <- factor(S3_N200_df$Time)

# Perform ANOVA with Electrode and Time as factors
anova_result <- aov(Activation ~ Electrode * Time, data = df)

# Check the ANOVA table
summary(anova_result)
######################

# averaging all rows and columns so that only one value per region
# Calculating column-wise means excluding NA
# Calculating the overall mean of column means





#in session3, exclude participant 3
#in session 4, exclude participants 11 and 7

