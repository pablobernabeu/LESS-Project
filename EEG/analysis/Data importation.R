library(plyr)
library(reshape2)
library(magrittr)
library(dplyr)
library(ggplot2)


# Grammaticality Grammaticality
# 101: correct 
# 102: violation of interest
# 103: ancillary violation


path <- "EEG/data/Session 2/Export/"
#session2, remove participants 2 and 7

#getting the list of files
Session2_gram_files <- list.files(pattern = "*S1.S101.txt", 
                                 path = path, full.names = TRUE)

Session2_violation_interest <- list.files(pattern = "*S1_S102.txt", 
                                path = path, full.names = TRUE)

Session2_ancillary_violation <- list.files(pattern = "*S1_S103.txt", 
                                          path = path, full.names = TRUE)

#Checking the file paths
print(Session2_gram_files)
print(Session2_violation_interest)
print(Session2_ancillary_violation)

# Constructing lists of data
Session2_gram_list = lapply(1:length(Session2_gram_files),function(x) {
 read.table(Session2_gram_files[x], header=FALSE) } )
#View(Session2_gram_list)

Session2_violation_interest_list = lapply(1:length(Session2_violation_interest),function(x) {
 read.table(Session2_violation_interest[x], header=FALSE) } )
#View(Session2_violation_interest_list)

Session2_ancillary_violation_list = lapply(1:length(Session2_ancillary_violation),function(x) {
  read.table(Session2_ancillary_violation [x], header=FALSE) } )
View(Session2_ancillary_violation_list)

# converting the lists into data frames
Session2_gram_data = ldply(Session2_gram_list, data.frame)
Session2_violation_interest_data = ldply(Session2_violation_interest_list, data.frame)
Session2_ancillary_violation_data = ldply (Session2_ancillary_violation_list, data.frame)


#should i combine the frames first before the manipulation?

#Sorting out column names

# the time column is organised in miliseconds, from -100 to 1098, and counted every 2 ms
seq = seq(-100, 1098, 2)

# the electrode column is formulated as a vector of electrode names that correspond to the time interval sequence
names(Session2_gram_data) = c('Electrode', seq)
names(Session2_violation_interest_data) = c('Electrode', seq)
names(Session2_ancillary_violation_data) = c ('Electrode', seq)
#View(Session2_gram_data)

# working on the participants' name column
#removing the path from the participants' names so their column can be named succintly
file_names_gram <- basename(Session2_gram_files)
files_names_violation_interest <- basename(Session2_violation_interest)
files_names_ancillary_violation <- basename(Session2_ancillary_violation)
#View(Session2_gram_data)

#Extract participant numbers
participants_gr <- sub("_.*", "", file_names_gram)
participants_violint = sub("_.*", "", files_names_violation_interest)
participants_ancvil = sub("_.*", "", files_names_ancillary_violation)

# adding participant columns to the data frames
Session2_gram_data$Participant <- rep(participants_gr, each = nrow(Session2_gram_data) / length(participants_gr))
Session2_violation_interest_data$Participant <- rep(participants_violint, each = nrow(Session2_violation_interest_data) / length(participants_violint))
Session2_ancillary_violation_data$Participant <- rep(participants_ancvil, each = nrow(Session2_ancillary_violation_data) / length(participants_ancvil))

# adding Grammaticality column to the dataframes
Session2_gram_data$Grammaticality <- 'Grammatical'
Session2_violation_interest_data$Grammaticality <- 'Violation of Interest'
Session2_ancillary_violation_data$Grammaticality <- 'Ancillary Violation'

# Combine all data frames into one
Session2_combined_data <- rbind(Session2_gram_data, Session2_violation_interest_data, Session2_ancillary_violation_data)

#dividing the electrodes into brain regions
# Define the mapping of electrodes to regions
electrode_to_region <- c(
  "T7" = "left medial",
  "C3" = "left medial",
  "CP5" = "left medial",
  "T8" = "right medial",
  "C4" = "right medial",
  "CP6" = "right medial",
  "Fp1" = "left anterior",
  "F3" = "left anterior",
  "F7" = "left anterior",
  "FT9" = "left anterior",
  "FC5" = "left anterior",
  "Fp2" = "right anterior",
  "F4" = "right anterior",
  "F8" = "right anterior",
  "FT10" = "right anterior",
  "FC6" = "right anterior",
  "P7" = "left posterior",
  "P3" = "left posterior",
  "O1" = "left posterior",
  "P8" = "right posterior",
  "P4" = "right posterior",
  "O2" = "right posterior",
  "Fz" = "midline anterior",
  "FC1" = "midline anterior",
  "FC2" = "midline anterior",
  "Cz" = "midline medial",
  "CP1" = "midline medial",
  "CP2" = "midline medial",
  "Pz" = "midline posterior",
  "Oz" = "midline posterior"
)

# Add Region column based on electrode_to_region mapping
Session2_combined_data <- Session2_combined_data %>%
  mutate(Region = electrode_to_region[Electrode])

# Melt the combined data frame to convert it from wide to long format
Session2_melted_data_dirty <- melt(Session2_combined_data, id.vars = c('Participant', 'Electrode', 'Grammaticality', 'Region'), variable.name = 'Time', value.name = 'Activation')

# Convert the 'Time' column to numeric
Session2_melted_data_dirty$Time <- as.numeric(as.character(Session2_melted_data_dirty$Time))

# Add the Session column
Session2_melted_data_dirty$Session <- 'Session 2'

# View the resulting melted data
View(Session2_melted_data_dirty)


# Removing rows where any column has NA or NaN values
Session2_melted_data <- Session2_melted_data_dirty %>%
  filter(complete.cases(.))  # Removes rows with NA values in any column

# Alternatively, if you want to remove rows with NaN values specifically
Session2_melted_data <- Session2_melted_data_dirty %>%
  filter(!is.nan(Activation)) %>%
  filter(complete.cases(.))  # Ensure no NA values are left

# View the cleaned data
View(Session2_melted_data)


# making sure that no Nas or NaNs have been introduced by coercion
rows_with_any_na_nan <- Session2_melted_data %>%
  filter(if_any(everything(), ~ is.na(.) | is.nan(.)))

print(rows_with_any_na_nan)


Session2_melted_data$Time <- as.factor(Session2_melted_data$Time)
Session2_melted_data$Region <- as.factor(Session2_melted_data$Region)
Session2_melted_data$Grammaticality <- as.factor(Session2_melted_data$Grammaticality)
Session2_melted_data$Participant <- as.factor(Session2_melted_data$Participant)

########################################################
######################################################


# Subset the data frame for the N200 time window (200-500 ms)
S2_N200 <- Session2_melted_data [Session2_melted_data$Time %in% seq(200, 500, 2),]

View(S2_N200)





















print(unique(Session2_combined_dtf$Region))

# Identify rows that could not be converted to numeric
problematic_rows <- S2_N200 %>%
  filter(is.na(Activation))

# Inspect problematic rows
print(problematic_rows)
View(problematic_rows)

# Ensure Time and Region are factors
S2_N200$Time <- factor(S2_N200$Time)
S2_N200$Region <- factor(S2_N200$Region)

# Check the levels of Time and Region
levels(S2_N200$Time)
levels(S2_N200$Region)

# Check for NA, NaN, and Inf values in the Activation column
summary(S2_N200$Activation)
View(S2_N200)


View(S2_N200)


# checking for NA, NaN, and Inf values in each column
check_values <- function(df) {
  sapply(df, function(x) any(is.na(x) | is.nan(x) | is.infinite(x)))
}

# Apply the function to the S2_N200 data frame
columns_with_issues <- check_values(S2_N200)

# Print columns that have any NA, NaN, or Inf values
names(columns_with_issues[columns_with_issues == TRUE])

# Check the subsetted data
print(S2_N200)
head(S2_N200)
str(S2_N200)
View(S2_N200)

# Perform ANOVA with Region and Time as factors, including the interaction between them
anova_result <- aov(Activation ~ Region * Time, data = S2_N200)

# Summary of Activation to check for NA, NaN, or Inf
summary(S2_N200$Activation)

# Remove rows where Activation is NA, NaN, or Inf
S2_N200_clean <- S2_N200[!is.na(S2_N200$Activation) & !is.nan(S2_N200$Activation) & !is.infinite(S2_N200$Activation), ]

# Check levels and NA presence in Region and Time
summary(factor(S2_N200$Region))
summary(factor(S2_N200$Time))

anova_result <- aov(Activation ~ Region * Time, data = S2_N200_clean)
summary(anova_result)


# Check data types of all relevant columns
sapply(S2_N200, class)

# Convert Activation from character to numeric
S2_N200$Activation <- as.numeric(S2_N200$Activation)

# Check for any NA introduced by the conversion
sum(is.na(S2_N200$Activation))

# Identify rows with NA in Activation after conversion
S2_N200[is.na(S2_N200$Activation), ]

# Assuming you decide to remove rows with NA in Activation
S2_N200_clean <- S2_N200[!is.na(S2_N200$Activation), ]

# Retry the ANOVA
anova_result <- aov(Activation ~ Region * Time, data = S2_N200_clean)
summary(anova_result)



# Check the levels of Region and Time
length(levels(S2_N200_clean$Region))
length(levels(S2_N200_clean$Time))

# Display unique values of Region and Time
unique(S2_N200_clean$Region)
unique(S2_N200_clean$Time)



# Assuming Time has only one level, run ANOVA only on Region
anova_result <- aov(Activation ~ Region, data = S2_N200_clean)
summary(anova_result)




















####################################

#the data must be divided into time windows

#Session 2, N200 (200-500 ms)
# Subset the Session2_combined_dtf data frame for the N200 time window 

S2_N200 <- Session2_combined_dtf[Session2_combined_dtf$Time %in% seq(200, 500, 2),]

# Optionally convert the Time column to a factor within the subset
S2_N200$Time <- factor(S2_N200$Time)

# Check the subsetted data
print(S2_N200)
head(S2_N200)
str(S2_N200)
View(S2_N200)

#####################################

#Session 2, P300 (300 - 600 ms)

# Subset the Session2_combined_dtf data frame for the P300 time window 

S2_P300 <- Session2_combined_dtf[Session2_combined_dtf$Time %in% seq(300, 600, 2),]

# Check the subsetted data
print(S2_P300)
head(S2_P300)
str(S2_P300)
View(S2_P300)

#######################################

#Session 2, P600 (400 - 900 ms)

# Subset the Session2_combined_dtf data frame for the P600 time window 

S2_P600 <- Session2_combined_dtf[Session2_combined_dtf$Time %in% seq(400, 900, 2),]

# Check the subsetted data
print(S2_P600)
head(S2_P600)
str(S2_P600)
View(S2_P600)


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
S2_N200_df$Time <- factor(S2_N200_df$Time)

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

