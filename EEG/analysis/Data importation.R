library(plyr)
library(reshape2)
library(magrittr)
library(dplyr)

#Grammaticality condition
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
#View(Session2_ancillary_violation_list)

# converting the lists into data frames
Session2_gram_data = ldply(Session2_gram_list, data.frame)
Session2_violation_interest_data = ldply(Session2_violation_interest_list, data.frame)
Session2_ancillary_violation_data = ldply (Session2_ancillary_violation_list, data.frame)


#should i combine the frames first before the manipulation?

#Sorting out column names
#time is organised in miliseconds, from -100 to 1098, and counted every 2 ms
seq = seq(-100, 1098, 2)

#assigning column names per electrode and time intervals
names(Session2_gram_data) = c('Electrode', seq)
names(Session2_violation_interest_data) = c('Electrode', seq)
names(Session2_ancillary_violation_data) = c ('Electrode', seq)
#View(Session2_gram_data)

#assigning participants' names
#removing the path from the participants' names so their column can be named accurately
file_names_gram <- basename(Session2_gram_files)
files_names_violation_interest <- basename(Session2_violation_interest)
files_names_ancillary_violation <- basename(Session2_ancillary_violation)
View(Session2_gram_data)

#Extract participant numbers
participants_gr <- sub("_.*", "", file_names_gram)
participants_violint = sub("_.*", "", files_names_violation_interest)
participants_ancvil = sub("_.*", "", files_names_ancillary_violation)

head(participants_gr)
head(participants_violint)
head(participants_ancvil)

####### IMPROVE by separating the Session and condition columns

# Melt the data frames and add a condition column
Session2_gram_melted = melt(Session2_gram_data, id.vars="Electrode") %>%
 mutate(condition = "Session2_grammatical")
names(Session2_gram_melted) = c('Electrode', 'Time', 'Activation','Condition')
#View(Session2_gram_melted)

Session2_violation_interest_melted = melt(Session2_violation_interest_data, id.vars="Electrode") %>%
 mutate(condition = "Session2_violation_interest")
names(Session2_violation_interest_melted) = c('Electrode', 'Time', 'Activation','Condition')
#View(Session2_violation_interest_melted)

Session2_ancillary_violation_melted = melt(Session2_violation_interest_data, id.vars="Electrode") %>%
  mutate(condition = "Session2_ancillary_violation")
names(Session2_ancillary_violation_melted) = c('Electrode', 'Time', 'Activation','Condition')
#View(Session2_ancillary_violation)

# Check the number of rows in each melted data frame
n_gram_rows <- nrow(Session2_gram_melted)
n_violation_interest_rows <- nrow(Session2_violation_interest_melted)
n_ancillary_violation_rows <- nrow(Session2_ancillary_violation_melted)


list(n_gram_rows)
list (n_violation_interest_rows)
list(n_ancillary_violation_rows)














 

# Ensure the number of participants matches the number of rows in the melted data
Session2_Gram_fulllist <- rep(participants_gr, each = n_gram_rows / length(participants_gr))
Session2_violation_interest_fulllist <- rep(participants_violint, each = n_violation_interest_rows / length(participants_violint))
Session2_ancillary_violation_fulllist <- rep(participants_ancvil, each = n_ancillary_violation_rows / length(participants_ancvil))


# Add participant columns to the melted data frames
Session2_gram_melted <- Session2_gram_melted %>%
  mutate(participant = Session2_Gram_fulllist)

Session2_violation_interest_melted <- Session2_violation_interest_melted %>%
  mutate(participant = Session2_violation_interest_fulllist)

Session2_ancillary_violation_melted <- Session2_ancillary_violation_melted %>%
  mutate(participant = Session2_ancillary_violation_fulllist)

# Combine the data frames
Session2_combined_dtf <- bind_rows(
  Session2_gram_melted, 
  Session2_violation_interest_melted, 
  Session2_ancillary_violation_melted
)





















# create participant lists
Session2_Gram_fulllist <- rep(participants_gr, each = nrow(Session2_gram_melted) / length(participants_gr))
Session2_violation_interest_fulllist <- rep(participants_violint, each = nrow(Session2_violation_interest_melted) / length(participants_violint))
Session2_ancillary_violation_fulllist <- rep(participants_ancvil, each = nrow(Session2_ancillary_violation_melted) / length(participants_ancvil))

# Adjust the lengths of the participant lists vectors if they don't match
#Session2_Gram_fulllist <- rep(Session2_Gram_fulllist, length.out = nrow(Session2_gram_melted))
#Session2_violation_interest_fulllist <- rep(Session2_violation_interest_fulllist, length.out = nrow(Session2_violation_interest_melted))
#Session2_ancillary_violation_fulllist <- rep(Session2_ancillary_violation_fulllist, length.out = nrow(Session2_ancillary_violation_melted))

# create a column in the melted dataframes saying which participant is at each data point
Session2_gram_melted <- Session2_gram_melted %>%
  mutate(participant = Session2_Gram_fulllist)

Session2_violation_interest_melted <- Session2_violation_interest_melted %>%
  mutate(participant = Session2_violation_interest_fulllist)

Session2_ancillary_violation_melted <- Session2_ancillary_violation_melted %>%
  mutate(participant = Session2_ancillary_violation_fulllist)

#testing the frame
print(Session2_gram_melted)
head(Session2_gram_melted)
View(Session2_gram_melted)
print(Session2_violation_interest_melted)
head(Session2_violation_interest_melted)
#View(Session2_violation_interest_melted)
print(Session2_ancillary_violation_melted)
head(Session2_ancillary_violation_melted)
#View(Session2_ancillary_violation_melted)


# Combine the two data frames
#Session2_combined_dtf <- rbind(Session2_gram_melted, Session2_violation_interest_melted, Session2_ancillary_violation_melted)
Session2_combined_dtf <- bind_rows(
  Session2_gram_melted, 
  Session2_violation_interest_melted, 
  Session2_ancillary_violation_melted
)


# Check the combined data
print(Session2_combined_dtf)
head(Session2_combined_dtf)
View(Session2_combined_dtf)

# Check unique values in Electrode column before subsetting
print(unique(Session2_combined_dtf$Electrode))

#dividing the electrodes into brain regions
Session2_combined_dtf <- Session2_combined_dtf %>%
  mutate(Region = case_when(
    Electrode %in% c("T7", "C3", "CP5") ~ "left medial",
    Electrode %in% c("T8", "C4", "CP6") ~ "right medial",
    Electrode %in% c("Fp1", "F3", "F7", "FT9", "FC5") ~ "left anterior",
    Electrode %in% c("Fp2", "F4", "F8", "FT10", "FC6") ~ "right anterior",
    Electrode %in% c("P7", "P3", "O1") ~ "left posterior",
    Electrode %in% c("P8", "P4", "O2", "FC6") ~ "right posterior",
    Electrode %in% c("Fz", "FC1", "FC2") ~ "midline anterior",
    Electrode %in% c("Cz", "CP1", "CP2") ~ "midline medial",
    Electrode %in% c("Pz", "Oz") ~ "midline posterior",
    TRUE ~ "Other"
  ))

# Check unique values in Region column
print(unique(Session2_combined_dtf$Region))
# View the updated data frame
View(Session2_combined_dtf)

# Subset the data frame for the N200 time window (200-500 ms)
S2_N200 <- Session2_combined_dtf[Session2_combined_dtf$Time %in% seq(200, 500, 2),]







########################################################
######################################################







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

