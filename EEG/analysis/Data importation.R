library(plyr)
library(reshape2)
library(magrittr)
library(dplyr)


path <- "EEG/data/Session 2/Export/"

#getting the list of files
Session2_gram_files <- list.files(pattern = "*S1_S101.txt", 
                                 path = path, full.names = TRUE)
Session2_ungram_files <- list.files(pattern = "*S1_S102.txt", 
                                path = path, full.names = TRUE)


#Check the file paths
str(Session2_gram_files)
print(Session2_gram_files)
str(Session2_ungram_files)
print(Session2_ungram_files)

#Check if files exist
file.exists(Session2_gram_files)
file.exists(Session2_ungram_files)

# Construct list of data
Session2_gram_list = lapply(1:length(Session2_gram_files),function(x) {
 read.table(Session2_gram_files[x], header=FALSE) } )
#View(Session2_gram_list)

Session2_ungram_list = lapply(1:length(Session2_ungram_files),function(x) {
 read.table(Session2_ungram_files[x], header=FALSE) } )
#View(Session2_ungram_list)

#lists as data frames
Session2_gram_data = ldply(Session2_gram_list, data.frame)
Session2_ungram_data = ldply(Session2_ungram_list, data.frame)

 #Sort out column names
seq = seq(-100, 1098, 2)	# In milliseconds: baseline period, trial
#period, measurement frequency (= 500 Hz)

names(Session2_gram_data) = c('electrode', seq)
names(Session2_ungram_data) = c('electrode', seq)

#Now put it in long format where microvolts is a variable/identifier and shove 
#them together

####### IMPOVE by separating the Session and condition columns
Session2_gram_melted = melt(Session2_gram_data, id.vars="electrode")
Session2_gram_melted = Session2_gram_melted %>%
 mutate(condition = "Session2_grammatical")
names(Session2_gram_melted) = c('Electrode', 'Time', 'Microvolts','Condition')

Session2_ungram_melted = melt(Session2_ungram_data, id.vars="electrode")
Session2_ungram_melted = Session2_ungram_melted %>%
 mutate(condition = "Session2_ungrammatical")
names(Session2_ungram_melted) = c('Electrode', 'Time', 'Microvolts','Condition')
#View(Session2_gram_melted)

# This works fine and creates two really long melted dataframes with data for 
#each electrode and condition. 
#BUT!!! no participant data yet. Couldn't just put in names of files because the 
#switch to long format doesn't like that


#removing the path from the participants' names
file_names_gram <- basename(Session2_gram_files)
files_names_ungram <- basename(Session2_ungram_files)

#Extract participant numbers
participants_gr <- sub("_.*", "", file_names_gram)
head(participants_gr)


participants_ungr = sub("_.*", "", files_names_ungram)
head(participants_ungr)

# create a list where the participant name occurs according 
# to the number of electrodes in that participant's file
Session2_Gram_fulllist = mapply(function(x,y) rep(x, y), participants_gr, 36)
# remove all factors, print long list
Session2_Gram_fulllist = unlist(Session2_Gram_fulllist)     
# correct for individual time point
Session2_Gram_fulllist = rep(Session2_Gram_fulllist, 600)   


#number of repetitions is the number of trials
Session2_Ungram_fulllist = mapply(function(x,y) rep(x, y), participants_ungr,36)
# remove all factors, print long list
Session2_Ungram_fulllist = unlist(Session2_Ungram_fulllist)    
# correct for individual time point
Session2_Ungram_fulllist = rep(Session2_Ungram_fulllist, 600)  

# Adjust the fulllists to match the number of rows in the melted data frame if necessary
Session2_Gram_fulllist <- Session2_Gram_fulllist[1:nrow(Session2_gram_melted)]
Session2_Ungram_fulllist <- Session2_Ungram_fulllist[1:nrow(Session2_ungram_melted)]

# Alternatively, extend the fulllists if they are too short
# Session2_Gram_fulllist <- rep(Session2_Gram_fulllist, length.out = nrow(Session2_gram_melted))
# Session2_Ungram_fulllist <- rep(Session2_Ungram_fulllist, length.out = nrow(Session2_ungram_melted))


# second, create a column in the melted dataframes saying which participant is at each data point
Session2_gram_melted = Session2_gram_melted %>%
  mutate(participants_gr= Session2_Gram_fulllist)

Session2_ungram_melted = Session2_ungram_melted %>%
  mutate(participants_ungr = Session2_Ungram_fulllist)

#testing the frame
print(Session2_gram_melted)
head(Session2_gram_melted)
print(Session2_ungram_melted)
head(Session2_ungram_melted)
View(Session2_ungram_melted)

# Ensure both data frames have the same column names for participants
Session2_gram_melted <- Session2_gram_melted %>%
  rename(participant = participants_gr)

Session2_ungram_melted <- Session2_ungram_melted %>%
  rename(participant = participants_ungr)

# Combine the two data frames
Session2_combined_dtf <- rbind(Session2_gram_melted, Session2_ungram_melted)

# Check the combined data
print(Session2_combined_dtf)
head(Session2_combined_dtf)
View(Session2_combined_dtf)

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
# Check unique values in Electrode and Region columns
print(unique(Session2_combined_dtf$Electrode))
print(unique(Session2_combined_dtf$Region))
# View the updated data frame
View(Session2_combined_dtf)

# Subset the data frame for the N200 time window (200-500 ms)
S2_N200 <- Session2_combined_dtf[Session2_combined_dtf$Time %in% seq(200, 500, 2),]

# Ensure Time and Region are factors
S2_N200$Time <- factor(S2_N200$Time)
S2_N200$Region <- factor(S2_N200$Region)
#region has no levels


# Check for NA, NaN, and Inf values in the Microvolts column
summary(S2_N200$Microvolts)


# Remove rows with NA, NaN, or Inf in the Microvolts column
S2_N200_clean <- S2_N200[!is.na(S2_N200$Microvolts) & !is.nan(S2_N200$Microvolts) & is.finite(S2_N200$Microvolts), ]
levels(S2_N200_clean$Time)
levels(S2_N200_clean$Region)

# Optionally convert the Time column to a factor within the subset
S2_N200$Time <- factor(S2_N200$Time)

# Check the subsetted data
print(S2_N200)
head(S2_N200)
str(S2_N200)
View(S2_N200)



# Check the levels again
table(S2_N200$Time)
table(S2_N200$Region)

# Remove rows with NA, NaN, or Inf in the Microvolts column
S2_N200_clean <- S2_N200[!is.na(S2_N200$Microvolts) & !is.nan(S2_N200$Microvolts) & is.finite(S2_N200$Microvolts), ]

# Perform ANOVA with Region and Time as factors, including the interaction between them
anova_result <- aov(Microvolts ~ Region * Time, data = S2_N200_clean)

# Check the ANOVA table
summary(anova_result)

# Perform ANOVA
# Ensure Time and Region are factors
S2_N200$Time <- factor(S2_N200$Time)
S2_N200$Region <- factor(S2_N200$Region)
# Perform ANOVA with Region and Time as factors, including the interaction between them
anova_result <- aov(Microvolts ~ Region * Time, data = S2_N200)

# Perform ANOVA with Region and Time as factors, including the interaction between them
anova_result <- aov(Microvolts ~ Region * Time, data = S2_N200_clean)

# Check the ANOVA table
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

# dividing electrode placements into areas of interest
# adding a new column with regions of interest labels
S2_N200_df <- S2_N200 %>%
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
# View the resulting data frame
View(S2_N200_df)


#Session 2, P300 (300 - 600 ms)

# Subset the Session2_combined_dtf data frame for the P300 time window 

S2_P300 <- Session2_combined_dtf[Session2_combined_dtf$Time %in% seq(300, 600, 2),]

# Optionally convert the Time column to a factor within the subset
S2_P300$Time <- factor(S2_P300$Time)

# Check the subsetted data
print(S2_P300)
head(S2_P300)
str(S2_P300)
View(S2_P300)

# dividing electrode placements into areas of interest
# adding a new column with regions of interest labels
S2_P300_df <- S2_P300 %>%
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
# View the resulting data frame
View(S2_P300_df)

#Session 2, P600 (400 - 900 ms)

# Subset the Session2_combined_dtf data frame for the P600 time window 

S2_P600 <- Session2_combined_dtf[Session2_combined_dtf$Time %in% seq(400, 900, 2),]

# Optionally convert the Time column to a factor within the subset
S2_P300$Time <- factor(S2_P600$Time)

# Check the subsetted data
print(S2_P600)
head(S2_P600)
str(S2_P600)
View(S2_P600)

# dividing electrode placements into areas of interest
# adding a new column with regions of interest labels
S2_P600_df <- S2_P600 %>%
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
# View the resulting data frame
View(S2_P600_df)

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
anova_result <- aov(Microvolts ~ Electrode * Time, data = df)

# Check the ANOVA table
summary(anova_result)
######################

# averaging all rows and columns so that only one value per region
# Calculating column-wise means excluding NA
# Calculating the overall mean of column means

