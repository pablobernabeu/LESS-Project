# Load the Background data CSV file
Background_data <- read.csv("Background/Background_data.csv", header = TRUE)

library(tidyverse)  
library(reshape2)   
library(janitor)   
library(plyr)       

# session2, remove participants 2 and 7 from anovas
Session2path <- "EEG/data/Session 2/Export/"

# creating patterns to import the files and recognise them as distinct conditions
# the final number in the file name indicates the Grammaticality of the trial
# files that end in:
# 101: the trial was grammatical 
# 102: the trial presented a violation of interest
# 103: the trial presented an ancillary violation
# Session 2 investigates Gender agreement, indicated by the marker S1
Session2_grammatical_files <- list.files(pattern = "*S1.S101.txt", 
                                         path = Session2path, full.names = TRUE)

Session2_violation_interest <- list.files(pattern = "*S1_S102.txt", 
                                          path = Session2path, full.names = TRUE)

Session2_ancillary_violation <- list.files(pattern = "*S1_S103.txt", 
                                           path = Session2path, full.names = TRUE)

# Constructing lists of data, one for each condition
Session2_grammatical_list = lapply(1:length(Session2_grammatical_files),function(x) {
  read.table(Session2_grammatical_files[x], header=FALSE) } )
# View(Session2_grammatical_list)

Session2_violation_interest_list = lapply(1:length(Session2_violation_interest),
                                          function(x) {
                                            read.table(Session2_violation_interest[x], header=FALSE) } )
# View(Session2_violation_interest_list)

Session2_ancillary_violation_list = lapply(1:length(Session2_ancillary_violation),
                                           function(x) {
                                             read.table(Session2_ancillary_violation [x], header=FALSE) } )
# View(Session2_ancillary_violation_list)

# converting the lists into data frames
Session2_grammatical_data = ldply(Session2_grammatical_list, data.frame)
Session2_violation_interest_data = ldply(Session2_violation_interest_list, 
                                         data.frame)
Session2_ancillary_violation_data = ldply(Session2_ancillary_violation_list, 
                                           data.frame)

# Sorting out column names and organising them

# time during the recording is organised in milliseconds, from -100 to 1098, 
# and recorded with 2 ms intervals
seq = seq(-100, 1098, 2)

# the Electrode column is formulated as a vector of electrode names that 
# correspond to the time interval sequence
names(Session2_grammatical_data) = c('Electrode', seq)
names(Session2_violation_interest_data) = c('Electrode', seq)
names(Session2_ancillary_violation_data) =c('Electrode', seq)
# View(Session2_grammatical_data)

# working on the participants' name column
# removing the path from the participants' file names
file_names_gram <- basename(Session2_grammatical_files)
files_names_violation_interest <- basename(Session2_violation_interest)
files_names_ancillary_violation <- basename(Session2_ancillary_violation)
# View(Session2_grammatical_data)

# Extracting the participant numbers from the file name
participants_gr <- sub("_.*", "", file_names_gram)
participants_violint = sub("_.*", "", files_names_violation_interest)
participants_ancvil = sub("_.*", "", files_names_ancillary_violation)

# adding a "Participant_number" column to the data frames
Session2_grammatical_data$Participant_number <- rep(participants_gr, each = 
                                                      nrow(Session2_grammatical_data) / length(participants_gr))
Session2_violation_interest_data$Participant_number <- rep(participants_violint, 
                                                           each = nrow(Session2_violation_interest_data) / length(participants_violint))
Session2_ancillary_violation_data$Participant_number <- rep(participants_ancvil, 
                                                            each = nrow(Session2_ancillary_violation_data) / length(participants_ancvil))

# adding a Grammaticality column to the data frames
Session2_grammatical_data$Grammaticality <- 'Grammatical'
Session2_violation_interest_data$Grammaticality <- 'Violation of Interest'
Session2_ancillary_violation_data$Grammaticality <- 'Ancillary Violation'

# Combine all data frames into one
Session2_combined_data <- rbind(Session2_grammatical_data, 
                                Session2_violation_interest_data, Session2_ancillary_violation_data)

# dividing the electrodes into brain regions
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

# Check if the 'Region' column already exists, if not, create it
Session2_combined_data$Region <- ifelse(
  Session2_combined_data$Electrode %in% names(electrode_to_region),
  electrode_to_region[match(Session2_combined_data$Electrode, names(electrode_to_region))],
  NA_character_
)

# Melt the combined data frame to convert it from wide to long format
Session2_melted_data_temporary <- melt(Session2_combined_data, id.vars = 
                                         c('Participant_number', 'Electrode', 'Grammaticality', 'Region'), 
                                       variable.name = 'Time', value.name = 'Activation')

# Convert the 'Time' column to numeric
Session2_melted_data_temporary$Time <- as.numeric(as.character
                                                   (Session2_melted_data_temporary$Time))

# Add a Session column
Session2_melted_data_temporary$Session <- 'Session 2'

# View the resulting melted data
# View(Session2_melted_data_temporary)

# Removing rows where any column has NA or NaN values
Session2_melted_data <- Session2_melted_data_temporary %>%
  filter(complete.cases(.))

# View the cleaned data
# View(Session2_melted_data)
# View(Background_data)


# adding the Background data to the Session2_melted_data
# Converting Participant_number in Backgound_data to character, in order to match 
# the Session2 data. Character has been chosen because Participant_number 
# is categorical (IDs)
# Performing the inner join function,due to the discrepancy between the number of 
# rows between the two data frames, so that no data is deleted

Background_data$Participant_number <- as.character(Background_data$Participant_number)

Session2_Background <- full_join(Background_data, Session2_melted_data, by = "Participant_number")


# setting the columns Time, Region, Grammaticality and Participant_number as factors 
# in order to run ANOVAs
# Session2_melted_data$Time <- as.factor(Session2_melted_data$Time)
Session2_melted_data$Region <- as.factor(Session2_melted_data$Region)
Session2_melted_data$Grammaticality <- as.factor(Session2_melted_data$Grammaticality)
Session2_melted_data$Participant_number <- as.factor(Session2_melted_data$Participant_number)

# Viewing and saving combined data frame
# View(Session2_Background)
write.csv(Session2_Background, "EEG/data/Session2_data_frame.csv", row.names = FALSE)

# Analysing per time window and saving those data frames to be used in the analysis

# Session 2, N200 time window (200-500 ms)
S2_N200 <- Session2_Background [Session2_Background$Time %in% seq(200, 500, 2),]

# View(S2_N200)
write.csv(S2_N200, "EEG/data/Session 2/Session2_N200_data_frame.csv", row.names = FALSE)

# Session 2, P300 (300 - 600 ms)
S2_P300 <- Session2_Background[Session2_Background$Time %in% seq(300, 600, 2),]

# View(S2_P300)
write.csv(S2_P300, "EEG/data/Session 2/Session2_P300_data_frame.csv", row.names = FALSE)

# Session 2, P600 (400 - 900 ms)
S2_P600 <- Session2_Background[Session2_Background$Time %in% seq(400, 900, 2),]

# View(S2_P600)
write.csv(S2_P600, "EEG/data/Session 2/Session2_P600_data_frame.csv", row.names = FALSE)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Session3path <- "EEG/data/Session 3/Export/"

# creating patterns to import the files and recognise them as distinct conditions
# the final number in the file name indicates the Grammaticality of the trial
# files that end in:
# 101: the trial was grammatical 
# 102: the trial presented a violation of interest
# 103: the trial presented an ancillary violation
# Session 3 investigates Gender marking, here GEN, as well as differential 
# object marking, here DOM

Session3_GenderAgreement_grammatical_files <- list.files(pattern = "*S1.S101.txt", 
                                                         path = Session3path, full.names = TRUE)

Session3_GenderAgreement_violation_interest <- list.files(pattern = "*S1_S102.txt", 
                                                          path = Session3path, full.names = TRUE)

Session3_GenderAgreement_ancillary_violation <- list.files(pattern = "*S1_S103.txt", 
                                                           path = Session3path, full.names = TRUE)

Session3_DOM_grammatical_files <- list.files(pattern = "*S2.S101.txt", 
                                             path = Session3path, full.names = TRUE)

Session3_DOM_violation_interest <- list.files(pattern = "*S2_S102.txt", 
                                              path = Session3path, full.names = TRUE)

Session3_DOM_ancillary_violation <- list.files(pattern = "*S2_S103.txt", 
                                               path = Session3path, full.names = TRUE)


# Constructing lists of data, one for each property and condition

# Gender
Session3_GenderAgreement_grammatical_list = lapply(1:length(Session3_GenderAgreement_grammatical_files),function(x) {
  read.table(Session3_GenderAgreement_grammatical_files[x], header=FALSE) } )
# View(Session3_GenderAgreement_grammatical_list)

Session3_GenderAgreement_violation_interest_list = lapply(1:length(Session3_GenderAgreement_violation_interest),
                                                          function(x) { read.table(Session3_GenderAgreement_violation_interest[x], header=FALSE) } )
# View(Session3_GenderAgreement_violation_interest_list)

Session3_GenderAgreement_ancillary_violation_list = lapply(1:length(Session3_GenderAgreement_ancillary_violation),
                                                           function(x) { read.table(Session3_GenderAgreement_ancillary_violation [x], header=FALSE) } )
# View(Session3_GenderAgreement_ancillary_violation_list)


# Differential object marking
Session3_DOM_grammatical_list = lapply(1:length(Session3_DOM_grammatical_files),function(x) {
  read.table(Session3_DOM_grammatical_files[x], header=FALSE) } )
# View(Session3_DOM_grammatical_list)

Session3_DOM_violation_interest_list = lapply(1:length(Session3_DOM_violation_interest),
                                              function(x) { read.table(Session3_DOM_violation_interest[x], header=FALSE) } )
# View(Session3_DOM_violation_interest_list)

Session3_DOM_ancillary_violation_list = lapply(1:length(Session3_DOM_ancillary_violation),
                                               function(x) { read.table(Session3_DOM_ancillary_violation [x], header=FALSE) } )
# View(Session3_DOM_ancillary_violation_list)


# converting the lists into data frames
# Gender
Session3_GenderAgreement_grammatical_data = ldply(Session3_GenderAgreement_grammatical_list, data.frame)
Session3_GenderAgreement_violation_interest_data = ldply(Session3_GenderAgreement_violation_interest_list, 
                                                         data.frame)
Session3_GenderAgreement_ancillary_violation_data = ldply(Session3_GenderAgreement_ancillary_violation_list, 
                                                           data.frame)


# Differential object marking

Session3_DOM_grammatical_data = ldply(Session3_DOM_grammatical_list, data.frame)
Session3_DOM_violation_interest_data = ldply(Session3_DOM_violation_interest_list, 
                                             data.frame)
Session3_DOM_ancillary_violation_data = ldply(Session3_DOM_ancillary_violation_list, 
                                               data.frame)

# Sorting out column names and organising them

# time during the recording is organised in milliseconds, from -100 to 1098, 
# and recorded with 2 ms intervals
seq = seq(-100, 1098, 2)

# the electrode column is formulated as a vector of electrode names that 
# correspond to the time interval sequence
# For Gender
names(Session3_GenderAgreement_grammatical_data) = c('Electrode', seq)
names(Session3_GenderAgreement_violation_interest_data) = c('Electrode', seq)
names(Session3_GenderAgreement_ancillary_violation_data) =c('Electrode', seq)
# View(Session3_GenderAgreement_grammatical_data)

# For Differnetial object marking
names(Session3_DOM_grammatical_data) = c('Electrode', seq)
names(Session3_DOM_violation_interest_data) = c('Electrode', seq)
names(Session3_DOM_ancillary_violation_data) =c('Electrode', seq)


# working on the participants' name column
# removing the path from the participants' file names
# for Gender
file_names_S3_GenderAgreement_grammatical <- basename(Session3_GenderAgreement_grammatical_files)
files_names_S3_GenderAgreement_violation_interest <- basename(Session3_GenderAgreement_violation_interest)
files_names_S3_GenderAgreement_ancillary_violation <- basename(Session3_GenderAgreement_ancillary_violation)
# View(Session2_grammatical_data)

# for Differential Object marking
file_names_S3_DOM_grammatical <- basename(Session3_DOM_grammatical_files)
files_names_S3_DOM_violation_interest <- basename(Session3_DOM_violation_interest)
files_names_S3_DOM_ancillary_violation <- basename(Session3_DOM_ancillary_violation)
# View(Session2_grammatical_data)

# Extracting the participant numbers from the file name
# for gender
participants_S3_GenderAgreement_grammatical <- sub("_.*", "", file_names_S3_GenderAgreement_grammatical)
participants_S3_GenderAgreement_violation_interest = sub("_.*", "", files_names_S3_GenderAgreement_violation_interest)
participants_S3_GenderAgreement_ancillary_violation = sub("_.*", "", files_names_S3_GenderAgreement_ancillary_violation)

# for differential object makring
participants_S3_DOM_grammatical <- sub("_.*", "", file_names_S3_DOM_grammatical)
participants_S3_DOM_violation_interest = sub("_.*", "", files_names_S3_DOM_violation_interest)
participants_S3_DOM_ancillary_violation = sub("_.*", "", files_names_S3_DOM_ancillary_violation)

# adding a "Participant_number" column to the data frames
# for Gender
Session3_GenderAgreement_grammatical_data$Participant_number <- rep(participants_S3_GenderAgreement_grammatical, each = 
                                                                      nrow(Session3_GenderAgreement_grammatical_data) / length(participants_S3_GenderAgreement_grammatical))
Session3_GenderAgreement_violation_interest_data$Participant_number <- rep(participants_S3_GenderAgreement_violation_interest, 
                                                                           each = nrow(Session3_GenderAgreement_violation_interest_data) / length(participants_S3_GenderAgreement_violation_interest))
Session3_GenderAgreement_ancillary_violation_data$Participant_number <- rep(participants_S3_GenderAgreement_ancillary_violation, 
                                                                            each = nrow(Session3_GenderAgreement_ancillary_violation_data) / length(participants_S3_GenderAgreement_ancillary_violation))

# for Differential object marking
Session3_DOM_grammatical_data$Participant_number <- rep(participants_S3_DOM_grammatical, each = 
                                                          nrow(Session3_DOM_grammatical_data) / length(participants_S3_DOM_grammatical))
Session3_DOM_violation_interest_data$Participant_number <- rep(participants_S3_DOM_violation_interest, 
                                                               each = nrow(Session3_DOM_violation_interest_data) / length(participants_S3_DOM_violation_interest))
Session3_DOM_ancillary_violation_data$Participant_number <- rep(participants_S3_DOM_ancillary_violation, 
                                                                each = nrow(Session3_DOM_ancillary_violation_data) / length(participants_S3_DOM_ancillary_violation))

# adding a Grammaticality column to the data frames
Session3_GenderAgreement_grammatical_data$Grammaticality <- 'Grammatical'
Session3_GenderAgreement_violation_interest_data$Grammaticality <- 'Violation of Interest'
Session3_GenderAgreement_ancillary_violation_data$Grammaticality <- 'Ancillary Violation'

Session3_DOM_grammatical_data$Grammaticality <- 'Grammatical'
Session3_DOM_violation_interest_data$Grammaticality <- 'Violation of Interest'
Session3_DOM_ancillary_violation_data$Grammaticality <- 'Ancillary Violation'


# Combine all data frames into one
Session3_combined_data <- rbind(Session3_GenderAgreement_grammatical_data, Session3_GenderAgreement_violation_interest_data,
                                Session3_GenderAgreement_ancillary_violation_data,Session3_DOM_grammatical_data,
                                Session3_DOM_violation_interest_data, Session3_DOM_ancillary_violation_data)

# View(Session2_combined_data)


# dividing the electrodes into brain regions
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

# Add a Region column based on the electrode_to_region mapping
Session3_combined_data <- Session3_combined_data %>%
  mutate(Region = ifelse(Electrode %in% names(electrode_to_region),
                         electrode_to_region[Electrode],
                         NA_character_))

# Add a Region column on the data frame based on the electrode_to_region mapping
Session3_combined_data <- Session3_combined_data %>%
  mutate(Region = electrode_to_region[Electrode])


# Melt the combined data frame to convert it from wide to long format
Session3_melted_data_temporary <- melt(Session3_combined_data, id.vars = 
                                         c('Participant_number', 'Electrode', 'Grammaticality', 'Region'), 
                                       variable.name = 'Time', value.name = 'Activation')

# Convert the 'Time' column to numeric
Session3_melted_data_temporary$Time <- as.numeric(as.character
                                                   (Session3_melted_data_temporary$Time))

# Add a Session column
Session3_melted_data_temporary$Session <- 'Session 3'

# View the resulting melted data
# View(Session3_melted_data_temporary)

# Removing rows where any column has NA or NaN values
Session3_melted_data <- Session3_melted_data_temporary %>%
  filter(complete.cases(.))

# adding the Background data to the Session3_melted_data
# Converting Participant_number in Backgound_data to character, in order to match 
# the Session2 data. Character has been chosen because Participant_number 
# is categorical (IDs)
# Performing the inner join function,due to the discrepancy between the number of 
# rows between the two data frames, so that no data is deleted

Backgound_data$Participant_number <- as.character(Backgound_data$Participant_number)

Session3_Background <- full_join(Backgound_data, Session3_melted_data, by = "Participant_number")


# setting the columns Time, Region, Grammaticality and Participant_number as factors 
# in order to run ANOVAs
# Session2_melted_data$Time <- as.factor(Session3_melted_data$Time)
Session3_melted_data$Region <- as.factor(Session3_melted_data$Region)
Session3_melted_data$Grammaticality <- as.factor(Session3_melted_data$Grammaticality)
Session3_melted_data$Participant_number <- as.factor(Session3_melted_data$Participant_number)

# View and save combined data frame
# View(Session3_Background)
write.csv(Session3_Background, "EEG/data/Session3_data_frame.csv", row.names = FALSE)

# separating into data frames per time window and saving them for analysis
# Session 3, N200 time window (200-500 ms)
S3_N200 <- Session3_Background [Session3_Background$Time %in% seq(200, 500, 2),]

# View(S3_N200)
write.csv(S3_N200, "EEG/data/Session 3/Session3_N200_data_frame.csv", row.names = FALSE)

# Session 3, P300 (300 - 600 ms)
S3_P300 <- Session2_Background[Session3_Background$Time %in% seq(300, 600, 2),]

# View(S3_P300)
write.csv(S3_P300, "EEG/data/Session 3/Session3_P300_data_frame.csv", row.names = FALSE)

# Session 3, P600 (400 - 900 ms)
S3_P600 <- Session2_Background[Session3_Background$Time %in% seq(400, 900, 2),]

# View(S3_P600)
write.csv(S3_P600, "EEG/data/Session 3/Session3_P600_data_frame.csv", row.names = FALSE)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Session 4

Session4path <- "EEG/data/Session 4/Export/"


# creating patterns to import the files and recognise them as distinct conditions
# the final number in the file name indicates the Grammaticality of the trial
# files that end in:
# 101: the trial was grammatical 
# 102: the trial presented a violation of interest
# 103: the trial presented an ancillary violation
# Session 4 investigates Gender marking, here GEN, as well as differential 
# object marking, here DOM, as well as Verb-Object bumber Agreement, here VOA

Session4_GenderAgreement_grammatical_files <- list.files(pattern = "*S1_S101.txt", 
                                                         path = Session4path, full.names = TRUE)

Session4_GenderAgreement_violation_interest <- list.files(pattern = "*S1_S102.txt", 
                                                          path = Session4path, full.names = TRUE)

Session4_GenderAgreement_ancillary_violation <- list.files(pattern = "*S1_S103.txt", 
                                                           path = Session4path, full.names = TRUE)

Session4_DOM_grammatical_files <- list.files(pattern = "*S2_S101.txt", 
                                             path = Session4path, full.names = TRUE)

Session4_DOM_violation_interest <- list.files(pattern = "*S2_S102.txt", 
                                              path = Session4path, full.names = TRUE)

Session4_DOM_ancillary_violation <- list.files(pattern = "*S2_S103.txt", 
                                               path = Session4path, full.names = TRUE)


Session4_VOA_grammatical_files <- list.files(pattern = "*S3_S101.txt", 
                                             path = Session4path, full.names = TRUE)

Session4_VOA_violation_interest <- list.files(pattern = "*S3_S102.txt", 
                                              path = Session4path, full.names = TRUE)

Session4_VOA_ancillary_violation <- list.files(pattern = "*S3_S103.txt", 
                                               path = Session4path, full.names = TRUE)

# constructing lists of data from the files, once for each condition

Session4_GenderAgreement_grammatical_list = lapply(1:length(Session4_GenderAgreement_grammatical_files),function(x) {
  read.table(Session4_GenderAgreement_grammatical_files[x], header=FALSE) } )
# View(Session3_GenderAgreement_grammatical_list)

Session4_GenderAgreement_violation_interest_list = 
  lapply(1:length(Session4_GenderAgreement_violation_interest),
         function(x) { read.table(Session4_GenderAgreement_violation_interest [x], header=FALSE) } )
# View(Session4_GenderAgreement_ancillary_violation_list)

Session4_GenderAgreement_ancillary_violation_list = 
  lapply(1:length(Session4_GenderAgreement_ancillary_violation),
         function(x) { read.table(Session4_GenderAgreement_ancillary_violation [x], header=FALSE) } )
# View(Session4_GenderAgreement_ancillary_violation_list)


Session4_DOM_grammatical_list = lapply(1:length(Session4_DOM_grammatical_files),function(x) {
  read.table(Session4_DOM_grammatical_files[x], header=FALSE) } )
# View(Session4_DOM_grammatical_list)

Session4_DOM_violation_interest_list = lapply(1:length(Session4_DOM_violation_interest),
                                              function(x) { read.table(Session4_DOM_violation_interest[x], header=FALSE) } )
# View(Session4_DOM_violation_interest_list)

Session4_DOM_ancillary_violation_list = lapply(1:length(Session4_DOM_ancillary_violation),
                                               function(x) { read.table(Session4_DOM_ancillary_violation [x], header=FALSE) } )
# View(Session4_DOM_ancillary_violation_list)

Session4_VOA_grammatical_list = lapply(1:length(Session4_VOA_grammatical_files),function(x) {
  read.table(Session4_VOA_grammatical_files[x], header=FALSE) } )
# View(Session4_VOA_grammatical_list)

Session4_VOA_violation_interest_list = lapply(1:length(Session4_VOA_violation_interest),
                                              function(x) { read.table(Session4_VOA_violation_interest[x], header=FALSE) } )
# View(Session4_VOA_violation_interest_list)

Session4_VOA_ancillary_violation_list = lapply(1:length(Session4_VOA_ancillary_violation),
                                               function(x) { read.table(Session4_VOA_ancillary_violation [x], header=FALSE) } )
# View(Session4_VOA_ancillary_violation_list)

# constructing data frames 
Session4_GenderAgreement_grammatical_data = ldply(Session4_GenderAgreement_grammatical_list, data.frame)
Session4_GenderAgreement_violation_interest_data = ldply(Session4_GenderAgreement_violation_interest_list, 
                                                         data.frame)
Session4_GenderAgreement_ancillary_violation_data = ldply(Session4_GenderAgreement_ancillary_violation_list, 
                                                           data.frame)

Session4_DOM_grammatical_data = ldply(Session4_DOM_grammatical_list, data.frame)
Session4_DOM_violation_interest_data = ldply(Session4_DOM_violation_interest_list, 
                                             data.frame)
Session4_DOM_ancillary_violation_data = ldply(Session4_DOM_ancillary_violation_list, 
                                               data.frame)


Session4_VOA_grammatical_data = ldply(Session4_VOA_grammatical_list, data.frame)
Session4_VOA_violation_interest_data = ldply(Session4_VOA_violation_interest_list, 
                                             data.frame)
Session4_VOA_ancillary_violation_data = ldply(Session4_VOA_ancillary_violation_list, 
                                               data.frame)
# Sorting out column names and organising them

# time during the recording is organised in milliseconds, from -100 to 1098, 
# and recorded with 2 ms intervals
seq = seq(-100, 1098, 2)


# the electrode column is formulated as a vector of electrode names that 
# correspond to the time interval sequence
names(Session4_GenderAgreement_grammatical_data) = c('Electrode', seq)
names(Session4_GenderAgreement_violation_interest_data) = c('Electrode', seq)
names(Session4_GenderAgreement_ancillary_violation_data) =c('Electrode', seq)
# View(Session4_GenderAgreement_grammatical_data)

names(Session4_DOM_grammatical_data) = c('Electrode', seq)
names(Session4_DOM_violation_interest_data) = c('Electrode', seq)
names(Session4_DOM_ancillary_violation_data) =c('Electrode', seq)
# View(Session4_DOM_grammatical_data)

names(Session4_VOA_grammatical_data) = c('Electrode', seq)
names(Session4_VOA_violation_interest_data) = c('Electrode', seq)
names(Session4_VOA_ancillary_violation_data) =c('Electrode', seq)
# View(Session4_VOA_grammatical_data)

# working on the participants' name column
# removing the path from the participants' file names
file_names_S4_GenderAgreement_grammatical <- basename(Session4_GenderAgreement_grammatical_files)
files_names_S4_GenderAgreement_violation_interest <- basename(Session4_GenderAgreement_violation_interest)
files_names_S4_GenderAgreement_ancillary_violation <- basename(Session4_GenderAgreement_ancillary_violation)

# View(file_names_S4_GenderAgreement_grammatical)

file_names_S4_DOM_grammatical <- basename(Session4_DOM_grammatical_files)
files_names_S4_DOM_violation_interest <- basename(Session4_DOM_violation_interest)
files_names_S4_DOM_ancillary_violation <- basename(Session4_DOM_ancillary_violation)

# View(file_names_S4_DOM_grammatical)

file_names_S4_VOA_grammatical <- basename(Session4_VOA_grammatical_files)
files_names_S4_VOA_violation_interest <- basename(Session4_VOA_violation_interest)
files_names_S4_VOA_ancillary_violation <- basename(Session4_VOA_ancillary_violation)

# View(file_names_S4_VOA_grammatical)

# Extracting the participant numbers from the file name
participants_S4_GenderAgreement_grammatical <- sub("_.*", "", file_names_S4_GenderAgreement_grammatical)
participants_S4_GenderAgreement_violint = sub("_.*", "", files_names_S4_GenderAgreement_violation_interest)
participants_S4_GenderAgreement_ancvil = sub("_.*", "", files_names_S4_GenderAgreement_ancillary_violation)

participants_S4_DOM_grammatical <- sub("_.*", "", file_names_S4_DOM_grammatical)
participants_S4_DOM_violint = sub("_.*", "", files_names_S4_DOM_violation_interest)
participants_S4_DOM_ancvil = sub("_.*", "", files_names_S4_DOM_ancillary_violation)

participants_S4_VOA_grammatical <- sub("_.*", "", file_names_S4_VOA_grammatical)
participants_S4_VOA_violint = sub("_.*", "", files_names_S4_VOA_violation_interest)
participants_S4_VOA_ancvil = sub("_.*", "", files_names_S4_VOA_ancillary_violation)


# adding a "Participant_number" column to the data frames
Session4_GenderAgreement_grammatical_data$Participant_number <- rep(participants_S4_GenderAgreement_grammatical, 
                                                                    each = nrow(Session4_GenderAgreement_grammatical_data) / length(participants_S4_GenderAgreement_grammatical))
Session4_GenderAgreement_violation_interest_data$Participant_number <- rep(participants_S4_GenderAgreement_violint, 
                                                                           each = nrow(Session4_GenderAgreement_violation_interest_data) / length(participants_S4_GenderAgreement_violint))
Session4_GenderAgreement_ancillary_violation_data$Participant_number <- rep(participants_S4_GenderAgreement_ancvil, 
                                                                            each = nrow(Session4_GenderAgreement_ancillary_violation_data) / length(participants_S4_GenderAgreement_ancvil))


Session4_DOM_grammatical_data$Participant_number <- rep(participants_S4_DOM_grammatical, 
                                                        each = nrow(Session4_DOM_grammatical_data) / length(participants_S4_DOM_grammatical))
Session4_DOM_violation_interest_data$Participant_number <- rep(participants_S4_DOM_violint, 
                                                               each = nrow(Session4_DOM_violation_interest_data) / length(participants_S4_DOM_violint))
Session4_DOM_ancillary_violation_data$Participant_number <- rep(participants_S4_DOM_ancvil, 
                                                                each = nrow(Session4_DOM_ancillary_violation_data) / length(participants_S4_DOM_ancvil))

Session4_VOA_grammatical_data$Participant_number <- rep(participants_S4_VOA_grammatical, 
                                                        each = nrow(Session4_VOA_grammatical_data) / length(participants_S4_VOA_grammatical))
Session4_VOA_violation_interest_data$Participant_number <- rep(participants_S4_VOA_violint, 
                                                               each = nrow(Session4_VOA_violation_interest_data) / length(participants_S4_VOA_violint))
Session4_VOA_ancillary_violation_data$Participant_number <- rep(participants_S4_VOA_ancvil, 
                                                                each = nrow(Session4_VOA_ancillary_violation_data) / length(participants_S4_VOA_ancvil))

# Adding a Property column to the data frames
Session4_GenderAgreement_grammatical_data$Property <- 'Gender_Agreement'
Session4_GenderAgreement_violation_interest_data$Property <- 'Gender_Agreement'
Session4_GenderAgreement_ancillary_violation_data$Property <- 'Gender_Agreement'

Session4_DOM_grammatical_data$Property <- 'Differential_Object_Marking'
Session4_DOM_violation_interest_data$Property <- 'Differential_Object_Marking'
Session4_DOM_ancillary_violation_data$Property <- 'Differential_Object_Marking'

Session4_VOA_grammatical_data$Property <- 'Verb_Object_Number_Agreement'
Session4_VOA_violation_interest_data$Property <- 'Verb_Object_Number_Agreement'
Session4_VOA_ancillary_violation_data$Property <- 'Verb_Object_Number_Agreement'

# adding a Grammaticality column to the data frames

Session4_GenderAgreement_grammatical_data$Grammaticality <- 'Grammatical'
Session4_GenderAgreement_violation_interest_data$Grammaticality <- 'Violation_of_Interest'
Session4_GenderAgreement_ancillary_violation_data$Grammaticality <- 'Ancillary_Violation'

Session4_DOM_grammatical_data$Grammaticality <- 'Grammatical'
Session4_DOM_violation_interest_data$Grammaticality <- 'Violation_of_Interest'
Session4_DOM_ancillary_violation_data$Grammaticality <- 'Ancillary_Violation'

Session4_VOA_grammatical_data$Grammaticality <- 'Grammatical'
Session4_VOA_violation_interest_data$Grammaticality <- 'Violation_of_Interest'
Session4_VOA_ancillary_violation_data$Grammaticality <- 'Ancillary_Violation'

# Combine all data frames into one
Session4_combined_data <- rbind(Session4_GenderAgreement_grammatical_data,
                                Session4_GenderAgreement_violation_interest_data, 
                                Session4_GenderAgreement_ancillary_violation_data,
                                Session4_DOM_grammatical_data, 
                                Session4_DOM_violation_interest_data,  
                                Session4_DOM_ancillary_violation_data,
                                Session4_VOA_grammatical_data, 
                                Session4_VOA_violation_interest_data,  
                                Session4_VOA_ancillary_violation_data)

# View(Session4_combined_data)


# dividing the electrodes into brain regions
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

# Add a Region column on the data frame based on the electrode_to_region mapping
Session4_combined_data <- Session4_combined_data %>%
  mutate(Region = electrode_to_region[Electrode])

# Melt the combined data frame to convert it from wide to long format
Session4_melted_data_temporary <- melt(Session4_combined_data, id.vars = 
                                         c('Participant_number', 'Electrode', 'Grammaticality', 'Region', 'Property'), 
                                       variable.name = 'Time', value.name = 'Activation')

# Convert the 'Time' column to numeric
Session4_melted_data_temporary$Time <- as.numeric(as.character
                                                   (Session4_melted_data_temporary$Time))

# Add a Session column
Session4_melted_data_temporary$Session <- 'Session 4'

# View the resulting melted data
# View(Session4_melted_data_temporary)

# Removing rows where any column has NA or NaN values
Session4_melted_data <- Session4_melted_data_temporary %>%
  filter(complete.cases(.))

# View the cleaned data
# View(Session4_melted_data)

# adding the Background data to the Session2_melted_data
# Converting Participant_number in Backgound_data to character, in order to match 
# the Session2 data. Character has been chosen because Participant_number 
# is categorical (IDs)
# Performing the inner join function,due to the discrepancy between the number of 
# rows between the two data frames, so that no data is deleted

Backgound_data$Participant_number <- as.character(Backgound_data$Participant_number)
Session4_Background <- full_join(Backgound_data, Session4_melted_data, by = "Participant_number")

# View and save combined data frame and time windows to be analysed
# View(Session4_Background)

write.csv(Session4_Background, "EEG/data/Session4_data_frame.csv", row.names = FALSE)

# Session 4, N200 time window (200-500 ms)
S4_N200 <- Session4_Background [Session4_Background$Time %in% seq(200, 500, 2),]

# View(S4_N200)
write.csv(S4_N200, "EEG/data/Session 4/Session4_N200_data_frame.csv", row.names = FALSE)

# Session 4, P300 (300 - 600 ms)
S4_P300 <- Session4_Background[Session4_Background$Time %in% seq(300, 600, 2),]

# View(S4_P300)
write.csv(S4_P300, "EEG/data/Session 4/Session4_P300_data_frame.csv", row.names = FALSE)

# Session 4, P600 (400 - 900 ms)
S4_P600 <- Session4_Background[Session4_Background$Time %in% seq(400, 900, 2),]

# View(S4_P600)
write.csv(S4_P600, "EEG/data/Session 4/Session4_P600_data_frame.csv", row.names = FALSE)

# # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # 


# Session 6 importation



Session6path <- "EEG/data/Session 6/Export/"


# creating patterns to import the files and recognise them as distinct conditions
# the final number in the file name indicates the Grammaticality of the trial
# files that end in:
# 101: the trial was grammatical 
# 102: the trial presented a violation of interest
# 103: the trial presented an ancillary violation
# Session 4 investigates Gender marking, here GEN, as well as differential 
# object marking, here DOM, as well as Verb-Object number Agreement, here VOA

Session6_GenderAgreement_grammatical_files <- list.files(pattern = "*S1_S101.txt", 
                                                         path = Session6path, full.names = TRUE)

Session6_GenderAgreement_violation_interest <- list.files(pattern = "*S1_S102.txt", 
                                                          path = Session6path, full.names = TRUE)

Session6_GenderAgreement_ancillary_violation <- list.files(pattern = "*S1_S103.txt", 
                                                           path = Session6path, full.names = TRUE)

Session6_DOM_grammatical_files <- list.files(pattern = "*S2_S101.txt", 
                                             path = Session6path, full.names = TRUE)

Session6_DOM_violation_interest <- list.files(pattern = "*S2_S102.txt", 
                                              path = Session6path, full.names = TRUE)

Session6_DOM_ancillary_violation <- list.files(pattern = "*S2_S103.txt", 
                                               path = Session6path, full.names = TRUE)


Session6_VOA_grammatical_files <- list.files(pattern = "*S3_S101.txt", 
                                             path = Session6path, full.names = TRUE)

Session6_VOA_violation_interest <- list.files(pattern = "*S3_S102.txt", 
                                              path = Session6path, full.names = TRUE)

Session6_VOA_ancillary_violation <- list.files(pattern = "*S3_S103.txt", 
                                               path = Session6path, full.names = TRUE)

# constructing lists of data from the files, once for each condition

Session6_GenderAgreement_grammatical_list = lapply(1:length(Session6_GenderAgreement_grammatical_files),function(x) {
  read.table(Session6_GenderAgreement_grammatical_files[x], header=FALSE) } )
# View(Session6_GenderAgreement_grammatical_list)

Session6_GenderAgreement_violation_interest_list = lapply(1:length(Session6_GenderAgreement_violation_interest),
                                                          function(x) { read.table(Session6_GenderAgreement_violation_interest [x], header=FALSE) } )
# View(Session6_GenderAgreement_ancillary_violation_list)

Session6_GenderAgreement_ancillary_violation_list = lapply(1:length(Session6_GenderAgreement_ancillary_violation),
                                                           function(x) { read.table(Session6_GenderAgreement_ancillary_violation [x], header=FALSE) } )
# View(Session6_GenderAgreement_ancillary_violation_list)


Session6_DOM_grammatical_list = lapply(1:length(Session6_DOM_grammatical_files),function(x) {
  read.table(Session6_DOM_grammatical_files[x], header=FALSE) } )
# View(Session6_DOM_grammatical_list)

Session6_DOM_violation_interest_list = lapply(1:length(Session6_DOM_violation_interest),
                                              function(x) { read.table(Session6_DOM_violation_interest[x], header=FALSE) } )
# View(Session4_DOM_violation_interest_list)

Session6_DOM_ancillary_violation_list = lapply(1:length(Session6_DOM_ancillary_violation),
                                               function(x) { read.table(Session6_DOM_ancillary_violation [x], header=FALSE) } )
# View(Session6_DOM_ancillary_violation_list)

Session6_VOA_grammatical_list = lapply(1:length(Session6_VOA_grammatical_files),function(x) {
  read.table(Session6_VOA_grammatical_files[x], header=FALSE) } )
# View(Session6_VOA_grammatical_list)

Session6_VOA_violation_interest_list = lapply(1:length(Session6_VOA_violation_interest),
                                              function(x) { read.table(Session6_VOA_violation_interest[x], header=FALSE) } )
# View(Session6_VOA_violation_interest_list)

Session6_VOA_ancillary_violation_list = lapply(1:length(Session6_VOA_ancillary_violation),
                                               function(x) { read.table(Session6_VOA_ancillary_violation [x], header=FALSE) } )
# View(Session6_VOA_ancillary_violation_list)


# constructing data frames 
Session6_GenderAgreement_grammatical_data = ldply(Session6_GenderAgreement_grammatical_list, data.frame)
Session6_GenderAgreement_violation_interest_data = ldply(Session6_GenderAgreement_violation_interest_list, 
                                                         data.frame)
Session6_GenderAgreement_ancillary_violation_data = ldply(Session6_GenderAgreement_ancillary_violation_list, 
                                                           data.frame)

Session6_DOM_grammatical_data = ldply(Session6_DOM_grammatical_list, data.frame)
Session6_DOM_violation_interest_data = ldply(Session6_DOM_violation_interest_list, 
                                             data.frame)
Session6_DOM_ancillary_violation_data = ldply(Session6_DOM_ancillary_violation_list, 
                                               data.frame)


Session6_VOA_grammatical_data = ldply(Session6_VOA_grammatical_list, data.frame)
Session6_VOA_violation_interest_data = ldply(Session6_VOA_violation_interest_list, 
                                             data.frame)
Session6_VOA_ancillary_violation_data = ldply(Session6_VOA_ancillary_violation_list, 
                                               data.frame)
# Sorting out column names and organising them

# time during the recording is organised in milliseconds, from -100 to 1098, 
# and recorded with 2 ms intervals
seq = seq(-100, 1098, 2)


# the electrode column is formulated as a vector of electrode names that 
# correspond to the time interval sequence
names(Session6_GenderAgreement_grammatical_data) = c('Electrode', seq)
names(Session6_GenderAgreement_violation_interest_data) = c('Electrode', seq)
names(Session6_GenderAgreement_ancillary_violation_data) =c('Electrode', seq)
# View(Session6_GenderAgreement_grammatical_data)

names(Session6_DOM_grammatical_data) = c('Electrode', seq)
names(Session6_DOM_violation_interest_data) = c('Electrode', seq)
names(Session6_DOM_ancillary_violation_data) =c('Electrode', seq)
# View(Session6_DOM_grammatical_data)

names(Session6_VOA_grammatical_data) = c('Electrode', seq)
names(Session6_VOA_violation_interest_data) = c('Electrode', seq)
names(Session6_VOA_ancillary_violation_data) =c('Electrode', seq)
# View(Session6_VOA_grammatical_data)


# working on the participants' name column
# removing the path from the participants' file names
file_names_S6_GenderAgreement_grammatical <- basename(Session6_GenderAgreement_grammatical_files)
files_names_S6_GenderAgreement_violation_interest <- basename(Session6_GenderAgreement_violation_interest)
files_names_S6_GenderAgreement_ancillary_violation <- basename(Session6_GenderAgreement_ancillary_violation)

# View(file_names_S6_GenderAgreement_grammatical)

file_names_S6_DOM_grammatical <- basename(Session6_DOM_grammatical_files)
files_names_S6_DOM_violation_interest <- basename(Session6_DOM_violation_interest)
files_names_S6_DOM_ancillary_violation <- basename(Session6_DOM_ancillary_violation)

# View(file_names_S6_DOM_grammatical)

file_names_S6_VOA_grammatical <- basename(Session6_VOA_grammatical_files)
files_names_S6_VOA_violation_interest <- basename(Session6_VOA_violation_interest)
files_names_S6_VOA_ancillary_violation <- basename(Session6_VOA_ancillary_violation)

# View(file_names_S6_VOA_grammatical)

# Extracting the participant numbers from the file name
participants_S6_GenderAgreement_grammatical <- sub("_.*", "", file_names_S6_GenderAgreement_grammatical)
participants_S6_GenderAgreement_violint = sub("_.*", "", files_names_S6_GenderAgreement_violation_interest)
participants_S6_GenderAgreement_ancvil = sub("_.*", "", files_names_S6_GenderAgreement_ancillary_violation)

participants_S6_DOM_grammatical <- sub("_.*", "", file_names_S6_DOM_grammatical)
participants_S6_DOM_violint = sub("_.*", "", files_names_S6_DOM_violation_interest)
participants_S6_DOM_ancvil = sub("_.*", "", files_names_S6_DOM_ancillary_violation)

participants_S6_VOA_grammatical <- sub("_.*", "", file_names_S6_VOA_grammatical)
participants_S6_VOA_violint = sub("_.*", "", files_names_S6_VOA_violation_interest)
participants_S6_VOA_ancvil = sub("_.*", "", files_names_S6_VOA_ancillary_violation)


# adding a "Participant_number" column to the data frames
Session6_GenderAgreement_grammatical_data$Participant_number <- rep(participants_S6_GenderAgreement_grammatical, 
                                                                    each = nrow(Session6_GenderAgreement_grammatical_data) / length(participants_S6_GenderAgreement_grammatical))
Session6_GenderAgreement_violation_interest_data$Participant_number <- rep(participants_S6_GenderAgreement_violint, 
                                                                           each = nrow(Session6_GenderAgreement_violation_interest_data) / length(participants_S6_GenderAgreement_violint))
Session6_GenderAgreement_ancillary_violation_data$Participant_number <- rep(participants_S6_GenderAgreement_ancvil, 
                                                                            each = nrow(Session6_GenderAgreement_ancillary_violation_data) / length(participants_S6_GenderAgreement_ancvil))


Session6_DOM_grammatical_data$Participant_number <- rep(participants_S6_DOM_grammatical, 
                                                        each = nrow(Session6_DOM_grammatical_data) / length(participants_S6_DOM_grammatical))
Session6_DOM_violation_interest_data$Participant_number <- rep(participants_S6_DOM_violint, 
                                                               each = nrow(Session6_DOM_violation_interest_data) / length(participants_S6_DOM_violint))
Session6_DOM_ancillary_violation_data$Participant_number <- rep(participants_S6_DOM_ancvil, 
                                                                each = nrow(Session6_DOM_ancillary_violation_data) / length(participants_S6_DOM_ancvil))

Session6_VOA_grammatical_data$Participant_number <- rep(participants_S6_VOA_grammatical, 
                                                        each = nrow(Session6_VOA_grammatical_data) / length(participants_S6_VOA_grammatical))
Session6_VOA_violation_interest_data$Participant_number <- rep(participants_S6_VOA_violint, 
                                                               each = nrow(Session6_VOA_violation_interest_data) / length(participants_S6_VOA_violint))
Session6_VOA_ancillary_violation_data$Participant_number <- rep(participants_S6_VOA_ancvil, 
                                                                each = nrow(Session6_VOA_ancillary_violation_data) / length(participants_S6_VOA_ancvil))


# Adding a Property column to the data frames
Session6_GenderAgreement_grammatical_data$Property <- 'Gender_Agreement'
Session6_GenderAgreement_violation_interest_data$Property <- 'Gender_Agreement'
Session6_GenderAgreement_ancillary_violation_data$Property <- 'Gender_Agreement'

Session6_DOM_grammatical_data$Property <- 'Differential_Object_Marking'
Session6_DOM_violation_interest_data$Property <- 'Differential_Object_Marking'
Session6_DOM_ancillary_violation_data$Property <- 'Differential_Object_Marking'

Session6_VOA_grammatical_data$Property <- 'Verb_Object_Number_Agreement'
Session6_VOA_violation_interest_data$Property <- 'Verb_Object_Number_Agreement'
Session6_VOA_ancillary_violation_data$Property <- 'Verb_Object_Number_Agreement'

# adding a Grammaticality column to the data frames

Session6_GenderAgreement_grammatical_data$Grammaticality <- 'Grammatical'
Session6_GenderAgreement_violation_interest_data$Grammaticality <- 'Violation_of_Interest'
Session6_GenderAgreement_ancillary_violation_data$Grammaticality <- 'Ancillary_Violation'

Session6_DOM_grammatical_data$Grammaticality <- 'Grammatical'
Session6_DOM_violation_interest_data$Grammaticality <- 'Violation_of_Interest'
Session6_DOM_ancillary_violation_data$Grammaticality <- 'Ancillary_Violation'

Session6_VOA_grammatical_data$Grammaticality <- 'Grammatical'
Session6_VOA_violation_interest_data$Grammaticality <- 'Violation_of_Interest'
Session6_VOA_ancillary_violation_data$Grammaticality <- 'Ancillary_Violation'


Session6_combined_data <- rbind(Session6_GenderAgreement_grammatical_data,
                                Session6_GenderAgreement_violation_interest_data, 
                                Session6_GenderAgreement_ancillary_violation_data,
                                Session6_DOM_grammatical_data, 
                                Session6_DOM_violation_interest_data,  
                                Session6_DOM_ancillary_violation_data,
                                Session6_VOA_grammatical_data, 
                                Session6_VOA_violation_interest_data,  
                                Session6_VOA_ancillary_violation_data)

# View(Session6_combined_data)


# dividing the electrodes into brain regions
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

# Add a Region column on the data frame based on the electrode_to_region mapping
Session6_combined_data <- Session6_combined_data %>%
  mutate(Region = electrode_to_region[Electrode])



# Add a Region column on the data frame based on the electrode_to_region mapping
Session6_combined_data <- Session6_combined_data %>%
  mutate(Region = electrode_to_region[Electrode])

# Melt the combined data frame to convert it from wide to long format
Session6_melted_data_temporary <- melt(Session6_combined_data, id.vars = 
                                         c('Participant_number', 'Electrode', 'Grammaticality', 'Region', 'Property'), 
                                       variable.name = 'Time', value.name = 'Activation')

# Convert the 'Time' column to numeric
Session6_melted_data_temporary$Time <- as.numeric(as.character
                                                   (Session6_melted_data_temporary$Time))

# Add a Session column
Session6_melted_data_temporary$Session <- 'Session 6'

# View the resulting melted data
# View(Session6_melted_data_temporary)

# Removing rows where any column has NA or NaN values
Session6_melted_data <- Session6_melted_data_temporary %>%
  filter(complete.cases(.))

# View the cleaned data
# View(Session6_melted_data)

# adding the Background data to the Session2_melted_data
# Converting Participant_number in Backgound_data to character, in order to match 
# the Session2 data. Character has been chosen because Participant_number 
# is categorical (IDs)
# Performing the inner join function,due to the discrepancy between the number of 
# rows between the two data frames, so that no data is deleted

Backgound_data$Participant_number <- as.character(Backgound_data$Participant_number)
Session6_Background <- full_join(Backgound_data, Session6_melted_data, by = "Participant_number")

# View and save combined data frame and time windows to be analysed
# View(Session6_Background)

write.csv(Session6_Background, "EEG/data/Session6_data_frame.csv", row.names = FALSE)

# Session 6, N200 time window (200-500 ms)
S6_N200 <- Session6_Background [Session6_Background$Time %in% seq(200, 500, 2),]

# View(S6_N200)
write.csv(S6_N200, "EEG/data/Session 6/Session4_N200_data_frame.csv", row.names = FALSE)

# Session 6, P300 (300 - 600 ms)
S6_P300 <- Session4_Background[Session6_Background$Time %in% seq(300, 600, 2),]

# View(S6_P300)
write.csv(S6_P300, "EEG/data/Session 6/Session6_P300_data_frame.csv", row.names = FALSE)

# Session 6, P600 (400 - 900 ms)
S6_P600 <- Session6_Background[Session6_Background$Time %in% seq(400, 900, 2),]

# View(S6_P600)
write.csv(S6_P600, "EEG/data/Session 6/Session6_P600_data_frame.csv", row.names = FALSE)




















# in session3, exclude participant 3
# in session 4, exclude participants 11 and 7

