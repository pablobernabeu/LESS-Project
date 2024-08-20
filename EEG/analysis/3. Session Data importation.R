# importing lhq3 script, which will be used to complete the session data frames
source("EEG/analysis/LHQ3 Importation script.R")

library(tidyverse)
library(janitor)
library(plyr)
library(reshape2)
library(magrittr)
library(dplyr)
library(ggplot2)

#session2, remove participants 2 and 7 from anovas



Session2path <- "EEG/data/Session 2/Export/"

#creating patterns to import the files and recognise them as distinct conditions
# the final number in the file name indicates the Grammaticality of the trial
#files that end in:
# 101: the trial was grammatical 
# 102: the trial presented a violation of interest
# 103: the trial presented an ancillary violation
#Session 2 investigates Gender agreement, indicated by the marker S1
Session2_gram_files <- list.files(pattern = "*S1.S101.txt", 
                                 path = Session2path, full.names = TRUE)

Session2_violation_interest <- list.files(pattern = "*S1_S102.txt", 
                                path = Session2path, full.names = TRUE)

Session2_ancillary_violation <- list.files(pattern = "*S1_S103.txt", 
                                          path = Session2path, full.names = TRUE)

# Constructing lists of data, one for each condition
Session2_gram_list = lapply(1:length(Session2_gram_files),function(x) {
 read.table(Session2_gram_files[x], header=FALSE) } )
#View(Session2_gram_list)

Session2_violation_interest_list = lapply(1:length(Session2_violation_interest),
                                          function(x) {
 read.table(Session2_violation_interest[x], header=FALSE) } )
#View(Session2_violation_interest_list)

Session2_ancillary_violation_list = lapply(1:length(Session2_ancillary_violation),
                                           function(x) {
  read.table(Session2_ancillary_violation [x], header=FALSE) } )
#View(Session2_ancillary_violation_list)

# converting the lists into data frames
Session2_gram_data = ldply(Session2_gram_list, data.frame)
Session2_violation_interest_data = ldply(Session2_violation_interest_list, 
                                         data.frame)
Session2_ancillary_violation_data = ldply (Session2_ancillary_violation_list, 
                                           data.frame)

#Sorting out column names and organising them

# time during the recording is organised in milliseconds, from -100 to 1098, 
#and recorded with 2 ms intervals
seq = seq(-100, 1098, 2)

# the electrode column is formulated as a vector of electrode names that 
#correspond to the time interval sequence
names(Session2_gram_data) = c('Electrode', seq)
names(Session2_violation_interest_data) = c('Electrode', seq)
names(Session2_ancillary_violation_data) = c ('Electrode', seq)
#View(Session2_gram_data)

# working on the participants' name column
#removing the path from the participants' file names
file_names_gram <- basename(Session2_gram_files)
files_names_violation_interest <- basename(Session2_violation_interest)
files_names_ancillary_violation <- basename(Session2_ancillary_violation)
#View(Session2_gram_data)

#Extracting the participant numbers from the file name
participants_gr <- sub("_.*", "", file_names_gram)
participants_violint = sub("_.*", "", files_names_violation_interest)
participants_ancvil = sub("_.*", "", files_names_ancillary_violation)

# adding a "Participant_number" column to the data frames
Session2_gram_data$Participant_number <- rep(participants_gr, each = 
                    nrow(Session2_gram_data) / length(participants_gr))
Session2_violation_interest_data$Participant_number <- rep(participants_violint, 
  each = nrow(Session2_violation_interest_data) / length(participants_violint))
Session2_ancillary_violation_data$Participant_number <- rep(participants_ancvil, 
  each = nrow(Session2_ancillary_violation_data) / length(participants_ancvil))

# adding a Grammaticality column to the data frames
Session2_gram_data$Grammaticality <- 'Grammatical'
Session2_violation_interest_data$Grammaticality <- 'Violation of Interest'
Session2_ancillary_violation_data$Grammaticality <- 'Ancillary Violation'

# Combine all data frames into one
Session2_combined_data <- rbind(Session2_gram_data, 
        Session2_violation_interest_data, Session2_ancillary_violation_data)

View(Session2_combined_data)


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

# Add a Region column on the data frame based on the electrode_to_region mapping
Session2_combined_data <- Session2_combined_data %>%
  mutate(Region = electrode_to_region[Electrode])

# Melt the combined data frame to convert it from wide to long format
Session2_melted_data_dirty <- melt(Session2_combined_data, id.vars = 
    c('Participant_number', 'Electrode', 'Grammaticality', 'Region'), 
    variable.name = 'Time', value.name = 'Activation')

# Convert the 'Time' column to numeric
Session2_melted_data_dirty$Time <- as.numeric (as.character
                                               (Session2_melted_data_dirty$Time))

# Add a Session column
Session2_melted_data_dirty$Session <- 'Session 2'

# View the resulting melted data
View(Session2_melted_data_dirty)

# Removing rows where any column has NA or NaN values
Session2_melted_data <- Session2_melted_data_dirty %>%
  filter(complete.cases(.))

# View the cleaned data
View(Session2_melted_data)
View(LHQ3_final)

# making sure that no Nas or NaNs have been introduced by coercion
rows_with_any_na_nan <- Session2_melted_data %>%
  filter(if_any(everything(), ~ is.na(.) | is.nan(.)))

print(rows_with_any_na_nan)

rows_with_any_na_nan <- LHQ3_final %>%
  filter(if_any(everything(), ~ is.na(.) | is.nan(.)))

print(rows_with_any_na_nan)


#adding the LHQ3 data to the Session2_melted_data
#Converting Participant_number in LHQ3_final to character, in order to match 
#the Session2 data. Character has been chosen because Participant_number 
#is categorical (IDs)
#Performing the inner join function,due to the discrepancy between the number of 
#rows between the two data frames, so that no data is deleted

LHQ3_final$Participant_number <- as.character(LHQ3_final$Participant_number)

Session2_LHQ3 <- full_join(LHQ3_final, Session2_melted_data, by = "Participant_number")


#setting the columns Time, Region, Grammaticality and Participant_number as factors 
#in order to run ANOVAs
#Session2_melted_data$Time <- as.factor(Session2_melted_data$Time)
Session2_melted_data$Region <- as.factor(Session2_melted_data$Region)
Session2_melted_data$Grammaticality <- as.factor(Session2_melted_data$Grammaticality)
Session2_melted_data$Participant_number <- as.factor(Session2_melted_data$Participant_number)

# Print combined data frame
View(Session2_LHQ3)
############################################








#INSERT GORILLA DATA
#CHANGE DATAFRAME NAME TO sESSION2_DATA









###############################################################################
#Analysing per time window

# Session 2, N200 time window (200-500 ms)
S2_N200 <- Session2_LHQ3 [Session2_LHQ3$Time %in% seq(200, 500, 2),]

View(S2_N200)

#####################################

#Session 2, P300 (300 - 600 ms)
S2_P300 <- Session2_LHQ3[Session2_LHQ3$Time %in% seq(300, 600, 2),]

View(S2_P300)

#######################################

#Session 2, P600 (400 - 900 ms)
S2_P600 <- Session2_LHQ3[Session2_LHQ3$Time %in% seq(400, 900, 2),]

View(S2_P600)




#####################################################################
######################################################################
#######################################################################

Session3path <- "EEG/data/Session 3/Export/"

#creating patterns to import the files and recognise them as distinct conditions
# the final number in the file name indicates the Grammaticality of the trial
#files that end in:
# 101: the trial was grammatical 
# 102: the trial presented a violation of interest
# 103: the trial presented an ancillary violation

#Session 3 investigates Gender marking, here GEN, as well as differential 
#object marking, here DOM

Session3_GEN_gram_files <- list.files(pattern = "*S1.S101.txt", 
                                  path = Session3path, full.names = TRUE)

Session3_GEN_violation_interest <- list.files(pattern = "*S1_S102.txt", 
                                          path = Session3path, full.names = TRUE)

Session3_GEN_ancillary_violation <- list.files(pattern = "*S1_S103.txt", 
                                           path = Session3path, full.names = TRUE)

Session3_DOM_gram_files <- list.files(pattern = "*S2.S101.txt", 
                                      path = Session3path, full.names = TRUE)

Session3_DOM_violation_interest <- list.files(pattern = "*S2_S102.txt", 
                                              path = Session3path, full.names = TRUE)

Session3_DOM_ancillary_violation <- list.files(pattern = "*S2_S103.txt", 
                                               path = Session3path, full.names = TRUE)



# Constructing lists of data, one for each property and condition

#Gender
Session3_GEN_gram_list = lapply(1:length(Session3_GEN_gram_files),function(x) {
  read.table(Session3_GEN_gram_files[x], header=FALSE) } )
#View(Session3_GEN_gram_list)

Session3_GEN_violation_interest_list = lapply(1:length(Session3_GEN_violation_interest),
 function(x) { read.table(Session3_GEN_violation_interest[x], header=FALSE) } )
#View(Session3_GEN_violation_interest_list)

Session3_GEN_ancillary_violation_list = lapply(1:length(Session3_GEN_ancillary_violation),
function(x) { read.table(Session3_GEN_ancillary_violation [x], header=FALSE) } )
#View(Session3_GEN_ancillary_violation_list)


#Differential object marking
Session3_DOM_gram_list = lapply(1:length(Session3_DOM_gram_files),function(x) {
  read.table(Session3_DOM_gram_files[x], header=FALSE) } )
#View(Session3_DOM_gram_list)

Session3_DOM_violation_interest_list = lapply(1:length(Session3_DOM_violation_interest),
  function(x) { read.table(Session3_DOM_violation_interest[x], header=FALSE) } )
#View(Session3_DOM_violation_interest_list)

Session3_DOM_ancillary_violation_list = lapply(1:length(Session3_DOM_ancillary_violation),
  function(x) { read.table(Session3_DOM_ancillary_violation [x], header=FALSE) } )
#View(Session3_DOM_ancillary_violation_list)


# converting the lists into data frames

#Gender
Session3_GEN_gram_data = ldply(Session3_GEN_gram_list, data.frame)
Session3_GEN_violation_interest_data = ldply(Session3_GEN_violation_interest_list, 
                                         data.frame)
Session3_GEN_ancillary_violation_data = ldply (Session3_GEN_ancillary_violation_list, 
                                           data.frame)
#Differential object marking

Session3_DOM_gram_data = ldply(Session3_DOM_gram_list, data.frame)
Session3_DOM_violation_interest_data = ldply(Session3_DOM_violation_interest_list, 
                                             data.frame)
Session3_DOM_ancillary_violation_data = ldply (Session3_DOM_ancillary_violation_list, 
                                               data.frame)

#Sorting out column names and organising them

# time during the recording is organised in milliseconds, from -100 to 1098, 
#and recorded with 2 ms intervals
seq = seq(-100, 1098, 2)

# the electrode column is formulated as a vector of electrode names that 
#correspond to the time interval sequence
names(Session3_GEN_gram_data) = c('Electrode', seq)
names(Session3_GEN_violation_interest_data) = c('Electrode', seq)
names(Session3_GEN_ancillary_violation_data) = c ('Electrode', seq)
#View(Session3_GEN_gram_data)

names(Session3_DOM_gram_data) = c('Electrode', seq)
names(Session3_DOM_violation_interest_data) = c('Electrode', seq)
names(Session3_DOM_ancillary_violation_data) = c ('Electrode', seq)
#View(Session3_DOM_gram_data)

# working on the participants' name column
#removing the path from the participants' file names
file_names_S3_GEN_grammatical <- basename(Session3_GEN_gram_files)
files_names_S3_GEN_violation_interest <- basename(Session3_GEN_violation_interest)
files_names_S3_GEN_ancillary_violation <- basename(Session3_GEN_ancillary_violation)

#View(file_names_S3_GEN_grammatical)

file_names_S3_DOM_grammatical <- basename(Session3_DOM_gram_files)
files_names_S3_DOM_violation_interest <- basename(Session3_DOM_violation_interest)
files_names_S3_DOM_ancillary_violation <- basename(Session3_DOM_ancillary_violation)

#View(file_names_S3_DOM_grammatical)

#Extracting the participant numbers from the file name
participants_S3_GEN_grammatical <- sub("_.*", "", file_names_S3_GEN_grammatical)
participants_S3_GEN_violint = sub("_.*", "", files_names_S3_GEN_violation_interest)
participants_S3_GEN_ancvil = sub("_.*", "", files_names_S3_GEN_ancillary_violation)

participants_S3_DOM_grammatical <- sub("_.*", "", file_names_S3_DOM_grammatical)
participants_S3_DOM_violint = sub("_.*", "", files_names_S3_DOM_violation_interest)
participants_S3_DOM_ancvil = sub("_.*", "", files_names_S3_DOM_ancillary_violation)


# adding a "Participant_number" column to the data frames
Session3_GEN_gram_data$Participant_number <- rep(participants_S3_GEN_grammatical, 
each =  nrow(Session3_GEN_gram_data) / length(participants_S3_GEN_grammatical))
Session3_GEN_violation_interest_data$Participant_number <- rep(participants_S3_GEN_violint, 
each = nrow(Session3_GEN_violation_interest_data) / length(participants_S3_GEN_violint))
Session3_GEN_ancillary_violation_data$Participant_number <- rep(participants_S3_GEN_ancvil, 
each = nrow(Session3_GEN_ancillary_violation_data) / length(participants_S3_GEN_ancvil))


Session3_DOM_gram_data$Participant_number <- rep(participants_S3_DOM_grammatical,  
each = nrow(Session3_DOM_gram_data) / length(participants_S3_DOM_grammatical))
Session3_DOM_violation_interest_data$Participant_number <- rep(participants_S3_DOM_violint, 
each = nrow(Session3_DOM_violation_interest_data) / length(participants_S3_DOM_violint))
Session3_DOM_ancillary_violation_data$Participant_number <- rep(participants_S3_DOM_ancvil, 
each = nrow(Session3_DOM_ancillary_violation_data) / length(participants_S3_DOM_ancvil))

#Adding a Property column to the data frames
Session3_GEN_gram_data$Property <- 'Gender'
Session3_GEN_violation_interest_data$Property <- 'Gender_Agreement'
Session3_GEN_ancillary_violation_data$Property <- 'Gender_Agreement'

Session3_DOM_gram_data$Property <- 'Differential_Object_Marking'
Session3_DOM_violation_interest_data$Property <- 'Differential_Object_Marking'
Session3_DOM_ancillary_violation_data$Property <- 'Differential_Object_Marking'


# adding a Grammaticality column to the data frames

Session3_GEN_gram_data$Grammaticality <- 'Grammatical'
Session3_GEN_violation_interest_data$Grammaticality <- 'Violation_of_Interest'
Session3_GEN_ancillary_violation_data$Grammaticality <- 'Ancillary_Violation'

Session3_DOM_gram_data$Grammaticality <- 'Grammatical'
Session3_DOM_violation_interest_data$Grammaticality <- 'Violation_of_Interest'
Session3_DOM_ancillary_violation_data$Grammaticality <- 'Ancillary_Violation'


# Combine all data frames into one
Session3_combined_data <- rbind(Session3_GEN_gram_data,
                                Session3_GEN_violation_interest_data, 
                                Session3_GEN_ancillary_violation_data,
                                Session3_DOM_gram_data, 
                                Session3_DOM_violation_interest_data,  
                                Session3_DOM_ancillary_violation_data)

View(Session3_combined_data)

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

# Add a Region column on the data frame based on the electrode_to_region mapping
Session3_combined_data <- Session3_combined_data %>%
  mutate(Region = electrode_to_region[Electrode])

# Melt the combined data frame to convert it from wide to long format
Session3_melted_data_dirty <- melt(Session3_combined_data, id.vars = 
 c('Participant_number', 'Electrode', 'Grammaticality', 'Region', 'Property'), 
      variable.name = 'Time', value.name = 'Activation')

# Convert the 'Time' column to numeric
Session3_melted_data_dirty$Time <- as.numeric (as.character
                                               (Session3_melted_data_dirty$Time))

# Add a Session column
Session3_melted_data_dirty$Session <- 'Session_3'

# View the resulting melted data
View(Session3_melted_data_dirty)

# Removing rows where any column has NA or NaN values
Session3_melted_data <- Session3_melted_data_dirty %>%
  filter(complete.cases(.))

# View the cleaned data
View(Session3_melted_data)

# making sure that no Nas or NaNs have been introduced by coercion
rows_with_any_na_nan <- Session3_melted_data %>%
  filter(if_any(everything(), ~ is.na(.) | is.nan(.)))

print(rows_with_any_na_nan)

#setting the columns Time, Region, Grammaticality and Participant_number as factors 
#in order to run ANOVAs
#Session3_melted_data$Time <- as.factor(Session3_melted_data_S1$Time)
Session3_melted_data$Region <- as.factor(Session3_melted_data$Region)
Session3_melted_data$Grammaticality <- as.factor(Session3_melted_data$Grammaticality)
Session3_melted_data$Participant_number <- as.factor(Session3_melted_data$Participant_number)

#adding the LHQ3 data to the Session2_melted_data
#Converting Participant_number in LHQ3_final to character, in order to match 
#the Session2 data. Character has been chosen because Participant_number 
#is categorical (IDs)
#Performing the inner join function,due to the discrepancy between the number of 
#rows between the two data frames, so that no data is deleted

LHQ3_final$Participant_number <- as.character(LHQ3_final$Participant_number)
Session3_LHQ3 <- full_join(LHQ3_final, Session3_melted_data, by = "Participant_number")

# Print combined data frame
View(Session3_LHQ3)


########################
####################
########################
#######################


#Session 4 data importation

Session4path <- "EEG/data/Session 4/Export/"

#creating patterns to import the files and recognise them as distinct conditions
# the final number in the file name indicates the Grammaticality of the trial
#files that end in:
# 101: the trial was grammatical 
# 102: the trial presented a violation of interest
# 103: the trial presented an ancillary violation

#Session 4 investigates Gender marking, here GEN, Differential 
#Object Marking, here DOM, and Verb-Object Number Agreement, here VOA

Session4_GEN_gram_files <- list.files(pattern = "*S1_S101.txt", 
                                      path = Session4path, full.names = TRUE)

Session4_GEN_violation_interest <- list.files(pattern = "*S1_S102.txt", 
                                              path = Session4path, full.names = TRUE)

Session4_GEN_ancillary_violation <- list.files(pattern = "*S1_S103.txt", 
                                               path = Session4path, full.names = TRUE)

Session4_DOM_gram_files <- list.files(pattern = "*S2_S101.txt", 
                                      path = Session4path, full.names = TRUE)

Session4_DOM_violation_interest <- list.files(pattern = "*S2_S102.txt", 
                                              path = Session4path, full.names = TRUE)

Session4_DOM_ancillary_violation <- list.files(pattern = "*S2_S103.txt", 
                                               path = Session4path, full.names = TRUE)


Session4_VOA_gram_files <- list.files(pattern = "*S3_S101.txt", 
                                      path = Session4path, full.names = TRUE)

Session4_VOA_violation_interest <- list.files(pattern = "*S3_S102.txt", 
                                              path = Session4path, full.names = TRUE)

Session4_VOA_ancillary_violation <- list.files(pattern = "*S3_S103.txt", 
                                               path = Session4path, full.names = TRUE)

# Constructing lists of data, one for each property and condition

#Gender

Session4_GEN_gram_list = lapply(1:length(Session4_GEN_gram_files),function(x) {
  read.table(Session4_GEN_gram_files[x], header=FALSE) } )
#View(Session3_GEN_gram_list)

Session4_GEN_violation_interest_list = lapply(1:length(Session4_GEN_violation_interest),
 function(x) { read.table(Session4_GEN_violation_interest[x], header=FALSE) } )
#View(Session3_GEN_violation_interest_list)

Session4_GEN_ancillary_violation_list = lapply(1:length(Session4_GEN_ancillary_violation),
function(x) { read.table(Session4_GEN_ancillary_violation [x], header=FALSE) } )
View(Session4_GEN_ancillary_violation_list)

#Differential object marking

Session4_DOM_gram_list = lapply(1:length(Session4_DOM_gram_files),function(x) {
  read.table(Session4_DOM_gram_files[x], header=FALSE) } )
#View(Session4_DOM_gram_list)

Session4_DOM_violation_interest_list = lapply(1:length(Session4_DOM_violation_interest),
  function(x) { read.table(Session4_DOM_violation_interest[x], header=FALSE) } )
#View(Session4_DOM_violation_interest_list)

Session4_DOM_ancillary_violation_list = lapply(1:length(Session4_DOM_ancillary_violation),
function(x) { read.table(Session4_DOM_ancillary_violation [x], header=FALSE) } )
#View(Session4_DOM_ancillary_violation_list)
















#Verb Object Number Agreement

Session4_VOA_gram_list = lapply(1:length(Session4_VOA_gram_files),function(x) {
  read.table(Session4_VOA_gram_files[x], header=FALSE) } )
#View(Session4_DOM_gram_list)

Session4_VOA_violation_interest_list = lapply(1:length(Session4_VOA_violation_interest),
 function(x) { read.table(Session4_VOA_violation_interest[x], header=FALSE) } )
#View(Session3_DOM_violation_interest_list)

Session4_VOA_ancillary_violation_list = lapply(1:length(Session4_VOA_ancillary_violation),
 function(x) { read.table(Session4_VOA_ancillary_violation [x], header=FALSE) } )
#View(Session3_DOM_ancillary_violation_list)


# converting the lists into data frames

#Gender

Session4_GEN_gram_data = ldply(Session4_GEN_gram_list, data.frame)
Session4_GEN_violation_interest_data = ldply(Session4_GEN_violation_interest_list, 
                                             data.frame)
Session4_GEN_ancillary_violation_data = ldply (Session4_GEN_ancillary_violation_list, 
                                               data.frame)
#Differential object marking

Session4_DOM_gram_data = ldply(Session4_DOM_gram_list, data.frame)
Session4_DOM_violation_interest_data = ldply(Session4_DOM_violation_interest_list, 
                                             data.frame)
Session4_DOM_ancillary_violation_data = ldply (Session4_DOM_ancillary_violation_list, 
                                               data.frame)
#Verb Object Number Agreement

Session4_VOA_gram_data = ldply(Session4_VOA_gram_list, data.frame)
Session4_VOA_violation_interest_data = ldply(Session4_VOA_violation_interest_list, 
                                             data.frame)
Session4_VOA_ancillary_violation_data = ldply (Session4_VOA_ancillary_violation_list, 
                                               data.frame)


#Sorting out column names and organising them

# time during the recording is organised in milliseconds, from -100 to 1098, 
#and recorded with 2 ms intervals
seq = seq(-100, 1098, 2)

# the electrode column is formulated as a vector of electrode names that 
#correspond to the time interval sequence
names(Session4_GEN_gram_data) = c('Electrode', seq)
names(Session4_GEN_violation_interest_data) = c('Electrode', seq)
names(Session4_GEN_ancillary_violation_data) = c ('Electrode', seq)
#View(Session4_GEN_gram_data)

names(Session4_DOM_gram_data) = c('Electrode', seq)
names(Session4_DOM_violation_interest_data) = c('Electrode', seq)
names(Session4_DOM_ancillary_violation_data) = c ('Electrode', seq)
#View(Session4_DOM_gram_data)

names(Session4_VOA_gram_data) = c('Electrode', seq)
names(Session4_VOA_violation_interest_data) = c('Electrode', seq)
names(Session4_VOA_ancillary_violation_data) = c ('Electrode', seq)
#View(Session4_VOA_gram_data)

# working on the participants' name column
#removing the path from the participants' file names
file_names_S4_GEN_grammatical <- basename(Session4_GEN_gram_files)
files_names_S4_GEN_violation_interest <- basename(Session4_GEN_violation_interest)
files_names_S4_GEN_ancillary_violation <- basename(Session4_GEN_ancillary_violation)

#View(file_names_S4_GEN_grammatical)

file_names_S4_DOM_grammatical <- basename(Session4_DOM_gram_files)
files_names_S4_DOM_violation_interest <- basename(Session4_DOM_violation_interest)
files_names_S4_DOM_ancillary_violation <- basename(Session4_DOM_ancillary_violation)

#View(file_names_S4_DOM_grammatical)

file_names_S4_VOA_grammatical <- basename(Session4_VOA_gram_files)
files_names_S4_VOA_violation_interest <- basename(Session4_VOA_violation_interest)
files_names_S4_VOA_ancillary_violation <- basename(Session4_VOA_ancillary_violation)

#View(file_names_S4_VOA_grammatical)

#Extracting the participant numbers from the file name
participants_S4_GEN_grammatical <- sub("_.*", "", file_names_S4_GEN_grammatical)
participants_S4_GEN_violint = sub("_.*", "", files_names_S4_GEN_violation_interest)
participants_S4_GEN_ancvil = sub("_.*", "", files_names_S4_GEN_ancillary_violation)

participants_S4_DOM_grammatical <- sub("_.*", "", file_names_S4_DOM_grammatical)
participants_S4_DOM_violint = sub("_.*", "", files_names_S4_DOM_violation_interest)
participants_S4_DOM_ancvil = sub("_.*", "", files_names_S4_DOM_ancillary_violation)

participants_S4_VOA_grammatical <- sub("_.*", "", file_names_S4_VOA_grammatical)
participants_S4_VOA_violint = sub("_.*", "", files_names_S4_VOA_violation_interest)
participants_S4_VOA_ancvil = sub("_.*", "", files_names_S4_VOA_ancillary_violation)


# adding a "Participant_number" column to the data frames
Session4_GEN_gram_data$Participant_number <- rep(participants_S4_GEN_grammatical, 
  each = nrow(Session4_GEN_gram_data) / length(participants_S4_GEN_grammatical))
Session4_GEN_violation_interest_data$Participant_number <- rep(participants_S4_GEN_violint, 
  each = nrow(Session4_GEN_violation_interest_data) / length(participants_S4_GEN_violint))
Session4_GEN_ancillary_violation_data$Participant_number <- rep(participants_S4_GEN_ancvil, 
  each = nrow(Session4_GEN_ancillary_violation_data) / length(participants_S4_GEN_ancvil))


Session4_DOM_gram_data$Participant_number <- rep(participants_S4_DOM_grammatical, 
each = nrow(Session4_DOM_gram_data) / length(participants_S4_DOM_grammatical))
Session4_DOM_violation_interest_data$Participant_number <- rep(participants_S4_DOM_violint, 
each = nrow(Session4_DOM_violation_interest_data) / length(participants_S4_DOM_violint))
Session4_DOM_ancillary_violation_data$Participant_number <- rep(participants_S4_DOM_ancvil, 
each = nrow(Session4_DOM_ancillary_violation_data) / length(participants_S4_DOM_ancvil))

Session4_VOA_gram_data$Participant_number <- rep(participants_S4_VOA_grammatical, 
each = nrow(Session4_VOA_gram_data) / length(participants_S4_VOA_grammatical))
Session4_VOA_violation_interest_data$Participant_number <- rep(participants_S4_VOA_violint, 
 each = nrow(Session4_VOA_violation_interest_data) / length(participants_S4_VOA_violint))
Session4_VOA_ancillary_violation_data$Participant_number <- rep(participants_S4_VOA_ancvil, 
 each = nrow(Session4_VOA_ancillary_violation_data) / length(participants_S4_VOA_ancvil))

#Adding a Property column to the data frames
Session4_GEN_gram_data$Property <- 'Gender_Agreement'
Session4_GEN_violation_interest_data$Property <- 'Gender_Agreement'
Session4_GEN_ancillary_violation_data$Property <- 'Gender_Agreement'

Session4_DOM_gram_data$Property <- 'Differential_Object_Marking'
Session4_DOM_violation_interest_data$Property <- 'Differential_Object_Marking'
Session4_DOM_ancillary_violation_data$Property <- 'Differential_Object_Marking'

Session4_VOA_gram_data$Property <- 'Verb_Object_Number_Agreement'
Session4_VOA_violation_interest_data$Property <- 'Verb_Object_Number_Agreement'
Session4_VOA_ancillary_violation_data$Property <- 'Verb_Object_Number_Agreement'

# adding a Grammaticality column to the data frames

Session4_GEN_gram_data$Grammaticality <- 'Grammatical'
Session4_GEN_violation_interest_data$Grammaticality <- 'Violation_of_Interest'
Session4_GEN_ancillary_violation_data$Grammaticality <- 'Ancillary_Violation'

Session4_DOM_gram_data$Grammaticality <- 'Grammatical'
Session4_DOM_violation_interest_data$Grammaticality <- 'Violation_of_Interest'
Session4_DOM_ancillary_violation_data$Grammaticality <- 'Ancillary_Violation'

Session4_VOA_gram_data$Grammaticality <- 'Grammatical'
Session4_VOA_violation_interest_data$Grammaticality <- 'Violation_of_Interest'
Session4_VOA_ancillary_violation_data$Grammaticality <- 'Ancillary_Violation'

# Combine all data frames into one
Session4_combined_data <- rbind(Session4_GEN_gram_data,
                                Session4_GEN_violation_interest_data, 
                                Session4_GEN_ancillary_violation_data,
                                Session4_DOM_gram_data, 
                                Session4_DOM_violation_interest_data,  
                                Session4_DOM_ancillary_violation_data,
                                Session4_VOA_gram_data, 
                                Session4_VOA_violation_interest_data,  
                                Session4_VOA_ancillary_violation_data)

View(Session4_combined_data)


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

# Add a Region column on the data frame based on the electrode_to_region mapping
Session4_combined_data <- Session4_combined_data %>%
  mutate(Region = electrode_to_region[Electrode])

# Melt the combined data frame to convert it from wide to long format
Session4_melted_data_dirty <- melt(Session4_combined_data, id.vars = 
   c('Participant_number', 'Electrode', 'Grammaticality', 'Region', 'Property'), 
         variable.name = 'Time', value.name = 'Activation')

# Convert the 'Time' column to numeric
Session4_melted_data_dirty$Time <- as.numeric (as.character
                                               (Session4_melted_data_dirty$Time))

# Add a Session column
Session4_melted_data_dirty$Session <- 'Session 4'

# View the resulting melted data
View(Session4_melted_data_dirty)

# Removing rows where any column has NA or NaN values
Session4_melted_data <- Session4_melted_data_dirty %>%
  filter(complete.cases(.))

# View the cleaned data
View(Session4_melted_data)

# making sure that no Nas or NaNs have been introduced by coercion
rows_with_any_na_nan <- Session4_melted_data %>%
  filter(if_any(everything(), ~ is.na(.) | is.nan(.)))

print(rows_with_any_na_nan)

#adding the LHQ3 data to the Session2_melted_data
#Converting Participant_number in LHQ3_final to character, in order to match 
#the Session2 data. Character has been chosen because Participant_number 
#is categorical (IDs)
#Performing the inner join function,due to the discrepancy between the number of 
#rows between the two data frames, so that no data is deleted

LHQ3_final$Participant_number <- as.character(LHQ3_final$Participant_number)
Session4_LHQ3 <- full_join(LHQ3_final, Session4_melted_data, by = "Participant_number")

# Print combined data frame
View(Session4_LHQ3)

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

