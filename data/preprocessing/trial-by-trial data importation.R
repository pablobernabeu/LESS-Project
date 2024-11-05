# # # Trial by trial Session data importation

library(tidyverse)  
library(reshape2)   
library(janitor)   
library(plyr)       

# Session2

# session2, remove participants 2 and 7 from anovas

Session2pathtbt = "EEG/data/Session 2/Export/"

# creating patterns to import the files and recognise them as distinct conditions
# the final number in the file name indicates the Grammaticality of the trial
# files that end in:
# 101: the trial was grammatical 
# 102: the trial presented a violation of interest
# 103: the trial presented an ancillary violation
# Session 2 investigates Gender agreement, indicated by the marker S1
Session2_tbt_grammatical_files = list.files(pattern = "*^[0-9]+_trialbytrial_S1.S101.txt", 
                                      path = Session2pathtbt, full.names = TRUE)

list(Session2_tbt_grammatical_files)

Session2_tbt_violation_interest = list.files(pattern = "*^[0-9]+_trialbytrial_S1_S102.txt", 
                                              path = Session2pathtbt, full.names = TRUE)

Session2_tbt_ancillary_violation = list.files(pattern = "*^[0-9]+_trialbytrial_S1_S103.txt", 
                                               path = Session2pathtbt, full.names = TRUE)



# Constructing lists of data, one for each condition
Session2_tbt_grammatical_list = lapply(1:length(Session2_tbt_grammatical_files),function(x) {
  read.table(Session2_tbt_grammatical_files[x], header=FALSE) } )
# View(Session2_tbt_grammatical_list)

Session2_tbt_violation_interest_list = lapply(1:length(Session2_tbt_violation_interest),
                                              function(x) {
                                                read.table(Session2_tbt_violation_interest[x], header=FALSE) } )
# View(Session2_tbt_violation_interest_list)

Session2_tbt_ancillary_violation_list = lapply(1:length(Session2_tbt_ancillary_violation),
                                               function(x) {
                                                 read.table(Session2_tbt_ancillary_violation [x], header=FALSE) } )

# converting the lists into data frames
Session2_tbt_grammatical_data = ldply(Session2_tbt_grammatical_list, data.frame)
Session2_tbt_violation_interest_data = ldply(Session2_tbt_violation_interest_list, 
                                             data.frame)
Session2_tbt_ancillary_violation_data = ldply(Session2_tbt_ancillary_violation_list, 
                                               data.frame)

# the Electrode column is formulated as a vector of electrode names that 
# correspond to the time interval sequence
names(Session2_tbt_grammatical_data) = c('Electrode', seq)
names(Session2_tbt_violation_interest_data) = c('Electrode', seq)
names(Session2_tbt_ancillary_violation_data) =c('Electrode', seq)

# participants' name column
# removing the path from the participants' file names
file_names_tbt_gram = basename(Session2_tbt_grammatical_files)
files_names_tbt_violation_interest = basename(Session2_tbt_violation_interest)
files_names_tbt_ancillary_violation = basename(Session2_tbt_ancillary_violation)

# Extracting the participant numbers from the file name
participants_tbt_gr = sub("_.*", "", file_names_tbt_gram)
participants_tbt_violint = sub("_.*", "", files_names_tbt_violation_interest)
participants_tbt_ancvil = sub("_.*", "", files_names_tbt_ancillary_violation)

# adding a "Participant_number" column to the data frames
Session2_tbt_grammatical_data$Participant_number = rep(participants_tbt_gr, each = 
                                               nrow(Session2_tbt_grammatical_data) / length(participants_tbt_gr))
Session2_tbt_violation_interest_data$Participant_number = rep(participants_tbt_violint, 
                                                           each = nrow(Session2_tbt_violation_interest_data) / length(participants_tbt_violint))
Session2_tbt_ancillary_violation_data$Participant_number = rep(participants_tbt_ancvil, 
                                                            each = nrow(Session2_tbt_ancillary_violation_data) / length(participants_tbt_ancvil))

# adding a Grammaticality column to the data frames
Session2_tbt_grammatical_data$Grammaticality = 'Grammatical'
Session2_tbt_violation_interest_data$Grammaticality = 'Violation of Interest'
Session2_tbt_ancillary_violation_data$Grammaticality = 'Ancillary Violation'

# Combine all data frames into one
Session2_tbt_combined_data = rbind(Session2_tbt_grammatical_data, 
                                Session2_tbt_violation_interest_data, Session2_tbt_ancillary_violation_data)

seq = seq(-100, 1098, 2)

# View(Session2_tbt_combined_data)
# dividing the electrodes into brain regions
# Define the mapping of electrodes to regions
electrode_to_region = c(
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

# Creating the 'Region' column 
Session2_tbt_combined_data$Region = ifelse(
  Session2_tbt_combined_data$Electrode %in% names(electrode_to_region),
  electrode_to_region[match(Session2_tbt_combined_data$Electrode, names(electrode_to_region))],
  NA_character_
)

# View(Session2_tbt_combined_data)

# Melting the combined data frame to convert it from wide to long format
Session2_tbt_melted_data_temporary = melt(Session2_tbt_combined_data, id.vars = 
                                     c('Participant_number', 'Electrode', 'Grammaticality', 'Region'), 
                                   variable.name = 'Time', value.name = 'Activation')

# Converting the 'Time' column to numeric
Session2_tbt_melted_data_temporary$Time = as.numeric(as.character
                                               (Session2_tbt_melted_data_temporary$Time))

# Adding a Session column
Session2_tbt_melted_data_temporary$Session = 'Session 2'

# Removing rows where any column has NA or NaN values
Session2_tbt_melted_data = Session2_tbt_melted_data_temporary %>%
  filter(complete.cases(.))
# View(Session2_tbt_melted_data)
