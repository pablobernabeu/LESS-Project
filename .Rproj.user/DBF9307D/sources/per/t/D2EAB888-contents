#LHQ3 data importation and transformation
library(readxl)
library(dplyr)
library(tidyr)
library(here)

file_path <- here("Background", "LHQ3", "LHQ3 results raw.xlsx")
# Read the Excel file and treat the first row as regular text

# Read the Excel file and treat the first row as regular text as it is just stating the type of questionnaire used when automatically downloaded form the LHQ3 website
LHQ3_results_raw <- read_excel(file_path, sheet = "Sheet1", col_names = FALSE)

# Remove the first row that reads "LHQ3"
LHQ3_results_raw <- LHQ3_results_raw[-1, ]




#making sure that the question parameters are visible in every column
#Starting with Question 6

# Update the value in column 6, row 1 to "parents' education"
LHQ3_results_raw[1, 6] <- "Parents' education"
# Get the value from column 9, row 1
Q_parents_education <- LHQ3_results_raw[1, 6]
# Changing the values in row 2, columns 6 to 7 to include Q_parents_education by converting it into a character
Q_parents_education <- as.character(Q_parents_education)
LHQ3_results_raw[2, 6:7] <- lapply(LHQ3_results_raw[2, 6:7], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  # Create the new value by concatenating Q_parents_education, a comma, and the current value
  paste(Q_parents_education, x, sep = ", ")
})


#repeating for question 10, separated by language
# Update the value in column 9, row 1 to "L1 use"
LHQ3_results_raw[1, 9] <- "L1 use"
# Get the value from column 9, row 1
Q_L1_use <- LHQ3_results_raw[1, 9]
# Changing the values in row 2, columns 9 to 14 to include Q_L1_use by converting it into a character
Q_L1_use <- as.character(Q_L1_use)
LHQ3_results_raw[2, 9:14] <- lapply(LHQ3_results_raw[2, 9:14], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  # Create the new value by concatenating Q_L1_use, a comma, and the current value
  paste(Q_L1_use, x, sep = ", ")
})

# Update the value in column 15, row 1 to "L2 use"
LHQ3_results_raw[1, 15] <- "L2 use"
# Get the value from column 9, row 1
Q_L2_use <- LHQ3_results_raw[1, 15]
# Changing the values in row 2, columns 15 to 20 to include Q_L2_use by converting it into a character
Q_L2_use <- as.character(Q_L2_use)
LHQ3_results_raw[2, 15:20] <- lapply(LHQ3_results_raw[2, 15:20], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  # Create the new value by concatenating Q_L2_use, a comma, and the current value
  paste(Q_L2_use, x, sep = ", ")
})

# Update the value in column 21, row 1 to "L3 use"
LHQ3_results_raw[1, 21] <- "L3 use"
# Get the value from column 21, row 1
Q_L3_use <- LHQ3_results_raw[1, 21]
# Changing the values in row 2, columns 21 to 26 to include Q_L3_use by converting it into a character
Q_L3_use <- as.character(Q_L3_use)
LHQ3_results_raw[2, 21:26] <- lapply(LHQ3_results_raw[2, 21:26], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  # Create the new value by concatenating Q_L3_use, a comma, and the current value
  paste(Q_L3_use, x, sep = ", ")
})

# Update the value in column 27, row 1 to "L4 use"
LHQ3_results_raw[1, 27] <- "L4 use"
# Get the value from column 9, row 1
Q_L4_use <- LHQ3_results_raw[1, 27]
# Changing the values in row 2, columns 27 to 32 to include Q_L4_use by converting it into a character
Q_L4_use <- as.character(Q_L4_use)
LHQ3_results_raw[2, 27:32] <- lapply(LHQ3_results_raw[2, 27:32], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  # Create the new value by concatenating Q_L4_use, a comma, and the current value
  paste(Q_L4_use, x, sep = ", ")
})


View (LHQ3_results_raw)


#repeating for question 11, separated by country
# Update the value in column 35, row 1 to "Immersion country 1"
LHQ3_results_raw[1, 35] <- "Immersion country 1"
# Get the value from column 35, row 1
Q_immersion1 <- LHQ3_results_raw[1, 35]
# Changing the values in row 2, columns 35 to 38 to include Q_immersion1 
#converting value into a character
Q_immersion1 <- as.character(Q_immersion1)
LHQ3_results_raw[2, 35:38] <- lapply(LHQ3_results_raw[2, 35:38], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_immersion1, x, sep = ", ")
})

# Update the value in column 39, row 1 to "Immersion country 2"
LHQ3_results_raw[1, 39] <- "Immersion country 2"
# Get the value from column 35, row 1
Q_immersion2 <- LHQ3_results_raw[1, 39]
# Changing the values in row 2, columns 39 to 42 to include Q_immersion2
#converting value into a character
Q_immersion2 <- as.character(Q_immersion2)
LHQ3_results_raw[2, 39:42] <- lapply(LHQ3_results_raw[2, 39:42], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_immersion2, x, sep = ", ")
})

# Update the value in column 43, row 1 to "Immersion country 3"
LHQ3_results_raw[1, 43] <- "Immersion country 3"
# Get the value from column 35, row 1
Q_immersion3 <- LHQ3_results_raw[1, 43]
# Changing the values in row 2, columns 43 to 46 to include Q_immersion3 
#converting value into a character
Q_immersion3 <- as.character(Q_immersion3)
LHQ3_results_raw[2, 43:46] <- lapply(LHQ3_results_raw[2, 43:46], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_immersion3, x, sep = ", ")
})

# Update the value in column 47, row 1 to "Immersion country 4"
LHQ3_results_raw[1, 47] <- "Immersion country 4"
# Get the value from column 47, row 1
Q_immersion4 <- LHQ3_results_raw[1, 47]
# Changing the values in row 2, columns 47 to 50 to include Q_immersion4
#converting value into a character
Q_immersion4 <- as.character(Q_immersion4)
LHQ3_results_raw[2, 47:50] <- lapply(LHQ3_results_raw[2, 47:50], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_immersion4, x, sep = ", ")
})


View(LHQ3_results_raw)


#repeating for question 11, separated by language
# Update the value in column 51, row 1 to "Non-native L1 acquisition by"
LHQ3_results_raw[1, 51] <- "Non-native L1 acquisition by"
# Get the value from column 51, row 1
Q_acq_L1 <- LHQ3_results_raw[1, 51]
# Changing the values in row 2, columns 51 to 54 to include Q_acq_L1
#converting value into a character
Q_acq_L1 <- as.character(Q_acq_L1)
LHQ3_results_raw[2, 51:54] <- lapply(LHQ3_results_raw[2, 51:54], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_acq_L1, x, sep = "  ")
})


# Update the value in column 55, row 1 to "Non-native L2 acquisition by"
LHQ3_results_raw[1, 55] <- "Non-native L2 acquisition by"
# Get the value from column 55, row 1
Q_acq_L2 <- LHQ3_results_raw[1, 55]
# Changing the values in row 2, columns 55 to 58 to include Q_acq_L2
#converting value into a character
Q_acq_L2 <- as.character(Q_acq_L2)
LHQ3_results_raw[2, 55:58] <- lapply(LHQ3_results_raw[2, 55:58], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_acq_L2, x, sep = "  ")
})

# Update the value in column 59, row 1 to "Non-native L3 acquisition by"
LHQ3_results_raw[1, 59] <- "Non-native L3 acquisition by"
# Get the value from column 55, row 1
Q_acq_L3 <- LHQ3_results_raw[1, 59]
# Changing the values in row 2, columns 59 to 62 to include Q_acq_L3
#converting value into a character
Q_acq_L3 <- as.character(Q_acq_L3)
LHQ3_results_raw[2, 59:62] <- lapply(LHQ3_results_raw[2, 59:62], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_acq_L3, x, sep = "  ")
})

# Update the value in column 63, row 1 to "Non-native L4 acquisition by"
LHQ3_results_raw[1, 63] <- "Non-native L4 acquisition by"
# Get the value from column 55, row 1
Q_acq_L4 <- LHQ3_results_raw[1, 63]
# Changing the values in row 2, columns 63 to 62 to include Q_acq_L4
#converting value into a character
Q_acq_L4 <- as.character(Q_acq_L4)
LHQ3_results_raw[2, 63:66] <- lapply(LHQ3_results_raw[2, 63:66], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_acq_L4, x, sep = "  ")
})


View(LHQ3_results_raw)




#repeating for question 12, separated by language

# Update the value in column 67, row 1 to "L1 context of use"
LHQ3_results_raw[1, 67] <- "L1 context of use"
# Get the value from column 67, row 1
Q_context1 <- LHQ3_results_raw[1, 67]
# Changing the values in row 2, columns 67 to 73 to include Q_context1
#converting value into a character
Q_context1 <- as.character(Q_context1)
LHQ3_results_raw[2, 67:73] <- lapply(LHQ3_results_raw[2, 67:73], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_context1, x, sep = ", ")
})

# Update the value in column 74, row 1 to "L2 context of use"
LHQ3_results_raw[1, 74] <- "L2 context of use"
# Get the value from column 74, row 1
Q_context2 <- LHQ3_results_raw[1, 74]
# Changing the values in row 2, columns 74 to 80 to include Q_context2
#converting value into a character
Q_context2 <- as.character(Q_context2)
LHQ3_results_raw[2, 74:80] <- lapply(LHQ3_results_raw[2, 74:80], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_context2, x, sep = ", ")
})

# Update the value in column 81, row 1 to "L3 context of use"
LHQ3_results_raw[1, 81] <- "L3 context of use"
# Get the value from column 81, row 1
Q_context3 <- LHQ3_results_raw[1, 81]
# Changing the values in row 2, columns 81 to 87 to include Q_context3
#converting value into a character
Q_context3 <- as.character(Q_context3)
LHQ3_results_raw[2, 81:87] <- lapply(LHQ3_results_raw[2, 81:87], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_context3, x, sep = ", ")
})

# Update the value in column 88, row 1 to "L4 context of use"
LHQ3_results_raw[1, 88] <- "L4 context of use"
# Get the value from column 81, row 1
Q_context4 <- LHQ3_results_raw[1, 88]
# Changing the values in row 2, columns 88 to 94 to include Q_context4
#converting value into a character
Q_context4 <- as.character(Q_context4)
LHQ3_results_raw[2, 88:94] <- lapply(LHQ3_results_raw[2, 88:94], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_context4, x, sep = ", ")
})


View(LHQ3_results_raw)


#repeating for question 12
# Update the value in column 95, row 1 to "Language of education "
#LHQ3_results_raw[1, 95] <- "Language of education"
# Get the value from column 95, row 1
#Q_language_of_education <- LHQ3_results_raw[1, 95]
# Changing the values in row 2, columns 95 to 112 to include Q_language_of_education
#converting value into a character
#Q_language_of_education <- as.character(Q_language_of_education)
#LHQ3_results_raw[2, 95:112] <- lapply(LHQ3_results_raw[2, 95:112], function(x) {
  # Convert x to character if it is not already
 # x <- as.character(x)
  #paste(Q_language_of_education, x, sep = ", ")
#})















#repeating for question 15
# Update the value in column 114, row 1 to "Self-rated proficiency"
LHQ3_results_raw[1, 114] <- "Self-rated proficiency"
# Get the value from column 114, row 1
Q_Selfrated_proficiency <- LHQ3_results_raw[1, 114]
# Changing the values in row 2, columns 114 to 133 to include Q_Selfrated_proficiency
#converting value into a character
Q_Selfrated_proficiency <- as.character(Q_Selfrated_proficiency)
LHQ3_results_raw[2, 114:133] <- lapply(LHQ3_results_raw[2, 114:133], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_Selfrated_proficiency, x, sep = ", ")
})

#repeating for question 16
# Update the value in column 134, row 1 to "Self-rated accent thickness"
LHQ3_results_raw[1, 134] <- "Self-rated accent thickness"
# Get the value from column 134, row 1
Q_Selfrated_accent_thickness <- LHQ3_results_raw[1, 134]
# Changing the values in row 2, columns 134 to 141 to include Q_Selfrated_accent_thickness
#converting value into a character
Q_Selfrated_accent_thickness <- as.character(Q_Selfrated_accent_thickness)
LHQ3_results_raw[2, 134:141] <- lapply(LHQ3_results_raw[2, 134:141], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_Selfrated_accent_thickness, x, sep = ", ")
})


#repeating for question 17
# Update the value in column 142, row 1 to "Standardized testing"
LHQ3_results_raw[1, 142] <- "Standardized testing"
# Get the value from column 142, row 1
Q_Standardized_testing <- LHQ3_results_raw[1, 142]
# Changing the values in row 2, columns 142 to 161 to include Q_Standardized_testing
#converting value into a character
Q_Standardized_testing <- as.character(Q_Standardized_testing)
LHQ3_results_raw[2, 142:161] <- lapply(LHQ3_results_raw[2, 142:161], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_Standardized_testing, x, sep = ", ")
})


#repeating for question 18
# Update the value in column 162, row 1 to "Exposure per environment"
LHQ3_results_raw[1, 162] <- "Exposure per environment"
# Get the value from column 162, row 1
Q_environment_exposure <- LHQ3_results_raw[1, 162]
# Changing the values in row 2, columns 142 to 189 to include Q_environment_exposure
#converting value into a character
Q_environment_exposure <- as.character(Q_environment_exposure)
LHQ3_results_raw[2, 162:189] <- lapply(LHQ3_results_raw[2, 162:189], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_environment_exposure, x, sep = ", ")
})


#repeating for question 19
# Update the value in column 190, row 1 to "Daily social use"
LHQ3_results_raw[1, 190] <- "Daily social use hours"
# Get the value from column 190, row 1
Q_Daily_social_use <- LHQ3_results_raw[1, 190]
# Changing the values in row 2, columns 142 to 209 to include Q_Daily_social_use
#converting value into a character
Q_Daily_social_use <- as.character(Q_Daily_social_use)
LHQ3_results_raw[2, 190:209] <- lapply(LHQ3_results_raw[2, 190:209], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_Daily_social_use, x, sep = ", ")
})

#repeating for question 20
# Update the value in column 210, row 1 to "Daily code-switching hours"
LHQ3_results_raw[1, 210] <- "Daily code-switching hours"
# Get the value from column 210, row 1
Q_Daily_codeswitching <- LHQ3_results_raw[1, 210]
# Changing the values in row 2, columns 142 to 221 to include Q_Daily_codeswitching
#converting value into a character
Q_Daily_codeswitching <- as.character(Q_Daily_codeswitching)
LHQ3_results_raw[2, 210:221] <- lapply(LHQ3_results_raw[2, 210:221], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_Daily_codeswitching, x, sep = ", ")
})

#repeating for question 21
# Update the value in column 222, row 1 to "Self-rated language dominance"
LHQ3_results_raw[1, 222] <- "Self-rated language dominance"
# Get the value from column 222, row 1
Q_Selfrated_language_dominance <- LHQ3_results_raw[1, 222]
# Changing the values in row 2, columns 142 to 237 to include Q_Selfrated_language_dominance
#converting value into a character
Q_Selfrated_language_dominance <- as.character(Q_Selfrated_language_dominance)
LHQ3_results_raw[2, 222:237] <- lapply(LHQ3_results_raw[2, 222:237], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_Selfrated_language_dominance, x, sep = ", ")
})

#repeating for question 21, organising by language

# Update the value in column 238, row 1 to "Language 1 use for"
LHQ3_results_raw[1, 238] <- "L1 use for"
# Get the value from column 238, row 1
Q_L1_for <- LHQ3_results_raw[1, 238]
# Changing the values in row 2, columns 238 to 245 to include Q_L1_for
#converting value into a character
Q_L1_for <- as.character(Q_L1_for)
LHQ3_results_raw[2, 238:245] <- lapply(LHQ3_results_raw[2, 238:245], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_L1_for, x, sep = ", ")
})

# Update the value in column 246, row 1 to "Language 2 use for"
LHQ3_results_raw[1, 246] <- "L2 use for"
# Get the value from column 246, row 1
Q_L2_for <- LHQ3_results_raw[1, 246]
# Changing the values in row 2, columns 246 to 253 to include Q_L2_for
#converting value into a character
Q_L2_for <- as.character(Q_L2_for)
LHQ3_results_raw[2, 246:253] <- lapply(LHQ3_results_raw[2, 246:253], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_L2_for, x, sep = ", ")
})

# Update the value in column 254, row 1 to "Language 3 use for"
LHQ3_results_raw[1, 254] <- "L3 use for"
# Get the value from column 254, row 1
Q_L3_for <- LHQ3_results_raw[1, 254]
# Changing the values in row 2, columns 254 to 261 to include Q_L3_for
#converting value into a character
Q_L3_for <- as.character(Q_L3_for)
LHQ3_results_raw[2, 254:261] <- lapply(LHQ3_results_raw[2, 254:261], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_L3_for, x, sep = ", ")
})

# Update the value in column 262, row 1 to "Language 4 use for"
LHQ3_results_raw[1, 262] <- "L4 use for"
# Get the value from column 262, row 1
Q_L4_for <- LHQ3_results_raw[1, 262]
# Changing the values in row 2, columns 262 to 269 to include Q_L4_for
#converting value into a character
Q_L4_for <- as.character(Q_L4_for)
LHQ3_results_raw[2, 262:269] <- lapply(LHQ3_results_raw[2, 262:269], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_L4_for, x, sep = " ")
})


#repeating for question 23, organising by language

# Update the value in column 270, row 1 to "Percentage of friends who speak L1"
LHQ3_results_raw[1, 270] <- "Percentage of friends who speak L1"
# Get the value from column 270, row 1
Q_L1_competence_friends <- LHQ3_results_raw[1, 270]
# Changing the values in row 2, columns 270 to 271 to include Q_L1_competence_friends
#converting value into a character
Q_L1_competence_friends <- as.character(Q_L1_competence_friends)
LHQ3_results_raw[2, 270:271] <- lapply(LHQ3_results_raw[2, 270:271], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_L1_competence_friends, x, sep = " ")
})

# Update the value in column 272, row 1 to "Percentage of friends who speak L2"
LHQ3_results_raw[1, 272] <- "Percentage of friends who speak L2"
# Get the value from column 270, row 1
Q_L2_competence_friends <- LHQ3_results_raw[1, 272]
# Changing the values in row 2, columns 272 to 273 to include Q_L2_competence_friends
#converting value into a character
Q_L2_competence_friends <- as.character(Q_L2_competence_friends)
LHQ3_results_raw[2, 272:273] <- lapply(LHQ3_results_raw[2, 272:273], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_L2_competence_friends, x, sep = " ")
})

# Update the value in column 274, row 1 to "Percentage of friends who speak L3"
LHQ3_results_raw[1, 274] <- "Percentage of friends who speak L3"
# Get the value from column 270, row 1
Q_L2_competence_friends <- LHQ3_results_raw[1, 274]
# Changing the values in row 2, columns 274 to 275 to include Q_L2_competence_friends
#converting value into a character
Q_L2_competence_friends <- as.character(Q_L2_competence_friends)
LHQ3_results_raw[2, 274:275] <- lapply(LHQ3_results_raw[2, 274:275], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_L2_competence_friends, x, sep = " ")
})

# Update the value in column 276, row 1 to "Percentage of friends who speak L4"
LHQ3_results_raw[1, 276] <- "Percentage of friends who speak L4"
# Get the value from column 270, row 1
Q_L2_competence_friends <- LHQ3_results_raw[1, 276]
# Changing the values in row 2, columns 274 to 275 to include Q_L2_competence_friends
#converting value into a character
Q_L2_competence_friends <- as.character(Q_L2_competence_friends)
LHQ3_results_raw[2, 276:277] <- lapply(LHQ3_results_raw[2, 276:277], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_L2_competence_friends, x, sep = " ")
})


#repeating for question 24, organising by language

# Update the value in column 278, row 1 to "Cultural identification with L1"
LHQ3_results_raw[1, 278] <- "Identification with L1"
# Get the value from column 278, row 1
Q_L1_cultural_identification <- LHQ3_results_raw[1, 278]
# Changing the values in row 2, columns 278 to 184 to include Q_L1_cultural_identification
#converting value into a character
Q_L1_cultural_identification <- as.character(Q_L1_cultural_identification)
LHQ3_results_raw[2, 278:284] <- lapply(LHQ3_results_raw[2, 278:284], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_L1_cultural_identification, x, sep = " ")
})

# Update the value in column 285, row 1 to "Cultural identification with L2"
LHQ3_results_raw[1, 285] <- "Identification with L2"
# Get the value from column 285, row 1
Q_L2_cultural_identification <- LHQ3_results_raw[1, 285]
# Changing the values in row 2, columns 285 to 291 to include Q_L2_cultural_identification
#converting value into a character
Q_L2_cultural_identification <- as.character(Q_L2_cultural_identification)
LHQ3_results_raw[2, 285:291] <- lapply(LHQ3_results_raw[2, 285:291], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_L2_cultural_identification, x, sep = " ")
})


# Update the value in column 292, row 1 to "Cultural identification with L3"
LHQ3_results_raw[1, 292] <- "Identification with L3"
# Get the value from column 292, row 1
Q_L3_cultural_identification <- LHQ3_results_raw[1, 292]
# Changing the values in row 2, columns 292 to 298 to include Q_L3_cultural_identification
#converting value into a character
Q_L3_cultural_identification <- as.character(Q_L3_cultural_identification)
LHQ3_results_raw[2, 292:298] <- lapply(LHQ3_results_raw[2, 292:298], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_L3_cultural_identification, x, sep = " ")
})

# Update the value in column 299, row 1 to "Cultural identification with L4"
LHQ3_results_raw[1, 299] <- "Identification with L4"
# Get the value from column 299, row 1
Q_L4_cultural_identification <- LHQ3_results_raw[1, 299]
# Changing the values in row 2, columns 292 to 305 to include Q_L4_cultural_identification
#converting value into a character
Q_L4_cultural_identification <- as.character(Q_L4_cultural_identification)
LHQ3_results_raw[2, 299:305] <- lapply(LHQ3_results_raw[2, 299:305], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_L4_cultural_identification, x, sep = " ")
})


#repeating for question 25

# Update the value in column 278, row 1 to "Cultural identification with L1"
LHQ3_results_raw[1, 278] <- "Identification with L1"
# Get the value from column 278, row 1
Q_L1_cultural_identification <- LHQ3_results_raw[1, 278]
# Changing the values in row 2, columns 278 to 184 to include Q_L1_cultural_identification
#converting value into a character
Q_L1_cultural_identification <- as.character(Q_L1_cultural_identification)
LHQ3_results_raw[2, 278:284] <- lapply(LHQ3_results_raw[2, 278:284], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  paste(Q_L1_cultural_identification, x, sep = " ")
})


#indexing remaining columns
LHQ3_results_raw[2, 1] <- "Participant ID"
LHQ3_results_raw[2, 2] <- "Participant number"
LHQ3_results_raw[2, 3] <- "Age"
LHQ3_results_raw[2, 4] <- "Gender"
LHQ3_results_raw[2, 5] <- "Education"
LHQ3_results_raw[2, 8] <- "Handedness"
LHQ3_results_raw[2, 33] <- "Country of origin"
LHQ3_results_raw[2, 34] <- "Country of residence"
LHQ3_results_raw[2, 34] <- "Country of residence"





# View the updated data frame
View(LHQ3_results_raw)


#q25 comments
#q27 dialect


#q15 - proficiency in language x - the language also needs to be moved to title of property eg L1 listening
#q18, exposure per environment
#q19, daily social use
#move to row 2
#q13, self rated language learning skill,  row 113
#q14, s
#in the end, delete row 1 completely
#before that, ensure that if row 2 has NA value still, it is replaced by row 1 value
#pay attentoin to comment columns

#ensure the matching of participant code to participant number based on the norway logbook file
#calculate language entropy
#isolate language proficiency