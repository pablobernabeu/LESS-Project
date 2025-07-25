

# Preprocess of Language History Questionnaire LHQ3

# The results file of the LHQ3 requires thorough preprocessing because it contains 
# a header row split between two rows, with the first row containing merged cells. 
# The preprocessing encapsulates each question in one column.

library(dplyr)
library(readxl)

# Match participants' ID with the participants' number by using the Norway
# session logbook as guide.

participant_IDs <-
  read.csv("data/Participant IDs and session progress.csv", header = T) %>%
  # Name variable 'mini_language' for greater clarity
  rename(mini_language = language) %>%
  select(participant_LHQ3_ID, mini_language)

# Read the Excel file and treat the first row as regular text, as it is just 
# stating the type of questionnaire used.

LHQ3 = read_excel("data/raw data/language history/LHQ3 Raw Data.xlsx", 
                  sheet = "Sheet1", col_names = FALSE, skip = 1)

# Questions that correspond to a single-column answer are directly indexed
LHQ3[2, 1] = "participant_LHQ3_ID"
LHQ3[2, 2] = "faulty_LHQ3_ID"
LHQ3[2, 3] = "age"
LHQ3[2, 4] = "gender"
LHQ3[2, 5] = "Education"
LHQ3[2, 8] = "Handedness"
LHQ3[2, 33] = "Country_of_origin"
LHQ3[2, 34] = "Country_of_residence"
LHQ3[2, 113] = "Self_rated_Language_learning_skills"
LHQ3[2, 306] = "Comments1"
LHQ3[2, 307] = "Comments2"
LHQ3[2, 308] = "Dialects"


# Start with Question 5. Parents' education 
# Update the value in column 6, row 1 to "parents_education"
LHQ3[1, 6] = "parents_education"

# Get the value from column 9, row 1
Q_parents_education = LHQ3[1, 6]

# Change the values in row 2, columns 6 to 7 to include 
# Q_parents_education by converting it into a character.

Q_parents_education = as.character(Q_parents_education)

LHQ3[2, 6:7] = lapply(LHQ3[2, 6:7], function(x) {
  
  # Convert x to character if it is not already
  x = as.character(x)
  
  # Create the new value by concatenating Q_parents_education, a comma, and the 
  # current value
  paste(Q_parents_education, x, sep = "_")
})


# 7.Indicate your native language(s) and any other languages you have studied or
# learned, the age at which you started using each language in terms of listening, 
# speaking, reading, and writing, and the total number of years you have spent 
# using each language.

# Update the value in column 9, row 1 to "L1 Acquisition"
LHQ3[1, 9] = "L1_Acquisition"

# Get the value from column 9, row 1
Q_L1_acq = LHQ3[1, 9]

# Change the values in row 2, columns 9 to 14 to include Q_L1_acq by converting 
# it into a character.

Q_L1_acq = as.character(Q_L1_acq)

LHQ3[2, 9:14] = lapply(LHQ3[2, 9:14], function(x) {
  
  # Convert x to character if it is not already
  x = as.character(x)
  
  # Create the new value by concatenating Q_L1_acq, a comma, and the current value
  paste(Q_L1_acq, x, sep = "_")
})

# View(LHQ3)
# continue as outlined for L2, L3, L4, and other questions

LHQ3[1, 15] = "L2_Acquisition"
Q_L2_acq = LHQ3[1, 15]
Q_L2_acq = as.character(Q_L2_acq)
LHQ3[2, 15:20] = lapply(LHQ3[2, 15:20], function(x) {
  x = as.character(x)
  paste(Q_L2_acq, x, sep = "_")
})

LHQ3[1, 21] = "L3_Acquisition"
Q_L3_acq = LHQ3[1, 21]
Q_L3_acq = as.character(Q_L3_acq)
LHQ3[2, 21:26] = lapply(LHQ3[2, 21:26], function(x) {
  x = as.character(x)
  paste(Q_L3_acq, x, sep = "_")
})

LHQ3[1, 27] = "L4_Acquisition"
Q_L4_acq = LHQ3[1, 27]
Q_L4_acq = as.character(Q_L4_acq)
LHQ3[2, 27:32] = lapply(LHQ3[2, 27:32], function(x) {
  x = as.character(x)
  paste(Q_L4_acq, x, sep = "_")
})


# 10.If you have lived or traveled in countries other than your country of residence 
# for three months or more, then indicate the name of the country, your length of 
# stay (in Months), the language you used, and the frequency of your use of the 
# language for each country. 

LHQ3[1, 35] = "Immersion_country 1"
Q_immersion1 = LHQ3[1, 35]
Q_immersion1 = as.character(Q_immersion1)
LHQ3[2, 35:38] = lapply(LHQ3[2, 35:38], function(x) {
  x = as.character(x)
  paste(Q_immersion1, x, sep = "_")
})

LHQ3[1, 39] = "Immersion_country 2"
Q_immersion2 = LHQ3[1, 39]
Q_immersion2 = as.character(Q_immersion2)
LHQ3[2, 39:42] = lapply(LHQ3[2, 39:42], function(x) {
  x = as.character(x)
  paste(Q_immersion2, x, sep = "_")
})

LHQ3[1, 43] = "Immersion country 3"
Q_immersion3 = LHQ3[1, 43]
Q_immersion3 = as.character(Q_immersion3)
LHQ3[2, 43:46] = lapply(LHQ3[2, 43:46], function(x) {
  x = as.character(x)
  paste(Q_immersion3, x, sep = "_")
})

LHQ3[1, 47] = "Immersion country 4"
Q_immersion4 = LHQ3[1, 47]
Q_immersion4 = as.character(Q_immersion4)
LHQ3[2, 47:50] = lapply(LHQ3[2, 47:50], function(x) {
  x = as.character(x)
  paste(Q_immersion4, x, sep = "_")
})


# 11.Indicate the way you learned or acquired your non-native language(s). 
# Check one or more boxes that apply. 

LHQ3[1, 51] = "Nonnative_L1_acquisition_by"
Q_acq_L1 = LHQ3[1, 51]
Q_acq_L1 = as.character(Q_acq_L1)
LHQ3[2, 51:54] = lapply(LHQ3[2, 51:54], function(x) {
  x = as.character(x)
  paste(Q_acq_L1, x, sep = "_")
})

LHQ3[1, 55] = "Nonnative_L2_acquisition_by"
Q_acq_L2 = LHQ3[1, 55]
Q_acq_L2 = as.character(Q_acq_L2)
LHQ3[2, 55:58] = lapply(LHQ3[2, 55:58], function(x) {
  x = as.character(x)
  paste(Q_acq_L2, x, sep = "_")
})

LHQ3[1, 59] = "Nonnative_L3_acquisition_by"
Q_acq_L3 = LHQ3[1, 59]
Q_acq_L3 = as.character(Q_acq_L3)
LHQ3[2, 59:62] = lapply(LHQ3[2, 59:62], function(x) {
  x = as.character(x)
  paste(Q_acq_L3, x, sep = "_")
})

LHQ3[1, 63] = "Nonnative_L4_acquisition_by"
Q_acq_L4 = LHQ3[1, 63]
Q_acq_L4 = as.character(Q_acq_L4)
LHQ3[2, 63:66] = lapply(LHQ3[2, 63:66], function(x) {
  x = as.character(x)
  paste(Q_acq_L4, x, sep = "_")
})


# 12.Indicate the age at which you started using each of the languages you have 
# studied or learned in the following environments (Including native language).

LHQ3[1, 67] = "L1_context_of_use"
Q_context1 = LHQ3[1, 67]
Q_context1 = as.character(Q_context1)
LHQ3[2, 67:73] = lapply(LHQ3[2, 67:73], function(x) {
  x = as.character(x)
  paste(Q_context1, x, sep = "_")
})

LHQ3[1, 74] = "L2_context_of_use"
Q_context2 = LHQ3[1, 74]
Q_context2 = as.character(Q_context2)
LHQ3[2, 74:80] = lapply(LHQ3[2, 74:80], function(x) {
  x = as.character(x)
  paste(Q_context2, x, sep = "_")
})

LHQ3[1, 81] = "L3_context_of_use"
Q_context3 = LHQ3[1, 81]
Q_context3 = as.character(Q_context3)
LHQ3[2, 81:87] = lapply(LHQ3[2, 81:87], function(x) {
  x = as.character(x)
  paste(Q_context3, x, sep = "_")
})

LHQ3[1, 88] = "L4_context_of_use"
Q_context4 = LHQ3[1, 88]
Q_context4 = as.character(Q_context4)
LHQ3[2, 88:94] = lapply(LHQ3[2, 88:94], function(x) {
  x = as.character(x)
  paste(Q_context4, x, sep = "_")
})


# 13.Indicate the language used by your teachers for instruction at each educational 
# level. If the instructional language switched during any educational level, then 
# also indicate the "Switched to" language.If you had a bilingual education at any 
# educational level, then simply check the box under "Both Languages".

LHQ3[1, 95] = "Elementary_school_L"
Q_elementary = LHQ3[1, 95]
Q_elementary = as.character(Q_elementary)
LHQ3[2, 95:97] = lapply(LHQ3[2, 95:97], function(x) {
  x = as.character(x)
  paste(Q_elementary, x, sep = "_")
})

LHQ3[1, 98] = "Middle_school_L"
Q_middle = LHQ3[1, 98]
Q_middle = as.character(Q_middle)
LHQ3[2, 98:100] = lapply(LHQ3[2, 98:100], function(x) {
  x = as.character(x)
  paste(Q_middle, x, sep = "_")
})

LHQ3[1, 101] = "High_school_L"
Q_highsch = LHQ3[1, 101]
Q_highsch = as.character(Q_highsch)
LHQ3[2, 101:103] = lapply(LHQ3[2, 101:103], function(x) {
  x = as.character(x)
  paste(Q_highsch, x, sep = "_")
})

LHQ3[1, 104] = "BA_L"
Q_BA = LHQ3[1, 104]
Q_BA = as.character(Q_BA)
LHQ3[2, 104:106] = lapply(LHQ3[2, 104:106], function(x) {
  x = as.character(x)
  paste(Q_BA, x, sep = "_")
})

LHQ3[1, 107] = "MA_L"
Q_MA = LHQ3[1, 107]
Q_MA = as.character(Q_MA)
LHQ3[2, 107:109] = lapply(LHQ3[2, 107:109], function(x) {
  x = as.character(x)
  paste(Q_MA, x, sep = "_")
})

LHQ3[1, 110] = "PhD_L"
Q_PhD = LHQ3[1, 110]
Q_PhD = as.character(Q_PhD)
LHQ3[2, 110:112] = lapply(LHQ3[2, 110:112], function(x) {
  # Convert x to character if it is not already
  x = as.character(x)
  paste(Q_PhD, x, sep = ", ")
})


# 15.Rate your current ability in terms of listening,speaking, reading, and writing in
# each of the languages you have studied or learned (including the native language).

LHQ3[1, 114] = "Proficiency_L1"
Q_Proficiency1 = LHQ3[1, 114]
Q_Proficiency1 = as.character(Q_Proficiency1)
LHQ3[2, 114:118] = lapply(LHQ3[2, 114:118], function(x) {
  x = as.character(x)
  paste(Q_Proficiency1, x, sep = "_")
})

LHQ3[1, 119] = "Proficiency_L2"
Q_Proficiency2 = LHQ3[1, 119]
Q_Proficiency2 = as.character(Q_Proficiency2)
LHQ3[2, 119:123] = lapply(LHQ3[2, 119:123], function(x) {
  x = as.character(x) 
  paste(Q_Proficiency2, x, sep = "_")
})

LHQ3[1, 124] = "Proficiency_L3"
Q_Proficiency3 = LHQ3[1, 124]
Q_Proficiency3 = as.character(Q_Proficiency3)
LHQ3[2, 124:128] = lapply(LHQ3[2, 124:128], function(x) {
  x = as.character(x)
  paste(Q_Proficiency3, x, sep = "_")
})

LHQ3[1, 129] = "Proficiency_L4"
Q_Proficiency4 = LHQ3[1, 129]
Q_Proficiency4 = as.character(Q_Proficiency4)
LHQ3[2, 129:133] = lapply(LHQ3[2, 129:133], function(x) {
  x = as.character(x)
  paste(Q_Proficiency4, x, sep = "_")
})


# 16.Rate the strength of your foreign accent for each of the languages you have 
# studied or learned 

LHQ3[1, 134] = "L1_accent"
Q_accent1 = LHQ3[1, 134]
Q_accent1 = as.character(Q_accent1)
LHQ3[2, 134:135] = lapply(LHQ3[2, 134:135], function(x) {
  x = as.character(x)
  paste(Q_accent1, x, sep = "_")
})

LHQ3[1, 136] = "L2_accent"
Q_accent2 = LHQ3[1, 136]
Q_accent2 = as.character(Q_accent2)
LHQ3[2, 136:137] = lapply(LHQ3[2, 136:137], function(x) {
  x = as.character(x)
  paste(Q_accent2, x, sep = "_")
})

LHQ3[1, 138] = "L3_accent"
Q_accent3 = LHQ3[1, 138]
Q_accent3 = as.character(Q_accent3)
LHQ3[2, 138:139] = lapply(LHQ3[2, 138:139], function(x) {
  x = as.character(x)
  paste(Q_accent3, x, sep = "_")
})

LHQ3[1, 140] = "L4_accent"
Q_accent4 = LHQ3[1, 140]
Q_accent4 = as.character(Q_accent4)
LHQ3[2, 140:141] = lapply(LHQ3[2, 140:141], function(x) {
  x = as.character(x)
  paste(Q_accent4, x, sep = "_")
})


# 17.If you have taken any standardized language proficiency tests # (e.g., TOEFL, 
# IELTS, TOEIC, etc.), then indicate the name of the test, the language assessed, 
# and the score you received for each. If you do not remember the exact score, 
# then indicate an "Approximate score" instead.

LHQ3[1, 142] = "Standardized_testing_L1"
Q_testing1 = LHQ3[1, 142]
Q_testing1 = as.character(Q_testing1)
LHQ3[2, 142:146] = lapply(LHQ3[2, 142:146], function(x) {
  x = as.character(x)
  paste(Q_testing1, x, sep = "_")
})

LHQ3[1, 147] = "Standardized_testing_L2"
Q_testing2 = LHQ3[1, 147]
Q_testing2 = as.character(Q_testing2)
LHQ3[2, 147:151] = lapply(LHQ3[2, 147:151], function(x) {
  x = as.character(x)
  paste(Q_testing2, x, sep = "_")
})

LHQ3[1, 152] = "Standardized_testing_L3"
Q_testing3 = LHQ3[1, 152]
Q_testing3 = as.character(Q_testing3)
LHQ3[2, 152:156] = lapply(LHQ3[2, 152:156], function(x) {
  x = as.character(x)
  paste(Q_testing3, x, sep = "_")
})

LHQ3[1, 157] = "Standardized_testing_L4"
Q_testing4 = LHQ3[1, 157]
Q_testing4 = as.character(Q_testing4)
LHQ3[2, 157:161] = lapply(LHQ3[2, 157:161], function(x) {
  x = as.character(x)
  paste(Q_testing4, x, sep = "_")
})


# 18.Estimate how many hours per day you spend engaged in the following activities 
# in each of the languages you have studied or learned (including the native 
# language). 

LHQ3[1, 162] = "Daily_engagement_L1"
Q_daily1 = LHQ3[1, 162]
Q_daily1 = as.character(Q_daily1)
LHQ3[2, 162:168] = lapply(LHQ3[2, 162:168], function(x) {
  x = as.character(x)
  paste(Q_daily1, x, sep = "_")
})

LHQ3[1, 169] = "Daily_engagement_L2"
Q_daily2 = LHQ3[1, 169]
Q_daily2 = as.character(Q_daily2)
LHQ3[2, 169:175] = lapply(LHQ3[2, 169:175], function(x) {
  x = as.character(x)
  paste(Q_daily2, x, sep = "_")
})

LHQ3[1, 176] = "Daily_engagement_L3"
Q_daily3 = LHQ3[1, 176]
Q_daily3 = as.character(Q_daily3)
LHQ3[2, 176:182] = lapply(LHQ3[2, 176:182], function(x) {
  x = as.character(x)
  paste(Q_daily3, x, sep = "_")
})

LHQ3[1, 183] = "Daily_engagement_L4"
Q_daily4 = LHQ3[1, 183]
Q_daily4 = as.character(Q_daily4)
LHQ3[2, 183:189] = lapply(LHQ3[2, 183:189], function(x) {
  # Convert x to character if it is not already
  x = as.character(x)
  paste(Q_daily4, x, sep = "_")
})


# 19.Estimate how many hours per day you spend speaking with the following groups 
# of people in each of the languages you have studied or learned 
# (including the native language).

LHQ3[1, 190] = "L1_h/day"
Q_dailyuse1 = LHQ3[1, 190]
Q_dailyuse1 = as.character(Q_dailyuse1)
LHQ3[2, 190:194] = lapply(LHQ3[2, 190:194], function(x) {
  x = as.character(x)
  paste(Q_dailyuse1, x, sep = "_")
})

LHQ3[1, 195] = "L2_h/day"
Q_dailyuse2 = LHQ3[1, 195]
Q_dailyuse2 = as.character(Q_dailyuse2)
LHQ3[2, 195:199] = lapply(LHQ3[2, 195:199], function(x) {
  x = as.character(x)
  paste(Q_dailyuse2, x, sep = "_")
})

LHQ3[1, 200] = "L3_h/day"
Q_dailyuse3 = LHQ3[1, 200]
Q_dailyuse3 = as.character(Q_dailyuse3)
LHQ3[2, 200:204] = lapply(LHQ3[2, 200:204], function(x) {
  x = as.character(x)
  paste(Q_dailyuse3, x, sep = "_")
})

LHQ3[1, 205] = "L4_h/day"
Q_dailyuse4 = LHQ3[1, 205]
Q_dailyuse4 = as.character(Q_dailyuse4)
LHQ3[2, 205:209] = lapply(LHQ3[2, 205:209], function(x) {
  x = as.character(x)
  paste(Q_dailyuse4, x, sep = "_")
})


# 20.If you use mixed language in daily life, please indicate the languages that 
# you mix and estimate the frequency of mixing in normal conversation with the 
# following groups of people.

LHQ3[1, 210] = "codeswitching"
Q_Daily_codeswitching = LHQ3[1, 210]
Q_Daily_codeswitching = as.character(Q_Daily_codeswitching)
LHQ3[2, 210:221] = lapply(LHQ3[2, 210:221], function(x) {
  x = as.character(x)
  paste(Q_Daily_codeswitching, x, sep = "_")
})


# 21.In which language do you communicate best or feel most comfortable in terms 
# of listening, speaking, reading, and writing in each of the following environments? 
# You may be selecting the same language for all or some of the fields below.

LHQ3[1, 222] = "Dominance/comfort"
Q_Selfrated_language_dominance = LHQ3[1, 222]
Q_Selfrated_language_dominance = as.character(Q_Selfrated_language_dominance)
LHQ3[2, 222:237] = lapply(LHQ3[2, 222:237], function(x) {
  x = as.character(x)
  paste(Q_Selfrated_language_dominance, x, sep = "_")
})


# 22.How often do you use each of the languages you have studied or learned for 
# the following activities? (including the native language)

LHQ3[1, 238] = "L1_internal_use_for"
Q_L1_for = LHQ3[1, 238]
Q_L1_for = as.character(Q_L1_for)
LHQ3[2, 238:245] = lapply(LHQ3[2, 238:245], function(x) {
  x = as.character(x)
  paste(Q_L1_for, x, sep = "_")
})

LHQ3[1, 246] = "L2_internal_use_for"
Q_L2_for = LHQ3[1, 246]
Q_L2_for = as.character(Q_L2_for)
LHQ3[2, 246:253] = lapply(LHQ3[2, 246:253], function(x) {
  x = as.character(x)
  paste(Q_L2_for, x, sep = "_")
})

LHQ3[1, 254] = "L3_internal_use_for"
Q_L3_for = LHQ3[1, 254]
Q_L3_for = as.character(Q_L3_for)
LHQ3[2, 254:261] = lapply(LHQ3[2, 254:261], function(x) {
  x = as.character(x)
  paste(Q_L3_for, x, sep = "_")
})

LHQ3[1, 262] = "L4_internal_use_for"
Q_L4_for = LHQ3[1, 262]
Q_L4_for = as.character(Q_L4_for)
LHQ3[2, 262:269] = lapply(LHQ3[2, 262:269], function(x) {
  x = as.character(x)
  paste(Q_L4_for, x, sep = "_")
})


# 23.What percentage of your friends speaks each of the languages you have studied 
# or learned? (including the native language)

LHQ3[1, 270] = "%Friends_who_speak_L1"
Q_L1_competence_friends = LHQ3[1, 270]
Q_L1_competence_friends = as.character(Q_L1_competence_friends)
LHQ3[2, 270:271] = lapply(LHQ3[2, 270:271], function(x) {
  x = as.character(x)
  paste(Q_L1_competence_friends, x, sep = "_")
})

LHQ3[1, 272] = "%Friends_who_speak_L2"
Q_L2_competence_friends = LHQ3[1, 272]
Q_L2_competence_friends = as.character(Q_L2_competence_friends)
LHQ3[2, 272:273] = lapply(LHQ3[2, 272:273], function(x) {
  x = as.character(x)
  paste(Q_L2_competence_friends, x, sep = "_")
})

LHQ3[1, 274] = "%Friends_who_speak_L3"
Q_L2_competence_friends = LHQ3[1, 274]
Q_L2_competence_friends = as.character(Q_L2_competence_friends)
LHQ3[2, 274:275] = lapply(LHQ3[2, 274:275], function(x) {
  x = as.character(x)
  paste(Q_L2_competence_friends, x, sep = "_")
})

LHQ3[1, 276] = "%Friends_who_speak_L4%"
Q_L2_competence_friends = LHQ3[1, 276]
Q_L2_competence_friends = as.character(Q_L2_competence_friends)
LHQ3[2, 276:277] = lapply(LHQ3[2, 276:277], function(x) {
  x = as.character(x)
  paste(Q_L2_competence_friends, x, sep = "_")
})


# 24.Which cultures/languages do you identify with more strongly? Rate the strength 
# of your connection in the following categories for each culture/language.
# organising by language

LHQ3[1, 278] = "Identification_with_L1"
Q_L1_cultural_identification = LHQ3[1, 278]
Q_L1_cultural_identification = as.character(Q_L1_cultural_identification)
LHQ3[2, 278:284] = lapply(LHQ3[2, 278:284], function(x) {
  x = as.character(x)
  paste(Q_L1_cultural_identification, x, sep = "_")
})

LHQ3[1, 285] = "Identification_with_L2"
Q_L2_cultural_identification = LHQ3[1, 285]
Q_L2_cultural_identification = as.character(Q_L2_cultural_identification)
LHQ3[2, 285:291] = lapply(LHQ3[2, 285:291], function(x) {
  x = as.character(x)
  paste(Q_L2_cultural_identification, x, sep = "_")
})

LHQ3[1, 292] = "Identification_with_L3"
Q_L3_cultural_identification = LHQ3[1, 292]
Q_L3_cultural_identification = as.character(Q_L3_cultural_identification)
LHQ3[2, 292:298] = lapply(LHQ3[2, 292:298], function(x) {
  x = as.character(x)
  paste(Q_L3_cultural_identification, x, sep = "_")
})

LHQ3[1, 299] = "Identification_with_L4"
Q_L4_cultural_identification = LHQ3[1, 299]
Q_L4_cultural_identification = as.character(Q_L4_cultural_identification)
LHQ3[2, 299:305] = lapply(LHQ3[2, 299:305], function(x) {
  x = as.character(x)
  paste(Q_L4_cultural_identification, x, sep = "_")
})


# Organis the data frame so it can be easily analysed, first by making the 
# question/condition the header.
# Remove the first row which includes the shorthanded questions and NA 
# values, making the more informative second row a header.

LHQ3 = LHQ3[-1, ]
new_header = LHQ3[1, ]
LHQ3 = LHQ3[-1, ]
new_header = as.character(new_header)
names(LHQ3) = new_header

# Perform the left join using merge()
LHQ3 = merge(LHQ3, participant_IDs, by = "participant_LHQ3_ID", all.x = TRUE)

# Calculate the average score for code-switching between languages for each 
# participant. NA values are ignored

codeswitching = c("codeswitching_Frequency of mixing with family\nmembers", 
                  "codeswitching_Frequency of mixing with\nfriends", 
                  "codeswitching_Frequency of mixing with\nclassmates", 
                  "codeswitching_Frequency of mixing with\nothers")


# Step 2: Convert the specified columns to numeric
LHQ3[, codeswitching] = 
  lapply(LHQ3[, codeswitching], function(x) {
    num_x = as.numeric(as.character(x))
    return(num_x)
  })

# Step 3: Replace NA values with 0 in the converted columns
LHQ3[, codeswitching][is.na(LHQ3[, codeswitching])] = 0

# Step 4: Calculate the row-wise average for the specified columns
LHQ3$codeswitching_average = rowMeans(LHQ3[, codeswitching], na.rm = TRUE)

# View(LHQ3)

# Import a set of variables that were computed in the LHQ3 platform, including 
# proficiency scores and language Multilingual Language Diversity.
# Remove the first row that reads "LHQ3" and creating new header.
# Rename the first column (Participant ID).

LHQ3_aggregate_scores = 
  read_excel("data/raw data/language history/LHQ3 Aggregate Scores.xlsx", 
             sheet = "Sheet1", col_names = FALSE, skip = 1) %>%
  slice(-1) # Remove first row

# View(LHQ3_aggregate_scores)

# Rename columns using indices
names(LHQ3_aggregate_scores)[c(1, 6, 7, 14)] = 
  c("participant_LHQ3_ID", "L1_Proficiency", "L2_Proficiency", 
    "multilingual_language_diversity")


# Ensure 'participant_LHQ3_ID' is present in both data frames for merging
LHQ3_aggregate_scores = merge(LHQ3, LHQ3_aggregate_scores, by = "participant_LHQ3_ID")
# View(LHQ3_aggregate_scores)

# Select the necessary columns from LHQ3 and LHQ3_aggregate_scores.
# Combine the two data frames based on participant_LHQ3_ID.

LHQ3 = LHQ3 %>%
  select(participant_LHQ3_ID, mini_language, codeswitching_average)

LHQ3_aggregate_scores = LHQ3_aggregate_scores %>%
  select(participant_LHQ3_ID, L1_Proficiency, L2_Proficiency, 
         multilingual_language_diversity) %>%
  # Make MLD numeric
  mutate(multilingual_language_diversity = as.numeric(multilingual_language_diversity))

LHQ3 = merge(LHQ3, LHQ3_aggregate_scores, 
             by = "participant_LHQ3_ID", all.x = TRUE) %>% 
  select(participant_LHQ3_ID, L1_Proficiency, L2_Proficiency, 
         multilingual_language_diversity)

# View(LHQ3)

