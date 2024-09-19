

# Session 2: Gender agreement

# Compiling the stimuli for all parts of the session 
# in each site and within each mini-language.


library(dplyr)

# Load custom functions

source('stimulus_preparation/R_functions/Session2_Pretraining_vocabulary.R')

source('stimulus_preparation/R_functions/Session2_Training_gender_agreement.R')

source('stimulus_preparation/R_functions/Session2_Test_gender_agreement.R')

source('stimulus_preparation/R_functions/Session2_Experiment_gender_agreement.R')


#################################

####      Norway site       ####

#################################

# Load in base stimuli

Norway_site_stimuli = 
  read.csv('stimulus_preparation/Norway site, base stimuli.csv', 
           fileEncoding = 'latin1', encoding = 'UTF-8')

study_site = 'Norway'


# MINI-ENGLISH

language = 'Mini-English'

# Pre-training stimuli
Session2_Pretraining_vocabulary(
  Norway_site_stimuli, study_site, language)

# Training stimuli
Session2_Training_gender_agreement(
  Norway_site_stimuli, study_site, language)

# Test stimuli
Session2_Test_gender_agreement(
  Norway_site_stimuli, study_site, language)

# Experiment stimuli
Session2_Experiment_gender_agreement(
  Norway_site_stimuli, study_site, language, 
  verbose = TRUE)


# MINI-NORWEGIAN

language = 'Mini-Norwegian'

# Pre-training stimuli
Session2_Pretraining_vocabulary(
  Norway_site_stimuli, study_site, language)

# Training stimuli
Session2_Training_gender_agreement(
  Norway_site_stimuli, study_site, language)

# Test stimuli
Session2_Test_gender_agreement(
  Norway_site_stimuli, study_site, language)

# Experiment stimuli
Session2_Experiment_gender_agreement(
  Norway_site_stimuli, study_site, language, 
  verbose = TRUE)



#################################

####       Spain site       ####

#################################

# Load in base stimuli

Spain_site_stimuli = 
  read.csv('stimulus_preparation/Spain site, base stimuli.csv',
           fileEncoding = 'latin1', encoding = 'UTF-8')

study_site = 'Spain'


# MINI-ENGLISH

language = 'Mini-English'

# Pre-training stimuli
Session2_Pretraining_vocabulary(
  Spain_site_stimuli, study_site, language)

# Training stimuli
Session2_Training_gender_agreement(
  Spain_site_stimuli, study_site, language)

# Test stimuli
Session2_Test_gender_agreement(
  Spain_site_stimuli, study_site, language)

# Experiment stimuli
Session2_Experiment_gender_agreement(
  Spain_site_stimuli, study_site, language, 
  verbose = TRUE)


# MINI-SPANISH

language = 'Mini-Spanish'

# Pre-training stimuli
Session2_Pretraining_vocabulary(
  Spain_site_stimuli, study_site, language)

# Training stimuli
Session2_Training_gender_agreement(
  Spain_site_stimuli, study_site, language)

# Test stimuli
Session2_Test_gender_agreement(
  Spain_site_stimuli, study_site, language)

# Experiment stimuli
Session2_Experiment_gender_agreement(
  Spain_site_stimuli, study_site, language, 
  verbose = TRUE)

