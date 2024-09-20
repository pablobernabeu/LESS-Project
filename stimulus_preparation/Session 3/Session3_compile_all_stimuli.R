

# Session 3: Differential object marking and gender agreement

# Compiling the stimuli for all parts of the session in each 
# site and within each mini-language.


library(dplyr)

# Load custom functions

source('stimulus_preparation/R_functions/Session3_Pretraining_vocabulary.R')

source('stimulus_preparation/R_functions/Session3_Training_differential_object_marking.R')

source('stimulus_preparation/R_functions/Session3_Test_differential_object_marking.R')

source('stimulus_preparation/R_functions/Session3_Experiment_differential_object_marking.R')

source('stimulus_preparation/R_functions/Session3_Experiment_gender_agreement.R')

source('stimulus_preparation/R_functions/combine_Session3_experiments.R')

# Set path to Experiment files
experiment_path = 'session_materials/Session 3/stimuli/experiment/'



#################################

####    Norway site       ####

#################################

# Load in base stimuli

Norway_site_stimuli = 
  read.csv('stimulus_preparation/Norway site, base stimuli.csv', 
           fileEncoding = 'latin1', encoding = 'UTF-8')

study_site = 'Norway'


# MINI-ENGLISH

language = 'Mini-English'

# Stimuli for the pre-training
Session3_Pretraining_vocabulary(
  Norway_site_stimuli, study_site, language)

# Stimuli for the training
Session3_Training_differential_object_marking(
  Norway_site_stimuli, study_site, language)

# Stimuli for the test
Session3_Test_differential_object_marking(
  Norway_site_stimuli, study_site, language)

# Stimuli for the part of the experiment on differential object marking
Session3_Experiment_differential_object_marking(
  Norway_site_stimuli, study_site, language, 
  verbose = TRUE)

# Stimuli for the part of the experiment on gender agreement
Session3_Experiment_gender_agreement(
  Norway_site_stimuli, study_site, language, 
  verbose = TRUE)

# Combine both parts of the experiment
combine_Session3_experiments(study_site, language, experiment_path)


# MINI-NORWEGIAN

language = 'Mini-Norwegian'

# Stimuli for the pre-training
Session3_Pretraining_vocabulary(
  Norway_site_stimuli, study_site, language)

# Stimuli for the training
Session3_Training_differential_object_marking(
  Norway_site_stimuli, study_site, language)

# Stimuli for the test
Session3_Test_differential_object_marking(
  Norway_site_stimuli, study_site, language)

# Stimuli for the part of the experiment on differential object marking
Session3_Experiment_differential_object_marking(
  Norway_site_stimuli, study_site, language, 
  verbose = TRUE)

# Stimuli for the part of the experiment on gender agreement
Session3_Experiment_gender_agreement(
  Norway_site_stimuli, study_site, language, 
  verbose = TRUE)

# Combine both parts of the experiment
combine_Session3_experiments(study_site, language, experiment_path)



#################################

####     Spain site       ####

#################################

# Load in base stimuli

Spain_site_stimuli = 
  read.csv('stimulus_preparation/Spain site, base stimuli.csv',
           fileEncoding = 'latin1', encoding = 'UTF-8')

study_site = 'Spain'


# MINI-ENGLISH

language = 'Mini-English'

# Stimuli for the pre-training
Session3_Pretraining_vocabulary(
  Spain_site_stimuli, study_site, language)

# Stimuli for the training
Session3_Training_differential_object_marking(
  Spain_site_stimuli, study_site, language)

# Stimuli for the test
Session3_Test_differential_object_marking(
  Spain_site_stimuli, study_site, language)

# Stimuli for the part of the experiment on differential object marking
Session3_Experiment_differential_object_marking(
  Spain_site_stimuli, study_site, language, 
  verbose = TRUE)

# Stimuli for the part of the experiment on gender agreement
Session3_Experiment_gender_agreement(
  Spain_site_stimuli, study_site, language, 
  verbose = TRUE)

# Combine both parts of the experiment
combine_Session3_experiments(study_site, language, experiment_path)


# MINI-SPANISH

language = 'Mini-Spanish'

# Stimuli for the pre-training
Session3_Pretraining_vocabulary(
  Spain_site_stimuli, study_site, language)

# Stimuli for the training
Session3_Training_differential_object_marking(
  Spain_site_stimuli, study_site, language)

# Stimuli for the test
Session3_Test_differential_object_marking(
  Spain_site_stimuli, study_site, language)

# Stimuli for the part of the experiment on differential object marking
Session3_Experiment_differential_object_marking(
  Spain_site_stimuli, study_site, language, 
  verbose = TRUE)

# Stimuli for the part of the experiment on gender agreement
Session3_Experiment_gender_agreement(
  Spain_site_stimuli, study_site, language, 
  verbose = TRUE)

# Combine both parts of the experiment
combine_Session3_experiments(study_site, language, experiment_path)

