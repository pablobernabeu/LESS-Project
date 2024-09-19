

# Session 4: Verb-object agreement, differential object marking and gender agreement

# Compiling the stimuli for all parts of the session in each 
# site and within each mini-language.


library(dplyr)

# Load custom functions

source('stimulus_preparation/R_functions/Session4_Training_verb_object_agreement.R')

source('stimulus_preparation/R_functions/Session4_Test_verb_object_agreement.R')

source('stimulus_preparation/R_functions/Sessions_4_6_Experiment_verb_object_agreement.R')

source('stimulus_preparation/R_functions/Sessions_4_6_Experiment_differential_object_marking.R')

source('stimulus_preparation/R_functions/Sessions_4_6_Experiment_gender_agreement.R')

source('stimulus_preparation/R_functions/combine_Sessions_4_6_Experiments.R')

# Set path to Experiment files
experiment_path = 'session_materials/Session 4/stimuli/experiment/'



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

# Stimuli for the training
Session4_Training_verb_object_agreement(
  Norway_site_stimuli, study_site, language)

# Stimuli for the test
Session4_Test_verb_object_agreement(
  Norway_site_stimuli, study_site, language)

# Stimuli for the part of the experiment on verb-object agreement
Sessions_4_6_Experiment_verb_object_agreement(
  Norway_site_stimuli, study_site, language, 
  experiment_path, verbose = TRUE)

# Stimuli for the part of the experiment on differential object marking
Sessions_4_6_Experiment_differential_object_marking(
  Norway_site_stimuli, study_site, language,  
  experiment_path, verbose = TRUE)

# Stimuli for the part of the experiment on gender agreement
Sessions_4_6_Experiment_gender_agreement(
  Norway_site_stimuli, study_site, language,  
  experiment_path, verbose = TRUE)

# Combine both parts of the experiment
combine_Sessions_4_6_Experiments(study_site, language, experiment_path)


# MINI-NORWEGIAN

language = 'Mini-Norwegian'

# Stimuli for the training
Session4_Training_verb_object_agreement(
  Norway_site_stimuli, study_site, language)

# Stimuli for the test
Session4_Test_verb_object_agreement(
  Norway_site_stimuli, study_site, language)

# Stimuli for the part of the experiment on verb-object agreement
Sessions_4_6_Experiment_verb_object_agreement(
  Norway_site_stimuli, study_site, language,  
  experiment_path, verbose = TRUE)

# Stimuli for the part of the experiment on differential object marking
Sessions_4_6_Experiment_differential_object_marking(
  Norway_site_stimuli, study_site, language,  
  experiment_path, verbose = TRUE)

# Stimuli for the part of the experiment on gender agreement
Sessions_4_6_Experiment_gender_agreement(
  Norway_site_stimuli, study_site, language,  
  experiment_path, verbose = TRUE)

# Combine both parts of the experiment
combine_Sessions_4_6_Experiments(study_site, language, experiment_path)



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

# Stimuli for the training
Session4_Training_verb_object_agreement(
  Spain_site_stimuli, study_site, language)

# Stimuli for the test
Session4_Test_verb_object_agreement(
  Spain_site_stimuli, study_site, language)

# Stimuli for the part of the experiment on verb-object agreement
Sessions_4_6_Experiment_verb_object_agreement(
  Spain_site_stimuli, study_site, language,  
  experiment_path, verbose = TRUE)

# Stimuli for the part of the experiment on differential object marking
Sessions_4_6_Experiment_differential_object_marking(
  Spain_site_stimuli, study_site, language,  
  experiment_path, verbose = TRUE)

# Stimuli for the part of the experiment on gender agreement
Sessions_4_6_Experiment_gender_agreement(
  Spain_site_stimuli, study_site, language,  
  experiment_path, verbose = TRUE)

# Combine both parts of the experiment
combine_Sessions_4_6_Experiments(study_site, language, experiment_path)


# MINI-SPANISH

language = 'Mini-Spanish'

# Stimuli for the training
Session4_Training_verb_object_agreement(
  Spain_site_stimuli, study_site, language)

# Stimuli for the test
Session4_Test_verb_object_agreement(
  Spain_site_stimuli, study_site, language)

# Stimuli for the part of the experiment on verb-object agreement
Sessions_4_6_Experiment_verb_object_agreement(
  Spain_site_stimuli, study_site, language,  
  experiment_path, verbose = TRUE)

# Stimuli for the part of the experiment on differential object marking
Sessions_4_6_Experiment_differential_object_marking(
  Spain_site_stimuli, study_site, language,  
  experiment_path, verbose = TRUE)

# Stimuli for the part of the experiment on gender agreement
Sessions_4_6_Experiment_gender_agreement(
  Spain_site_stimuli, study_site, language,  
  experiment_path, verbose = TRUE)

# Combine both parts of the experiment
combine_Sessions_4_6_Experiments(study_site, language, experiment_path)

