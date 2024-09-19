

# Create participant-specific parameters. 

# The stimulus lists are described in the functions used to create the stimuli, 
# and in the columns whose name includes the word 'list' in the stimulus files.

# The numbers used below correspond to index positions, which are used in 
# OpenSesame to assign participant-specific parameters. To that end, for each 
# participant-specific factor in the stimulus files, the level selected is 
# that corresponding to the index position specified in the participant 
# parameters file that is created below.

# Some parameters are shared across sessions.


library(dplyr)

# Register current version of the materials
source('stimulus_preparation/materials_version.R')


# Session 2 parameters
Session2_resting_state_order = 1:2
Session2_training_list = 1:2
Session2_test_list = 1:2
Session2_experiment_list = 1:3

# Session 3 parameters
Session3_test_list = 1:2
Session3_experiment_list = 1:3

# Session 4 parameters
Session4_test_list = 1:2

# Sessions 4 and 6 parameters
Sessions_4_6_experiment_list = 1:3

# Create combinations of all factors within each session 

Session2_parameters = 
  expand.grid(Session2_resting_state_order = Session2_resting_state_order,
              Session2_training_list = Session2_training_list, 
              Session2_test_list = Session2_test_list, 
              Session2_experiment_list = Session2_experiment_list)

Session3_parameters = 
  expand.grid(Session3_test_list = Session3_test_list, 
              Session3_experiment_list = Session3_experiment_list)

Session4_parameters = 
  expand.grid(Session4_test_list = Session4_test_list)

Sessions_4_6_parameters = 
  expand.grid(Sessions_4_6_experiment_list = Sessions_4_6_experiment_list)

all_sessions = cbind(Session2_parameters, Session3_parameters, 
                     Session4_parameters, Sessions_4_6_parameters)


#################################################################


# Norway site

# Extend sample size by the following factor
extension_factor = 3

# Replicate other parameters identically in both mini-languages
rbind(all_sessions %>% mutate(language = 'Mini-English'),
      all_sessions %>% mutate(language = 'Mini-Norwegian')) %>%
  
  # Order dataframe to counterbalance 
  # mini-languages across participants. 
  arrange(pick(-language)) %>%
  
  # Repeat dataframe N times to cater for 
  # a sufficient number of participants.
  slice(rep(1:n(), extension_factor)) %>%
  
  # Assign each row to a participant 
  mutate(participant = 1:n()) %>%
  
  select( participant, language, 
          
          # Session 2
          Session2_resting_state_order,
          Session2_training_list, 
          Session2_test_list, 
          Session2_experiment_list, 
          
          # Session 3
          Session3_test_list, 
          Session3_experiment_list, 
          
          # Session 4
          Session4_test_list, 
          
          # Sessions 4 and 6
          Sessions_4_6_experiment_list ) %>%
  
  # Add materials version
  mutate(materials_version) %>%
  
  # save 
  write.csv('session_materials/parameters per participant/Norway site, parameters per participant.csv', 
            row.names = FALSE)


#################################################################


# Spain site

# Extend sample size by the following factor
extension_factor = 2

# The Spain site includes a group of heritage speakers of English, 
# in addition to the group of typical learners of English.
# Replicate other parameters identically in both groups.

all_Spain_sessions = 
  rbind(all_sessions %>% mutate(English_acquisition = 'non-heritage'),
        all_sessions %>% mutate(English_acquisition = 'heritage'))

# Replicate other parameters identically in both mini-languages
rbind(all_Spain_sessions %>% mutate(language = 'Mini-English'),
      all_Spain_sessions %>% mutate(language = 'Mini-Spanish')) %>%
  
  # Order dataframe to counterbalance English 
  # acquisition across participants. 
  arrange(arrange(pick(-English_acquisition))) %>%
  
  # Repeat dataframe N times to cater for 
  # a sufficient number of participants.
  slice(rep(1:n(), extension_factor)) %>%
  
  # Assign each row to a participant 
  mutate(participant = 1:n()) %>%
  
  select( participant, English_acquisition, language, 
          
          # Session 2
          Session2_resting_state_order,
          Session2_training_list, 
          Session2_test_list, 
          Session2_experiment_list, 
          
          # Session 3
          Session3_test_list, 
          Session3_experiment_list, 
          
          # Session 4
          Session4_test_list, 
          
          # Sessions 4 and 6
          Sessions_4_6_experiment_list ) %>%
  
  # Add materials version
  mutate(materials_version) %>%
  
  # save 
  write.csv('session_materials/parameters per participant/Spain site, parameters per participant.csv', 
            row.names = FALSE)

