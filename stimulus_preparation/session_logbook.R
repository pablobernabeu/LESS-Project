

# Create session logbooks. These logbooks will not be shared publicly 
# because they will contain personal information of the participants.

library(dplyr)


# NORWAY SITE

# Load in participant parameters
read.csv('session_materials/parameters per participant/Norway site, parameters per participant.csv') %>%
  
  select(participant, language) %>%
  
  rename(participant_lab_ID = participant) %>%
  
  mutate(participant_home_ID = '', participant_name = '', participant_email = '', 
         Session1_date_time = '', Session1_inspector = '', Session1_notes = '', 
         Session2_date_time = '', Session2_conductor = '', Session2_notes = '', 
         Session3_date_time = '', Session3_conductor = '', Session3_notes = '', 
         Session4_date_time = '', Session4_conductor = '', Session4_notes = '', 
         Session5_date_time = '', Session5_inspector = '', Session5_notes = '', 
         Session6_date_time = '', Session6_conductor = '', Session6_notes = '') %>%
  
  relocate(participant_home_ID, .before = participant_lab_ID) %>%
  
  relocate(c(participant_name, participant_email), .after = participant_lab_ID) %>%
  
  # save 
  write.csv('session_materials/session logbook/Norway site, session_logbook.csv', 
            row.names = FALSE)



# SPAIN SITE

# Load in participant parameters
read.csv('session_materials/parameters per participant/Spain site, parameters per participant.csv') %>%
  
  # The Spain site includes a group of heritage speakers of English, 
  # in addition to the group of typical learners of English.
  select(English_acquisition, participant, language) %>%
  
  rename(participant_lab_ID = participant) %>%
  
  mutate(participant_home_ID = '', participant_name = '', participant_email = '', 
         Session1_date_time = '', Session1_inspector = '', Session1_notes = '', 
         Session2_date_time = '', Session2_conductor = '', Session2_notes = '', 
         Session3_date_time = '', Session3_conductor = '', Session3_notes = '', 
         Session4_date_time = '', Session4_conductor = '', Session4_notes = '', 
         Session5_date_time = '', Session5_inspector = '', Session5_notes = '', 
         Session6_date_time = '', Session6_conductor = '', Session6_notes = '') %>%
  
  relocate(participant_home_ID, .before = participant_lab_ID) %>%
  
  relocate(c(participant_name, participant_email), .after = participant_lab_ID) %>%
  
  # save 
  write.csv('session_materials/session logbook/Spain site, session_logbook.csv', 
            row.names = FALSE)

