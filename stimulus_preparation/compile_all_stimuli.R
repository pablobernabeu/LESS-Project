

# Compose all stimuli for Sessions 2, 3, 4 and 6

# Register version of the materials in the 'session_materials' folder
source('stimulus_preparation/materials_version.R')
writeLines(paste0('materials_version = ', materials_version), 
           'session_materials/materials_version.txt')

# Frame base images
source('stimulus_preparation/base_images.R')

# Create participant-specific parameters
source('stimulus_preparation/participant_parameters.R')

# Session 2
source('stimulus_preparation/Session 2/Session2_compile_all_stimuli.R')

# Session 3
source('stimulus_preparation/Session 3/Session3_compile_all_stimuli.R')

# Session 4
source('stimulus_preparation/Session 4/Session4_compile_all_stimuli.R')

# Session 6
source('stimulus_preparation/Session 6/Session6_compile_all_stimuli.R')

