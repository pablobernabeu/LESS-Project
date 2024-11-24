

# Compose all stimuli for Sessions 2, 3, 4 and 6

# Read in version of the materials. Version numbers are recorded in the 
# behavioural results files (e.g., `subject-10.csv`) in the column 
# `materials_version` (but note that this column is absent from the 
# files of the first few sessions, e.g., subject-1.csv). The versions 
# are set in each stimulus file in the column `materials_version`.

source('stimulus_preparation/materials_version.R')

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

