

library(dplyr)
library(ggplot2)
library(patchwork)

# Import raw data to allow reliable mapping between numerically-recoded variables 
# and the original, factorial versions. This way, the plots will present 
# understandable labels even though the models were run with numerically-recoded 
# variables to facilitate better interpretation of the results (see scripts in 
# "analyses" folder). 
# A minimal amount of raw data is imported below, just enough to extract the 
# match between the original variables and the numerically-recoded versions. 

# # Load function
# source('data/R_functions/merge_trialbytrial_EEG_data.R')

EEG_genderAgr_MiniNor_300_600_midline_data <- 
  readRDS('data/merged data/EEG_genderAgr_MiniNor_300_600_midline_data.rds')

EEG_genderAgr_MiniNor_300_600_midline_lmerTest <- 
  readRDS('analyses/EEG/results/EEG_genderAgr_MiniNor_300_600_midline_lmerTest.rds')

# Load custom function for plotting interactions that involve 
# numerically-recoded variables.
source('analyses/R_functions/alias_interaction_plot.R')

plot1 <- alias_interaction_plot(
  model = EEG_genderAgr_MiniNor_300_600_midline_lmerTest,
  dataset = EEG_genderAgr_MiniNor_300_600_midline_data,
  x = 'z_session1_digit_span',
  x_title = 'Digit span (*z*)',
  fill = 'z_recoded_grammaticality',
  fill_alias = 'grammaticality',
  fill_title = 'Grammaticality',
  y_title = 'Predicted amplitude (*z*)',
) + 
  theme(plot.tag.position = c(0, 1), 
        legend.position = c(.9, .82))


plot2 =
  deciles_interaction_plot(
    model = EEG_genderAgr_MiniNor_300_600_midline_lmerTest,
    x = 'z_recoded_grammaticality',
    fill = 'z_session1_Stroop',
    fill_nesting_factor = 'participant_lab_ID',
    x_title = 'Grammaticality (*z*)',
    y_title = 'Predicted amplitude (*z*)',
    fill_title = 'Stroop<br>(*z*, deciles)'
  ) +
  theme(plot.tag.position = c(0, 1))


plot3 =
  deciles_interaction_plot(
    model = EEG_genderAgr_MiniNor_300_600_midline_lmerTest,
    x = 'z_recoded_grammaticality',
    fill = 'z_session1_digit_span',
    fill_nesting_factor = 'participant_lab_ID',
    x_title = 'Grammaticality (*z*)',
    y_title = 'Predicted amplitude (*z*)',
    fill_title = 'Digit span<br>(*z*, deciles)'
  ) +
  theme(plot.tag.position = c(0, 1))

