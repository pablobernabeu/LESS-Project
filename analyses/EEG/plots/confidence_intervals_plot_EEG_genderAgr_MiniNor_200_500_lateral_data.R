

# Plot mean and 95% confidence interval for each effect

library(dplyr)
library(stringr)
library(ggplot2)

# Load custom function
source('analyses/R_functions/plot_95_confidence_intervals.R')

# Model summary with Satterthwaite p values
Satterthwaite_summary_EEG_genderAgr_MiniNor_200_500_lateral_lmerTest =
  readRDS('EEG/analyses/results/Satterthwaite_summary_EEG_genderAgr_MiniNor_200_500_lateral_lmerTest.rds')

# Confidence intervals
confint_EEG_genderAgr_MiniNor_200_500_lateral_lmerTest =
  readRDS('EEG/analyses/results/confint_EEG_genderAgr_MiniNor_200_500_lateral_lmerTest.rds')

# Translate names of effects into plain language

# Specify the variable to be removed
excluded_variables <- "z_baseline_predictor"

# Select all variables except the specified one
selected_vars <- 
  setdiff(
    rownames(Satterthwaite_summary_EEG_genderAgr_MiniNor_200_500_lateral_lmerTest$coefficients), 
    excluded_variables
  )

rownames(KR_summary_semanticpriming_lmerTest$coefficients) =
  rownames(KR_summary_semanticpriming_lmerTest$coefficients) %>%
  
  # First, adjust names of variables (both in main effects and in interactions)
  str_replace(pattern = 'z_target_word_frequency',
              replacement = 'Target-word frequency') %>%
  str_replace(pattern = 'z_target_number_syllables',
              replacement = 'Number of target-word syllables') %>%
  str_replace(pattern = 'z_word_concreteness_diff',
              replacement = 'Word-concreteness difference') %>%
  str_replace(pattern = 'z_cosine_similarity',
              replacement = 'Language-based similarity') %>%
  str_replace(pattern = 'z_visual_rating_diff',
              replacement = 'Visual-strength difference') %>%
  str_replace(pattern = 'z_attentional_control',
              replacement = 'Attentional control') %>%
  str_replace(pattern = 'z_vocabulary_size',
              replacement = 'Vocabulary size') %>%
  str_replace(pattern = 'z_recoded_participant_gender',
              replacement = 'Gender') %>%
  str_replace(pattern = 'z_recoded_interstimulus_interval',
              replacement = 'SOA') %>%
  # Show acronym in main effect of SOA
  str_replace(pattern = '^SOA$',
              replacement = 'Stimulus onset asynchrony (SOA)') %>%
  
  # Second, adjust order of effects in interactions. In the output from the model, 
  # the word-level variables of interest (i.e., 'z_cosine_similarity' and 
  # 'z_visual_rating_diff') sometimes appeared second in their interactions. For 
  # better consistency, the code below moves those word-level variables (with 
  # their new names) to the first position in their interactions. Note that the 
  # order does not affect the results in any way.
  sub("(\\w+.*):(Language-based similarity|Visual-strength difference)", 
      '\\2:\\1', 
      .)

rownames(confint_semanticpriming_lmerTest) =
  rownames(confint_semanticpriming_lmerTest) %>%
  
  # First, adjust names of variables (both in main effects and in interactions)
  str_replace(pattern = 'z_target_word_frequency',
              replacement = 'Target-word frequency') %>%
  str_replace(pattern = 'z_target_number_syllables',
              replacement = 'Number of target-word syllables') %>%
  str_replace(pattern = 'z_word_concreteness_diff',
              replacement = 'Word-concreteness difference') %>%
  str_replace(pattern = 'z_cosine_similarity',
              replacement = 'Language-based similarity') %>%
  str_replace(pattern = 'z_visual_rating_diff',
              replacement = 'Visual-strength difference') %>%
  str_replace(pattern = 'z_attentional_control',
              replacement = 'Attentional control') %>%
  str_replace(pattern = 'z_vocabulary_size',
              replacement = 'Vocabulary size') %>%
  str_replace(pattern = 'z_recoded_participant_gender',
              replacement = 'Gender') %>%
  str_replace(pattern = 'z_recoded_interstimulus_interval',
              replacement = 'SOA') %>%
  # Show acronym in main effect of SOA
  str_replace(pattern = '^SOA$',
              replacement = 'Stimulus onset asynchrony (SOA)') %>%
  
  # Second, adjust order of effects in interactions. In the output from the model, 
  # the word-level variables of interest (i.e., 'z_cosine_similarity' and 
  # 'z_visual_rating_diff') sometimes appeared second in their interactions. For 
  # better consistency, the code below moves those word-level variables (with 
  # their new names) to the first position in their interactions. Note that the 
  # order does not affect the results in any way.
  sub('(\\w+.*):(Language-based similarity|Visual-strength difference)', 
      '\\2:\\1', 
      .)


( plot_95_confidence_intervals(
  Satterthwaite_summary_EEG_genderAgr_MiniNor_200_500_lateral_lmerTest, 
  confint_EEG_genderAgr_MiniNor_200_500_lateral_lmerTest, 
  x_title = 'Effect size (&beta;)', 
  interaction_symbol_x = TRUE,
  vertical_line_at_x = 0, 
  select_effects = selected_vars
) + 
    theme(plot.margin = margin(9, 4, 14, 12)) ) %>%
  # Save plot
  ggsave(filename = 'EEG/analyses/plots/semanticpriming/frequentist_analysis/plots/semanticpriming_confidence_intervals_plot.pdf', 
         device = cairo_pdf, width = 5, height = 7, dpi = 900, create.dir = TRUE)

