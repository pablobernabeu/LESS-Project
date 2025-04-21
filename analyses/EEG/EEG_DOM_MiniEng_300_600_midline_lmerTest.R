

# Analysis of EEG data for differential object marking in Mini-English group, within time window of 
# 300-600 ms and within midline electrodes.

library(dplyr)  # data wrangling
# The following lme4-relevant package was installed before lme4 to avoid a conflict 
# noted at https://cran.r-project.org/web/packages/lme4/lme4.pdf
library(RcppEigen)
library(lme4)   # Mixed-effects models
library(lmerTest)  # Compute p values
# library(MuMIn)   # R^2

# Terminological note: "brain regions" refer to nine standard sets of electrodes that were 
# used in González Alonso et al. (2020; https://doi.org/10.1016/j.jneuroling.2020.100939). 
# "Macroregions" refer to midline and midline sets of electrodes, also used in González 
# Alonso et al. (2020). 

# Filter data to the Mini-English group (namely, odd participant IDs), to the property of 
# differential object marking (i.e., marker S2), to the grammatical and ungrammatical trials 
# (namely, markers S101 and S102), and to the appropriate time window and macroregion.

# Load function
source('data/R_functions/merge_trialbytrial_EEG_data.R') 

EEG_DOM_MiniEng_300_600_midline_data <- 
  merge_trialbytrial_EEG_data(EEG_file_pattern = '^\\d*[13579]_trialbytrial_S2_S10[12]\\.',
                              min_time = 300, max_time = 598, # 598 = time point up to 600 ms
                              include_baseline = TRUE,
                              aggregate_electrodes = TRUE, 
                              aggregate_time_points = TRUE, 
                              selected_macroregion = 'midline')

# Compute baseline predictor following Alday (2019; http://doi.org/10.1111/psyp.13451).
# Tutorial: https://mne.tools/stable/auto_tutorials/epochs/15_baseline_regression.html.
# The predictor is computed per participant, per brain region and per trial. 
# It is then z-scored per participant. 

baseline_predictor <- 
  EEG_DOM_MiniEng_300_600_midline_data %>% 
  filter(time_window == 'baseline') %>%
  group_by(participant_lab_ID, brain_region, sentence_marker) %>%
  summarise(
    baseline_predictor = mean(amplitude, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  group_by(participant_lab_ID) %>%
  mutate(z_baseline_predictor = scale(baseline_predictor))

# Import baseline predictor to main data set
EEG_DOM_MiniEng_300_600_midline_data <- EEG_DOM_MiniEng_300_600_midline_data %>%
  left_join(baseline_predictor, by = c('participant_lab_ID', 'brain_region', 'sentence_marker')) %>%
  
  # Select variables
  select(contains(c('amplitude', 'baseline', 'grammaticality', 'session', 'multilingual', 
                    'hemisphere', 'caudality', 'participant_lab_ID', 'sentence_marker')))

# Save data to disk
saveRDS(EEG_DOM_MiniEng_300_600_midline_data, 
        'data/merged data/EEG_DOM_MiniEng_300_600_midline_data.rds')

# MODEL
# Measure running time
system.time({
  
  EEG_DOM_MiniEng_300_600_midline_lmerTest <- lmerTest::lmer(
    
    # Dependent variable
    z_amplitude ~ 
      
      # Following Alday (2019; http://doi.org/10.1111/psyp.13451)
      z_baseline_predictor +
      
      # Independent variables
      z_recoded_grammaticality +
      z_recoded_session +
      z_session1_digit_span +
      z_session1_Stroop +
      z_session1_ASRT +
      z_multilingual_language_diversity +
      z_recoded_caudality  +
      
      # Interactions
      z_recoded_grammaticality : z_recoded_session +
      z_recoded_grammaticality : z_session1_digit_span +
      z_recoded_grammaticality : z_session1_Stroop +
      z_recoded_grammaticality : z_session1_ASRT +
      z_recoded_grammaticality : z_multilingual_language_diversity +
      z_recoded_grammaticality : z_recoded_caudality  +
      
      # RANDOM EFFECTS: maximal structure constructed following the guidelines of Brauer and Curtin 
      # (2018; http://doi.org/10.1037/met0000159).
      # Interactions only require random slopes if all variables involved vary within the same units.
      # Double vertical bars (||) are used to remove correlations among random effects, with the aim 
      # of aiding the convergence of the model (i.e., Remedy 15 in Table 17 of Brauer & Curtin, 2018).
      
      # Random intercepts
      (1 | participant_lab_ID) + (1 | sentence_marker) +
      
      # In the random slopes below, the prefix `0 +` helps avoid redundant random intercepts 
      # (see https://github.com/lme4/lme4/issues/625) and reduces the random-effects
      # structure (Brauer & Curtin, 2018). Some random slopes omitted to aid convergence. 
      # Random slopes not needed for interactions involving a between-participants variable 
      # and a between-items variable (Brauer & Curtin, 2018), which is the case of most 
      # interactions with Grammaticality.
      
      # By-participant random slopes for between-items variables
      (0 + z_recoded_grammaticality || participant_lab_ID) +
      (0 + z_recoded_session || participant_lab_ID) +
      
      # By-sentence random slopes for between-participants variables
      (0 + z_session1_digit_span || sentence_marker) +
      (0 + z_session1_Stroop || sentence_marker) +
      (0 + z_session1_ASRT || sentence_marker) +
      (0 + z_multilingual_language_diversity || sentence_marker),
    
    data = EEG_DOM_MiniEng_300_600_midline_data,
    
    # Set maximum number of iterations to 1m to facilitate convergence 
    # (Brauer & Curtin, 2018; Singmann & Kellen, 2019)
    control = lmerControl(optCtrl = list(maxfun = 1e6))
  )
})

saveRDS(EEG_DOM_MiniEng_300_600_midline_lmerTest, 
        'analyses/EEG/results/EEG_DOM_MiniEng_300_600_midline_lmerTest.rds')

EEG_DOM_MiniEng_300_600_midline_lmerTest <-
  readRDS('analyses/EEG/results/EEG_DOM_MiniEng_300_600_midline_lmerTest.rds')

# Calculate p values using Satterthwaite method (Luke, 2017; 
# https://doi.org/10.3758/s13428-016-0809-y)
summary(EEG_DOM_MiniEng_300_600_midline_lmerTest, ddf = 'Satterthwaite') %>%
  saveRDS('analyses/EEG/results/Satterthwaite_summary_EEG_DOM_MiniEng_300_600_midline_lmerTest.rds')

# Calculate R^2. The result must be interpreted with caution as it differs from the 
# traditional R^2 (Nakagawa et al., 2017; https://doi.org/10.1098/rsif.2017.0213)
# MuMIn::r.squaredGLMM(EEG_DOM_MiniEng_300_600_midline_lmerTest) %>%
#   saveRDS('analyses/EEG/results/Nakagawa2017_R2_EEG_DOM_MiniEng_300_600_midline_lmerTest.rds')

# Calculate 95% confidence intervals of fixed effects
lme4::confint.merMod(
  EEG_DOM_MiniEng_300_600_midline_lmerTest, method = 'Wald',
  # Compute 95% CIs for every effect, as well as for the intercept
  parm = rownames(summary(EEG_DOM_MiniEng_300_600_midline_lmerTest)$coefficients)
) %>%
  saveRDS('analyses/EEG/results/confint_EEG_DOM_MiniEng_300_600_midline_lmerTest.rds')

# Save random effects
lme4::ranef(EEG_DOM_MiniEng_300_600_midline_lmerTest) %>%
  saveRDS('analyses/EEG/results/ranef_EEG_DOM_MiniEng_300_600_midline_lmerTest.rds')

