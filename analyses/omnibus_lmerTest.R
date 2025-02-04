

# Omnibus analysis of longitudinal progress in gender agreement

library(dplyr)  # data wrangling
# The following lme4-relevant package was installed before lme4 to avoid a conflict 
# noted at https://cran.r-project.org/web/packages/lme4/lme4.pdf
library(RcppEigen)
library(lme4)   # Mixed-effects models
library(lmerTest)  # Compute p values
library(MuMIn)   # R^2

# Load data
source('data/preprocessing/merge trial-by-trial EEG data.R')


# Compute baseline predictor following Alday (2019; http://doi.org/10.1111/psyp.13451).
# Tutorial: https://mne.tools/stable/auto_tutorials/epochs/15_baseline_regression.html.
# The predictor is computed per brain region and per trial. 

baseline_predictor <- 
  trialbytrial_EEG_data %>% filter(time < 0) %>%
  group_by(brain_region, sentence_marker) %>%
  summarise(baseline_predictor = mean(amplitude, na.rm = TRUE), 
            .groups = "drop")

# Import baseline predictor to main data set
trialbytrial_EEG_data <- trialbytrial_EEG_data %>%
  left_join(baseline_predictor, by = c(brain_region, sentence_marker))

# Aggregate data across electrodes and time points
trialbytrial_EEG_data <- trialbytrial_EEG_data %>%
  
  # Drop columns
  select(-electrode, -time) %>%
  
  # Group by all columns except amplitude
  group_by(across(-amplitude)) %>%
  
  # Aggregate amplitude by mean
  summarise(amplitude = mean(amplitude, na.rm = TRUE), .groups = "drop")


# Z-score amplitude around each participant's own mean to preserve individual differences 
# (Faust et al., 1999; https://doi.org/10.1037/0033-2909.125.6.777)

trialbytrial_EEG_data$z_RTclean <- scale_by(RTclean ~ Participant, trialbytrial_EEG_data)

# Z-score between-participants predictors, following Brauer and Curtin (2018; 
# https://doi.org/10.1037/met0000159).

trialbytrial_EEG_data$stroop <- scale(trialbytrial_EEG_data$stroop)
trialbytrial_EEG_data$ASRT <- scale(trialbytrial_EEG_data$ASRT)
trialbytrial_EEG_data$digit_span <- scale(trialbytrial_EEG_data$digit_span)

# Z-score between-items predictors around each participant's own mean, 
# following Brauer and Curtin (2018; https://doi.org/10.1037/met0000159).

trialbytrial_EEG_data$grammaticality <- scale_by(grammaticality ~ Participant, trialbytrial_EEG_data)
trialbytrial_EEG_data$grammaticality <- scale_by(grammaticality ~ Participant, trialbytrial_EEG_data)
trialbytrial_EEG_data$grammaticality <- scale_by(grammaticality ~ Participant, trialbytrial_EEG_data)
trialbytrial_EEG_data$grammaticality <- scale_by(grammaticality ~ Participant, trialbytrial_EEG_data)
trialbytrial_EEG_data$grammaticality <- scale_by(grammaticality ~ Participant, trialbytrial_EEG_data)

# Model
# Measure running time
system.time({
  
  lmerTest_omnibus_longitudinal_gender_agreement <- lmerTest::lmer(
    
    # Dependent variable
    amplitude ~ 
      
      # Independent variables
      grammaticality + 
      session +
      mini_language + 
      stroop + 
      ASRT + 
      digit_span + 
      multilingual_language_diversity +
      time_window + 
      hemisphere + 
      caudality +
      
      # Interactions 
      grammaticality : session + 
      grammaticality : mini_language + 
      grammaticality : stroop + 
      grammaticality : ASRT + 
      grammaticality : digit_span + 
      grammaticality : multilingual_language_diversity + 
      grammaticality : time_window +
      grammaticality : hemisphere  +
      grammaticality : caudality  +
      
      # RANDOM EFFECTS: maximal structure constructed following the guidelines of Brauer and Curtin (2018;
      # https://psych.wisc.edu/Brauer/BrauerLab/wp-content/uploads/2014/04/Brauer-Curtin-2018-on-LMEMs.pdf).
      # Interactions only require random slopes if all variables involved vary within the same units.
      # Double vertical bars (||) are used to remove correlations among random effects, with the aim of
      # aiding the convergence of the model (i.e., Remedy 15 in Table 17 of Brauer & Curtin, 2018).
      
      # Random intercepts
      (1 | participant_lab_ID) + (1 | sentence_marker) +
      
      # In the random slopes below, the prefix `0 +` helps avoid redundant random intercepts 
      # (see https://github.com/lme4/lme4/issues/625) and reduces the random-effects
      # structure (Brauer & Curtin, 2018).
      
      # By-participant random slopes
      (0 + z_word_cooccurrence || participant_lab_ID) + 
      (0 + z_visual_rating || participant_lab_ID) +
      
      # By-sentence random slopes
      (0 + z_vocabulary_size || sentence_marker) + 
      (0 + z_recoded_participant_gender || sentence_marker),
    
    data = trialbytrial_EEG_data,
    
    # Set maximum number of iterations to 1m to facilitate convergence 
    # (Brauer & Curtin, 2018; Singmann & Kellen, 2019)
    control = lmerControl(optCtrl <- list(maxfun = 1e6))
  )
})

saveRDS(trialbytrial_EEG_data_lmerTest, 
        'analyses/results/trialbytrial_EEG_data_lmerTest.rds')

# Calculate p values using Kenward-Roger method (Luke, 2017; 
# https://doi.org/10.3758/s13428-016-0809-y)
summary(trialbytrial_EEG_data_lmerTest, ddf <- 'Kenward-Roger') %>%
  saveRDS('analyses/results/KR_summary_trialbytrial_EEG_data_lmerTest.rds')

# Calculate R^2. The result must be interpreted with caution as it differs from the 
# traditional R^2 (Nakagawa et al., 2017; https://doi.org/10.1098/rsif.2017.0213)
MuMIn::r.squaredGLMM(trialbytrial_EEG_data_lmerTest) %>%
  saveRDS('analyses/results/Nakagawa2017_R2_trialbytrial_EEG_data_lmerTest.rds')

# Calculate 95% confidence intervals of fixed effects
lme4::confint.merMod(trialbytrial_EEG_data_lmerTest, method <- 'profile',
                     # Compute 95% CIs for every effect, as well as for the intercept
                     parm <- rownames(summary(trialbytrial_EEG_data_lmerTest)$coefficients)) %>%
  saveRDS('analyses/results/confint_trialbytrial_EEG_data_lmerTest.rds')

# Save random effects
lme4::ranef(trialbytrial_EEG_data_lmerTest) %>%
  saveRDS('analyses/results/ranef_trialbytrial_EEG_data_lmerTest.rds')

