# Consolidated analysis of EEG data for verb-object number agreement across all time
# windows, brain regions, and mini-language groups. This revised analysis reduces
# the number of models and implements family-wise error rate correction for post-hoc
# contrasts using the Holm-Bonferroni method via emmeans.

library(dplyr) # data wrangling
library(emmeans) # post-hoc contrasts with multiple comparison correction
# The following lme4-relevant package was installed before lme4 to avoid a conflict
# noted at https://cran.r-project.org/web/packages/lme4/lme4.pdf
library(RcppEigen)
library(lme4) # Mixed-effects models
library(lmerTest) # Compute p values
library(MuMIn) # R^2
library(allFit) # Convergence diagnostics

# Source custom diagnostic functions
source("https://raw.githubusercontent.com/pablobernabeu/language-sensorimotor-simulation-PhD-thesis/main/R_functions/residuals_plot.R")
source("https://raw.githubusercontent.com/pablobernabeu/plot.fixef.allFit/main/plot.fixef.allFit.R")

# Terminological note: "brain regions" refer to nine standard sets of electrodes that were
# used in González Alonso et al. (2020; https://doi.org/10.1016/j.jneuroling.2020.100939).
# Time windows (200-500ms, 300-600ms, 400-900ms) are based on previous ERP research and
# overlap intentionally, serving as planned comparisons across different temporal and
# spatial scales.

# Load function
source("data/R_functions/merge_trialbytrial_EEG_data.R")

# Import data for VONA (verb-object number agreement, marker S3) across all time windows,
# brain regions, and both mini-language groups
EEG_VONA_data <-
    merge_trialbytrial_EEG_data(
        EEG_file_pattern = "^\\d*_trialbytrial_S3_S10[12]\\.",
        min_time = 200, max_time = 898, # 898 = time point up to 900 ms
        include_baseline = TRUE,
        aggregate_electrodes = TRUE,
        aggregate_time_points = TRUE
    )

# Compute baseline predictor following Alday (2019; http://doi.org/10.1111/psyp.13451).
# Tutorial: https://mne.tools/stable/auto_tutorials/epochs/15_baseline_regression.html.
# The predictor is computed per participant, per brain region and per trial.
# It is then z-scored per participant.

baseline_predictor <-
    EEG_VONA_data %>%
    filter(time_window == "baseline") %>%
    group_by(participant_lab_ID, brain_region, sentence_marker) %>%
    summarise(
        baseline_predictor = mean(amplitude, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    group_by(participant_lab_ID) %>%
    mutate(z_baseline_predictor = scale(baseline_predictor))

# Import baseline predictor to main data set and filter to analysis time windows
EEG_VONA_data <- EEG_VONA_data %>%
    left_join(baseline_predictor, by = c("participant_lab_ID", "brain_region", "sentence_marker")) %>%
    # Filter to the three planned time windows for analysis
    filter(time_window %in% c("200-500", "300-600", "400-900")) %>%
    # Set factor levels for proper ordering
    mutate(
        time_window = factor(time_window, levels = c("200-500", "300-600", "400-900")),
        brain_region = factor(
            brain_region,
            levels = c(
                "left anterior", "midline anterior", "right anterior",
                "left medial", "midline medial", "right medial",
                "left posterior", "midline posterior", "right posterior"
            )
        )
    ) %>%
    # Select variables for analysis
    select(contains(c(
        "amplitude", "baseline", "mini_language", "grammaticality",
        "days_from_session2", "multilingual", "brain_region", "time_window",
        "participant_lab_ID", "sentence_marker"
    )))

# Save data to disk
saveRDS(EEG_VONA_data, "data/merged data/EEG_VONA_consolidated_data.rds")

# MODEL
# Measure running time
system.time({
    EEG_VONA_lmerTest <- lmerTest::lmer(

        # Dependent variable
        z_amplitude ~

            # Following Alday (2019; http://doi.org/10.1111/psyp.13451)
            z_baseline_predictor +

            # Independent variables
            recoded_mini_language +
            recoded_grammaticality +
            z_days_from_session2 +
            time_window +
            brain_region +
            z_session1_digit_span +
            z_session1_Stroop +
            z_session1_ASRT +
            z_multilingual_language_diversity +

            # Key interactions
            recoded_mini_language:recoded_grammaticality +
            recoded_grammaticality:z_days_from_session2 +
            recoded_grammaticality:time_window +
            recoded_grammaticality:brain_region +
            recoded_grammaticality:z_session1_digit_span +
            recoded_grammaticality:z_session1_Stroop +
            recoded_grammaticality:z_session1_ASRT +
            recoded_grammaticality:z_multilingual_language_diversity +

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
            (0 + recoded_grammaticality || participant_lab_ID) +
            (0 + z_days_from_session2 || participant_lab_ID) +
            (0 + time_window || participant_lab_ID) +
            (0 + brain_region || participant_lab_ID) +

            # By-sentence random slopes for between-participants variables
            (0 + z_session1_digit_span || sentence_marker) +
            (0 + z_session1_Stroop || sentence_marker) +
            (0 + z_session1_ASRT || sentence_marker) +
            (0 + z_multilingual_language_diversity || sentence_marker) +
            (0 + recoded_mini_language || sentence_marker),
        data = EEG_VONA_data,

        # Set maximum number of iterations to 1m to facilitate convergence
        # (Brauer & Curtin, 2018; Singmann & Kellen, 2019)
        control = lmerControl(optCtrl = list(maxfun = 1e6))
    )
})

saveRDS(EEG_VONA_lmerTest, "analyses/EEG/test_revised_analysis/results/EEG_VONA_lmerTest.rds")

# Calculate p values using Satterthwaite method (Luke, 2017;
# https://doi.org/10.3758/s13428-016-0809-y)
summary(EEG_VONA_lmerTest, ddf = "Satterthwaite") %>%
    saveRDS("analyses/EEG/test_revised_analysis/results/Satterthwaite_summary_EEG_VONA_lmerTest.rds")

# Calculate R^2. The result must be interpreted with caution as it differs from the
# traditional R^2 (Nakagawa et al., 2017; https://doi.org/10.1098/rsif.2017.0213)
MuMIn::r.squaredGLMM(EEG_VONA_lmerTest) %>%
    saveRDS("analyses/EEG/test_revised_analysis/results/Nakagawa2017_R2_EEG_VONA_lmerTest.rds")

# Calculate 95% confidence intervals of fixed effects
lme4::confint.merMod(
    EEG_VONA_lmerTest,
    method = "Wald",
    # Compute 95% CIs for every effect, as well as for the intercept
    parm = rownames(summary(EEG_VONA_lmerTest)$coefficients)
) %>%
    saveRDS("analyses/EEG/test_revised_analysis/results/confint_EEG_VONA_lmerTest.rds")

# Save random effects
lme4::ranef(EEG_VONA_lmerTest) %>%
    saveRDS("analyses/EEG/test_revised_analysis/results/ranef_EEG_VONA_lmerTest.rds")

# ============================================================================
# POST-HOC CONTRASTS
# ============================================================================
# Note: Holm-Bonferroni correction is applied only where multiple comparisons occur.
# Single comparisons (contrasts 1 and 2) do not require adjustment.

# 1. Grammaticality effects across mini-language groups
# Single comparison per group (grammatical vs ungrammatical), no adjustment needed
emm_grammaticality_by_language <- emmeans(
    EEG_VONA_lmerTest,
    pairwise ~ recoded_grammaticality | recoded_mini_language
)
summary(emm_grammaticality_by_language) %>%
    saveRDS("analyses/EEG/test_revised_analysis/results/emmeans_VONA_grammaticality_by_language.rds")

# 2. Grammaticality effects over time (linear trends in days_from_session2)
# Tests whether the slope of time differs between grammaticality conditions.
# Single comparison (grammatical slope vs ungrammatical slope), no adjustment needed.
emm_grammaticality_by_time <- emtrends(
    EEG_VONA_lmerTest,
    pairwise ~ recoded_grammaticality,
    var = "z_days_from_session2"
)
summary(emm_grammaticality_by_time) %>%
    saveRDS("analyses/EEG/test_revised_analysis/results/emmeans_VONA_grammaticality_by_time_trend.rds")

# 3. Grammaticality effects across time windows
# Multiple comparisons across 3 time windows; Holm-Bonferroni correction applied
emm_grammaticality_by_timewindow <- emmeans(
    EEG_VONA_lmerTest,
    pairwise ~ recoded_grammaticality | time_window,
    adjust = "holm"
)
summary(emm_grammaticality_by_timewindow) %>%
    saveRDS("analyses/EEG/test_revised_analysis/results/emmeans_VONA_grammaticality_by_timewindow.rds")

# 4. Grammaticality effects across brain regions
# Multiple comparisons across 9 brain regions; Holm-Bonferroni correction applied
emm_grammaticality_by_region <- emmeans(
    EEG_VONA_lmerTest,
    pairwise ~ recoded_grammaticality | brain_region,
    adjust = "holm"
)
summary(emm_grammaticality_by_region) %>%
    saveRDS("analyses/EEG/test_revised_analysis/results/emmeans_VONA_grammaticality_by_region.rds")

# ============================================================================
# MODEL DIAGNOSTICS
# ============================================================================

# 1. Residuals plot
png("analyses/EEG/test_revised_analysis/results/diagnostics_VONA_residuals.png",
    width = 800, height = 600
)
residuals_plot(EEG_VONA_lmerTest)
dev.off()

# 2. Convergence diagnostics using allFit
# Test model convergence using multiple optimizers
EEG_VONA_allFit <- allFit(EEG_VONA_lmerTest)
saveRDS(EEG_VONA_allFit, "analyses/EEG/test_revised_analysis/results/allFit_EEG_VONA_lmerTest.rds")

# Plot fixed effects estimates across optimizers
png("analyses/EEG/test_revised_analysis/results/diagnostics_VONA_convergence.png",
    width = 1200, height = 800
)
plot.fixef.allFit(EEG_VONA_allFit)
dev.off()

# Summary of convergence
summary(EEG_VONA_allFit) %>%
    saveRDS("analyses/EEG/test_revised_analysis/results/allFit_summary_EEG_VONA_lmerTest.rds")
