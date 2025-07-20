# Plot reaction time across conditions and sessions in the 
# behavioural data from the lab sessions.

library(dplyr)
library(ggplot2)
library(scales)
# install.packages("devtools")
# devtools::install_github("psyteachr/introdataviz")
library(introdataviz)

source("data/importation and preprocessing/import and preprocess behavioural data from lab sessions.R")

# Create common theme function (reuse from experiment script)
create_experiment_theme <- function() {
  theme_minimal() +
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(size = 16),
      legend.position = "top",
      legend.justification = "center",
      legend.title = element_blank(),
      legend.text = element_text(
        size = 14,
        margin = margin(r = 10, l = 3, unit = "pt")
      ),
      legend.key.width = unit(1.2, "cm"),
      legend.key.height = unit(0.5, "cm"),
      plot.title = element_text(
        size = 16,
        hjust = 0.5,
        margin = margin(t = 12, b = 1, unit = "pt")
      ),
      panel.border = element_blank(),
      strip.background = element_rect(
        fill = "gray90",
        colour = "gray70",
        linewidth = 0.5
      ),
      strip.text = element_text(size = 14, face = "bold"),
      panel.spacing = unit(0.5, "cm"),
      panel.spacing.x = unit(1111, "cm"),
      legend.key = element_rect(fill = "gray90", color = NA),
      legend.box.spacing = unit(1, "cm"),
      legend.margin = margin(20, 5, 0.001, 5),
      plot.margin = margin(t = 1, b = 1, l = 10, r = 10)
    )
}

# Create common RT boxplot function
create_rt_boxplot <- function(data, title, session_labels, y_breaks = seq(0, 2700, by = 300)) {
  ggplot(
    data %>%
      mutate(
        grammaticality = factor(grammaticality),
        facet_group = factor(
          mini_language,
          levels = c("Mini-Norwegian", 
                    setdiff(unique(mini_language), "Mini-Norwegian"))
        )
      ),
    aes(x = session, y = response_time, fill = grammaticality, color = grammaticality)
  ) +
    geom_boxplot(
      width = .6, alpha = 1, fatten = NULL, show.legend = TRUE,
      outlier.shape = NA
    ) +
    stat_summary(
      fun.data = "mean_se", geom = "pointrange", show.legend = FALSE,
      position = position_dodge(width = 0.6), colour = "white"
    ) +
    scale_x_discrete(
      name = "",
      labels = session_labels,
      expand = expansion(mult = c(0.1, 0.1))
    ) +
    scale_y_continuous(
      name = "Reaction time (ms)",
      limits = c(200, 4000),
      breaks = y_breaks
    ) +
    scale_color_manual(values = grammaticality_colours) +
    scale_fill_manual(values = grammaticality_colours) +
    ggtitle(title) +
    create_experiment_theme() +
    facet_wrap(~facet_group, ncol = 1)
}

# Gender agreement RT plot
gender_agreement_RT <- behavioural_lab_data %>%
  filter(
    grammatical_property == "gender agreement",
    session_part == "Experiment",
    grammaticality %in% c("Grammatical", "Ungrammatical", "Number\nviolation")
  ) %>%
  mutate(grammaticality = factor(grammaticality, 
                                levels = c("Grammatical", "Ungrammatical", "Number\nviolation"))) %>%
  create_rt_boxplot(
    title = "RTs on gender agreement in the experiment",
    session_labels = c("Session 2", "Session 3", "Session 4", "Session 6")
  )

print(gender_agreement_RT)

# Differential object marking RT plot
DOM_RT <- behavioural_lab_data %>%
  filter(
    grammatical_property == "differential object marking",
    session_part == "Experiment",
    grammaticality %in% c("Grammatical", "Ungrammatical", "Article\nmisplacement")
  ) %>%
  mutate(grammaticality = factor(grammaticality,
    levels = c("Grammatical", "Ungrammatical", "Article\nmisplacement")
  )) %>%
  create_rt_boxplot(
    title = "RTs on differential object marking in the experiment",
    session_labels = c("Session 3", "Session 4", "Session 6")
  )

print(DOM_RT)

# Verb object agreement RT plot
VOA_RT <- behavioural_lab_data %>%
  filter(
    grammatical_property == "verb object agreement",
    session_part == "Experiment", 
    grammaticality %in% c("Grammatical", "Ungrammatical", "Article\nmisplacement")
  ) %>%
  mutate(grammaticality = factor(grammaticality,
    levels = c("Grammatical", "Ungrammatical", "Article\nmisplacement")
  )) %>%
  create_rt_boxplot(
    title = "RTs on verb object agreement in the experiment",
    session_labels = c("Session 3", "Session 4", "Session 6")
  )

print(VOA_RT)

# Optional raincloud plot for gender agreement
rain_height <- .1

gender_agreement_raincloud <- behavioural_lab_data %>%
  filter(
    grammatical_property == "gender agreement",
    session_part == "Experiment",
    grammaticality %in% c("Grammatical", "Ungrammatical", "Number\nviolation")
  ) %>%
  ggplot(aes(x = response_time, y = session, fill = mini_language)) +
  # clouds
  introdataviz::geom_flat_violin(trim = FALSE, alpha = 0.4,
                                position = position_nudge(x = rain_height + .05)) +
  # rain
  geom_point(aes(colour = grammaticality), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  scale_color_manual(values = grammaticality_colours) +
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               position = position_nudge(x = -rain_height * 2)) +
  # mean and SE point in the cloud
  stat_summary(fun.data = mean_cl_normal, mapping = aes(color = grammaticality), show.legend = FALSE,
               position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height * 3, 0, 0, 0.7)) +
  scale_y_continuous(name = "Reaction time (ms)",
                     breaks = seq(200, 800, 100), 
                     limits = c(200, 800)) +
  coord_flip() +
  facet_wrap(~factor(mini_language, 
                     levels = c("Mini-Norwegian", "Mini-English"), 
                     labels = c("Mini-Norwegian", "Mini-English")), 
             nrow = 2) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Language group") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.8, 0.8),
        legend.background = element_rect(fill = "white", color = "white"))

# print(gender_agreement_raincloud)

# Optional: Save plots
# ggsave("gender_agreement_RT.png", plot = gender_agreement_RT,
#        width = 7, height = 10, dpi = 300)
# ggsave("DOM_RT.png", plot = DOM_RT,
#        width = 7, height = 10, dpi = 300)
# ggsave("VOA_RT.png", plot = VOA_RT,
#        width = 7, height = 10, dpi = 300)
