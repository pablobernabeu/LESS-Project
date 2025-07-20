# Plot accuracy for grammatical and ungrammatical conditions in the experiment

library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
# install.packages("devtools")
# devtools::install_github("psyteachr/introdataviz")
library(introdataviz)
library(patchwork)
library(ggtext)

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

# Create split violin plot function for two conditions
create_split_violin_plot <- function(data, title, session_labels) {
  ggplot(
    data %>%
      filter(grammaticality %in% c("Grammatical", "Ungrammatical")) %>%
      mutate(
        facet_group = factor(
          mini_language,
          levels = c("Mini-Norwegian", 
                    setdiff(unique(mini_language), "Mini-Norwegian"))
        )
      ),
    aes(x = session, y = accuracy * 100, color = grammaticality)
  ) +
    introdataviz::geom_split_violin(alpha = 0.4, trim = TRUE,
                                   position = position_dodge(width = 0.1)) +
    geom_boxplot(width = .1, alpha = 1, fatten = NULL, show.legend = FALSE, 
                 outlier.shape = NA) +
    stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = FALSE,
                 position = position_dodge(width = 0.1), colour = "white") +
    scale_x_discrete(
      name = "",
      labels = session_labels,
      expand = expansion(mult = c(0.1, 0.1))
    ) +
    scale_y_continuous(
      name = "Accuracy",
      breaks = seq(0, 100, by = 20),
      limits = c(0, 100),
      labels = function(x) paste0(x, "%")
    ) +
    scale_color_manual(values = grammaticality_colours) +
    ggtitle(title) +
    create_experiment_theme() +
    facet_wrap(~facet_group, ncol = 1)
}

# Gender agreement plot
gender_agreement_plot <- behavioural_lab_data %>%
  filter(
    grammatical_property == "gender agreement",
    session_part == "Experiment"
  ) %>%
  create_split_violin_plot(
    title = "Accuracy on gender agreement in the experiment",
    session_labels = c("Session 2", "Session 3", "Session 4", "Session 6")
  )

print(gender_agreement_plot)

# Differential object marking plot
DOM_experiment_plot <- behavioural_lab_data %>%
  filter(
    grammatical_property == "differential object marking",
    session_part == "Experiment"
  ) %>%
  create_split_violin_plot(
    title = "Accuracy on differential object marking in the experiment",
    session_labels = c("Session 3", "Session 4", "Session 6")
  )

print(DOM_experiment_plot)

# Verb object agreement plot
VOA_plot <- behavioural_lab_data %>%
  filter(
    grammatical_property == "verb object agreement",
    session_part == "Experiment"
  ) %>%
  create_split_violin_plot(
    title = "Accuracy on verb object agreement in the experiment",
    session_labels = c("Session 3", "Session 4", "Session 6")
  )

print(VOA_plot)

# Optional: Save plots
# ggsave("gender_agreement_experiment_plot.png", plot = gender_agreement_plot, 
#        width = 7, height = 10, dpi = 300)
# ggsave("DOM_experiment_plot.png", plot = DOM_experiment_plot, 
#        width = 7, height = 10, dpi = 300)
# ggsave("VOA_plot.png", plot = VOA_plot, 
#        width = 7, height = 10, dpi = 300)
