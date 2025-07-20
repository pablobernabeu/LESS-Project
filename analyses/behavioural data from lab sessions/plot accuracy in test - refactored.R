# Plot results from the tests

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
      legend.key = element_rect(fill = "gray90", color = NA),
      legend.box.spacing = unit(0.2, "cm"),
      legend.margin = margin(20, 5, 0.001, 5),
      plot.margin = margin(t = 1, b = 1, l = 10, r = 10)
    )
}

# Test accuracy plot
test_accuracy_plot <- behavioural_lab_data %>%
  filter(session_part == "Test") %>%
  mutate(
    mini_language = factor(mini_language, 
                          levels = c("Mini-Norwegian", "Mini-English"))
  ) %>%
  ggplot(aes(x = session, y = accuracy * 100, 
             fill = mini_language, 
             color = grammaticality)) +  
  geom_boxplot(width = .3, alpha = 0.7, fatten = NULL, 
               outlier.shape = NA, position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = FALSE, 
               position = position_dodge(width = 0.5), colour = "white") +
  scale_x_discrete(
    name = "", 
    labels = c("Gender Agreement", "Differential Object Marking", "Verb-Object Agreement"),
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  scale_y_continuous(
    name = "Accuracy", 
    breaks = seq(0, 100, by = 20), 
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +  
  scale_color_manual(values = grammaticality_colours) +
  scale_fill_manual(values = c("Mini-Norwegian" = "#1f78b4",  # Blue
                               "Mini-English" = "#e31a1c")) + # Red
  ggtitle("Accuracy across properties during the Test") +  
  create_experiment_theme()

print(test_accuracy_plot)

# Optional: Save plot
# ggsave("test_accuracy_plot.png", plot = test_accuracy_plot, 
#        width = 7, height = 10, dpi = 300)
