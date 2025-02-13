library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(introdataviz) 
library(patchwork)
library(ggtext)

# the ggsave commands are disabled and found at lines 97, 179, 261
# Creating boxplots for behavioural data during the experiment


source("analyses/behavioural data from lab sessions/plot accuracy for two grammaticality conditions in experiment.R")


# Gender agreement

Gender_agreement_boxplot_df_violin <- Gender_agreement_df %>%
  filter(grammaticality %in% c("Grammatical", "Ungrammatical", "Number\nviolation"))


# Create the facet_group variable in the dataset
Gender_agreement_boxplot_df_violin$facet_group <- Gender_agreement_boxplot_df_violin$mini_language  # Use mini_language for faceting

# Ensure the 'facet_group' factor is ordered with "Mini-Norwegian" first
Gender_agreement_boxplot_df_violin$facet_group <- factor(
  Gender_agreement_boxplot_df_violin$facet_group,
  levels = c("Mini-Norwegian", 
             setdiff(unique(Gender_agreement_boxplot_df_violin$facet_group), "Mini-Norwegian"))
)

# Now plot the violin plot, faceting by `facet_group` (mini_language)
Gender_agreement_boxplot <- ggplot(Gender_agreement_boxplot_df_violin %>%
                                     mutate(grammaticality = factor(grammaticality, 
                                                                    levels = c("Grammatical", "Ungrammatical", "Number\nviolation"))),
                                   aes(x = Session, y = accuracy * 100, 
                                       fill = grammaticality, 
                                       color = grammaticality)) +  
  geom_boxplot(width = .6, alpha = 1, fatten = NULL, show.legend = TRUE, 
               outlier.shape = NA) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = FALSE, 
               position = position_dodge(width = 0.6), colour = "white") +
  scale_x_discrete(
    name = "", 
    labels = c("Session 2", "Session 3", "Session 4", "Session 6"),
    expand = expansion(mult = c(0.1, 0.1))  # Adjust space on both sides of the axis (mult)
  ) +
  scale_y_continuous(
    name = "Accuracy", 
    breaks = seq(0, 100, by = 20), 
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +  
  scale_color_manual(values = grammaticality_colours) +
  scale_fill_manual(values = grammaticality_colours) +
  ggtitle("Accuracy on gender agreement in the experiment") +  # Updated title
  
  # Customize the plot with your specified theme
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    legend.position = 'top',  # Move legend to the top
    legend.justification = 'center',  # Center legend
    legend.title = element_blank(),  # No legend title
    legend.text = element_text(
      size = 14, 
      margin = margin(r = 10, l = 3, unit = 'pt')  # Spacing around legend text
    ),
    legend.key.width = unit(1.2, 'cm'),  # Adjust width of legend keys
    legend.key.height = unit(0.5, 'cm'),  # Adjust height of legend keys
    plot.title = element_text(
      size = 16, 
      hjust = 0.5,  # Center the title
      margin = margin(t = 12, b = 1, unit = 'pt')  # Add margin to title
    ),
    panel.border = element_blank(),  # Remove panel border
    strip.background = element_rect(
      fill = 'gray90', 
      colour = 'gray70', 
      linewidth = 0.5  # Facet background color and border
    ),
    strip.text = element_text(size = 14, face = 'bold'),  # Facet label style
    panel.spacing = unit(0.5, 'cm'), 
    panel.spacing.x = unit(1111, 'cm'),# Add spacing between panels
    legend.key = element_rect(fill = "gray90", color = NA),  # Background for legend keys
    legend.box.spacing = unit(1, "cm"),  # Space between legend items
    legend.margin = margin(20, 5, 0.001, 5),
    plot.margin = margin(t = 1, b = 1, l = 10, r = 10) # Reduced margin around the legend (adjust to your preference)
  ) +
  facet_wrap(~facet_group, ncol = 1)  # Facet by mini_language without sample size


print(Gender_agreement_boxplot)

#ggsave("Gender_agreement_boxplot.png", plot = Gender_agreement_boxplot, width = 7, height = 10, dpi = 300)


################
## differential object marking
DOM_experiment_boxplot_df <- Differential_object_marking_df %>%
  filter(grammaticality %in% c("Grammatical", "Ungrammatical", "Article\nmisplacement"))

# Create the facet_group variable in the dataset
DOM_experiment_boxplot_df$facet_group <- DOM_experiment_boxplot_df$mini_language  # Use mini_language for faceting

# Ensure the 'facet_group' factor is ordered with "Mini-Norwegian" first
DOM_experiment_boxplot_df$facet_group <- factor(
  DOM_experiment_boxplot_df$facet_group,
  levels = c("Mini-Norwegian", 
             setdiff(unique(DOM_experiment_boxplot_df$facet_group), "Mini-Norwegian"))
)


DOM_experiment_boxplot <- ggplot(DOM_experiment_boxplot_df %>%
                                   mutate(grammaticality = factor(grammaticality, 
                                                                  levels = c("Grammatical", "Ungrammatical", "Article\nmisplacement"))),
                                 aes(x = Session, y = accuracy * 100, 
                                     fill = grammaticality, 
                                     color = grammaticality)) +  
  geom_boxplot(width = .6, alpha = 1, fatten = NULL, show.legend = TRUE, 
               outlier.shape = NA) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = FALSE, 
               position = position_dodge(width = 0.6), colour = "white") +
  scale_x_discrete(
    name = "", 
    labels = c("Session 3", "Session 4", "Session 6"),
    expand = expansion(mult = c(0.1, 0.1))  # Adjust space on both sides of the axis (mult)
  ) +
  scale_y_continuous(
    name = "Accuracy", 
    breaks = seq(0, 100, by = 20), 
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +  
  scale_color_manual(values = grammaticality_colours) +
  scale_fill_manual(values = grammaticality_colours) +
  ggtitle("Accuracy on differential object marking in the experiment") +  # Updated title
  
  # Customize the plot with your specified theme
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    legend.position = 'top',  # Move legend to the top
    legend.justification = 'center',  # Center legend
    legend.title = element_blank(),  # No legend title
    legend.text = element_text(
      size = 14, 
      margin = margin(r = 10, l = 3, unit = 'pt')  # Spacing around legend text
    ),
    legend.key.width = unit(1.2, 'cm'),  # Adjust width of legend keys
    legend.key.height = unit(0.5, 'cm'),  # Adjust height of legend keys
    plot.title = element_text(
      size = 16, 
      hjust = 0.5,  # Center the title
      margin = margin(t = 12, b = 1, unit = 'pt')  # Add margin to title
    ),
    panel.border = element_blank(),  # Remove panel border
    strip.background = element_rect(
      fill = 'gray90', 
      colour = 'gray70', 
      linewidth = 0.5  # Facet background color and border
    ),
    strip.text = element_text(size = 14, face = 'bold'),  # Facet label style
    panel.spacing = unit(0.5, 'cm'), 
    panel.spacing.x = unit(1111, 'cm'),# Add spacing between panels
    legend.key = element_rect(fill = "gray90", color = NA),  # Background for legend keys
    legend.box.spacing = unit(1, "cm"),  # Space between legend items
    legend.margin = margin(20, 5, 0.001, 5),
    plot.margin = margin(t = 1, b = 1, l = 10, r = 10) # Reduced margin around the legend (adjust to your preference)
  ) +
  facet_wrap(~facet_group, ncol = 1)  # Facet by mini_language without sample size

print(DOM_experiment_boxplot)
#ggsave("DOM_experiment_boxplot.png", plot = DOM_experiment_boxplot, 
#       width = 7, height = 10, dpi = 300)

########################
#### for verb object agreement

Verb_object_agreement_df_boxplot <- Verb_object_agreement_df %>%
  filter(grammaticality %in% c("Grammatical", "Ungrammatical", "Article\nmisplacement"))


# Create the facet_group variable in the dataset
Verb_object_agreement_df_boxplot$facet_group <- Verb_object_agreement_df_boxplot$mini_language  # Use mini_language for faceting

# Ensure the 'facet_group' factor is ordered with "Mini-Norwegian" first
Verb_object_agreement_df_boxplot$facet_group <- factor(Verb_object_agreement_df_boxplot$facet_group,
                                                       levels = c("Mini-Norwegian", setdiff(unique(Verb_object_agreement_df_boxplot$facet_group), "Mini-Norwegian")))


Verb_object_agreement_boxplot <- ggplot(Verb_object_agreement_df_boxplot %>%
                                          mutate(grammaticality = factor(grammaticality, 
                                                                         levels = c("Grammatical", "Ungrammatical", "Article\nmisplacement"))),
                                        aes(x = Session, y = accuracy * 100, 
                                            fill = grammaticality, 
                                            color = grammaticality)) +  
  geom_boxplot(width = .6, alpha = 1, fatten = NULL, show.legend = TRUE, 
               outlier.shape = NA) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = FALSE, 
               position = position_dodge(width = 0.6), colour = "white") +
  scale_x_discrete(
    name = "", 
    labels = c("Session 3", "Session 4", "Session 6"),
    expand = expansion(mult = c(0.1, 0.1))  # Adjust space on both sides of the axis (mult)
  ) +
  scale_y_continuous(
    name = "Accuracy", 
    breaks = seq(0, 100, by = 20), 
    limits = c(0, 100),
    labels = function(x) paste0(x, "%")
  ) +  
  scale_color_manual(values = grammaticality_colours) +
  scale_fill_manual(values = grammaticality_colours) +
  ggtitle("Accuracy on verb object agreement in the experiment") +  # Updated title
  
  # Customize the plot with your specified theme
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    legend.position = 'top',  # Move legend to the top
    legend.justification = 'center',  # Center legend
    legend.title = element_blank(),  # No legend title
    legend.text = element_text(
      size = 14, 
      margin = margin(r = 10, l = 3, unit = 'pt')  # Spacing around legend text
    ),
    legend.key.width = unit(1.2, 'cm'),  # Adjust width of legend keys
    legend.key.height = unit(0.5, 'cm'),  # Adjust height of legend keys
    plot.title = element_text(
      size = 16, 
      hjust = 0.5,  # Center the title
      margin = margin(t = 12, b = 1, unit = 'pt')  # Add margin to title
    ),
    panel.border = element_blank(),  # Remove panel border
    strip.background = element_rect(
      fill = 'gray90', 
      colour = 'gray70', 
      linewidth = 0.5  # Facet background color and border
    ),
    strip.text = element_text(size = 14, face = 'bold'),  # Facet label style
    panel.spacing = unit(0.5, 'cm'), 
    panel.spacing.x = unit(1111, 'cm'),# Add spacing between panels
    legend.key = element_rect(fill = "gray90", color = NA),  # Background for legend keys
    legend.box.spacing = unit(1, "cm"),  # Space between legend items
    legend.margin = margin(20, 5, 0.001, 5),
    plot.margin = margin(t = 1, b = 1, l = 10, r = 10) # Reduced margin around the legend (adjust to your preference)
  ) +
  facet_wrap(~facet_group, ncol = 1)  # Facet by mini_language without sample size

print(Verb_object_agreement_boxplot)

#ggsave("Verb_object_agreement_boxplot.png", plot = Verb_object_agreement_boxplot, 
   #    width = 7, height = 10, dpi = 300)
