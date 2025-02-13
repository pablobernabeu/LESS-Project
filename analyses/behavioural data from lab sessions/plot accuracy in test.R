library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(introdataviz) 
library(patchwork)
library(ggtext)

# the ggsave command is disabled and found at line 90


#Plotting for behavioural data during training 

source("analyses/behavioural data from lab sessions/plot accuracy for 
       two grammaticality conditions in experiment.R")

# gender agreement

# Create a new data frame with only "gender agreement" in grammatical_property
test_df <- clean_combined_df %>%
  filter(session_part == "Test")

test_df$correct <- as.numeric(as.character(test_df$correct))
test_df$mini_language <- as.factor(test_df$mini_language)
test_df$Session <- as.factor(test_df$Session)
test_df$grammaticality <- as.factor(test_df$grammaticality)

# Add accuracy as a new column in test_df
test_df <- test_df %>%
  group_by(subject_id, grammaticality) %>%
  mutate(accuracy = mean(correct)) %>%
  ungroup()

# Create the facet_group variable in the dataset
test_df$facet_group <- test_df$mini_language  # Use mini_language for faceting

# Ensure the 'facet_group' factor is ordered with "Mini-Norwegian" first
test_df$facet_group <- factor(
  test_df$facet_group,
  levels = c("Mini-Norwegian", 
             setdiff(unique(test_df$facet_group), "Mini-Norwegian"))
)

# Ensure 'mini_language' is a factor with the correct order
test_df$mini_language <- factor(test_df$mini_language, 
                                levels = c("Mini-Norwegian", "Mini-English"))

# Now plot the boxplot with both mini_language groups in the same plot
test_accuracy_plot <- ggplot(test_df,
                             aes(x = Session, y = accuracy * 100, 
                                 fill = mini_language, 
                                 color = grammaticality)) +  
  geom_boxplot(width = .3, alpha = 0.7, fatten = NULL, 
               outlier.shape = NA, position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = FALSE, 
               position = position_dodge(width = 0.5), colour = "white") +
  scale_x_discrete(
    name = "", 
    labels = c("Gender Agreement", "Differential Object Marking", "Verb-Object Agreement"),
    expand = expansion(mult = c(0.1, 0.1))  # Adjust space on both sides of the axis
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
  
  # Customize the plot with your specified theme
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    legend.position = 'top',  
    legend.justification = 'center',  
    legend.title = element_blank(),  
    legend.text = element_text(size = 14, 
                               margin = margin(r = 10, l = 3, unit = 'pt')),  
    legend.key.width = unit(1.2, 'cm'),  
    legend.key.height = unit(0.5, 'cm'),  
    plot.title = element_text(size = 16, 
                              hjust = 0.5,  
                              margin = margin(t = 12, b = 1, unit = 'pt')),  
    panel.border = element_blank(),  
    strip.background = element_rect(fill = 'gray90', colour = 'gray70', linewidth = 0.5),
    strip.text = element_text(size = 14, face = 'bold'),
    panel.spacing = unit(05., 'cm'), 
    legend.key = element_rect(fill = "gray90", color = NA),  
    legend.box.spacing = unit(.2, "cm"),  
    legend.margin = margin(20, 5, 0.001, 5),
    plot.margin = margin(t = 1, b = 1, l = 10, r = 10)  
  )

print(test_accuracy_plot)



#ggsave("test_accuracy_plot.png", plot = test_accuracy_plot, width = 7, height = 10, dpi = 300)


