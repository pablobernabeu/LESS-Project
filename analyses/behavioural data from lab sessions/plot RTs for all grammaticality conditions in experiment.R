

# Plot reaction time across conditions and sessions in the 
# behavioural data from the lab sessions.

source("analyses/behavioural data from lab sessions/plot accuracy in test.R")

library(dplyr) 


# for gender
Gender_agreement_RT_df <- Gender_agreement_df %>%
  filter(response_time > 200 & response_time < 4000) %>%
  mutate(response_time = as.numeric(as.character(response_time)))  # Ensure numeric


Gender_agreement_RT_df$facet_group <- Gender_agreement_RT_df$mini_language

Gender_agreement_RT_df$facet_group <- factor(
  Gender_agreement_RT_df$facet_group,
  levels = c("Mini-Norwegian", 
             setdiff(unique(Gender_agreement_RT_df$facet_group), "Mini-Norwegian"))
)

Gender_agreement_RT <- ggplot(Gender_agreement_RT_df  %>%
                                     mutate(grammaticality = factor(grammaticality, 
                                                                    levels = c("Grammatical", "Ungrammatical", "Number violation"))),
                                   aes(x = Session, y = response_time, 
                                       fill = grammaticality, 
                                       color = grammaticality)) +  
  geom_boxplot(width = .6, alpha = 1, fatten = NULL, show.legend = TRUE, 
               outlier.shape = NA) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = FALSE, 
               position = position_dodge(width = 0.6), colour = "white") +
  scale_x_discrete(
    name = "", 
    labels = c("Session 2", "Session 3", "Session 4", "Session 6"),
    expand = expansion(mult = c(0.1, 0.1))  # Adjust space on both sides of the axis
  ) +
    scale_y_continuous(
      name = "Reaction time (ms)", 
      limits = c(200, 4000),
      breaks = seq(0, 2700, by = 300)  # Generates breaks at every 500ms
  ) +  
  scale_color_manual(values = grammaticality_colours) +
  scale_fill_manual(values = grammaticality_colours) +
  ggtitle("RTs on gender agreement in the experiment") +  
  
  # Customize the plot with your specified theme
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    legend.position = 'top',  # Move legend to the top
    legend.justification = 'center',  # Center legend
    legend.title = element_blank(),  
    legend.text = element_text(
      size = 14, 
      margin = margin(r = 10, l = 3, unit = 'pt')  
    ),
    legend.key.width = unit(1.2, 'cm'),  # Adjust width of legend keys
    legend.key.height = unit(0.5, 'cm'),  # Adjust height of legend keys
    plot.title = element_text(
      size = 16, 
      hjust = 0.5,  
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


print(Gender_agreement_RT)


ggplot(Gender_agreement_RT_df, aes(x = Session, y= response_time)) +
  geom_violin() +
  geom_boxplot(width = .2, fatten = NULL) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = .1) +
  facet_wrap(~mini_language)+
  theme_minimal()
#######

rain_height <- .1

ggplot(Gender_agreement_RT_df, aes(x = response_time, y = Session, fill = mini_language)) +
  # clouds
  introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                 position = position_nudge(x = rain_height+.05)) +
  # rain
  geom_point(aes(colour = grammaticality), size = 2, alpha = .5, show.legend = FALSE, 
             position = position_jitter(width = rain_height, height = 0)) +
  scale_color_manual(values = grammaticality_colours) + # Ensures correct color mapping
  geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE, 
               outlier.shape = NA,
               position = position_nudge(x = -rain_height*2)) +
  # mean and SE point in the cloud
  stat_summary(fun.data = mean_cl_normal, mapping = aes(color = grammaticality_colours), show.legend = FALSE,
               position = position_nudge(x = rain_height * 3)) +
  # adjust layout
  scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
  scale_y_continuous(name = "Reaction time (ms)",
                     breaks = seq(200, 800, 100), 
                     limits = c(200, 800)) +
  coord_flip() +
  facet_wrap(~factor(mini_language, 
                     levels = c("Mini_Norwegian", "Mini_English"), 
                     labels = c("Mini_Norwegian", "Mini_English")), 
             nrow = 2) +
  # custom colours and theme
  scale_fill_brewer(palette = "Dark2", name = "Language group") +
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        legend.position.inside = c(0.8, 0.8),
        legend.background = element_rect(fill = "white", color = "white"))
