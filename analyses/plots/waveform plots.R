

library(dplyr)
library(ggplot2)


# Rename factor levels and remove control violation condition 

merged_data = merged_data %>%
  filter(!grammaticality == 'Control violation')


# Define standard error function
std <- function(x) sd(x) / sqrt(length(x))

# Initialize an empty list to store all data
all_data <- list()

# Ensure 'Mini-Norwegian' appears in the upper facet by reordering levels
merged_data$language <- 
  factor(merged_data$language, 
         levels = c('Mini-Norwegian', 
                    setdiff(unique(na.omit(merged_data$language)), 
                            'Mini-Norwegian')))

for (session in unique(na.omit(merged_data$session))) {
  for (grammatical_property in unique(na.omit(merged_data$grammatical_property))) {
    for (region in unique(na.omit(merged_data$region))) {
      
      # Filter data for the current combination of factors
      df2 = aggregate(
        amplitude ~ grammaticality * time * region * language,
        merged_data[merged_data$session == session &
                      merged_data$grammatical_property == grammatical_property &
                      merged_data$region == region,], 
        mean)
      
      df3 = merged_data[merged_data$session == session &
                          merged_data$grammatical_property == grammatical_property &
                          merged_data$region == region,]
      
      # Compute SD, SE, Confidence Intervals
      SD = rep(NA, length(df2$time))       # vector SD per time
      SE = rep(NA, length(df2$time))       # vector SE per time
      CIupper = rep(NA, length(df2$time))  # vector upper 95% conf int
      CIlower = rep(NA, length(df2$time))  # vector lower 95% conf int
      
      for (i in 1:length(df2$time)) {
        something = subset(df3, time == df2$time[i] & 
                             language == df2$language[i], 
                           select = amplitude)
        SE[i] = std(something$amplitude)
        CIlower[i] = df2$amplitude[i] - (SE[i] * 1.96)
        CIupper[i] = df2$amplitude[i] + (SE[i] * 1.96)
      }
      
      df2$CIL = CIlower
      df2$CIU = CIupper
      
      # Create a valid R object name by sanitizing the factors.
      # Replace non-alphanumeric characters with '_'.
      plot_name = paste('Session', session, grammatical_property, region, sep = '_')
      plot_name = gsub('[^[:alnum:]_]', '_', plot_name)  
      
      # Create the plot and save it as a named object
      assign(plot_name, 
             
             ggplot(df2, aes(x = time, y = amplitude, color = grammaticality)) +
               
               geom_ribbon(aes(ymin = CIL, ymax = CIU, fill = grammaticality), 
                           alpha = 0.15, colour = NA) +
               geom_line(linewidth = 0.5, alpha = .9) +
               
               scale_x_continuous(expand = c(0, 0), 
                                  breaks = pretty(df2$time, n = 5)) + 
               
               # Reverse Y axis
               scale_y_continuous(trans = 'reverse', expand = c(0, 0), 
                                  breaks = pretty(df2$amplitude, n = 5)) +  
               
               # Colors: reversed assignment
               scale_color_manual(values = c('Grammatical' = 'forestgreen', 
                                             'Ungrammatical' = 'firebrick1')) +
               
               scale_fill_manual(values = c('Grammatical' = 'forestgreen', 
                                            'Ungrammatical' = 'firebrick1')) +
               
               # Title and axis lines
               ggtitle(paste0('Session ', session, '; ', region, ' region')) +
               
               # Extend horizontal line at y = 0
               geom_vline(xintercept = 0, linewidth = 0.5) +
               geom_segment(x = min(df2$time), y = 0, 
                            xend = max(df2$time), yend = 0, 
                            linewidth = 0.5, color = 'black') +
               
               # Guides
               guides(color = guide_legend(
                 override.aes = list(size = 5, shape = 15)
               )) +
               
               # Title and axis labels
               labs(x = 'Time (ms)', y = 'Amplitude (μV)') +
               
               # Theme customizations
               theme_bw() +
               theme(axis.title = element_text(size = 14),
                     axis.text = element_text(size = 12),
                     legend.position = c(0.99, 0.98),
                     legend.justification = c(1, 1),
                     legend.background = element_rect(colour = 'black', 
                                                      fill = 'transparent', 
                                                      size = 0.5),
                     legend.title = element_blank(), 
                     legend.text = element_text(size = 12, face = 'bold'),
                     plot.title = element_text(size = 16, hjust = 0.5),
                     panel.border = element_blank(),
                     strip.text = element_text(size = 14, face = 'bold')) + 
               
               # Facet by language
               facet_wrap(~ language, ncol = 1)
      )
      
      ggsave(paste0(plot_name, '.png'), plot = get(plot_name), 
             path = 'analyses/plots/')
    }
  }
}

