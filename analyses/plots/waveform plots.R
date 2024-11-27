

# Waveform plots

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggtext)

merged_data = read.csv('data/final data/merged_data.csv')

# Rename factor levels and remove control violation condition 
merged_data = merged_data %>%
  filter(!grammaticality == 'Control violation')

# Define standard error function
std = function(x) sd(x) / sqrt(length(x))

# Initialize an empty list to store all data
all_data = list()

# Ensure 'Mini-Norwegian' appears in the upper facet by reordering levels
merged_data$language = 
  factor(merged_data$language, 
         levels = c('Mini-Norwegian', 
                    setdiff(unique(na.omit(merged_data$language)), 
                            'Mini-Norwegian')))

for (session in unique(na.omit(merged_data$session))) {
  for (grammatical_property in unique(na.omit(merged_data$grammatical_property))) {
    for (region in unique(na.omit(merged_data$region))) {
      
      # Skip iteration if combination of factors does not exist in data set.
      # For instance, Session 2 only contains the property of gender agreement. 
      
      if( merged_data[merged_data$session == session &
                      merged_data$grammatical_property == grammatical_property &
                      merged_data$region == region,] %>% nrow == 0 ) next
      
      # Calculate the sample size for each language group
      sample_sizes = merged_data %>%
        group_by(language) %>%
        summarise(n = n_distinct(participant_home_ID))
      
      # Filter data for the current combination of factors
      df2 = aggregate(
        amplitude ~ grammaticality * time * region * language,
        merged_data[merged_data$session == session &
                      merged_data$grammatical_property == grammatical_property &
                      merged_data$region == region,], 
        mean)
      
      # Merge the sample sizes into df2
      df2 = df2 %>%
        left_join(sample_sizes, by = 'language') %>%
        mutate(language_with_N = paste0(language, ' (*n* = ', n, ')'))
      
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
      
      plot_name = paste0(grammatical_property, '_Session', session, 
                         '_', str_to_sentence(region), ' region')
      
      plot_name = gsub('[^[:alnum:]_]', '_', plot_name)
      
      plot_title = paste0(str_to_sentence(grammatical_property), '; ', 
                          'Session ', session, '; ', 
                          str_to_sentence(region), ' region')
      
      group_colours = c('Grammatical' = 'forestgreen', 
                        'Ungrammatical' = 'firebrick1')
      
      # Create and export plot
      ( ggplot(df2, aes(x = time, y = amplitude, color = grammaticality)) +
          
          geom_ribbon(aes(ymin = CIL, ymax = CIU, fill = grammaticality), 
                      alpha = 0.15, colour = NA) +
          geom_line(linewidth = 0.5, alpha = .9) +
          
          scale_x_continuous(expand = c(0, 0), breaks = pretty(df2$time, n = 5)) + 
          
          # Reverse Y axis to follow standard EEG format
          scale_y_continuous(trans = 'reverse', expand = c(0, 0), 
                             breaks = pretty(df2$amplitude, n = 5)) +  
          
          scale_color_manual(values = group_colours) +
          scale_fill_manual(values = group_colours) +
          
          guides(color = guide_legend(
            override.aes = list(size = 5, shape = 15, alpha = .7)
          )) +
          
          ggtitle(plot_title) +
          
          # Lines at x = 0 and y = 0
          geom_vline(xintercept = 0, linewidth = 0.5, colour = 'grey60') +
          geom_segment(x = min(df2$time), y = 0, 
                       xend = max(df2$time), yend = 0, 
                       linewidth = 0.5, color = 'grey60') +
          
          labs(x = 'Time (ms)', y = 'Amplitude (μV)') +
          
          theme_bw() +
          theme(axis.title = element_text(size = 14),
                axis.text = element_text(size = 12),
                legend.position = 'top', legend.justification = 'center',
                legend.title = element_blank(), 
                legend.text = 
                  element_text(size = 14, margin = margin(r = 10, l = 3, unit = 'pt')),
                legend.key.width = unit(1.2, 'cm'),
                legend.key.height = unit(0.5, 'cm'),
                plot.title = element_text(size = 16, hjust = 0.5, 
                                          margin = margin(t = 4, b = 6, unit = 'pt')),
                panel.border = element_blank(),
                strip.background = element_rect(fill = 'grey92', colour = 'grey70'),
                strip.text = element_markdown(size = 14, face = 'bold'), 
                panel.spacing = unit(0.5, 'cm')) + 
          
          # Facet by language
          facet_wrap(~ language_with_N, ncol = 1)
      ) %>%
        
        ggsave(filename = paste0(plot_name, '.png'), path = 'analyses/plots/', 
               width = 10, height = 8, dpi = 300, units = 'in')
    }
  }
}

# Free up memory
gc()

