

# Waveform plots

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggtext)

# Read in data
source('data/preprocessing/merging data.R')

# Rename factor levels and remove control violation condition 
merged_data = merged_data %>%
  filter(!grammaticality == 'Control violation')

# Define standard error function
std = function(x) sd(x) / sqrt(length(x))

# Initialize an empty list to store all data
all_data = list()

# Create plots by iterating over factors. They'll be saved to disk one by one, 
# taking several minutes.

for(i_session in unique(na.omit(merged_data$session))) {
  for(i_grammatical_property in unique(na.omit(merged_data$grammatical_property))) {
    for(i_region in unique(na.omit(merged_data$region))) {
      
      # Create iteration data
      iteration_data = merged_data %>%
        filter(session == i_session,
               grammatical_property == i_grammatical_property,
               region == i_region)
      
      # Skip iteration if combination of factors does not exist in data set.
      # For instance, Session 2 only contains the property of gender agreement.
      if( nrow(iteration_data) == 0 ) next
      
      # Calculate sample size for each language group
      sample_sizes = iteration_data %>% 
        group_by(language) %>%
        summarise(n = n_distinct(participant_home_ID), .groups = 'drop')
      
      # Filter data for the current combination of factors
      df2 = aggregate(
        amplitude ~ grammaticality * time * region * language,
        iteration_data, 
        mean)
      
      # Merge the sample sizes into df2
      df2 = df2 %>%
        left_join(sample_sizes, by = 'language') %>%
        mutate(language_with_N = paste0(language, ' (*n* = ', n, ')'))
      
      # Compute SD, SE, Confidence Intervals
      SD = rep(NA, length(df2$time))       # vector SD per time
      SE = rep(NA, length(df2$time))       # vector SE per time
      CIupper = rep(NA, length(df2$time))  # vector upper 95% conf int
      CIlower = rep(NA, length(df2$time))  # vector lower 95% conf int
      
      for (i in 1:length(df2$time)) {
        something = subset(iteration_data, time == df2$time[i] & 
                             language == df2$language[i], 
                           select = amplitude)
        SE[i] = std(something$amplitude)
        CIlower[i] = df2$amplitude[i] - (SE[i] * 1.96)
        CIupper[i] = df2$amplitude[i] + (SE[i] * 1.96)
      }
      
      df2$CIL = CIlower
      df2$CIU = CIupper
      
      # Create sanitized plot name
      plot_name = paste0(i_grammatical_property, "_Session", i_session, 
                         "_", str_to_sentence(i_region), " region") %>%
        gsub("[^[:alnum:]_]", "_", .)
      
      plot_title = paste0(str_to_sentence(i_grammatical_property), "; ", 
                          "Session ", i_session, "; ", 
                          str_to_sentence(i_region), " region")
      
      # Ensure 'Mini-Norwegian' appears in the upper facet by reversing 
      # the default alphabetical order of language_with_N.
      df2$language_with_N = factor(
        df2$language_with_N,
        levels = rev(levels(factor(df2$language_with_N))) 
      )
      
      # Map colours to grammaticality conditions
      group_colours = c('Grammatical' = 'forestgreen', 'Ungrammatical' = 'firebrick1')
      
      # Create and export plot
      (
        ggplot(df2, aes(x = time, y = amplitude, color = grammaticality)) +
          
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
          facet_wrap(~ language_with_N, ncol = 1, )
      ) %>%
        
        ggsave(filename = paste0(plot_name, '.png'), path = 'analyses/plots/', 
               width = 10, height = 8, dpi = 300, units = 'in')
    }
  }
}

# Free up memory
gc()

