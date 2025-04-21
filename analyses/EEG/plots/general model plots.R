
# General model plots with confidence intervals. Names of variables 
# adjusted in script 'plot_95_confidence_intervals.R'.

require(dplyr)
require(stringr)
require(ggplot2)
require(grid)
require(ggtext)
require(patchwork)

source('analyses/R_functions/plot_95_confidence_intervals.R')

plots_path <- 'analyses/EEG/plots/general model plots'

param_grid <- list(
  grammatical_property = c('Gender agreement', 'Differential object marking', 
                           'Verb-object number agreement'),
  mini_language = c('Mini-Norwegian', 'Mini-English'),
  time_window = c('200&ndash;500', '300&ndash;600', '400&ndash;900'),
  macroregion = c('lateral', 'midline')
) %>%
  
  # Create the full grid of combinations
  expand.grid(stringsAsFactors = FALSE) %>%
  
  mutate(
    grammatical_property = factor(
      grammatical_property,
      levels = c('Gender agreement', 'Differential object marking', 
                 'Verb-object number agreement')
    ),
    mini_language = factor(
      mini_language, levels = c('Mini-Norwegian', 'Mini-English')
    ),
    time_window = factor(
      time_window, levels = c('200&ndash;500', '300&ndash;600', '400&ndash;900')
    ),
    macroregion = factor(
      macroregion, levels = c('lateral', 'midline')
    ),
    abbr_grammatical_property = factor(
      case_when(
        grammatical_property == 'Gender agreement' ~ 'genderAgr',
        grammatical_property == 'Differential object marking' ~ 'DOM',
        grammatical_property == 'Verb-object number agreement' ~ 'VONA',
        .default = NA
      ),
      levels = c('genderAgr', 'DOM', 'VONA')
    ),
    abbr_mini_language = factor(
      case_when(
        mini_language == 'Mini-Norwegian' ~ 'MiniNor',
        mini_language == 'Mini-English' ~ 'MiniEng',
        .default = NA
      ),
      levels = c('MiniNor', 'MiniEng')
    )
  ) %>%
  
  select(grammatical_property, abbr_grammatical_property, mini_language, 
         abbr_mini_language, time_window, macroregion) %>% 
  
  arrange(grammatical_property, time_window, macroregion, mini_language)


# INDIVIDUAL MODEL PLOTS

# Iterate over each combination
for(i in seq_len(nrow(param_grid))) {
  
  row <- param_grid[i, ]
  
  # Extract parameter values
  grammatical_property <- row$grammatical_property
  abbr_grammatical_property <- row$abbr_grammatical_property
  mini_language <- row$mini_language
  abbr_mini_language <- row$abbr_mini_language
  time_window <- row$time_window
  macroregion <- row$macroregion
  
  # Construct file identifiers
  file_properties <- paste(abbr_grammatical_property, abbr_mini_language,
                           gsub('&ndash;', '_', time_window),
                           macroregion, 'lmerTest', sep = '_')
  
  # Define file paths
  
  summary_file <- paste0('analyses/EEG/results/Satterthwaite_summary_EEG_',
                         file_properties, '.rds')
  
  confint_file <- paste0('analyses/EEG/results/confint_EEG_',
                         file_properties, '.rds')
  
  plot_name <- paste0('model plot for EEG_', file_properties)
  
  # message('Checking files:', summary_file, ' and ', confint_file)
  
  # Load data if files exist
  if(file.exists(summary_file) & file.exists(confint_file)) {
    
    # message('Files found. Loading data...')
    model_summary <- readRDS(summary_file)
    confints <- readRDS(confint_file)
    
    if(!'coefficients' %in% names(model_summary)) {
      message("Error: 'coefficients' not found in summary data")
      next
    }
    
    # Generate the plot
    plot <- plot_95_confidence_intervals(
      model_summary, confints,
      x_title = 'Effect size (&beta;)',
      interaction_symbol_x = TRUE,
      vertical_line_at_x = 0,
      axis_text_size = 11,
      break_x_axis = TRUE,
      x_decimals = 0.001
    ) +
      labs(caption = paste0('*Note*. X-axis truncated to reduce gap between ',
                            'largest and second-largest effect.')) +
      ggtitle(paste0(grammatical_property, ', ', mini_language, ', ',
                     time_window, ' ms, ', macroregion, ' region')) +
      theme(plot.title = element_markdown(hjust = 0.5, margin = margin(t = 5)),
            axis.title.x = element_markdown(margin = margin(t = -2)),
            plot.caption = element_markdown(
              size = 10, margin = margin(t = 8, b = 4, r = 4)
            ),
            plot.margin = margin(20, 10, 120, 0))
    
    message('Saving ', plot_name)
    
    # Save as PNG
    ggsave(plot, filename = paste0(plot_name, '.png'), path = plots_path,
           width = 6.5, height = 6, dpi = 900)
    
    # Save as PDF
    ggsave(plot, filename = paste0(plot_name, '.pdf'), path = plots_path,
           width = 6.5, height = 6, dpi = 900)
    
    # Also print to multi-page PDF
    print(plot)
    
    # Clean up for next iteration
    rm(plot)
    
  } else {
    message('Skipping: Files not found')
  }
}


# MODEL PLOTS ACROSS GROUPS

# Generate combined plots showing both groups side by side.
# Unlike the individual plots above, the combined plots below do not include 
# the baseline predictor due to a conflict with the break in the X axis. A
# lower caption is included to explain the removal of the baseline predictor.

# Open a PDF device to save all plots to one file
pdf('analyses/EEG/plots/general model plots/model plots across groups.pdf', 
    width = 9, height = 5.5)

for(i_grammatical_property in levels(param_grid$grammatical_property)) {
  for(time_window in levels(param_grid$time_window)) {
    for(macroregion in levels(param_grid$macroregion)) {
      for(mini_language in levels(param_grid$mini_language)) {
        
        abbr_grammatical_property <- param_grid %>%
          filter(grammatical_property == i_grammatical_property) %>%
          pull(abbr_grammatical_property) %>% unique()
        
        abbr_mini_language <- ifelse(mini_language == 'Mini-Norwegian', 
                                     'MiniNor', 'MiniEng')
        
        file_properties <- paste(abbr_grammatical_property, abbr_mini_language, 
                                 gsub('&ndash;', '_', time_window), 
                                 macroregion, 'lmerTest', sep = '_')
        
        summary_file <- paste0('analyses/EEG/results/Satterthwaite_summary_EEG_', 
                               file_properties, '.rds')
        
        confint_file <- paste0('analyses/EEG/results/confint_EEG_', 
                               file_properties, '.rds')
        
        if(!file.exists(summary_file) | !file.exists(confint_file)) {
          message('Skipping (combined): missing files for ', mini_language)
          next
        }
        
        model_summary <- readRDS(summary_file)
        confints <- readRDS(confint_file)
        
        # Recreate plot with tailored parameters
        plot <- plot_95_confidence_intervals(
          model_summary, confints, 
          x_title = 'Effect size (&beta;)',
          interaction_symbol_x = TRUE,
          vertical_line_at_x = 0, 
          exclude_effects = 'z_baseline_predictor', # see matching caption below
          axis_text_size = 11, 
          break_x_axis = FALSE,
          x_decimals = 0.001
        ) +
          ggtitle(mini_language) + 
          theme(
            plot.margin = margin(20, 0, 16, 0),
            plot.title = element_markdown(hjust = 0.5, margin = margin(b = 8)),
            axis.title.x = element_markdown(hjust = 0.5, margin = margin(t = 5))
          )
        
        if(mini_language == 'Mini-Norwegian') {
          miniNorwegian_plot <- plot + 
            # Draw right border
            annotation_custom( 
              grob = linesGrob(x = unit(c(1, 1), 'npc'), 
                               y = unit(c(0, 1), 'npc'),
                               gp = gpar(col = 'black', lwd = 2))
            )
        } else if(mini_language == 'Mini-English') {
          miniEnglish_plot <- plot + 
            # Draw left border
            annotation_custom( 
              grob = linesGrob(x = unit(c(0, 0), 'npc'), 
                               y = unit(c(0, 1), 'npc'),
                               gp = gpar(col = 'black', lwd = 2))
            ) +
            # Remove Y axis from Mini-English plot
            theme(axis.text.y = element_blank(), 
                  axis.ticks.y = element_blank())
        }
      }
      
      if(exists('miniNorwegian_plot') & exists('miniEnglish_plot')) {
        combined_plot <- (miniNorwegian_plot | miniEnglish_plot) +
          plot_layout(guides = 'collect') +
          plot_annotation(
            title = paste0(i_grammatical_property, ', ', time_window, ' ms, ', 
                           macroregion, ' region'),
            caption = paste0('*Note*. EEG baseline predictor not shown due to ',
                             'large distance from other effects.'),
            theme = theme(
              plot.title = element_markdown(hjust = 0.5, size = 14, face = 'bold',
                                            margin = margin(b = -10)),
              axis.title.x = element_markdown(margin = margin(b = -10)),
              plot.caption = element_markdown(size = 10, margin = margin(t = -9)),
              plot.margin = margin(10, 14, 9, 13)
            )
          )
        
        combined_plot_name <- paste0('model plot across groups for EEG_', 
                                     i_grammatical_property, ', ', 
                                     gsub('&ndash;', '_', time_window), '_', 
                                     macroregion)
        
        message('Saving ', combined_plot_name)
        
        # Save as PNG
        ggsave(combined_plot, filename = paste0(combined_plot_name, '.png'), 
               path = plots_path, width = 9, height = 5.5, dpi = 900)
        
        # Save as PDF
        ggsave(combined_plot, filename = paste0(combined_plot_name, '.pdf'), 
               path = plots_path, width = 9, height = 5.5, dpi = 900)
        
        # Also print to multi-page PDF
        print(combined_plot)
        
        # Clean up for next iteration
        rm(miniNorwegian_plot, miniEnglish_plot)
        
      } else {
        message('Skipping combination: One or both plots could not be created')
      }
    }
  }
}

# Close PDF device
dev.off()

