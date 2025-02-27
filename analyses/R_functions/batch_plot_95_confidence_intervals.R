

# Generate confidence interval plots for the mixed-effects models. 
# The function iterates over the models. 

batch_plot_95_confidence_intervals <- function(excluded_variables = NULL) {
  
  require(dplyr)
  require(stringr)
  require(ggplot2)
  require(ggtext)
  require(patchwork)
  
  source('analyses/R_functions/plot_95_confidence_intervals.R')
  
  # Define levels for iteration with explicit mapping
  param_grid <- data.frame(
    mini_language = as.character(rep(c('Mini-Norwegian', 'Mini-English'), each = 6)),
    abbr_mini_language = as.character(rep(c('MiniNor', 'MiniEng'), each = 6)),
    time_window = as.character(rep(c('200&ndash;500', '300&ndash;600', '400&ndash;900'), times = 4)),
    macroregion = as.character(rep(c('lateral', 'midline'), each = 3, times = 2)),
    stringsAsFactors = FALSE
  )
  
  # Store plots in a list
  plot_list <- list()
  
  # Iterate over each combination
  for (i in seq_len(nrow(param_grid))) {
    
    row <- param_grid[i, ]
    
    # Extract parameter values
    mini_language <- row$mini_language
    abbr_mini_language <- row$abbr_mini_language
    time_window <- row$time_window
    macroregion <- row$macroregion
    
    # Construct file identifiers
    file_suffix <- paste(abbr_mini_language, gsub('&ndash;', '_', time_window), 
                         macroregion, 'lmerTest', sep = '_')
    
    # Define file paths
    
    summary_file <- paste0('analyses/EEG/results/Satterthwaite_summary_EEG_genderAgr_', 
                           file_suffix, '.rds')
    
    confint_file <- paste0('analyses/EEG/results/confint_EEG_genderAgr_', 
                           file_suffix, '.rds')
    
    plot_file <- paste0('analyses/EEG/plots/confint_Satterthwaite_summary_EEG_genderAgr_', 
                        file_suffix, '.png')
    
    message("Checking files:", summary_file, " and ", confint_file)
    
    # Load data if files exist
    if (file.exists(summary_file) & file.exists(confint_file)) {
      
      message("Files found. Loading data...")
      summary_data <- readRDS(summary_file)
      confint_data <- readRDS(confint_file)
      
      # Adjust names of variables for clarity
      
      rownames(summary_data$coefficients) <- 
        rownames(summary_data$coefficients) %>%
        str_replace(pattern = 'z_recoded_grammaticality',
                    replacement = 'Grammaticality') %>%
        str_replace(pattern = 'z_recoded_session',
                    replacement = 'Session') %>%
        str_replace(pattern = 'z_session1_digit_span',
                    replacement = 'Digit span (working memory)') %>%
        str_replace(pattern = 'z_session1_Stroop',
                    replacement = 'Stroop (inhibitory control)') %>%
        str_replace(pattern = 'z_session1_ASRT',
                    replacement = 'Serial reaction time (implicit learning)') %>%
        str_replace(pattern = 'z_multilingual_language_diversity',
                    replacement = 'Multilingual language diversity') %>%
        str_replace(pattern = 'z_recoded_caudality',
                    replacement = 'Caudality') %>%
        str_replace(pattern = 'z_recoded_hemisphere',
                    replacement = 'Hemisphere')
      
      rownames(confint_data) <- rownames(confint_data) %>%
        str_replace(pattern = 'z_recoded_grammaticality',
                    replacement = 'Grammaticality') %>%
        str_replace(pattern = 'z_recoded_session',
                    replacement = 'Session') %>%
        str_replace(pattern = 'z_session1_digit_span',
                    replacement = 'Digit span (working memory)') %>%
        str_replace(pattern = 'z_session1_Stroop',
                    replacement = 'Stroop (inhibitory control)') %>%
        str_replace(pattern = 'z_session1_ASRT',
                    replacement = 'Serial reaction time (implicit learning)') %>%
        str_replace(pattern = 'z_multilingual_language_diversity',
                    replacement = 'Multilingual language diversity') %>%
        str_replace(pattern = 'z_recoded_caudality',
                    replacement = 'Caudality') %>%
        str_replace(pattern = 'z_recoded_hemisphere',
                    replacement = 'Hemisphere')
      
      if (!"coefficients" %in% names(summary_data)) {
        message("Error: 'coefficients' not found in summary data")
        next
      }
      
      # Select effects excluding baseline predictor
      selected_vars <- setdiff(rownames(summary_data$coefficients), excluded_variables)
      # message("Selected variables: ", paste(selected_vars, collapse=", "))
      
      # Generate the plot
      plot <- plot_95_confidence_intervals(
        summary_data, 
        confint_data, 
        x_title = 'Effect size (&beta;)',
        interaction_symbol_x = TRUE,
        vertical_line_at_x = 0, 
        select_effects = selected_vars, 
        axis_text_size = 11
      ) + theme(plot.margin = margin(9, 4, 8, 6),
                plot.title = element_markdown(hjust = 1)) +
        ggtitle(paste0(mini_language, ", ", time_window, " ms, ", 
                       macroregion, " region"))
      
      # Store the plot in the list
      plot_list[[file_suffix]] <- plot
      
      # Save individual plots in a wider format
      message("Saving plot: ", plot_file)
      ggsave(filename = plot_file, plot = plot, width = 6.5, height = 6, dpi = 900)
      
    } else {
      message("Skipping: Files not found")
    }
  }
  
  # Generate combined plots for Mini-Norwegian and Mini-English
  for (time_window in unique(param_grid$time_window)) {
    for (macroregion in unique(param_grid$macroregion)) {
      
      # Identify plot names
      
      miniNorwegian_plot_name <- paste0('MiniNor_', gsub('&ndash;', '_', time_window), '_', 
                                        macroregion, '_lmerTest')
      
      miniEnglish_plot_name <- paste0('MiniEng_', gsub('&ndash;', '_', time_window), '_', 
                                      macroregion, '_lmerTest')
      
      # Check if both plots exist
      if (!is.null(plot_list[[miniNorwegian_plot_name]]) & 
          !is.null(plot_list[[miniEnglish_plot_name]])) {
        
        message("Combining plots: ", miniNorwegian_plot_name, " and ", miniEnglish_plot_name)
        
        # Set titles and themes
        
        miniNorwegian_plot <- plot_list[[miniNorwegian_plot_name]] + 
          ggtitle("Mini-Norwegian") +
          theme(plot.title = element_markdown(hjust = 0.5))
        
        miniEnglish_plot <- plot_list[[miniEnglish_plot_name]] + 
          ggtitle("Mini-English") +
          # Remove Y axis from right-hand plot
          theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                plot.title = element_markdown(hjust = 0.5))
        
        # Combine using patchwork and collect Y-axis
        combined_plot <- (miniNorwegian_plot | miniEnglish_plot) +
          plot_layout(guides = 'collect') +
          plot_annotation(
            title = paste0(time_window, " ms, ", macroregion, " region"),
            theme = theme(plot.title = element_markdown(
              hjust = 0.5, size = 14, face = "bold"
            )))
        
        # Save combined plot
        
        combined_plot_file <- 
          paste0('analyses/EEG/plots/general model plots/confint_Satterthwaite_summary_EEG_genderAgr_',
                 gsub('&ndash;', '_', time_window), '_', macroregion, '_combined.png')
        message("Saving combined plot: ", combined_plot_file)
        ggsave(filename = combined_plot_file, plot = combined_plot, width = 9, 
               height = 5.5, dpi = 900)
        
      } else {
        message("Skipping combination: One or both plots missing")
      }
    }
  }
}

