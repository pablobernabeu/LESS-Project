

# Plot effects in 'lmerTest' models including 95% confidence intervals. Two arguments 
# are required for this function: a summary of a 'lmerTest' model and confidence 
# intervals from lme4::confint.merMod(). The names of the predictors must be 
# identical in both these objects.

plot_95_confidence_intervals <- function(
    
  model_summary, confidence_intervals, 
  show_intercept = TRUE, select_effects = NULL, 
  exclude_effects = NULL, order_effects = NULL, 
  x_title = 'Estimate', axis_text_size = 11, 
  # number of decimal places to show in X axis (format: 0.1, 0.01, etc.)
  x_decimals = NULL, 
  remove_y_axis = FALSE, # useful for combining plots side by side
  
  # If interaction_symbol_x = TRUE, replace double colons with times 
  # symbols followed by line breaks and indentation. Then, replace 
  # single colons with times symbols.
  interaction_symbol_x = FALSE,
  
  # Vertical line at specified value of X axis
  vertical_line_at_x = NULL,
  
  # Break X axis between second largest and largest value of Estimate.
  # This is especially useful because the effect of z_baseline_predictor 
  # is far larger than the nearest effect.  
  break_x_axis = FALSE
) {
  
  require(dplyr)
  require(forcats)
  require(ggplot2)
  require(stringr)
  require(scales)
  require(ggtext)
  require(ggbreak)
  
  # Combine core model summary with confidence intervals
  model_summary <- 
    data.frame(Effect = rownames(model_summary$coefficients),
               model_summary$coefficients, row.names = NULL) %>%
    left_join(
      data.frame(Effect = rownames(confidence_intervals), 
                 CI_2.5 = confidence_intervals[,'2.5 %'], 
                 CI_97.5 = confidence_intervals[,'97.5 %'], 
                 row.names = NULL), 
      by = 'Effect'
    )
  
  # Adjust names of variables for clarity
  message('\nNames of variables adjusted in ',
          '"analyses/R_functions/plot_95_confidence_intervals.R".')
  
  model_summary$Effect <- model_summary$Effect %>%
    str_replace(pattern = 'z_recoded_grammaticality',
                replacement = 'Grammaticality') %>%
    str_replace(pattern = 'z_recoded_session',
                replacement = 'Session') %>%
    str_replace(pattern = 'z_session1_digit_span',
                replacement = 'Working memory') %>%
    str_replace(pattern = 'z_session1_Stroop',
                replacement = 'Inhibitory control') %>%
    str_replace(pattern = 'z_session1_ASRT',
                replacement = 'Implicit learning') %>%
    str_replace(pattern = 'z_multilingual_language_diversity',
                replacement = 'Multilingual diversity') %>%
    str_replace(pattern = 'z_recoded_caudality',
                replacement = 'Caudality') %>%
    str_replace(pattern = 'z_recoded_hemisphere',
                replacement = 'Hemisphere')
  
  # If show_intercept = FALSE, remove it
  if(isFALSE(show_intercept)) {
    model_summary <- model_summary %>% filter(!Effect == '(Intercept)')
  }
  
  # If select_effects was supplied, apply it and order effects accordingly
  if(!is.null(select_effects)) {
    model_summary <- model_summary %>% 
      filter(Effect %in% select_effects) %>%
      arrange(factor(Effect, levels = select_effects))
  }
  
  # If exclude_effects was supplied, apply it 
  if(!is.null(exclude_effects)) {
    model_summary <- model_summary %>% 
      filter(!Effect %in% exclude_effects)
  }
  
  # Exclude any effects passed to `exclude_effects`
  selected_effects <- setdiff(rownames(model_summary$coefficients), 
                              exclude_effects)
  
  # If order_effects was supplied, apply order
  if(!is.null(order_effects)) {
    model_summary <- model_summary %>%
      arrange(factor(Effect, levels = order_effects))
  }
  
  # If interaction_symbol_x = TRUE, replace double colons with times 
  # symbols followed by line breaks and indentation. Then, replace 
  # single colons with times symbols.
  if(interaction_symbol_x) {
    model_summary$Effect <- model_summary$Effect %>% 
      gsub('::', ' &times; <br> &nbsp;&nbsp;&nbsp;&nbsp;', .) %>%
      gsub(':', ' &times; ', .)
  }
  
  # Set current order of effects to prevent default alphabetical order
  model_summary$Effect <- 
    factor(model_summary$Effect, levels = model_summary$Effect)
  
  # PLOT
  plot <- model_summary %>%
    ggplot(aes(x = Estimate, 
               # Override default, bottom-up order on Y-axis
               y = fct_rev(Effect))) + 
    
    # Add vertical line
    geom_vline(xintercept = vertical_line_at_x, col = 'grey60') +
    
    # Add confidence intervals from frequentist model
    geom_errorbarh(aes(xmin = CI_2.5, xmax = CI_97.5), 
                   height = 0.3, colour = 'grey20') +
    
    # Add frequentist estimate
    geom_point(aes(x = Estimate), colour = 'black', size = 1) +
    
    scale_y_discrete(expand = c(0.033, 0)) +
    
    # X-axis title
    xlab(x_title) +
    
    theme_minimal() +
    theme(
      plot.title = element_markdown(hjust = 0.5, margin = margin(b = 0)),
      axis.title.x = element_markdown(
        size = axis_text_size * 1.1, 
        margin = margin(t = axis_text_size * 0.04, b = axis_text_size * 0.2),
        hjust = 0.73
      ), 
      axis.text.x = element_text(
        size = axis_text_size * 0.9, margin = margin(t = axis_text_size * 0.5)
      ), 
      axis.ticks.x = element_line(),
      axis.title.y = element_blank(), 
      axis.text.y = element_markdown(
        size = axis_text_size, margin = margin(l = -6)),
      panel.grid.major.y = element_line(colour = 'grey85'),
      plot.margin = margin(20, 10, 120, 0)
    )
  
  if(remove_y_axis) {
    plot <- plot + theme(axis.text.y = element_blank())
  }
  
  if(!is.null(x_decimals)) {
    number_x_decimals <- nchar(
      sub('0+$', '', sub('^.*\\.', '', format(x_decimals, scientific = FALSE)))
    )
  }
  
  # Determine break points for the plot, which is useful when there 
  # are large gaps between values along the X axis, as is the case 
  # with the EEG baseline predictor.
  
  if(break_x_axis) {
    
    sorted_estimates <- sort(unique(model_summary$Estimate))
    sorted_CI_2.5 <- sort(unique(model_summary$CI_2.5))
    sorted_CI_97.5 <- sort(unique(model_summary$CI_97.5))
    
    if (length(sorted_estimates) >= 2) {
      
      # Determine margin around break based on range of sorted_estimates
      estimate_range <- diff(range(sorted_estimates))
      margin <- 0.005 * estimate_range
      
      # Set the start of the break following the second-largest effect
      second_largest <- sorted_CI_97.5[length(sorted_CI_97.5) - 1]
      break_start <- second_largest + margin
      
      # Set the end of the break prior to the largest effect
      largest <- sorted_CI_2.5[length(sorted_CI_2.5)]
      break_end <- largest - margin
      
      # When conditions for break are met and x_decimal has not been supplied
      if(!is.na(break_start) && !is.na(break_end) && break_start < break_end &&
         is.null(x_decimals)) {
        
        plot <- plot + 
          scale_x_break(
            c(break_start, break_end), scales = 0.2, 
            ticklabels = largest
          ) +
          # Prevent bug (https://github.com/YuLab-SMU/ggbreak/issues/51)
          theme(axis.text.x.top = element_blank(), 
                axis.ticks.x.top = element_blank(),
                axis.text.y.right = element_blank(), 
                axis.ticks.y.right = element_blank())
        
        # When conditions for break are met and x_decimal has been supplied
      } else if(!is.na(break_start) && !is.na(break_end) && 
                break_start < break_end && !is.null(x_decimals)) {
        
        plot <- plot + 
          scale_x_break(
            c(break_start, break_end), scales = 0.2, 
            ticklabels = round(
              largest, digits = ifelse(!is.null(x_decimals), number_x_decimals, 0)
            )
          ) +
          # Prevent bug (https://github.com/YuLab-SMU/ggbreak/issues/51)
          theme(axis.text.x.top = element_blank(), 
                axis.ticks.x.top = element_blank(),
                axis.text.y.right = element_blank(), 
                axis.ticks.y.right = element_blank())
      }
    }
  }
  
  if(!is.null(x_decimals)) {
    plot <- plot +
      scale_x_continuous(labels = label_number(accuracy = x_decimals))
  }
  
  return(plot)
}

