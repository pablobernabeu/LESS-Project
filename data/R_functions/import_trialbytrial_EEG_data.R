
import_trialbytrial_EEG_data <-
  
  # Default file pattern catches all data
  function(EEG_file_pattern = '^\\d+_trialbytrial_S[123]_S10[123]\\.',
           min_time = -100, max_time = 1100,
           include_baseline = TRUE,
           aggregate_electrodes = FALSE,
           aggregate_time_points = FALSE,
           selected_macroregion = NULL) {
    
    require(dplyr)
    require(tidyr)
    require(stringr)
    require(data.table)
    require(purrr)
    
    # List all relevant .txt files in the directory
    txt_files <- list.files(
      pattern = paste0(EEG_file_pattern, 'txt$'),
      full.names = TRUE, recursive = TRUE,
      path = 'data/raw data/EEG'
    )
    
    # List all relevant .vmrk files in the directory
    vmrk_files <- list.files(
      pattern = paste0(EEG_file_pattern, 'vmrk$'),
      full.names = TRUE, recursive = TRUE,
      path = 'data/raw data/EEG'
    )
    
    # Pair txt and vmrk files by matching names (excluding extensions)
    paired_files <- tibble(
      txt_file = txt_files,
      base_path = sub('\\.txt$', '', txt_files) # path without file extension
    ) %>%
      inner_join(
        tibble(
          vmrk_file = vmrk_files,
          base_path = sub('\\.vmrk$', '', vmrk_files) # path without file extension
        ),
        by = 'base_path',
      ) %>%
      mutate(
        participant_lab_ID = sub('.*Export\\/([0-9]+)_.*', '\\1', base_path),
        session = sub('.*Session (\\d).*', '\\1', base_path),
        grammatical_property = sub('.*_(S[1-3])_.*', '\\1', base_path),
        grammaticality = sub('.*_(S10[1-3])$', '\\1', base_path)
      )
    
    # Initialize the output data frame
    trialbytrial_EEG_data <- NULL
    
    # Process each pair
    for (i in seq_len(nrow(paired_files))) {
      
      txt_file <- paired_files$txt_file[i]
      vmrk_file <- paired_files$vmrk_file[i]
      
      # Extract markers from the .vmrk file
      markers <- readLines(vmrk_file)
      marker_start <- grep('\\[Marker Infos\\]', markers)
      
      if (length(marker_start) == 0) {
        stop('The [Marker Infos] section is missing in the .vmrk file.')
      }
      
      marker_infos <- markers[marker_start:length(markers)]
      sentence_markers <- marker_infos[grep('S1[1-9][0-9]|S2[0-4][0-9]|S25[0-3]', marker_infos)]
      
      if (length(sentence_markers) == 0) {
        stop('No valid sentence markers (S110–S253) found in the .vmrk file.')
      }
      
      sentence_markers <- gsub('.*S(\\d{3}).*', '\\1', sentence_markers)
      
      # Define hard-coded time points
      time_points <- seq(-100, 1098, by = 2)
      
      # Read the raw .txt file line by line
      raw_file_content <- readLines(txt_file)
      
      # Extract electrode names and data
      parsed_lines <- lapply(raw_file_content, function(line) strsplit(line, '\\s+')[[1]])
      electrode_names <- sapply(parsed_lines, `[[`, 1) # First word of each line
      data_values <- do.call(rbind, lapply(parsed_lines, function(x) x[-1])) # Remaining numbers
      
      # Check for parsing issues
      if (any(is.na(data_values))) {
        stop('NAs detected in data values. Check the .txt file for non-numeric content.')
      }
      
      # Ensure electrode names and data are aligned
      if (length(electrode_names) != nrow(data_values)) {
        stop('Mismatch between electrode names and data rows.')
      }
      
      # Validate the data structure
      expected_columns <- length(time_points) * length(sentence_markers)
      if (ncol(data_values) != expected_columns) {
        stop('Data structure mismatch: The number of columns does not align with trials and time points.')
      }
      
      # Reshape the data into a tidy format
      reshaped_data <- as.data.frame(data_values) %>%
        mutate(electrode = electrode_names) %>%
        pivot_longer(
          cols = -electrode,
          names_to = 'placeholder_time_index', # removed below
          values_to = 'amplitude'
        ) %>%
        select(-placeholder_time_index) %>%
        
        mutate(
          
          # Convert commas to points in amplitude values
          amplitude = as.numeric(gsub(',', '.', amplitude)),
          
          # Import values from paired_files
          participant_lab_ID = paired_files$participant_lab_ID[i],
          session = as.character(paired_files$session[i]),
          grammatical_property = as.character(paired_files$grammatical_property[i]),
          grammaticality = as.character(paired_files$grammaticality[i]),
          time = as.numeric(rep(time_points, times = length(sentence_markers) * length(electrode_names))),
          sentence_marker = as.character(rep(rep(sentence_markers, each = length(time_points)),
                                             times = length(electrode_names)))
        )
      
      # Incrementally append the reshaped data to the main output data frame
      trialbytrial_EEG_data <- bind_rows(trialbytrial_EEG_data, reshaped_data)
      
      # --- START OF CHANGE ---
      assign("trialbytrial_EEG_data_temp", trialbytrial_EEG_data, envir = .GlobalEnv)
      # --- END OF CHANGE ---
      
      # Print progress
      cat('Processed file:', txt_file, '\n')
    }
    
    # Translate markers into labels and create time windows, brain regions, etc.
    trialbytrial_EEG_data <- trialbytrial_EEG_data %>%
      mutate(
        grammatical_property = as.character(case_when(
          grammatical_property == 'S1' ~ 'Gender agreement',
          grammatical_property == 'S2' ~ 'Differential object marking',
          grammatical_property == 'S3' ~ 'Verb-object number agreement',
          TRUE ~ NA_character_
        )),
        grammaticality = as.character(case_when(
          grammaticality == 'S101' ~ 'Grammatical',
          grammaticality == 'S102' ~ 'Ungrammatical',
          
          # Ancillary violation conditions varying by grammatical property
          
          grammatical_property == 'Gender agreement' &
            grammaticality == 'S103' ~ 'Number agreement violation',
          
          grammatical_property != 'Gender agreement' &
            grammaticality == 'S103' ~ 'Article location violation',
          
          TRUE ~ NA_character_
        )),
        time_window = as.character(case_when(
          time >= 200 & time <= 498 ~ '200_500',
          time >= 300 & time <= 598 ~ '300_600',
          time >= 400 & time <= 898 ~ '400_900',
          TRUE ~ NA_character_
        )),
        brain_region = as.character(case_when(
          electrode %in% c('Fp1', 'F3', 'F7', 'FT9', 'FC5') ~ 'left anterior',
          electrode %in% c('Fz', 'FC1', 'FC2') ~ 'midline anterior',
          electrode %in% c('Fp2', 'F4', 'F8', 'FT10', 'FC6') ~ 'right anterior',
          electrode %in% c('T7', 'C3', 'CP5') ~ 'left medial',
          electrode %in% c('Cz', 'CP1', 'CP2') ~ 'midline medial',
          electrode %in% c('T8', 'C4', 'CP6') ~ 'right medial',
          electrode %in% c('P7', 'P3', 'O1') ~ 'left posterior',
          electrode %in% c('Pz', 'Oz') ~ 'midline posterior',
          electrode %in% c('P8', 'P4', 'O2') ~ 'right posterior',
          TRUE ~ NA_character_
        )),
        macroregion = as.character(case_when(
          str_detect(brain_region, 'left|right') ~ 'lateral',
          str_detect(brain_region, 'midline') ~ 'midline',
          TRUE ~ NA_character_
        )),
        hemisphere = as.character(case_when(
          str_detect(brain_region, 'left') ~ 'left',
          str_detect(brain_region, 'right') ~ 'right',
          TRUE ~ NA_character_
        )),
        caudality = as.character(case_when(
          str_detect(brain_region, 'anterior') ~ 'anterior',
          str_detect(brain_region, 'medial') ~ 'medial',
          str_detect(brain_region, 'posterior') ~ 'posterior',
          TRUE ~ NA_character_
        )),
        
        # Recode dichotomous predictors (Brauer & Curtin, 2018; https://doi.org/10.1037/met0000159).
        # Session is specifically recoded following Michael Clark's recommendation at
        # https://m-clark.github.io/sem/growth-curves.html#numbering-the-time-points.
        
        recoded_grammaticality = as.numeric(case_when(
          grammaticality == 'Grammatical' ~ 0.5,
          grammaticality == 'Number agreement violation' ~ 0,
          grammaticality == 'Article location violation' ~ 0,
          grammaticality == 'Ungrammatical' ~ -0.5,
          TRUE ~ NA_real_
        )),
        recoded_session = as.numeric(case_when(
          session == 2 ~ 0,
          session == 3 ~ 1,
          session == 4 ~ 2,
          session == 6 ~ 3,
          TRUE ~ NA_real_
        )),
        recoded_brain_region = as.numeric(case_when(
          brain_region == 'left anterior' ~ 1,
          brain_region == 'midline anterior' ~ 0.75,
          brain_region == 'right anterior' ~ 0.5,
          brain_region == 'left medial' ~ 0.25,
          brain_region == 'midline medial' ~ 0,
          brain_region == 'right medial' ~ -0.25,
          brain_region == 'left posterior' ~ -0.5,
          brain_region == 'midline posterior' ~ -0.75,
          brain_region == 'right posterior' ~ -1,
          TRUE ~ NA_real_
        )),
        recoded_hemisphere = as.numeric(case_when(
          hemisphere == 'right' ~ 0.5,
          hemisphere == 'left' ~ -0.5,
          TRUE ~ NA_real_
        )),
        recoded_caudality = as.numeric(case_when(
          caudality == 'anterior' ~ 0.5,
          caudality == 'medial' ~ 0,
          caudality == 'posterior' ~ -0.5,
          TRUE ~ NA_real_
        ))
      )
    
    if(include_baseline) {
      trialbytrial_EEG_data <- trialbytrial_EEG_data %>%
        filter(time < 0 | time >= min_time & time <= max_time) %>%
        mutate(time_window = case_when(time < 0 ~ 'baseline',
                                       TRUE ~ time_window))
    } else {
      trialbytrial_EEG_data <- trialbytrial_EEG_data %>%
        filter(time >= min_time & time <= max_time)
    }
    
    # Z-score amplitude around each participant's own mean to preserve individual
    # differences (Faust et al., 1999; https://doi.org/10.1037/0033-2909.125.6.777)
    # Similarly, z-score continuous between-items predictors around each participant's 
    # own mean (Brauer & Curtin, 2018; https://doi.org/10.1037/met0000159).
    
    trialbytrial_EEG_data <- trialbytrial_EEG_data %>%
      group_by(participant_lab_ID) %>%
      mutate(
        z_amplitude = ifelse(is.na(amplitude) | amplitude == '', NA, as.numeric(scale(amplitude)))
      ) %>%
      ungroup()
    
    if(aggregate_electrodes & aggregate_time_points) {
      trialbytrial_EEG_data <- trialbytrial_EEG_data %>%
        select(-electrode, -time) %>%
        # Group by all columns except amplitude
        group_by(across(-amplitude)) %>%
        # Aggregate amplitude by mean
        summarise(amplitude = mean(amplitude, na.rm = TRUE), .groups = 'drop')
      
    } else if(aggregate_electrodes) {
      trialbytrial_EEG_data <- trialbytrial_EEG_data %>%
        select(-electrode) %>%
        # Group by all columns except amplitude
        group_by(across(-amplitude)) %>%
        # Aggregate amplitude by mean
        summarise(amplitude = mean(amplitude, na.rm = TRUE), .groups = 'drop')
      
    } else if(aggregate_time_points) {
      trialbytrial_EEG_data <- trialbytrial_EEG_data %>%
        select(-time) %>%
        # Group by all columns except amplitude
        group_by(across(-amplitude)) %>%
        # Aggregate amplitude by mean
        summarise(amplitude = mean(amplitude, na.rm = TRUE), .groups = 'drop')
    }
    
    if(!is.null(selected_macroregion)) {
      trialbytrial_EEG_data <- trialbytrial_EEG_data %>%
        filter(macroregion == selected_macroregion)
    }
    
    # Replace empty cells with NAs and remove rows that have NAs in all columns
    trialbytrial_EEG_data <- trialbytrial_EEG_data %>%
      mutate(across(where(is.factor), as.character), # Necessary for NA input below
             across(where(is.character), ~ na_if(.x, '')),
             across(where(is.character), as.factor)) %>% # Convert character vars back to factors
      filter(if_any(everything(), ~ !is.na(.))) %>%
      
      # Set factor levels
      mutate(
        session = factor(session, levels = c(2, 3, 4, 6)),
        grammatical_property = factor(
          grammatical_property,
          levels = c('Gender agreement', 'Differential object marking',
                     'Verb-object number agreement')
        ),
        brain_region = factor(
          brain_region,
          levels = c('left anterior', 'midline anterior', 'right anterior',
                     'left medial', 'midline medial', 'right medial',
                     'left posterior', 'midline posterior', 'right posterior')
        )
      )
    
    cat('Summary of the processed EEG data:\n')
    print(summary(trialbytrial_EEG_data))
    
    # Free unused memory
    gc()
    
    return(trialbytrial_EEG_data)
  }
