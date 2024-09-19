

# Creation of stimuli for the vocabulary pre-training of Session 3.
# All objects from all sessions are shown once, half of them in 
# singular and the other half in plural.


Session3_Pretraining_vocabulary = 
  
  function(stimuli, study_site, language) {
    
    require(dplyr)        # data wrangling
    require(tidyr)        # data wrangling
    require(stringr)      # text processing
    require(magick)       # image processing
    
    # Store all base stimuli, which will be 
    # used towards the end of the script.
    all_stimuli = stimuli
    
    # Select language, select items included in the present session
    
    stimuli = stimuli[(stimuli$language == language | stimuli$language == 'both') &
                        stimuli$Session_3 == 'included',]
    
    # Enable gender canonicity column in Mini-Spanish only. This column 
    # is then processed throughout the script using text evaluation, 
    # namely, `!!! rlang::syms`. Thus, for the Mini-Spanish stimuli, 
    # `gender_canonicity` will be read as `Spanish_gender_canonicity`, 
    # whereas in the other languages it will be read as `NULL`.
    
    if(language == 'Mini-Spanish') {
      gender_canonicity = 'Spanish_gender_canonicity'
    } else gender_canonicity = NULL 
    
    
    # Select nouns only and distribute grammatical numbers across nouns. 
    # Following González Alonso et al. (2020), ensure that half of 
    # the trials in plural contain an image with two items and the 
    # other half contain an image with three items.     
    
    combinations = stimuli %>% 
      filter(noun != '') %>%
      mutate( number = rep(c('singular', 'plural'), times = n()/2),
              number_of_items_in_image = 
                ifelse(number == 'singular', 1, 
                       ifelse(number == 'plural', rep(2:3, each = n()/2),
                              '')) )
    
    
    # Select article based on gender and number of noun
    
    combinations$article = NA
    
    for(i in 1:nrow(combinations)) {
      combinations[i,] = combinations[i,] %>%
        mutate(article = stimuli[stimuli$article != '' &
                                   stimuli$gender == combinations[i, 'gender'] & 
                                   stimuli$number == combinations[i, 'number'], 
                                 'article'])
    }
    
    # Create determiner phrase including the article and the noun.
    # In Norwegian, the article is appended as a suffix.
    
    if(language == 'Mini-Norwegian') {
      combinations = combinations %>%
        mutate(determiner_phrase = paste0(noun, article))
    } else {
      combinations = combinations %>%
        mutate(determiner_phrase = paste(article, noun))
    }
    
    
    # For each trial, create a label that will be used to name the corresponding 
    # image(s). These labels will be formed of the nouns and adjectives in 
    # English. To allow this also in the Mini-Norwegian and Mini-Spanish files, 
    # the English equivalents of the words will be used. In addition, the 
    # compound images (more than one object) will be created.
    
    combinations$image = NA
    
    image_dir = paste0('session_materials/images/', study_site, ' site/')
    
    # Compose labels and images 
    
    if(language == 'Mini-English') {
      
      for(i in 1:nrow(combinations)) {  # iterate over rows
        
        if(combinations[i, 'number'] == 'singular') {
          
          combinations[i,] = combinations[i,] %>%
            mutate(image = paste0(unprocessed_noun, '.png'))
          
        } else {  # more than one item in the image
          
          combinations[i,] = combinations[i,] %>%
            mutate(image = paste0(unprocessed_noun, '_', 
                                  number_of_items_in_image,
                                  '.png'))
          
          # Assemble the image
          
          image = 
            paste0(image_dir, combinations[i, 'unprocessed_noun'], '.png') %>% 
            image_read() %>% image_trim()  # trim margins off
          
          # Tailor composition to the number of items, overlay 
          # them on a square background, and trim margins off.
          
          if(combinations[i, 'number_of_items_in_image'] == 2) {
            image = 
              image_read(paste0(image_dir, 'background.png')) %>%
              image_composite(image_append(c(image, image)) %>% 
                                image_scale('720x720') %>% 
                                image_convert(colorspace = 'sRGB'),  # preserve colours
                              gravity = 'center')
            
          } else if(combinations[i, 'number_of_items_in_image'] == 3) {
            image = 
              image_read(paste0(image_dir, 'background.png')) %>%
              image_composite(image_append(c(image, image, image)) %>% 
                                image_scale('720x720') %>% 
                                image_convert(colorspace = 'sRGB'),  # preserve colours
                              gravity = 'center')
          }
          
          # Save image
          image_write(image,
                      paste0(image_dir,
                             combinations[i, 'unprocessed_noun'], '_',
                             combinations[i, 'number_of_items_in_image'],
                             '.png'),
                      format = 'PNG')
        }
      }
      
      
      # If language is not Mini-English, fetch English translations.
      # The images are not composed again because they would be the same.
    } else {
      
      for(i in 1:nrow(combinations)) {  # iterate over rows
        
        # For each element, store IDs and fetch the 
        # English counterparts matching those IDs.
        
        i_noun_ID = all_stimuli %>% 
          filter(unprocessed_noun == 
                   combinations[i,] %>% 
                   pull(unprocessed_noun)) %>%
          pull(noun_ID)
        
        i_noun_label = all_stimuli %>% 
          filter(language == 'Mini-English',
                 noun_ID == i_noun_ID) %>%
          pull(unprocessed_noun)
        
        # Compose labels depending on grammatical number
        
        if(combinations[i, 'number'] == 'singular') {
          combinations[i,] = combinations[i,] %>%
            mutate(image = paste0(i_noun_label, '.png'))
        } else {
          combinations[i,] = combinations[i,] %>%
            mutate(image = paste0(i_noun_label, '_', 
                                  number_of_items_in_image, 
                                  '.png'))
        }
      }
    }
    
    
    # Certain stimuli and experimental conditions should appear equally often to 
    # prevent repetition effects. To ascertain this, check whether all elements 
    # in certain columns appear equally often. If they do not, show warnings.
    # Please note that this basic check only helps prevent blatant disparities, 
    # but it does not verify all the controls that have been applied.
    
    columns_to_check = c('gender', 'number', 'noun')
    
    for(i in seq_along(columns_to_check)) {
      
      column = columns_to_check[i]
      
      number_of_unique_frequencies = 
        combinations %>% 
        filter(complete.cases(get(column)), get(column) != '') %>% 
        group_by(get(column)) %>% tally() %>% select(n) %>% 
        n_distinct()
      
      if(number_of_unique_frequencies != 1) {
        warning(paste0('Some elements in the column `', column, 
                       '` appear more often than others.'))
      }
    }
    
    
    # Register current version of the materials
    source('stimulus_preparation/materials_version.R')
    
    combinations %>%
      
      mutate( materials_version,
              
              # To save space in the data set, replace NAs with blanks
              across(names(.), ~replace(., is.na(.), '')) ) %>%
      
      # Store all components associated with each trial
      select(materials_version, gender, number, determiner_phrase, image) %>%
      
      # Save
      write.csv(paste0('session_materials/Session 3/stimuli/pre-training/', 
                       study_site, ' site, ', language, 
                       ', Session3_Pretraining_vocabulary.csv'), 
                row.names = FALSE, fileEncoding = 'UTF-8')
    
  }

