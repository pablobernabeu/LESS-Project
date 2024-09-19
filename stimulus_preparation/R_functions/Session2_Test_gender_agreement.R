

# Creation of stimuli for the test administered after the training in gender 
# and number agreement. Participants can only move on to the experiment if 
# they score above 80%, as in González Alonso et al. (2020; 
# https://doi.org/10.1016/j.jneuroling.2020.100939).

# Definition of code items with regard to González Alonso et al. 
# (2020; https://doi.org/10.1016/j.jneuroling.2020.100939).
# 
# `noun`: the noun in the initial determiner phrase in each sentence


Session2_Test_gender_agreement = 
  
  function(stimuli, study_site, language) {
    
    require(dplyr)  # data wrangling
    require(tidyr)  # data wrangling
    require(stringr)  # text processing
    
    # Set initial seed number to ensure reproducibility of functions that
    # produce random variations, such as `sample`. Before those functions, 
    # the same seed will always be set by using the code `set.seed(seed)`
    # or `set.seed(seed + i)` (where `i` is the row number).
    
    seed = 123
    
    # Store all base stimuli, which will be 
    # used towards the end of the script.
    all_stimuli = stimuli
    
    # Select language and select items included in the present session
    stimuli = stimuli[(stimuli$language == language | stimuli$language == 'both') & 
                        stimuli$Session_2 == 'included',]
    
    # Enable gender canonicity column in Mini-Spanish only. This column 
    # is then processed throughout the script using text evaluation, 
    # namely, `!!! rlang::syms`. Thus, for the Mini-Spanish stimuli, 
    # `gender_canonicity` will be read as `Spanish_gender_canonicity`, 
    # whereas in the other languages it will be read as `NULL`.
    
    if(language == 'Mini-Spanish') {
      gender_canonicity = 'Spanish_gender_canonicity'
    } else gender_canonicity = NULL 
    
    
    # Since there must be 36 trials, two lists will be created to ensure that, 
    # overall, across participants, all noun-adjective combinations appear 
    # equally often. The lists are:
    # 
    # List 1: first half of noun-adjective combinations
    # List 2: second half of noun-adjective combinations
    # 
    # Both halves of the nouns contain both genders in equal proportions.
    
    first_half_nouns = c( stimuli[stimuli$noun != '',] %>% 
                            filter(gender == stimuli %>% 
                                     filter(noun != '', gender != '') %>% 
                                     pull(gender) %>% unique() %>% nth(1)) %>%
                            slice_head(n = nrow(.)/2) %>% 
                            pull(noun),
                          
                          stimuli[stimuli$noun != '',] %>% 
                            filter(gender == stimuli %>% 
                                     filter(noun != '', gender != '') %>% 
                                     pull(gender) %>% unique() %>% nth(2)) %>%
                            slice_head(n = nrow(.)/2) %>% 
                            pull(noun) )
    
    second_half_nouns = 
      stimuli[stimuli$noun != '',] %>% 
      filter(!noun %in% first_half_nouns) %>% 
      pull(noun)
    
    first_half_adjectives = 
      stimuli[stimuli$adjective_root != '',] %>% 
      slice_head(n = nrow(.)/2) %>% 
      pull(adjective_root)
    
    second_half_adjectives = 
      stimuli[stimuli$adjective_root != '',] %>% 
      filter(!adjective_root %in% first_half_adjectives) %>% 
      pull(adjective_root)
    
    # Create lists
    combinations = 
      
      # List 1
      stimuli %>% 
      mutate(list = 'List 1: first half of noun-adjective combinations') %>%
      filter(noun %in% first_half_nouns |
               adjective_root %in% first_half_adjectives) %>%
      
      # Add List 2
      rbind(stimuli %>% 
              mutate(list = 'List 2: second half of noun-adjective combinations') %>%
              filter(noun %in% second_half_nouns |
                       adjective_root %in% second_half_adjectives))
    
    
    # In each list, combine all nouns with all adjectives
    
    combinations = combinations %>%
      group_by(list) %>%
      expand(nesting(noun_ID, unprocessed_noun, noun, gender, !!! rlang::syms(gender_canonicity)),
             nesting(adjective_ID, unprocessed_adjective, adjective_root, adjective_contrast_ID)) %>%
      ungroup() %>%
      
      # Remove rows with incomplete combinations
      filter(complete.cases(noun_ID) & complete.cases(adjective_ID))
    
    
    # Ensure every noun (i.e., initial determiner phrase) in 
    # every list appears as often in singular as in plural.
    
    combinations = combinations %>%
      group_by(list, noun) %>%
      mutate(number = rep(c('singular', 'plural'), times = n()/2)) %>%
      ungroup()
    
    
    # Select article based on the gender and the number of the noun.
    # Select verb form based on the number of the noun.
    
    combinations$article = NA
    combinations$verb_type = NA
    combinations$verb = NA
    combinations$correct_adjective = NA
    combinations$adjective_gender_violation = NA
    combinations$adjective_number_violation = NA
    combinations$adjective_gender_number_violation = NA
    combinations$adjective_semantic_violation = NA
    
    for(i in 1:nrow(combinations)) {
      
      # Select contrastive adjective for the semantic violation condition
      
      i_adjective_root = combinations[i, 'adjective_root'] %>% pull
      
      i_contrastive_adjective_ID = stimuli %>%
        filter(adjective_root == i_adjective_root) %>%
        pull(adjective_contrast_ID) %>% unique
      
      contrastive_adjective_root = stimuli %>%
        filter(adjective_contrast_ID == i_contrastive_adjective_ID & 
                 adjective_root != i_adjective_root) %>%
        pull(adjective_root)
      
      # Proceed to create columns
      
      combinations[i,] = combinations[i,] %>%
        
        mutate( article = stimuli[stimuli$article != '' &
                                    stimuli$gender == combinations[i, 'gender'] %>% pull & 
                                    stimuli$number == combinations[i, 'number'] %>% pull, 
                                  'article'],
                
                verb_type = stimuli[stimuli$verb_type != '' &
                                      stimuli$number == combinations[i, 'number'] %>% pull, 
                                    'verb_type'],
                
                verb = stimuli[stimuli$verb != '' &
                                 stimuli$number == combinations[i, 'number'] %>% pull, 
                               'verb'],
                
                # Create the five conditions as in González Alonso et al. 
                # (2020; https://doi.org/10.1016/j.jneuroling.2020.100939)
                # 
                # 1. correct
                # 2. gender violation
                # 3. number violation
                # 4. gender and number violation
                # 5. semantic violation
                
                correct_adjective = 
                  paste0(adjective_root,
                         stimuli[stimuli$adjective_suffix != '' & 
                                   stimuli$gender == combinations[i, 'gender'] %>% pull & 
                                   stimuli$number == combinations[i, 'number'] %>% pull, 
                                 'adjective_suffix']),
                
                adjective_gender_violation = 
                  paste0(adjective_root,
                         stimuli[stimuli$adjective_suffix != '' & 
                                   !stimuli$gender == combinations[i, 'gender'] %>% pull & 
                                   stimuli$number == combinations[i, 'number'] %>% pull, 
                                 'adjective_suffix']),
                
                adjective_number_violation = 
                  paste0(adjective_root,
                         stimuli[stimuli$adjective_suffix != '' & 
                                   stimuli$gender == combinations[i, 'gender'] %>% pull & 
                                   !stimuli$number == combinations[i, 'number'] %>% pull, 
                                 'adjective_suffix']),
                
                adjective_gender_number_violation = 
                  paste0(adjective_root,
                         stimuli[stimuli$adjective_suffix != '' & 
                                   !stimuli$gender == combinations[i, 'gender'] %>% pull & 
                                   !stimuli$number == combinations[i, 'number'] %>% pull, 
                                 'adjective_suffix']),
                
                adjective_semantic_violation = 
                  paste0(contrastive_adjective_root,
                         stimuli[stimuli$adjective_suffix != '' & 
                                   stimuli$gender == combinations[i, 'gender'] %>% pull & 
                                   stimuli$number == combinations[i, 'number'] %>% pull, 
                                 'adjective_suffix'])
        )
    }
    
    # In Mini-Norwegian, append articles to nouns
    if(language == 'Mini-Norwegian') {
      combinations = combinations %>%
        mutate(noun_with_article = paste0(noun, article))
    }
    
    # Resolve repetition of vowels at the end of adjectives
    
    combinations$correct_adjective = 
      combinations$correct_adjective %>%
      str_replace_all(c('eeju$' = 'eju', 'eezu$' = 'ezu',
                        'eejur$' = 'ejur', 'eezur$' = 'ezur'))
    
    combinations = combinations %>%
      mutate(across(ends_with('_violation'),  # all columns ending with '_violation'
                    ~str_replace_all(., c('eeju$' = 'eju', 'eezu$' = 'ezu',
                                          'eejur$' = 'ejur', 'eezur$' = 'ezur'))))
    
    
    # Compose sentences tailored to the language to adjust the location of the articles
    
    combinations$correct_sentence = NA
    combinations$sentence_gender_violation = NA
    combinations$sentence_number_violation = NA
    combinations$sentence_gender_number_violation = NA
    combinations$sentence_semantic_violation = NA
    
    for(i in 1:nrow(combinations)) {
      
      combinations[i,] = combinations[i,] %>%
        
        mutate( correct_sentence =
                  ifelse(language == 'Mini-Norwegian',
                         paste0(noun_with_article, ' ', verb, ' ', correct_adjective, '.'),
                         ifelse(language == 'Mini-English' | language == 'Mini-Spanish',
                                paste0(article, ' ', noun, ' ', verb, ' ', correct_adjective, '.'),
                                NA)) %>% 
                  str_to_sentence(),  # Make first letter in sentence uppercase
                
                sentence_gender_violation =
                  ifelse(language == 'Mini-Norwegian',
                         paste0(noun_with_article, ' ', verb, ' ', adjective_gender_violation, '.'),
                         ifelse(language == 'Mini-English' | language == 'Mini-Spanish',
                                paste0(article, ' ', noun, ' ', verb, ' ', adjective_gender_violation, '.'),
                                NA)) %>% 
                  str_to_sentence(),  # Make first letter in sentence uppercase
                
                sentence_number_violation =
                  ifelse(language == 'Mini-Norwegian',
                         paste0(noun_with_article, ' ', verb, ' ', adjective_number_violation, '.'),
                         ifelse(language == 'Mini-English' | language == 'Mini-Spanish',
                                paste0(article, ' ', noun, ' ', verb, ' ', adjective_number_violation, '.'),
                                NA)) %>% 
                  str_to_sentence(),  # Make first letter in sentence uppercase
                
                sentence_gender_number_violation =
                  ifelse(language == 'Mini-Norwegian',
                         paste0(noun_with_article, ' ', verb, ' ', adjective_gender_number_violation, '.'),
                         ifelse(language == 'Mini-English' | language == 'Mini-Spanish',
                                paste0(article, ' ', noun, ' ', verb, ' ', adjective_gender_number_violation, '.'),
                                NA)) %>% 
                  str_to_sentence(),  # Make first letter in sentence uppercase
                
                sentence_semantic_violation =
                  ifelse(language == 'Mini-Norwegian',
                         paste0(noun_with_article, ' ', verb, ' ', adjective_semantic_violation, '.'),
                         ifelse(language == 'Mini-English' | language == 'Mini-Spanish',
                                paste0(article, ' ', noun, ' ', verb, ' ', adjective_semantic_violation, '.'),
                                NA)) %>% 
                  str_to_sentence()  # Make first letter in sentence uppercase
        )
      
      
      # Distribute the five sentences randomly across five numbered sentences 
      # to show the five conditions (i.e., correct, gender violation, etc.) 
      # in different orders across trials.
      
      # Set seed number to ensure reproducibility, and add row number to 
      # the seed number to create different combinations across rows.
      set.seed(seed + i)
      
      random_order = sample(c('correct_sentence', 
                              'sentence_gender_violation', 
                              'sentence_number_violation', 
                              'sentence_gender_number_violation', 
                              'sentence_semantic_violation'), 
                            replace = FALSE)
      
      combinations[i, 'sentence_1_grammaticality'] = random_order[1]
      combinations[i, 'sentence_2_grammaticality'] = random_order[2]
      combinations[i, 'sentence_3_grammaticality'] = random_order[3]
      combinations[i, 'sentence_4_grammaticality'] = random_order[4]
      combinations[i, 'sentence_5_grammaticality'] = random_order[5]
      
      combinations[i, 'sentence_1'] = combinations[i, random_order[1]]
      combinations[i, 'sentence_2'] = combinations[i, random_order[2]]
      combinations[i, 'sentence_3'] = combinations[i, random_order[3]]
      combinations[i, 'sentence_4'] = combinations[i, random_order[4]]
      combinations[i, 'sentence_5'] = combinations[i, random_order[5]]
    }
    
    
    # Following González Alonso et al. (2020), ensure that half of 
    # the trials in plural contain an image with two items and the 
    # other half contain an image with three items. 
    
    combinations = combinations %>%
      group_by(list) %>%
      mutate(number_of_items_in_image = 
               ifelse(number == 'singular', 1, 
                      ifelse(number == 'plural', rep(2:3, each = n()/4),
                             ''))) %>%
      ungroup()
    
    
    # For each trial, create labels that will be used to name the corresponding 
    # images. These labels will be formed of the nouns and adjectives in English.
    # To allow this also in the Mini-Norwegian and Mini-Spanish files, the 
    # English equivalents of the words will be used.
    
    combinations$noun_image = NA
    combinations$adjective_image = NA
    
    
    if(language == 'Mini-English') {
      
      for(i in 1:nrow(combinations)) {
        
        # Compose labels by specifying number of items in image
        combinations[i,] = combinations[i,] %>%
          mutate( noun_image = paste0(unprocessed_noun, 
                                      ifelse(number_of_items_in_image == 1, '', 
                                             paste0('_', number_of_items_in_image)),
                                      '.png'),
                  adjective_image = paste0(unprocessed_adjective, '.png') )
      }
      
      
      # If language is not Mini-English, fetch English translations
    } else {
      
      for(i in 1:nrow(combinations)) {
        
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
        
        i_adjective_ID = all_stimuli %>% 
          filter(unprocessed_adjective == 
                   combinations[i,] %>% 
                   pull(unprocessed_adjective)) %>%
          pull(adjective_ID)
        
        i_adjective_label = all_stimuli %>% 
          filter(language == 'Mini-English',
                 adjective_ID == i_adjective_ID) %>%
          pull(unprocessed_adjective)
        
        # Compose labels by specifying number of items in image
        combinations[i,] = combinations[i,] %>%
          mutate( noun_image = paste0(i_noun_label, 
                                      ifelse(number_of_items_in_image == 1, '', 
                                             paste0('_', number_of_items_in_image)), 
                                      '.png'),
                  adjective_image = paste0(i_adjective_label, '.png') )
      }
    }
    
    
    # Certain stimuli and experimental conditions should appear equally often to 
    # prevent repetition effects. To ascertain this, check whether all elements 
    # in certain columns appear equally often. If they do not, show warnings.
    # Please note that this basic check only helps prevent blatant disparities, 
    # but it does not verify all the controls that have been applied.
    
    columns_to_check = c('noun', 'correct_adjective', 'adjective_gender_violation',
                         'adjective_number_violation', 'adjective_gender_number_violation', 
                         'adjective_semantic_violation')
    
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
    
    combinations = combinations %>%
      
      # Add version and language
      mutate( materials_version, language,
              
              # To save space in the data set, replace NAs with blanks
              across(names(.), ~replace(., is.na(.), '')) ) %>%
      
      # Store key components associated with each sentence
      select(materials_version, language, list, gender, 
             !!! rlang::syms(gender_canonicity), number, correct_sentence, 
             sentence_1, sentence_2, sentence_3, sentence_4, sentence_5, 
             noun_image, adjective_image)
    
    
    # Output: save lists in different files
    
    combinations %>% 
      filter(list == combinations %>% 
               filter(complete.cases(list), list != '') %>% 
               pull(list) %>% unique() %>% nth(1) | 
               list == '') %>%
      write.csv(paste0('session_materials/Session 2/stimuli/test/', 
                       study_site, ' site, ', language, ', ',
                       'Session2_Test_gender_agreement, List 1.csv'), 
                row.names = FALSE, fileEncoding = 'UTF-8')
    
    combinations %>% 
      filter(list == combinations %>% 
               filter(complete.cases(list), list != '') %>% 
               pull(list) %>% unique() %>% nth(2) | 
               list == '') %>%
      write.csv(paste0('session_materials/Session 2/stimuli/test/', 
                       study_site, ' site, ', language, ', ',
                       'Session2_Test_gender_agreement, List 2.csv'), 
                row.names = FALSE, fileEncoding = 'UTF-8')
    
  }

