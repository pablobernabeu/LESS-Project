

# Creation of stimuli for the test administered after the training in verb-object
# agreement (VOA). Participants can only move on to the experiment if they score 
# above 80%, as in González Alonso et al. (2020; 
# https://doi.org/10.1016/j.jneuroling.2020.100939).

# Example sentence structure: 'Amelia chosevo fi zer truck'

# There are five conditions, inspired by González Alonso et al. (2020; 
# https://doi.org/10.1016/j.jneuroling.2020.100939):
# 
# 1. correct
# 2. VOA violation
# 3. number violation
# 4. article location violation
# 5. noun with semantic violation (i.e., noun different from image)


Session4_Test_verb_object_agreement = 
  
  function(stimuli, study_site, language) {
    
    require(dplyr)    # data wrangling
    require(tidyr)    # data wrangling
    require(stringr)  # text processing
    require(magick)   # image processing
    
    # Set initial seed number to ensure reproducibility of functions that
    # produce random variations, such as `sample`. Before those functions, 
    # the same seed will always be set by using the code `set.seed(seed)`
    # or `set.seed(seed + i)` (where `i` is the row number).
    
    seed = 123
    
    # Store all base stimuli, which will be 
    # used towards the end of the script.
    all_stimuli = stimuli
    
    # Select language, select items included in the present session, and remove 
    # nouns that were included in Session 2. That is, the nouns that were not 
    # used in Session 2 (focussed on gender agreement), and were used in 
    # Session 3 for the content on differential object marking, are now used in 
    # Session 4 for the content on verb-object agreement. In this way, the 
    # nouns are rotated across the grammatical properties and the sessions.
    
    stimuli = stimuli[(stimuli$language == language | stimuli$language == 'both') & 
                        stimuli$Session_4 == 'included' &
                        !(stimuli$noun != '' & stimuli$Session_2 == 'included'),]
    
    
    # Since there must be 36 trials, two lists will be created to ensure that, 
    # overall, across participants, all verb-noun combinations appear equally 
    # often. The lists are:
    # 
    # List 1: first half of verb-noun combinations
    # List 2: second half of verb-noun combinations
    # 
    # Both halves of the nouns contain both genders in equal proportions.
    
    first_half_verbs = 
      stimuli[stimuli$verb_type == 'transitive',] %>% 
      slice_head(n = nrow(.)/2) %>% 
      pull(verb)
    
    second_half_verbs = 
      stimuli[stimuli$verb_type == 'transitive',] %>% 
      filter(!verb %in% first_half_verbs) %>% 
      pull(verb)
    
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
    
    # Create lists
    combinations = 
      
      # List 1
      stimuli %>% 
      mutate(list = 'List 1: first half of verb-noun combinations') %>%
      filter(verb %in% first_half_verbs | noun %in% first_half_nouns) %>%
      
      # Add List 2
      rbind(stimuli %>% 
              mutate(list = 'List 2: second half of verb-noun combinations') %>%
              filter(verb %in% second_half_verbs | noun %in% second_half_nouns))
    
    
    # In each list, combine all transitive verbs with all nouns
    
    combinations = combinations %>% 
      group_by(list) %>%
      filter(verb_type == 'transitive' | complete.cases(noun_ID)) %>% 
      expand(nesting(verb_ID, verb_type, verb, verb_contrast_ID),
             nesting(noun_ID, unprocessed_noun, noun, gender)) %>%
      ungroup() %>%
      
      # Remove rows with incomplete combinations
      filter(complete.cases(verb_ID) & complete.cases(noun_ID)) %>%
      
      # Counterbalance plural and singular within nouns
      group_by(noun) %>%
      mutate(number = rep(c('plural', 'singular'), times = n()/2)) %>%
      ungroup()
    
    
    # Incorporate person names, which will be the subjects of the 
    # transitive sentences. The names are randomised within lists 
    # and within pairs of verbs (e.g., 'chose' / 'refused'). The 
    # other member of each pair of verbs is added further down.
    
    # Ensure reproducibility
    set.seed(seed)
    
    combinations = combinations %>%
      group_by(list, verb_contrast_ID) %>%
      mutate( person = 
                stimuli[stimuli$person != '', 'person'] %>%
                sample(replace = FALSE) ) %>%
      ungroup()
    
    
    # Get noun for the semantic violation condition by taking the next noun in the list 
    # of unique nouns. This process is circular. That is, for the the last noun in the 
    # list, the noun fetched is the first noun in the list.
    
    # Create circular shift function to iterate over row numbers circularly
    # (source: https://stackoverflow.com/a/30542172/7050882).
    
    shifter = function(x, n = 1) {
      if(n == 0) x else c(tail(x, -n), head(x, n))
    }
    
    combinations_temp = combinations[0,]
    
    for(i in seq_along(combinations %>% distinct(list) %>% pull)) {
      
      i_list = 
        combinations[combinations$list != '',] %>% 
        pull(list) %>% unique() %>% nth(i)
      
      nouns = combinations %>% filter(list == i_list) %>% select(noun)
      
      noun_semantic_violation = 
        nouns %>% slice(rep(shifter(1:nrow(nouns), 1))) %>%
        rename(noun_semantic_violation = noun)
      
      # Find out gender of `noun_semantic_violation`
      gender_noun_semantic_violation = as.character()
      
      for(i in 1:nrow(noun_semantic_violation)) {
        gender_noun_semantic_violation = 
          rbind( gender_noun_semantic_violation, 
                 stimuli %>% 
                   filter(noun == noun_semantic_violation %>% 
                            slice(i) %>% pull(noun_semantic_violation)) %>%
                   pull(gender) )
      }
      
      combinations_temp = 
        rbind( combinations_temp, 
               cbind(combinations %>% filter(list == i_list), 
                     noun_semantic_violation,
                     gender_noun_semantic_violation = gender_noun_semantic_violation[,1]) )
    }
    
    # Consolidate result
    combinations = combinations_temp
    rm(combinations_temp)
    
    
    # Get article for each condition and compose sentences.
    # The morphemes of verb object agreement and differential 
    # object marking are inserted at this point. 
    
    DOM_morpheme = stimuli[stimuli$DOM_morpheme != '', 'DOM_morpheme']
    VOA_morpheme_temp = stimuli[stimuli$VOA_morpheme != '', 'VOA_morpheme']
    
    combinations$VOA_morpheme = NA
    combinations$VOA_morpheme_violation = NA
    combinations$article = NA
    combinations$article_number_violation = NA
    combinations$article_semantic_violation = NA
    combinations$correct_sentence = NA
    combinations$sentence_VOA_violation = NA
    combinations$sentence_number_violation = NA
    combinations$sentence_article_location_violation = NA
    combinations$sentence_semantic_violation = NA
    
    for(i in 1:nrow(combinations)) {
      
      combinations[i,] = combinations[i,] %>% mutate( 
        
        VOA_morpheme = 
          case_when( number == 'plural' & 
                       str_detect(verb, '[aeiou]$') ~ VOA_morpheme_temp,
                     # Add epenthetic vowel 'e' when verb ends in consonant
                     number == 'plural' & 
                       str_detect(verb, '[^aeiou]$') ~ paste0('e', VOA_morpheme_temp), 
                     .default = '' ),
        
        VOA_morpheme_violation = 
          case_when( number == 'singular' & 
                       str_detect(verb, '[aeiou]$') ~ VOA_morpheme_temp,
                     # Add epenthetic vowel 'e' when verb ends in consonant
                     number == 'singular' & 
                       str_detect(verb, '[^aeiou]$') ~ paste0('e', VOA_morpheme_temp), 
                     .default = '' ),
        
        article = 
          paste0(stimuli[stimuli$article != '' & 
                           stimuli$gender == combinations[i, 'gender'] & 
                           stimuli$number == combinations[i, 'number'], 
                         'article']),
        
        article_number_violation = 
          paste0(stimuli[stimuli$article != '' & 
                           stimuli$gender == combinations[i, 'gender'] & 
                           !stimuli$number == combinations[i, 'number'], 
                         'article']),
        
        article_semantic_violation = 
          paste0(stimuli[stimuli$article != '' & 
                           stimuli$gender == combinations[i, 'gender_noun_semantic_violation'] & 
                           stimuli$number == combinations[i, 'number'], 
                         'article']),
        
        # Compose sentences tailored to the language to adjust the location of the articles
        
        correct_sentence =
          ifelse(language == 'Mini-Norwegian',
                 paste0(person, ' ', paste0(verb, VOA_morpheme), ' ', DOM_morpheme, ' ', 
                        paste0(noun, article), '.'),
                 ifelse(language == 'Mini-English' | language == 'Mini-Spanish',
                        paste0(person, ' ', paste0(verb, VOA_morpheme), ' ', DOM_morpheme, ' ', 
                               article, ' ', noun, '.'),
                        NA)) %>% 
          str_to_sentence(),  # Make first letter in sentence uppercase
        
        sentence_VOA_violation =
          ifelse(language == 'Mini-Norwegian',
                 paste0(person, ' ', paste0(verb, VOA_morpheme_violation), ' ', DOM_morpheme, ' ', 
                        paste0(noun, article), '.'),
                 ifelse(language == 'Mini-English' | language == 'Mini-Spanish',
                        paste0(person, ' ', paste0(verb, VOA_morpheme_violation), ' ', 
                               DOM_morpheme, ' ', article, ' ', noun, '.'),
                        NA)) %>% 
          str_to_sentence(),  # Make first letter in sentence uppercase
        
        sentence_number_violation =
          ifelse(language == 'Mini-Norwegian',
                 paste0(person, ' ', paste0(verb, VOA_morpheme), ' ', DOM_morpheme, ' ', 
                        paste0(noun, article_number_violation), '.'),
                 ifelse(language == 'Mini-English' | language == 'Mini-Spanish',
                        paste0(person, ' ', paste0(verb, VOA_morpheme), ' ', DOM_morpheme, ' ', 
                               article_number_violation, ' ', noun, '.'),
                        NA)) %>% 
          str_to_sentence(),  # Make first letter in sentence uppercase
        
        sentence_article_location_violation =
          ifelse(language == 'Mini-Norwegian',
                 paste0(person, ' ', paste0(verb, VOA_morpheme), ' ', DOM_morpheme, ' ', 
                        article, noun, '.'),
                 ifelse(language == 'Mini-English' | language == 'Mini-Spanish',
                        paste0(person, ' ', paste0(verb, VOA_morpheme), ' ', DOM_morpheme, ' ', 
                               article, noun, '.'),
                        NA)) %>% 
          str_to_sentence(),  # Make first letter in sentence uppercase
        
        sentence_semantic_violation =
          ifelse(language == 'Mini-Norwegian',
                 paste0(person, ' ', paste0(verb, VOA_morpheme), ' ', DOM_morpheme, ' ', 
                        paste0(noun_semantic_violation, article_semantic_violation), '.'),
                 ifelse(language == 'Mini-English' | language == 'Mini-Spanish',
                        paste0(person, ' ', paste0(verb, VOA_morpheme), ' ', DOM_morpheme, ' ', 
                               article_semantic_violation, ' ', noun_semantic_violation, '.'),
                        NA)) %>% 
          str_to_sentence()  # Make first letter in sentence uppercase
      )
      
      
      # Distribute the five sentences randomly across five numbered sentences 
      # so that they can be presented in varying orders across trials.
      
      # Set seed number to ensure reproducibility, and add row number to 
      # the seed number to create different combinations across rows.
      set.seed(seed + i)
      
      random_order = sample(c('correct_sentence', 
                              'sentence_VOA_violation', 
                              'sentence_number_violation', 
                              'sentence_article_location_violation', 
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
    # images. These labels will be formed of the names, verbs and nouns in 
    # English. To allow this also in the Mini-Norwegian and Mini-Spanish files, 
    # the English equivalents of the words will be used. In addition, the 
    # images will be created.
    
    # Placeholder images entered below to prevent error in OpenSesame due to
    # differences between target and filler trials.
    
    combinations$verb_image_1 = 'NA.png'
    combinations$verb_image_2 = 'NA.png'
    combinations$verb_image_3 = 'NA.png'
    combinations$verb_image = 'NA.png'
    combinations$noun_image = 'NA.png'
    
    image_dir = paste0('session_materials/images/', study_site, ' site/')
    
    
    if(language == 'Mini-English') {
      
      # Iterate over rows
      for(i in 1:nrow(combinations)) {
        
        combinations[i,] = combinations[i,] %>%
          mutate( verb_image_1 = paste0(verb, '_1', '.png'), 
                  verb_image_2 = paste0(verb, '_2', '.png'), 
                  verb_image_3 = paste0(verb, '_3', '.png'), 
                  verb_image = paste0(verb, '.png'), 
                  noun_image = paste0(noun, 
                                      ifelse(number_of_items_in_image == 1, '', 
                                             paste0('_', number_of_items_in_image)), 
                                      '.png') )
        
        # Compose image for the verb, distributed into four frames, 
        # which will be sequentially presented in OpenSesame.
        
        image_read(paste0(image_dir, 'background.png')) %>%
          image_composite(
            image_read(paste0(image_dir, combinations[i, 'verb'], '.png')) %>% 
              image_scale('120x120') %>%
              image_convert(colorspace = 'gray'),  # render verb image in gray scale,
            gravity = 'center', offset = '+0-310') %>%  # place image at the top
          image_write(., paste0(image_dir, combinations[i, 'verb_image_1']),
                      format = 'PNG')
        
        image_read(paste0(image_dir, 'background.png')) %>%
          image_composite(
            image_read(paste0(image_dir, combinations[i, 'verb'], '.png')) %>% 
              image_scale('250x250') %>%
              image_convert(colorspace = 'gray'),  # render verb image in gray scale,
            gravity = 'center', offset = '+0-310') %>%  # place image at the top
          image_write(., paste0(image_dir, combinations[i, 'verb_image_2']),
                      format = 'PNG')
        
        image_read(paste0(image_dir, 'background.png')) %>%
          image_composite(
            image_read(paste0(image_dir, combinations[i, 'verb'], '.png')) %>% 
              image_scale('500x500') %>%
              image_convert(colorspace = 'gray'),  # render verb image in gray scale,
            gravity = 'center', offset = '+0-180') %>%  # place image further down
          image_write(., paste0(image_dir, combinations[i, 'verb_image_3']),
                      format = 'PNG')
        
        image_read(paste0(image_dir, 'background.png')) %>%
          image_composite(
            image_read(paste0(image_dir, combinations[i, 'verb'], '.png')) %>% 
              image_scale('720x720') %>% 
              image_convert(colorspace = 'gray'),  # render verb image in gray scale,
            gravity = 'center') %>%  # place image at the centre
          image_write(., paste0(image_dir, combinations[i, 'verb_image']),
                      format = 'PNG')
      }
      
      
      # If language is not Mini-English, fetch English translations. 
      # The images are not composed again because they would be the same.
    } else {
      
      for(i in 1:nrow(combinations)) {
        
        # For each element, store IDs and fetch the 
        # English counterparts matching those IDs.
        
        i_verb_ID = all_stimuli %>% 
          filter(verb == combinations[i,] %>% pull(verb)) %>%
          pull(verb_ID)
        
        i_verb_label = all_stimuli %>% 
          filter(language == 'Mini-English', 
                 verb_ID == i_verb_ID) %>%
          pull(verb)
        
        i_noun_ID = all_stimuli %>% 
          filter(unprocessed_noun == 
                   combinations[i,] %>% 
                   pull(unprocessed_noun)) %>%
          pull(noun_ID)
        
        i_noun_label = all_stimuli %>% 
          filter(language == 'Mini-English', 
                 noun_ID == i_noun_ID) %>%
          pull(unprocessed_noun)
        
        combinations[i,] = combinations[i,] %>%
          mutate( verb_image_1 = paste0(i_verb_label, '_1', '.png'),
                  verb_image_2 = paste0(i_verb_label, '_2', '.png'),
                  verb_image_3 = paste0(i_verb_label, '_3', '.png'),
                  verb_image = paste0(i_verb_label, '.png'),
                  noun_image = paste0(i_noun_label, 
                                      ifelse(number_of_items_in_image == 1, '', 
                                             paste0('_', number_of_items_in_image)), 
                                      '.png') )
      }
    }
    
    
    # Certain stimuli and experimental conditions should appear equally often to 
    # prevent repetition effects. To ascertain this, check whether all elements 
    # in certain columns appear equally often. If they do not, show warnings.
    # Please note that this basic check only helps prevent blatant disparities, 
    # but it does not verify all the controls that have been applied.
    
    columns_to_check = c('person', 'verb', 'noun')
    
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
      select(materials_version, language, list, gender, person, correct_sentence, 
             sentence_1, sentence_2, sentence_3, sentence_4, sentence_5, 
             verb_image_1, verb_image_2, verb_image_3, verb_image, noun_image)
    
    
    # Output: save lists in different files. Each file also includes 
    # the fillers, which don't have any list assigned.
    
    combinations %>% 
      filter(list == combinations %>% 
               filter(complete.cases(list), list != '') %>% 
               pull(list) %>% unique() %>% nth(1) | 
               list == '') %>%
      write.csv(paste0('session_materials/Session 4/stimuli/test/', 
                       study_site, ' site, ', language, ', ',
                       'Session4_Test_verb_object_agreement, List 1.csv'), 
                row.names = FALSE, fileEncoding = 'UTF-8')
    
    combinations %>% 
      filter(list == combinations %>% 
               filter(complete.cases(list), list != '') %>% 
               pull(list) %>% unique() %>% nth(2) | 
               list == '') %>%
      write.csv(paste0('session_materials/Session 4/stimuli/test/', 
                       study_site, ' site, ', language, ', ',
                       'Session4_Test_verb_object_agreement, List 2.csv'), 
                row.names = FALSE, fileEncoding = 'UTF-8')
    
  }

