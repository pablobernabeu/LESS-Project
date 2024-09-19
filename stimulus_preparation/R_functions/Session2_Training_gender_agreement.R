

# Creation of stimuli for the training part of Session 2 (gender agreement)

# Target trials comprise two sentences containing the grammatical property of interest.
# Example structure: 'Ze truck is redezu' | 'Ze truck is amarillezu'
# 
# Filler trials comprise two sentences containing a locative adverbial clause.
# Example structure: 'Ze truck is above je key' | 'Ze truck is above jer table'

# Definition of code items with regard to González Alonso et al. 
# (2020; https://doi.org/10.1016/j.jneuroling.2020.100939).
# 
# `noun1`: the noun in the initial determiner phrase in each sentence
# 
# `noun2`: the second noun in locative filler sentences


Session2_Training_gender_agreement = 
  
  function(stimuli, study_site, language) {
    
    require(dplyr)  # data wrangling
    require(tidyr)  # data wrangling
    require(stringr)  # text processing
    require(magick)   # image processing
    
    # Store all base stimuli, which will be 
    # used towards the end of the script.
    all_stimuli = stimuli
    
    # Select language and select items included in the present session
    stimuli = stimuli[(stimuli$language == language | stimuli$language == 'both') & 
                        stimuli$Session_2 == 'included',]
    
    # Enable gender canonicity column in Mini-Spanish only. This column 
    # is then processed throughout the code using text evaluation, 
    # namely, `!!! rlang::syms`. Thus, for the Mini-Spanish stimuli, 
    # `gender_canonicity` will be read as `Spanish_gender_canonicity`, 
    # whereas in the other languages it will be read as `NULL`.
    
    if(language == 'Mini-Spanish') {
      gender_canonicity = 'Spanish_gender_canonicity'
    } else gender_canonicity = NULL 
    
    
    # Combine all nouns with all adjectives. This combination only includes the first 
    # adjective from each pair of contrastive adjectives (e.g., 'big' but not 'small'). 
    # In a subsequent step, the contrastive adjectives will be incorporated to form 
    # pairs of sentences, as in Figure 2 in González Alonso et al. (2020; 
    # https://doi.org/10.1016/j.jneuroling.2020.100939).
    
    combinations = stimuli %>%
      
      # Select nouns and first half of the adjectives
      filter(noun != '' | !duplicated(adjective_contrast_ID)) %>% 
      
      # Make the combinations
      expand(nesting(noun_ID, unprocessed_noun, noun, gender, !!! rlang::syms(gender_canonicity)),
             nesting(adjective_ID, unprocessed_adjective, adjective_root, adjective_contrast_ID)) %>%
      
      # Remove rows with incomplete combinations
      filter(complete.cases(noun_ID) & complete.cases(adjective_ID)) %>%
      
      # Rename 'noun' columns as 'noun1' and 'adjective' columns as 'adjective1' 
      # to allow for contrastive and filler sentences that will be added later.
      
      rename(noun1_ID = noun_ID, 
             unprocessed_noun1 = unprocessed_noun, 
             noun1 = noun,
             noun1_gender = gender,
             adjective1_ID = adjective_ID, 
             unprocessed_adjective1 = unprocessed_adjective, 
             adjective1_root = adjective_root)
    
    
    # Following González Alonso et al. (2020), create lists to counterbalance 
    # grammatical number across noun-adjective combinations. The lists are:
    # 
    # List 1: singular, plural
    # List 2: plural, singular
    # 
    # In each list, all nouns and all adjectives appear equally often. 
    # These lists will be administered to different participants. 
    
    combinations = 
      
      # List 1
      combinations %>% 
      mutate(list = 'List 1: singular, plural',
             noun1_number = rep(c('singular', 'plural'), times = n()/2)) %>%
      
      # Add List 2
      rbind( combinations %>% 
               mutate(list = 'List 2: plural, singular',
                      noun1_number = rep(c('plural', 'singular'), times = n()/2)) )
    
    
    # Select article based on the gender and the number of noun1.
    # Select verb form based on the number of noun1.
    # Append ending to adjective based on the characteristics of noun1.
    # Incorporate contrastive adjective ('adjective2').
    
    combinations$article = NA
    combinations$verb = NA
    combinations$adjective1 = NA
    combinations$adjective2_ID = NA
    combinations$unprocessed_adjective2 = NA
    combinations$adjective2_root = NA
    combinations$adjective2 = NA
    
    for(i in 1:nrow(combinations)) {
      
      combinations[i,] = combinations[i,] %>% mutate( 
        
        article = stimuli[stimuli$article != '' &
                            stimuli$gender == combinations[i, 'noun1_gender'] %>% pull & 
                            stimuli$number == combinations[i, 'noun1_number'] %>% pull, 
                          'article'],
        
        verb = stimuli[stimuli$verb != '' &
                         stimuli$number == combinations[i, 'noun1_number'] %>% pull, 
                       'verb'],
        
        adjective1 = 
          paste0(adjective1_root,
                 stimuli[stimuli$adjective_suffix != '' & 
                           stimuli$gender == combinations[i, 'noun1_gender'] %>% pull & 
                           stimuli$number == combinations[i, 'noun1_number'] %>% pull, 
                         'adjective_suffix']),
        
        adjective2_ID = 
          stimuli[stimuli$adjective_contrast_ID == combinations[i, 'adjective_contrast_ID'] %>% pull &
                    stimuli$adjective_root != combinations[i, 'adjective1_root'] %>% pull,
                  'adjective_ID'],
        
        unprocessed_adjective2 = 
          stimuli[stimuli$adjective_contrast_ID == combinations[i, 'adjective_contrast_ID'] %>% pull &
                    stimuli$adjective_root != combinations[i, 'adjective1_root'] %>% pull,
                  'unprocessed_adjective'],
        
        adjective2_root = 
          stimuli[stimuli$adjective_suffix != '' &
                    stimuli$gender == combinations[i, 'noun1_gender'] %>% pull &
                    stimuli$number == combinations[i, 'noun1_number'] %>% pull,
                  'adjective_suffix'],
        
        adjective2 = 
          paste0(stimuli[stimuli$adjective_contrast_ID == combinations[i, 'adjective_contrast_ID'] %>% pull &
                           stimuli$adjective_root != combinations[i, 'adjective1_root'] %>% pull, 
                         'adjective_root'],
                 stimuli[stimuli$adjective_suffix != '' & 
                           stimuli$gender == combinations[i, 'noun1_gender'] %>% pull & 
                           stimuli$number == combinations[i, 'noun1_number'] %>% pull, 
                         'adjective_suffix'])
      )
    }
    
    
    # In Mini-Norwegian, append articles to nouns
    if(language == 'Mini-Norwegian') {
      combinations = combinations %>%
        mutate(noun1_with_article = paste0(noun1, article))
    }
    
    # Resolve repetition of vowels at the end of adjectives
    
    combinations$adjective1 = combinations$adjective1 %>%
      str_replace_all(c('eeju$' = 'eju', 'eezu$' = 'ezu',
                        'eejur$' = 'ejur', 'eezur$' = 'ezur'))
    
    combinations$adjective2 = combinations$adjective2 %>%
      str_replace_all(c('eeju$' = 'eju', 'eezu$' = 'ezu',
                        'eejur$' = 'ejur', 'eezur$' = 'ezur'))
    
    # Compose sentences depending on the language to adjust 
    # the locations of the articles.
    
    combinations$sentence1 = NA
    combinations$sentence2 = NA
    
    for(i in 1:nrow(combinations)) {
      
      combinations[i,] = combinations[i,] %>% mutate( 
        
        sentence1 = ifelse(language == 'Mini-Norwegian',
                           paste0(noun1_with_article, ' ', verb, ' ', adjective1, '.'),
                           ifelse(language == 'Mini-English' | language == 'Mini-Spanish',
                                  paste0(article, ' ', noun1, ' ', verb, ' ', adjective1, '.'),
                                  NA)) %>% 
          str_to_sentence(),  # Make first letter in sentence uppercase
        
        sentence2 = ifelse(language == 'Mini-Norwegian',
                           paste0(noun1_with_article, ' ', verb, ' ', adjective2, '.'),
                           ifelse(language == 'Mini-English' | language == 'Mini-Spanish',
                                  paste0(article, ' ', noun1, ' ', verb, ' ', adjective2, '.'),
                                  NA)) %>% 
          str_to_sentence()  # Make first letter in sentence uppercase
      )
    }
    
    
    
    # Create locative sentences that will be used as fillers. In each screen, 
    # there will be two sentences that have the same noun as the subject. The 
    # nouns in the locative adverbials will differ between the sentences.
    # Example structure of first sentence: 'Je bag is above je wall.'
    # Example structure of second sentence: 'Je bag is above zer pencil.'
    
    # Create basic structure without any rows. In Mini-Norwegian, include 
    # nouns with articles. 
    
    if(language == 'Mini-Norwegian') {
      locative_combinations = 
        data.frame(gender = NA, noun1_ID = NA, unprocessed_noun1 = NA, noun1 = NA, 
                   noun1_with_article = NA, noun2_ID = NA, unprocessed_noun2 = NA, 
                   noun2 = NA, noun2_with_article = NA) %>%
        slice(0) 
      
    } else {
      locative_combinations = 
        data.frame(gender = NA, noun1_ID = NA, unprocessed_noun1 = NA, noun1 = NA, 
                   noun2_ID = NA, unprocessed_noun2 = NA, noun2 = NA) %>%
        slice(0)
    }
    
    # Select noun2 and noun3 for each trial
    
    # Create basic structure with the nouns ordered alphabetically
    nouns = stimuli %>% 
      filter(noun != '') %>%
      select(gender, noun_ID, unprocessed_noun, noun) %>% 
      arrange(noun)  # order rows by `noun` column
    
    # First, in the `noun1` column, the nouns are repeated 
    # in alphabetical order six times, to obtain 72 rows.
    
    noun1 = nouns %>% 
      slice(rep(1:n(), times = 6)) %>%
      rename(noun1_ID = noun_ID, 
             unprocessed_noun1 = unprocessed_noun,
             noun1 = noun,
             noun1_gender = gender)
    
    # Next, in the `noun2` and `noun3` columns, the nouns are distributed in alphabetical order 
    # as well, but in rotated ways. Specifically, for `noun2`, the first sequence of nouns 
    # begins with the second noun and continues circularly up to the first noun (i.e., 'bat', 
    # 'bed', 'book', 'boy', 'bun', 'ball'). Next, the second sequence begins with the third 
    # noun, while the third sequence begins with the fourth noun, and so on. For `noun3`, the 
    # procedure is the same but begins from one position forward. 
    
    # Create circular shift function to iterate over row numbers circularly
    # (source: https://stackoverflow.com/a/30542172/7050882).
    
    shifter = function(x, n = 1) {
      if(n == 0) x else c(tail(x, -n), head(x, n))
    }
    
    noun2 = rbind(
      nouns %>% slice(rep(shifter(1:nrow(nouns), 1))),  # shift one row forward
      nouns %>% slice(rep(shifter(1:nrow(nouns), 2))),  # shift two rows forward
      nouns %>% slice(rep(shifter(1:nrow(nouns), 3))),
      nouns %>% slice(rep(shifter(1:nrow(nouns), 4))),
      nouns %>% slice(rep(shifter(1:nrow(nouns), 5))),
      nouns %>% slice(rep(shifter(1:nrow(nouns), 6)))
    ) %>%
      rename(noun2_ID = noun_ID, 
             unprocessed_noun2 = unprocessed_noun,
             noun2 = noun,
             noun2_gender = gender)
    
    noun3 = rbind(
      nouns %>% slice(rep(shifter(1:nrow(nouns), 2))),  # shift two rows forward
      nouns %>% slice(rep(shifter(1:nrow(nouns), 3))),  # shift three rows forward
      nouns %>% slice(rep(shifter(1:nrow(nouns), 4))),
      nouns %>% slice(rep(shifter(1:nrow(nouns), 5))),
      nouns %>% slice(rep(shifter(1:nrow(nouns), 6))), 
      nouns %>% slice(rep(shifter(1:nrow(nouns), 7)))
    ) %>%
      rename(noun3_ID = noun_ID, 
             unprocessed_noun3 = unprocessed_noun,
             noun3 = noun,
             noun3_gender = gender)
    
    # Combine all columns
    locative_combinations = 
      rbind(locative_combinations, data.frame(noun1, noun2, noun3))
    
    
    # Apply operations below within nouns to ensure 
    # an equal treatment of all nouns.
    
    locative_combinations = locative_combinations %>% 
      
      group_by(noun1) %>%
      
      # Assign grammatical number pseudo-randomly. The same number 
      # will be applied to noun2 and noun3. 
      mutate( noun1_number = rep(c('singular', 'plural'), each = n()/2),
              noun2_number = rep(c('singular', 'plural'), times = n()/2),
              noun3_number = rep(c('plural', 'singular'), times = n()/2),
              
              # Likewise, distribute locative adverbs 
              locative_adverb_ID = stimuli[complete.cases(stimuli$locative_adverb_ID),
                                           'locative_adverb_ID'] %>% 
                unique() %>% rep(times = n()/2)
      ) %>%
      ungroup()
    
    
    # Select articles based on the gender of the nouns. 
    # Apply same grammatical number to noun2 and noun3. 
    # Select verb form.
    
    locative_combinations$article_noun1 = NA
    locative_combinations$article_noun2 = NA
    locative_combinations$article_noun3 = NA
    locative_combinations$verb = NA
    locative_combinations$locative_adverb = NA
    
    for(i in 1:nrow(locative_combinations)) {  # iterate over sentences
      
      locative_combinations[i,] = locative_combinations[i,] %>% mutate( 
        
        article_noun1 = 
          stimuli[stimuli$article != '' &
                    stimuli$gender == locative_combinations[i, 'noun1_gender'] %>% pull & 
                    stimuli$number == locative_combinations[i, 'noun1_number'] %>% pull, 
                  'article'],
        
        article_noun2 = 
          stimuli[stimuli$article != '' &
                    stimuli$gender == locative_combinations[i, 'noun2_gender'] %>% pull & 
                    stimuli$number == locative_combinations[i, 'noun2_number'] %>% pull, 
                  'article'],
        
        article_noun3 = 
          stimuli[stimuli$article != '' &
                    stimuli$gender == locative_combinations[i, 'noun3_gender'] %>% pull & 
                    stimuli$number == locative_combinations[i, 'noun3_number'] %>% pull, 
                  'article'],
        
        verb = stimuli[stimuli$verb != '' & 
                         stimuli$number == locative_combinations[i, 'noun1_number'] %>% pull, 
                       'verb'],
        
        locative_adverb = 
          stimuli[stimuli$locative_adverb != '' &
                    stimuli$locative_adverb_ID == locative_combinations[i, 'locative_adverb_ID'] %>% pull, 
                  'locative_adverb']
      )
    }
    
    # In Mini-Norwegian, append articles to nouns
    if(language == 'Mini-Norwegian') {
      locative_combinations = locative_combinations %>%
        mutate(noun1_with_article = paste0(noun1, article_noun1),
               noun2_with_article = paste0(noun2, article_noun2),
               noun3_with_article = paste0(noun3, article_noun3))
    }
    
    # Compose sentences tailored to the language to adjust the location of the articles
    
    locative_combinations$sentence1 = NA
    locative_combinations$sentence2 = NA
    
    for(i in 1:nrow(locative_combinations)) {
      
      locative_combinations[i,] = locative_combinations[i,] %>% mutate( 
        
        sentence1 =
          ifelse(language == 'Mini-Norwegian',
                 paste0(noun1_with_article, ' ', verb, ' ', locative_adverb, ' ', 
                        noun2_with_article, '.'),
                 ifelse(language == 'Mini-English' | language == 'Mini-Spanish',
                        paste0(article_noun1, ' ', noun1, ' ', verb, ' ', 
                               locative_adverb, ' ', article_noun2, ' ', noun2, '.'),
                        NA)) %>% 
          str_to_sentence(),  # Make first letter in sentence uppercase 
        
        sentence2 =
          ifelse(language == 'Mini-Norwegian',
                 paste0(noun1_with_article, ' ', verb, ' ', locative_adverb, ' ', 
                        noun3_with_article, '.'),
                 ifelse(language == 'Mini-English' | language == 'Mini-Spanish',
                        paste0(article_noun1, ' ', noun1, ' ', verb, ' ', 
                               locative_adverb, ' ', article_noun3, ' ', noun3, '.'),
                        NA)) %>% 
          str_to_sentence()  # Make first letter in sentence uppercase 
      )
    }
    
    
    # To facilitate concatenating `combinations` and `locative_combinations`, 
    # match their columns.
    
    combinations[setdiff(names(locative_combinations), names(combinations)) ] = ''
    
    locative_combinations[setdiff(names(combinations), names(locative_combinations)) ] = ''
    
    # Stack data frames together 
    combinations = rbind(combinations, locative_combinations)
    
    
    # Following González Alonso et al. (2020), ensure that half of 
    # the trials in plural contain an image with two items and the 
    # other half contain an image with three items. 
    
    combinations = combinations %>% 
      
      group_by(list) %>%
      
      mutate( 
        number_of_items_in_noun1 = 
          ifelse(noun1_number == 'singular', 1, 
                 ifelse(noun1_number == 'plural', rep(2:3, each = n()/4),
                        '')),
        number_of_items_in_noun2 = 
          ifelse(noun2_number == 'singular', 1, 
                 ifelse(noun2_number == 'plural', rep(2:3, times = n()/4),
                        '')),
        number_of_items_in_noun3 = 
          ifelse(noun3_number == 'singular', 1, 
                 ifelse(noun3_number == 'plural', rep(2:3, times = n()/4),
                        ''))
      ) %>% 
      ungroup()
    
    
    # For each trial, create labels that will be used to name the corresponding 
    # images. These labels will be formed of the nouns and adjectives in English.
    # To allow this also in the Mini-Norwegian and Mini-Spanish files, the 
    # English equivalents of the words will be used. The images are composed in 
    # OpenSesame. 
    
    # Placeholder images entered below to prevent error in OpenSesame due to
    # differences between target and filler trials.
    
    combinations$noun_image = 'NA.png'
    combinations$adjective1_image = 'NA.png'
    combinations$adjective2_image = 'NA.png'
    combinations$upper_image1 = 'NA.png'
    combinations$lower_image1 = 'NA.png'
    combinations$upper_image2 = 'NA.png'
    combinations$lower_image2 = 'NA.png'
    
    
    if(language == 'Mini-English') {
      
      for(i in 1:nrow(combinations)) {
        
        # Critical items; skip rows without adjectives
        if(combinations[i, 'unprocessed_adjective1'] != '') {
          
          # Compose labels with number of items specified
          
          combinations[i,] = combinations[i,] %>% mutate( 
            noun_image = paste0(unprocessed_noun1, 
                                ifelse(number_of_items_in_noun1 == 1, '', 
                                       paste0('_', number_of_items_in_noun1)),
                                '.png'),
            adjective1_image = paste0(unprocessed_adjective1, '.png'),
            adjective2_image = paste0(unprocessed_adjective2, '.png') 
          )
          
          
          # Filler items; skip rows without noun2
        } else if(combinations[i, 'unprocessed_noun2'] != '') {
          
          # Compose labels with number of items specified. 
          # The position of the objects is taken into account. 
          # In left quadrant: upper_image1 and lower_image1.
          # In right quadrant: upper_image2 and lower_image2.
          # NB. The images for the locative sentences are composed in OpenSesame.
          
          if(combinations[i, 'locative_adverb_ID'] == 1) {  # adverb: 'above'
            
            combinations[i,] = combinations[i,] %>% mutate( 
              
              upper_image1 = paste0(unprocessed_noun1, 
                                    ifelse(number_of_items_in_noun1 > 1, 
                                           paste0('_', number_of_items_in_noun1), 
                                           ''),
                                    '.png'),
              lower_image1 = paste0(unprocessed_noun2, 
                                    ifelse(number_of_items_in_noun2 == 1, '', 
                                           paste0('_', number_of_items_in_noun2)),
                                    '.png'),
              upper_image2 = paste0(unprocessed_noun1, 
                                    ifelse(number_of_items_in_noun1 > 1, 
                                           paste0('_', number_of_items_in_noun1), 
                                           ''),
                                    '.png'),
              lower_image2 = paste0(unprocessed_noun3, 
                                    ifelse(number_of_items_in_noun3 == 1, '', 
                                           paste0('_', number_of_items_in_noun3)),
                                    '.png') 
            )
            
          } else if(combinations[i, 'locative_adverb_ID'] == 2) {  # adverb: 'below'
            
            combinations[i,] = combinations[i,] %>% mutate( 
              
              upper_image1 = paste0(unprocessed_noun2, 
                                    ifelse(number_of_items_in_noun2 == 1, '', 
                                           paste0('_', number_of_items_in_noun2)),
                                    '.png'),
              lower_image1 = paste0(unprocessed_noun1, 
                                    ifelse(number_of_items_in_noun1 == 1, '', 
                                           paste0('_', number_of_items_in_noun1)),
                                    '.png'),
              upper_image2 = paste0(unprocessed_noun3, 
                                    ifelse(number_of_items_in_noun3 == 1, '', 
                                           paste0('_', number_of_items_in_noun3)),
                                    '.png'),
              lower_image2 = paste0(unprocessed_noun1, 
                                    ifelse(number_of_items_in_noun1 == 1, '', 
                                           paste0('_', number_of_items_in_noun1)),
                                    '.png') 
            )
          }
        }
      }
      
      
      # If language is not Mini-English, fetch English translations
    } else {
      
      for(i in 1:nrow(combinations)) {
        
        # For each element, store IDs and fetch the 
        # English counterparts matching those IDs.
        
        i_noun1_ID = all_stimuli %>% 
          filter(unprocessed_noun == 
                   combinations[i,] %>% 
                   pull(unprocessed_noun1)) %>%
          pull(noun_ID)
        
        i_noun1_label = all_stimuli %>% 
          filter(language == 'Mini-English', 
                 noun_ID == i_noun1_ID) %>%
          pull(unprocessed_noun)
        
        # Critical items; skip rows without adjectives
        if(combinations[i, 'unprocessed_adjective1'] != '') {
          
          i_adjective1_ID = all_stimuli %>% 
            filter(unprocessed_adjective == 
                     combinations[i,] %>% 
                     pull(unprocessed_adjective1)) %>%
            pull(adjective_ID)
          
          i_adjective1_label = all_stimuli %>% 
            filter(language == 'Mini-English', 
                   adjective_ID == i_adjective1_ID) %>%
            pull(unprocessed_adjective)
          
          i_adjective2_ID = all_stimuli %>% 
            filter(unprocessed_adjective == 
                     combinations[i,] %>% 
                     pull(unprocessed_adjective2)) %>%
            pull(adjective_ID)
          
          i_adjective2_label = all_stimuli %>% 
            filter(language == 'Mini-English', 
                   adjective_ID == i_adjective2_ID) %>%
            pull(unprocessed_adjective)
          
          # Compose labels with number of items specified
          
          combinations[i,] = combinations[i,] %>% mutate( 
            noun_image = paste0(i_noun1_label, 
                                ifelse(number_of_items_in_noun1 == 1, '', 
                                       paste0('_', number_of_items_in_noun1)),
                                '.png'),
            adjective1_image = paste0(i_adjective1_label, '.png'),
            adjective2_image = paste0(i_adjective2_label, '.png') 
          )
          
          
          # Filler items; skip rows without noun2
        } else if(combinations[i, 'unprocessed_noun2'] != '') {
          
          i_noun2_ID = all_stimuli %>% 
            filter(unprocessed_noun == 
                     combinations[i,] %>% 
                     pull(unprocessed_noun2)) %>%
            pull(noun_ID)
          
          i_noun2_label = all_stimuli %>% 
            filter(language == 'Mini-English', 
                   noun_ID == i_noun2_ID) %>%
            pull(unprocessed_noun)
          
          i_noun3_ID = all_stimuli %>% 
            filter(unprocessed_noun == 
                     combinations[i,] %>% 
                     pull(unprocessed_noun3)) %>%
            pull(noun_ID)
          
          i_noun3_label = all_stimuli %>% 
            filter(language == 'Mini-English', 
                   noun_ID == i_noun3_ID) %>%
            pull(unprocessed_noun)
          
          i_locative_adverb_ID = all_stimuli %>% 
            filter(locative_adverb == 
                     combinations[i,] %>% 
                     pull(locative_adverb)) %>%
            pull(locative_adverb_ID)
          
          # Compose labels with number of items specified.
          # The position of the objects is taken into account. 
          # In left quadrant: upper_image1 and lower_image1.
          # In right quadrant: upper_image2 and lower_image2.
          # NB. The images for the locative sentences are composed in OpenSesame.
          
          if(i_locative_adverb_ID == 1) {  # adverb: equivalent of 'above'
            
            combinations[i,] = combinations[i,] %>% mutate( 
              
              upper_image1 = paste0(i_noun1_label, 
                                    ifelse(number_of_items_in_noun1 == 1, '', 
                                           paste0('_', number_of_items_in_noun1)),
                                    '.png'),
              lower_image1 = paste0(i_noun2_label, 
                                    ifelse(number_of_items_in_noun2 == 1, '', 
                                           paste0('_', number_of_items_in_noun2)),
                                    '.png'),
              upper_image2 = paste0(i_noun1_label, 
                                    ifelse(number_of_items_in_noun1 == 1, '', 
                                           paste0('_', number_of_items_in_noun1)),
                                    '.png'),
              lower_image2 = paste0(i_noun3_label, 
                                    ifelse(number_of_items_in_noun3 == 1, '', 
                                           paste0('_', number_of_items_in_noun3)),
                                    '.png') 
            )
            
          } else if(i_locative_adverb_ID == 2) {  # adverb: equivalent of 'below'
            
            combinations[i,] = combinations[i,] %>% mutate( 
              
              upper_image1 = paste0(i_noun2_label, 
                                    ifelse(number_of_items_in_noun2 == 1, '', 
                                           paste0('_', number_of_items_in_noun2)),
                                    '.png'),
              lower_image1 = paste0(i_noun1_label, 
                                    ifelse(number_of_items_in_noun1 == 1, '', 
                                           paste0('_', number_of_items_in_noun1)),
                                    '.png'),
              upper_image2 = paste0(i_noun3_label, 
                                    ifelse(number_of_items_in_noun3 == 1, '', 
                                           paste0('_', number_of_items_in_noun3)),
                                    '.png'),
              lower_image2 = paste0(i_noun1_label, 
                                    ifelse(number_of_items_in_noun1 == 1, '', 
                                           paste0('_', number_of_items_in_noun1)),
                                    '.png') 
            )
          }
        }
      }
    }
    
    
    # Certain stimuli and experimental conditions should appear equally often to 
    # prevent repetition effects. To ascertain this, check whether all elements 
    # in certain columns appear equally often. If they do not, show warnings.
    # Please note that this basic check only helps prevent blatant disparities, 
    # but it does not verify all the controls that have been applied.
    
    columns_to_check = c('noun1_gender', 'noun2_gender', 'noun3_gender', 
                         'noun1_number', 'noun2_number', 'noun3_number', 
                         'noun1', 'noun2', 'noun3', 'adjective1', 'adjective2')
    
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
      
      mutate( materials_version,
              
              # To save space in the data set, replace NAs with blanks
              across(names(.), ~replace(., is.na(.), '')) ) %>%
      
      # Store key components associated with each trial
      select(materials_version, list, noun1_gender, noun1_number, noun_image, 
             adjective1_image, adjective2_image, upper_image1, lower_image1, 
             upper_image2, lower_image2, sentence1, sentence2)
    
    # Output: save lists in different files. Each file also includes 
    # the fillers, which don't have any list assigned.
    
    combinations %>% 
      filter(list == combinations %>% 
               filter(complete.cases(list), list != '') %>% 
               pull(list) %>% unique() %>% nth(1) | 
               list == '') %>%
      write.csv(paste0('session_materials/Session 2/stimuli/training/', 
                       study_site, ' site, ', language, ', ',
                       'Session2_Training_gender_agreement, List 1.csv'), 
                row.names = FALSE, fileEncoding = 'UTF-8')
    
    combinations %>% 
      filter(list == combinations %>% 
               filter(complete.cases(list), list != '') %>% 
               pull(list) %>% unique() %>% nth(2) | 
               list == '') %>%
      write.csv(paste0('session_materials/Session 2/stimuli/training/', 
                       study_site, ' site, ', language, ', ',
                       'Session2_Training_gender_agreement, List 2.csv'), 
                row.names = FALSE, fileEncoding = 'UTF-8')
    
  }

