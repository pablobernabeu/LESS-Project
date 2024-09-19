

# Creation of stimuli for the experiment on gender agreement 
# administered in Session 2.

# Target trials contain the grammatical property of interest.
# Example structure: 'Ze pencil is bigezu and ze truck too'
# 
# Filler trials contain a locative adverbial clause.
# Example structure: 'The pencil is above the key and the street too'

# There are three conditions, following González Alonso et al. (2020; 
# https://doi.org/10.1016/j.jneuroling.2020.100939):
# 
# 1. grammatical
# 2. gender violation
# 3. number violation
# 
# These conditions are realised in the adjective in target sentences.


# Definition of code items with regard to González Alonso et al. (2020)
# 
# `noun1`: the noun in the initial determiner phrase in each sentence
# 
# `noun2`: the second noun in locative filler sentences
# 
# `wrapup_noun`: the noun in the final region of sentences, which 
# begins with a conjunction. This region is used to ward off 
# sentence wrap-up effects.

# For definitions of all EEG triggers, see README.txt in the 
# 'session_materials' folder.


Session2_Experiment_gender_agreement = 
  
  function(stimuli, study_site, language, verbose = FALSE) {
    
    require(dplyr)    # data wrangling
    require(magrittr)  # data wrangling
    require(tidyr)    # data wrangling
    require(stringr)  # text processing
    
    # Set initial seed number to ensure reproducibility of functions that
    # produce random variations, such as `sample`. Before those functions, 
    # the same seed will always be set by using the code `set.seed(seed)`
    # or `set.seed(seed + i)` (where `i` is the row number).
    
    seed = 123
    
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
    
    
    # Combine all nouns with all adjectives
    
    combinations = stimuli %>% 
      expand(nesting(noun_ID, unprocessed_noun, noun, gender, !!! rlang::syms(gender_canonicity)),
             nesting(adjective_ID, unprocessed_adjective, adjective_root)) %>%
      filter(complete.cases(noun_ID) & complete.cases(adjective_ID)) %>%
      
      # Rename 'noun' columns as 'noun1' to allow for other 
      # nouns that will be added later.
      
      rename(noun1_ID = noun_ID, 
             unprocessed_noun1 = unprocessed_noun, 
             noun1 = noun) %>%
      
      # Create ID for each noun-adjective combination
      mutate(noun_adj_ID = paste0(noun1_ID, '_', adjective_ID))
    
    
    # Select noun for the wrap-up buffer with the same gender as noun1.
    # Create object to store count of each noun in the location of wrapup_noun.
    # Every noun must appear the same number of times in both locations.
    
    counts = data.frame(noun = unique(combinations$noun1), 
                        wrapup_noun_count = 0)
    
    combinations$wrapup_noun = NA
    
    for(i in 1:nrow(combinations)) {
      
      # Store gender of current row
      i_gender = combinations %>% 
        slice(i) %>% pull(gender)
      
      # Set seed number to ensure reproducibility, and add row number to 
      # the seed number to create different combinations across rows.
      set.seed(seed + i)
      
      # Preselect nouns that differ from noun1, have the same gender as noun1, and 
      # appear in the wrapup_noun location with a frequency lower than the total 
      # number of unique nouns. Then, select one of the preselected nouns at 
      # random. This process is performed iteratively, ensuring an equal use of 
      # all nouns.
      
      wrapup_noun = combinations %>%
        filter(noun1 != combinations[i, 'noun1'] %>% pull &
                 gender == i_gender &
                 noun1 %in% counts[counts$wrapup_noun_count < 
                                     n_distinct(combinations$noun1), 
                                   'noun']) %>%
        select(noun1) %>% unique %>% 
        pull %>% sample(1)
      
      # After passing all conditions, add selected noun to 
      # wrapup_noun column and increase wrapup_noun_count.
      
      combinations[i, 'wrapup_noun'] = wrapup_noun
      
      counts[counts$noun == wrapup_noun, 'wrapup_noun_count'] =
        counts[counts$noun == wrapup_noun, 'wrapup_noun_count'] + 1
      
      # Display counts at the end of each iteration if option selected
      if(verbose) {
        print(counts)
        cat('\n')
      }
    }
    
    
    # Following González Alonso et al. (2020), create lists to counterbalance 
    # grammaticality conditions across noun-adjective combinations.
    # 
    # List 1: grammatical, gender violation, number violation
    # List 2: gender violation, number violation, grammatical
    # List 3: number violation, grammatical, gender violation
    # 
    # In each list, all nouns and all adjectives appear equally often. Furthermore, every 
    # noun in the initial determiner phrase (here called `noun1`) appears as often in 
    # singular as in plural. These lists will be administered to different participants.
    
    combinations = 
      
      # List 1
      combinations %>% 
      mutate(list = 'List 1: grammatical, gender violation, number violation') %>%
      
      # Apply operations below within nouns to ensure 
      # an equal treatment of all nouns.
      group_by(noun1) %>%
      
      # Ensure every noun1 (i.e., initial determiner phrase) 
      # appears as often in singular as in plural. 
      mutate(number = rep(c('singular', 'plural'), times = n()/2),
             
             # Distribute the three grammaticality conditions according to current list
             grammaticality = rep(c('grammatical', 'gender violation', 'number violation'), 
                                  times = n()/3)) %>%
      
      ungroup() %>%
      
      # Add subsequent lists 
      rbind(
        
        # List 2
        combinations %>% 
          mutate(list = 'List 2: gender violation, number violation, grammatical') %>%
          
          # Apply operations below within nouns to ensure 
          # an equal treatment of all nouns.
          group_by(noun1) %>%
          
          # Ensure every noun1 (i.e., initial determiner phrase) 
          # appears as often in singular as in plural. 
          mutate(number = rep(c('singular', 'plural'), times = n()/2),
                 
                 # Distribute the three grammaticality conditions according to current list
                 grammaticality = rep(c('gender violation', 'number violation', 'grammatical'), 
                                      times = n()/3)) %>%
          
          ungroup(),
        
        # List 3
        combinations %>% 
          mutate(list = 'List 3: number violation, grammatical, gender violation') %>%
          
          # Apply operations below within nouns to ensure 
          # an equal treatment of all nouns.
          group_by(noun1) %>%
          
          # Ensure every noun1 (i.e., initial determiner phrase) 
          # appears as often in singular as in plural. 
          mutate(number = rep(c('singular', 'plural'), times = n()/2),
                 
                 # Distribute the three grammaticality conditions according to current list
                 grammaticality = rep(c('number violation', 'grammatical', 'gender violation'), 
                                      times = n()/3)) %>%
          
          ungroup()
      )
    
    # Set responses for the grammaticality judgement task. As in González Alonso et al.
    # (2020), the responses will be entered using the mouse. Grammatical trials will be 
    # signalled with the left button of the mouse, whereas ungrammatical trials will be 
    # signalled with the right button.
    
    combinations = combinations %>% mutate( 
      correct_response = 
        ifelse(grammaticality == 'grammatical', 'left_button', 
               'right_button') 
    )
    
    # Add variable and EEG trigger to identify the grammatical property under study.
    # 
    # Afterwards, add EEG triggers to identify grammaticality conditions:
    # grammatical = 101; gender violation = 102; number violation = 103
    # 
    # Last, add variable to identify the session. 
    
    combinations = combinations %>% mutate( 
      
      grammatical_property = 'gender agreement',
      grammatical_property_trigger = 1,
      
      grammaticality_trigger = 
        ifelse(grammaticality == 'grammatical', 101,
               ifelse(grammaticality == 'gender violation', 102,
                      ifelse(grammaticality == 'number violation', 103, 
                             '' ) ) ),
      
      session = 'Session 2'
    )
    
    
    # Select article based on the gender and the number of noun1.
    # Select verb form based on the number of noun1.
    # Append ending to adjective based on the characteristics of 
    # noun1 and on the grammaticality condition.
    # Select conjunction and adverb for the wrap-up clause.
    
    combinations$article = NA
    combinations$verb_type = 'copula_be'
    combinations$verb = NA
    combinations$adjective = NA
    combinations$wrapup_format = NA
    combinations$conjunction = NA
    combinations$wrapup_adverb = NA
    
    for(i in 1:nrow(combinations)) {
      
      combinations[i,] = combinations[i,] %>% mutate( 
        
        article = stimuli[stimuli$article != '' &
                            stimuli$gender == combinations[i, 'gender'] %>% pull & 
                            stimuli$number == combinations[i, 'number'] %>% pull, 
                          'article'],
        
        verb = stimuli[stimuli$verb != '' &
                         stimuli$number == combinations[i, 'number'] %>% pull, 
                       'verb'],
        
        adjective = 
          ifelse(grammaticality == 'grammatical', 
                 paste0(adjective_root,
                        stimuli[stimuli$adjective_suffix != '' & 
                                  stimuli$gender == combinations[i, 'gender'] %>% pull & 
                                  stimuli$number == combinations[i, 'number'] %>% pull, 
                                'adjective_suffix']), 
                 ifelse(grammaticality == 'gender violation',
                        paste0(adjective_root,
                               stimuli[stimuli$adjective_suffix != '' & 
                                         # Notice `!=` below
                                         stimuli$gender != combinations[i, 'gender'] %>% pull & 
                                         stimuli$number == combinations[i, 'number'] %>% pull, 
                                       'adjective_suffix']), 
                        ifelse(grammaticality == 'number violation',
                               paste0(adjective_root,
                                      stimuli[stimuli$adjective_suffix != '' & 
                                                stimuli$gender == combinations[i, 'gender'] %>% pull & 
                                                # Notice `!=` below
                                                stimuli$number != combinations[i, 'number'] %>% pull, 
                                              'adjective_suffix'])
                        ))),
        
        wrapup_format = stimuli[stimuli$wrapup_format != '', 'wrapup_format'],
        
        conjunction = stimuli[stimuli$conjunction != '', 'conjunction'],
        
        wrapup_adverb = stimuli[stimuli$wrapup_adverb != '', 'wrapup_adverb']
      )
    }
    
    # In Mini-Norwegian, append articles to nouns
    if(language == 'Mini-Norwegian') {
      combinations = combinations %>%
        mutate(noun1_with_article = paste0(noun1, article),
               wrapup_noun_with_article = paste0(wrapup_noun, article))
    }
    
    # Resolve repetition of vowels at the end of adjectives
    combinations$adjective = combinations$adjective %>%
      str_replace_all(c('eeju$' = 'eju', 'eezu$' = 'ezu',
                        'eejur$' = 'ejur', 'eezur$' = 'ezur'))
    
    # Based on languages and conditions, identify ERP-target word 
    # and its location in the sentence, and compose the sentence.
    
    combinations$target_word_location = NA
    combinations$sentence = NA
    
    for(i in 1:nrow(combinations)) {
      
      if(language == 'Mini-Norwegian') {  # for Mini-Norwegian only
        
        combinations[i,] = combinations[i,] %>% mutate( 
          
          target_word_location = 'word3',
          
          sentence = paste0(noun1_with_article, ' ', verb, ' ', adjective, ' ', conjunction, ' ', 
                            wrapup_noun_with_article, ' ', wrapup_adverb, '.') %>% 
            str_to_sentence()  # Make first letter in sentence uppercase
        )
        
      } else {  # for other languages
        
        combinations[i,] = combinations[i,] %>% mutate(
          
          target_word_location = 'word4',
          
          sentence = paste0(article, ' ', noun1, ' ', verb, ' ', adjective, ' ', conjunction, ' ', 
                            article, ' ', wrapup_noun, ' ', wrapup_adverb, '.') %>% 
            str_to_sentence()  # Make first letter in sentence uppercase
        )
      }
    }
    
    
    # Assign EEG trigger (40--99) to each target word
    
    combinations$target_word_trigger = NA
    
    # temporary dataframe
    temp_combinations = combinations[0,]
    
    # Iterate over lists
    for(i_list in seq_along(unique(combinations$list))) {
      
      list_combinations = 
        combinations %>%
        filter(list == unique(list)[i_list])
      
      # Iterate over target words
      for(i_adjective_root in seq_along(unique(list_combinations$adjective_root))) {
        
        list_adjective_root_combinations = list_combinations %>%
          filter(adjective_root == unique(adjective_root)[i_adjective_root]) %>%
          mutate(target_word_trigger = i_adjective_root + 39)  # Add 39 so that the triggers start from 40
        
        temp_combinations = rbind(temp_combinations, list_adjective_root_combinations)
      }
    }
    
    # Apply change and delete temporary dataframe
    combinations = temp_combinations
    rm(temp_combinations)
    
    
    # Assign EEG trigger (110--253) to each target sentence
    
    combinations$sentence_trigger = NA
    
    # temporary dataframe
    temp_combinations = combinations[0,]
    
    # Iterate over lists
    for(i_list in seq_along(unique(combinations$list))) {
      
      list_combinations = 
        combinations %>%
        filter(list == unique(list)[i_list])
      
      # Iterate over target sentences
      for(i_sentence in seq_along(unique(list_combinations$sentence))) {
        
        list_sentence_combinations = list_combinations %>%
          filter(sentence == unique(sentence)[i_sentence]) %>%
          mutate(sentence_trigger = i_sentence + 109)  # Add 109 so that the triggers start from 110
        
        temp_combinations = rbind(temp_combinations, list_sentence_combinations)
      }
    }
    
    # Apply change and delete temporary dataframe
    combinations = temp_combinations
    rm(temp_combinations)
    
    
    
    # Create locative sentences that will be used as fillers. Example sentence
    # structure: 'Je bag is above je wall and je street too'
    
    # Create basic structure without any rows to store the data from both 
    # genders iteratively. In Mini-Norwegian, include nouns with articles. 
    # In Mini-Spanish, include gender canonicity.
    
    if(language == 'Mini-Norwegian') {
      locative_combinations = 
        data.frame(gender = NA, noun1_ID = NA, unprocessed_noun1 = NA, noun1 = NA, 
                   noun1_with_article = NA, noun2_ID = NA, unprocessed_noun2 = NA, 
                   noun2 = NA, noun2_with_article = NA, wrapup_noun_ID = NA,
                   unprocessed_wrapup_noun = NA, wrapup_noun = NA, 
                   wrapup_noun_with_article = NA) %>%
        slice(0) 
      
    } else if(language == 'Mini-Spanish') {
      locative_combinations = 
        data.frame(gender = NA, Spanish_gender_canonicity = NA, noun1_ID = NA, 
                   unprocessed_noun1 = NA, noun1 = NA, noun2_ID = NA, 
                   unprocessed_noun2 = NA, noun2 = NA, wrapup_noun_ID = NA, 
                   unprocessed_wrapup_noun = NA, wrapup_noun = NA) %>%
        mutate(!!! rlang::syms(gender_canonicity)) %>%
        slice(0)
      
    } else {
      locative_combinations = 
        data.frame(gender = NA, noun1_ID = NA, unprocessed_noun1 = NA, 
                   noun1 = NA, noun2_ID = NA, unprocessed_noun2 = NA, 
                   noun2 = NA, wrapup_noun_ID = NA, 
                   unprocessed_wrapup_noun = NA, wrapup_noun = NA) %>%
        slice(0)
    }
    
    
    # Create basic structure with the nouns ordered alphabetically
    
    nouns = stimuli[stimuli$noun != '',] %>% 
      select(gender, !!! rlang::syms(gender_canonicity), 
             noun_ID, unprocessed_noun, noun) %>% 
      arrange(noun)  # order rows by `noun` column
    
    # First, for the `noun1` column, the nouns are repeated in alphabetical order eight 
    # times to obtain 96 locative fillers, as in González Alonso et al. (2020). 
    
    noun1 = nouns %>% 
      slice(rep(1:n(), times = 8)) %>%
      rename(noun1_ID = noun_ID, 
             unprocessed_noun1 = unprocessed_noun,
             noun1 = noun,
             noun1_gender = gender)
    
    # Next, for the `noun2` column, the nouns are distributed in alphabetical order as well, 
    # but in rotated ways. Specifically, the first sequence of nouns begins with the second 
    # noun and continues circularly up to the first noun (i.e., 'bat', 'bed', 'book', 'boy', 
    # 'bun', 'ball'). Next, the second sequence begins with the third noun, while the third 
    # sequence begins with the fourth noun, and so on.
    
    # Create circular shift function to iterate over row numbers circularly
    # (source: https://stackoverflow.com/a/30542172/7050882)
    shifter = function(x, n = 1) {
      if(n == 0) x else c(tail(x, -n), head(x, n))
    }
    
    noun2 = rbind(
      nouns %>% slice(rep(shifter(1:nrow(nouns), 1))),  # shift one row forward
      nouns %>% slice(rep(shifter(1:nrow(nouns), 2))),  # shift two rows forward
      nouns %>% slice(rep(shifter(1:nrow(nouns), 3))),
      nouns %>% slice(rep(shifter(1:nrow(nouns), 4))),
      nouns %>% slice(rep(shifter(1:nrow(nouns), 5))),
      nouns %>% slice(rep(shifter(1:nrow(nouns), 1))),
      nouns %>% slice(rep(shifter(1:nrow(nouns), 2))),
      nouns %>% slice(rep(shifter(1:nrow(nouns), 3)))
    ) %>%
      rename(noun2_ID = noun_ID, 
             unprocessed_noun2 = unprocessed_noun,
             noun2 = noun,
             noun2_gender = gender)
    
    # Finally, for the wrapup_noun column, the first sequence begins with the third noun, 
    # the second sequence begins with the fourth noun, and so on. 
    
    wrapup_noun = rbind(
      nouns %>% slice(rep(shifter(1:nrow(nouns), 2))),  # shift two rows forward
      nouns %>% slice(rep(shifter(1:nrow(nouns), 3))),  # shift three rows forward
      nouns %>% slice(rep(shifter(1:nrow(nouns), 4))),
      nouns %>% slice(rep(shifter(1:nrow(nouns), 5))),
      nouns %>% slice(rep(shifter(1:nrow(nouns), 1))),
      nouns %>% slice(rep(shifter(1:nrow(nouns), 2))),
      nouns %>% slice(rep(shifter(1:nrow(nouns), 3))),
      nouns %>% slice(rep(shifter(1:nrow(nouns), 4)))
    ) %>%
      rename(wrapup_noun_ID = noun_ID,  # placeholder
             unprocessed_wrapup_noun = unprocessed_noun,  # placeholder
             wrapup_noun = noun,
             wrapup_noun_gender = gender)
    
    # Combine all columns
    locative_combinations = 
      rbind( 
        locative_combinations %>% 
          mutate(wrapup_noun_ID = NA, unprocessed_wrapup_noun = NA),  # placeholders
        data.frame(noun1, noun2, wrapup_noun) 
      ) %>% 
      select(-c(wrapup_noun_ID, unprocessed_wrapup_noun))  # remove placeholders
    
    
    # Apply operations below within nouns to ensure 
    # an equal treatment of all nouns.
    
    locative_combinations = locative_combinations %>% 
      
      group_by(noun1) %>%
      
      # Assign grammatical number pseudo-randomly. The same number 
      # will be applied to noun2 and noun3. 
      mutate( noun1_number = rep(c('singular', 'plural'), each = n()/2),
              noun2_number = rep(c('singular', 'plural'), times = n()/2),
              wrapup_noun_number = rep(c('plural', 'singular'), times = n()/2),
              
              # Likewise, distribute locative adverbs 
              locative_adverb_ID = stimuli[complete.cases(stimuli$locative_adverb_ID),
                                           'locative_adverb_ID'] %>% 
                unique() %>% rep(times = n()/2)
      ) %>%
      ungroup()
    
    
    # Select article based on the gender and the number of noun1.
    # Select verb form based on the number of noun1.
    # Select conjunction and adverb for the wrap-up clause.
    
    locative_combinations$article_noun1 = NA
    locative_combinations$article_noun2 = NA
    locative_combinations$article_wrapup_noun = NA
    locative_combinations$verb_type = 'copula_be'
    locative_combinations$verb = NA
    locative_combinations$locative_adverb = NA
    locative_combinations$wrapup_format = NA
    locative_combinations$conjunction = NA
    locative_combinations$wrapup_adverb = NA
    
    for(i in 1:nrow(locative_combinations)) {  # iterate over sentences
      
      locative_combinations[i,] = locative_combinations[i,] %>% mutate( 
        
        article_noun1 = 
          stimuli[stimuli$article != '' &
                    stimuli$gender == locative_combinations[i, 'noun1_gender'] %>% pull & 
                    stimuli$number == locative_combinations[i, 'noun1_number'] %>% pull, 
                  'article'],
        
        verb = stimuli[stimuli$verb != '' &
                         stimuli$number == locative_combinations[i, 'noun1_number'] %>% pull, 
                       'verb'],
        
        article_noun2 = 
          stimuli[stimuli$article != '' &
                    stimuli$gender == locative_combinations[i, 'noun2_gender'] %>% pull & 
                    stimuli$number == locative_combinations[i, 'noun2_number'] %>% pull, 
                  'article'],
        
        locative_adverb = 
          stimuli[stimuli$locative_adverb != '' &
                    stimuli$locative_adverb_ID == locative_combinations[i, 'locative_adverb_ID'] %>% pull, 
                  'locative_adverb'],
        
        wrapup_format = stimuli[stimuli$wrapup_format != '', 'wrapup_format'],
        conjunction = stimuli[stimuli$conjunction != '', 'conjunction'],
        wrapup_adverb = stimuli[stimuli$wrapup_adverb != '', 'wrapup_adverb'],
        
        article_wrapup_noun = 
          stimuli[stimuli$article != '' &
                    stimuli$gender == locative_combinations[i, 'wrapup_noun_gender'] %>% pull & 
                    stimuli$number == locative_combinations[i, 'noun2_number'] %>% pull, 
                  'article']
      )
    }
    
    # In Mini-Norwegian, append articles to nouns
    if(language == 'Mini-Norwegian') {
      locative_combinations = locative_combinations %>%
        mutate( noun1_with_article = paste0(noun1, article_noun1),
                noun2_with_article = paste0(noun2, article_noun2),
                wrapup_noun_with_article = paste0(wrapup_noun, article_wrapup_noun) )
    }
    
    # Compose sentences depending on the language to adjust 
    # the locations of the articles.
    
    locative_combinations$sentence = NA
    
    for(i in 1:nrow(locative_combinations)) {
      
      locative_combinations[i,] = locative_combinations[i,] %>% mutate( 
        
        sentence =
          
          ifelse(language == 'Mini-Norwegian',
                 paste0(noun1_with_article, ' ', verb, ' ', locative_adverb,' ', 
                        noun2_with_article, ' ', conjunction, ' ', wrapup_noun_with_article, 
                        ' ', wrapup_adverb, '.') %>% 
                   str_to_sentence(),  # Make first letter in sentence uppercase
                 
                 ifelse(language == 'Mini-English' | language == 'Mini-Spanish',
                        paste0(article_noun1, ' ', noun1, ' ', verb, ' ', locative_adverb, ' ', 
                               article_noun2, ' ', noun2, ' ', conjunction, ' ', 
                               article_wrapup_noun, ' ', wrapup_noun, ' ', wrapup_adverb, '.') %>% 
                          str_to_sentence(),  # Make first letter in sentence uppercase
                        '')
          )
      )
    }
    
    # Categorise fillers as grammatically correct,
    # requiring a left-click response in the task.
    
    locative_combinations = locative_combinations %>%
      mutate( grammaticality = 'grammatical', 
              correct_response = 'left_button' )
    
    
    # To facilitate concatenating `combinations` and `locative_combinations`, 
    # categorise types of trials and match their columns.
    
    combinations = combinations %>%
      mutate(trial_type = 'gender agreement')
    
    locative_combinations = locative_combinations %>%
      mutate(trial_type = 'filler')
    
    combinations[setdiff(names(locative_combinations), names(combinations)) ] = ''
    
    locative_combinations[setdiff(names(combinations), names(locative_combinations)) ] = ''
    
    # Stack data frames together 
    combinations = rbind(combinations, locative_combinations)
    
    
    # Distribute all words in each sentence across different columns 
    # to facilitate presentation in OpenSesame (for background, see 
    # https://stackoverflow.com/q/75869250/7050882).
    
    combinations = combinations %>%
      mutate(word1 = sentence %>% word(1),
             word2 = sentence %>% word(2),
             word3 = sentence %>% word(3),
             word4 = sentence %>% word(4),
             word5 = sentence %>% word(5),
             word6 = sentence %>% word(6),
             word7 = sentence %>% word(7),
             word8 = sentence %>% word(8),
             word9 = sentence %>% word(9),
             word10 = sentence %>% word(10))
    
    
    # Adjust duration of words to the number of letters. This is necessary due to
    # the large differences in length across words. These differences could result 
    # in the increase of cognitive load for participants, which could interfere 
    # with the important P300 wave (https://doi.org/10.1088/1741-2560/13/2/026019, 
    # https://doi.org/10.1016/j.clinph.2005.12.008, 
    # https://doi-org.mime.uit.no/10.1109/TNSRE.2019.2953975).
    
    # Words with three letters or less: 250 ms
    # Words with more than three letters: 250 ms + 35 ms for each additional letter
    
    combinations$word1_duration = NA
    combinations$word2_duration = NA
    combinations$word3_duration = NA
    combinations$word4_duration = NA
    combinations$word5_duration = NA
    combinations$word6_duration = NA
    combinations$word7_duration = NA
    combinations$word8_duration = NA
    combinations$word9_duration = NA
    combinations$word10_duration = NA
    
    base_duration = 250 # consistent with https://doi.org/10.1109/TNSRE.2019.2953975
    base_length = 3
    letter_increase = 35
    
    for(i in 1:nrow(combinations)) {
      
      combinations[i,] = combinations[i,] %>% mutate(
        
        word1_duration = 
          case_when( nchar(word1) <= base_length ~ base_duration,
                     nchar(word1) > base_length ~ 
                       base_duration + (nchar(word1) - base_length) * letter_increase, 
                     .default = NA ), 
        
        word2_duration = 
          case_when( nchar(word2) <= base_length ~ base_duration,
                     nchar(word2) > base_length ~ 
                       base_duration + (nchar(word2) - base_length) * letter_increase, 
                     .default = NA ), 
        
        word3_duration = 
          case_when( nchar(word3) <= base_length ~ base_duration,
                     nchar(word3) > base_length ~ 
                       base_duration + (nchar(word3) - base_length) * letter_increase, 
                     .default = NA ), 
        
        word4_duration = 
          case_when( nchar(word4) <= base_length ~ base_duration,
                     nchar(word4) > base_length ~ 
                       base_duration + (nchar(word4) - base_length) * letter_increase, 
                     .default = NA ), 
        
        word5_duration = 
          case_when( nchar(word5) <= base_length ~ base_duration,
                     nchar(word5) > base_length ~ 
                       base_duration + (nchar(word5) - base_length) * letter_increase, 
                     .default = NA ), 
        
        word6_duration = 
          case_when( nchar(word6) <= base_length ~ base_duration,
                     nchar(word6) > base_length ~ 
                       base_duration + (nchar(word6) - base_length) * letter_increase, 
                     .default = NA ), 
        
        word7_duration = 
          case_when( nchar(word7) <= base_length ~ base_duration,
                     nchar(word7) > base_length ~ 
                       base_duration + (nchar(word7) - base_length) * letter_increase, 
                     .default = NA ), 
        
        word8_duration = 
          case_when( nchar(word8) <= base_length ~ base_duration,
                     nchar(word8) > base_length ~ 
                       base_duration + (nchar(word8) - base_length) * letter_increase, 
                     .default = NA ), 
        
        word9_duration = 
          case_when( nchar(word9) <= base_length ~ base_duration,
                     nchar(word9) > base_length ~ 
                       base_duration + (nchar(word9) - base_length) * letter_increase, 
                     .default = NA ), 
        
        word10_duration = 
          case_when( nchar(word10) <= base_length ~ base_duration,
                     nchar(word10) > base_length ~ 
                       base_duration + (nchar(word10) - base_length) * letter_increase, 
                     .default = NA ) 
      )
    }
    
    # EEG triggers are sent at the onset of some words in target trials. The triggers 
    # that extends the duration of the relevant word. Thus, the latter duration is 
    # adjusted below by subtracting the lag.
    
    trigger_lag = 40
    
    combinations = combinations %>% mutate(
      
      word3_duration = case_when(
        target_word_location == 'word3' ~ word3_duration - trigger_lag,
        .default = word3_duration ),
      
      word4_duration = case_when(
        target_word_location == 'word4' ~ word4_duration - trigger_lag,
        .default = word4_duration )
    )
    
    
    # Certain stimuli and experimental conditions should appear equally often to 
    # prevent repetition effects. To ascertain this, check whether all elements 
    # in certain columns appear equally often. If they do not, show warnings.
    # Please note that this basic check only helps prevent blatant disparities, 
    # but it does not verify all the controls that have been applied.
    
    columns_to_check = c('gender', 'number', 'noun1', 'noun2', 
                         'adjective_root', 'wrapup_noun')
    
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
      select(materials_version, language, list, trial_type, noun_adj_ID, gender, 
             number, grammaticality, correct_response, grammatical_property, 
             grammatical_property_trigger, grammaticality_trigger, sentence, 
             sentence_trigger, session, target_word_location, target_word_trigger, 
             target_word_location, verb_type, word1, word1_duration, word2, 
             word2_duration, word3, word3_duration, word4, word4_duration, word5, 
             word5_duration, word6, word6_duration, word7, word7_duration, word8, 
             word8_duration, word9, word9_duration, word10, word10_duration)
    
    
    # Output: save lists in different files. Each file also includes 
    # the fillers, which don't have any list assigned.
    
    # Set path to files
    path = 'session_materials/Session 2/stimuli/experiment/'
    
    combinations %>% 
      filter(list == combinations %>% 
               filter(complete.cases(list), list != '') %>% 
               pull(list) %>% unique() %>% nth(1) | 
               list == '') %>%
      write.csv(paste0(path, study_site, ' site, ', language, ', ',
                       'Session2_Experiment_gender_agreement, List 1.csv'), 
                row.names = FALSE, fileEncoding = 'UTF-8')
    
    combinations %>% 
      filter(list == combinations %>% 
               filter(complete.cases(list), list != '') %>% 
               pull(list) %>% unique() %>% nth(2) | 
               list == '') %>%
      write.csv(paste0(path, study_site, ' site, ', language, ', ',
                       'Session2_Experiment_gender_agreement, List 2.csv'), 
                row.names = FALSE, fileEncoding = 'UTF-8')
    
    combinations %>% 
      filter(list == combinations %>% 
               filter(complete.cases(list), list != '') %>% 
               pull(list) %>% unique() %>% nth(3) | 
               list == '') %>%
      write.csv(paste0(path, study_site, ' site, ', language, ', ',
                       'Session2_Experiment_gender_agreement, List 3.csv'), 
                row.names = FALSE, fileEncoding = 'UTF-8')
    
  }

