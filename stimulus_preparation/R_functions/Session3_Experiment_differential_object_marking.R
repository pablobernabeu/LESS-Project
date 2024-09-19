

# Creation of stimuli for the part of the experiment on differential object 
# marking administered in Session 3.

# Target trials contain the grammatical property of interest.
# Example structure: 'Amelia chose fi ze truck and fi je street too'
# 
# There are three conditions, inspired by González Alonso et al. (2020; 
# https://doi.org/10.1016/j.jneuroling.2020.100939):
# 
# 1. grammatical
# 2. DOM violation (i.e., object noun without DOM)
# 3. article location violation (i.e., article and noun in one word)

# In target trials, all determiner phrases are in singular because the 
# use of plural would require using the morpheme of verb-object number 
# agreement, which is not introduced until Session 4.

# For definitions of all EEG triggers, see README.txt in the 
# 'session_materials' folder.


Session3_Experiment_differential_object_marking = 
  
  function(stimuli, study_site, language, verbose = FALSE) {
    
    require(dplyr)    # data wrangling
    require(tidyr)    # data wrangling
    require(stringr)  # text processing
    
    # Set initial seed number to ensure reproducibility of functions that
    # produce random variations, such as `sample`. Before those functions, 
    # the same seed will always be set by using the code `set.seed(seed)`
    # or `set.seed(seed + i)` (where `i` is the row number).
    
    seed = 123
    
    # Select language, select items included in the present session, and remove 
    # nouns that were NOT included in Session 2. That is, the nouns that were 
    # used in Session 2 (focussed on gender agreement) are used in Session 3 
    # for the content on differential object marking. In this way, the nouns 
    # are rotated across the grammatical properties and the sessions.
    
    stimuli = stimuli[(stimuli$language == language | stimuli$language == 'both') & 
                        stimuli$Session_3 == 'included' &
                        !(stimuli$noun != '' & stimuli$Session_2 != 'included'),]
    
    # Combine all transitive verbs with all nouns
    
    combinations = stimuli %>% 
      filter(verb_type == 'transitive' | complete.cases(noun_ID)) %>% 
      expand(nesting(verb_ID, verb, verb_contrast_ID),
             nesting(noun_ID, unprocessed_noun, noun, gender)) %>%
      
      # Remove rows with incomplete combinations
      filter(complete.cases(verb_ID) & complete.cases(noun_ID)) %>% 
      
      # Rename 'noun' columns as 'noun1' to allow for nouns for 
      # the wrap-up clause, which will be added later.
      
      rename( noun1_ID = noun_ID, 
              unprocessed_noun1 = unprocessed_noun, 
              noun1 = noun, 
              noun1_gender = gender ) %>%
      
      # Create ID for each verb-noun combination
      mutate(verb_noun_ID = paste0(verb_ID, '_', noun1_ID))
    
    
    # Select noun for the wrap-up buffer.
    # Create object to store count of each noun in the location of wrapup_noun.
    # Every noun appears the same number of times in both locations.
    
    counts = data.frame(noun = unique(combinations$noun1), 
                        wrapup_noun_count = 0)
    
    combinations$wrapup_noun = NA
    combinations$wrapup_noun_gender = NA
    
    for(i in 1:nrow(combinations)) {
      
      # Set seed number to ensure reproducibility, and add row number to 
      # the seed number to create different combinations across rows.
      set.seed(seed + i)
      
      # Preselect nouns that differ from noun1 and appear in the wrapup_noun 
      # location with a frequency lower than the total number of unique 
      # nouns. Then, select one of the preselected nouns at random. This 
      # process is performed iteratively, ensuring an equal use of all nouns.
      
      wrapup_noun = combinations %>%
        filter(noun1 != combinations[i, 'noun1'] %>% pull &
                 noun1 %in% counts[counts$wrapup_noun_count < 
                                     n_distinct(combinations$noun1), 
                                   'noun']) %>%
        select(noun1) %>% unique %>% 
        pull %>% sample(1)
      
      # After passing all conditions, add selected noun to wrapup_noun column, 
      # fetch the gender, and increase wrapup_noun_count.
      
      combinations[i, 'wrapup_noun'] = wrapup_noun
      
      combinations[i, 'wrapup_noun_gender'] = 
        combinations %>% 
        filter(noun1 == combinations[i, 'wrapup_noun'] %>% pull) %>%
        pull(noun1_gender) %>% unique()
      
      counts[counts$noun == wrapup_noun, 'wrapup_noun_count'] =
        counts[counts$noun == wrapup_noun, 'wrapup_noun_count'] + 1
      
      # Display counts at the end of each iteration if option selected
      if(verbose) {
        print(counts)
        cat('\n')
      }
    }
    
    
    # Following González Alonso et al. (2020), create lists to counterbalance 
    # grammaticality conditions across verb-noun combinations.
    # 
    # List 1: grammatical, DOM violation, article location violation
    # List 2: DOM violation, article location violation, grammatical
    # List 3: article location violation, grammatical, DOM violation
    # 
    # In each list, all verbs and all nouns appear equally often. 
    # These lists will be administered to different participants. 
    
    combinations = 
      
      # List 1
      combinations %>% 
      mutate(list = 'List 1: grammatical, DOM violation, article location violation',
             grammaticality = rep(c('grammatical', 'DOM violation', 'article location violation'), 
                                  times = n()/3)) %>%
      
      # Add subsequent lists 
      rbind(
        
        # List 2
        combinations %>% 
          mutate(list = 'List 2: DOM violation, article location violation, grammatical',
                 grammaticality = rep(c('DOM violation', 'article location violation', 'grammatical'), 
                                      times = n()/3)),
        
        # List 3
        combinations %>% 
          mutate(list = 'List 3: article location violation, grammatical, DOM violation',
                 grammaticality = rep(c('article location violation', 'grammatical', 'DOM violation'), 
                                      times = n()/3))
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
    # grammatical = 101; DOM violation = 102; article location violation = 103
    # 
    # Last, add variable to identify the session. 
    
    combinations = combinations %>% mutate( 
      
      grammatical_property = 'differential object marking',
      grammatical_property_trigger = 2,
      
      grammaticality_trigger = 
        ifelse(grammaticality == 'grammatical', 101,
               ifelse(grammaticality == 'DOM violation', 102,
                      ifelse(grammaticality == 'article location violation', 103, 
                             '' ) ) ),
      
      session = 'Session 3'
    )
    
    
    # Incorporate person names, which will be the subjects of the transitive sentences. 
    # Next, randomise the formats of the wrap-up clauses (i.e., additive/adversative).
    # Both the names and the wrap-up formats are randomised within lists and within verbs. 
    
    people = stimuli[stimuli$person != '', 'person']
    
    # Ensure reproducibility
    set.seed(seed)
    
    combinations = combinations %>%
      group_by(list, verb) %>%
      mutate( person = sample(people, replace = FALSE), 
              wrapup_format = 
                stimuli[stimuli$wrapup_format != '', 'wrapup_format'] %>% 
                unique() %>% rep(times = n()/2)
      ) %>%
      ungroup()
    
    
    # Assign singular number to all trials, as 
    # explained at the top of this script.
    combinations$number = 'singular'
    
    # Select article based on the gender of noun1.
    # Select conjunction and adverb for the wrap-up clause.
    
    combinations$article_noun1 = NA
    combinations$article_wrapup_noun = NA
    combinations$conjunction = NA
    combinations$wrapup_adverb = NA
    
    for(i in 1:nrow(combinations)) {
      
      combinations[i,] = combinations[i,] %>% mutate( 
        
        article_noun1 = 
          stimuli[stimuli$article != '' &
                    stimuli$gender == combinations[i, 'noun1_gender'] %>% pull & 
                    stimuli$number == combinations[i, 'number'] %>% pull, 
                  'article'],
        
        conjunction = stimuli[stimuli$conjunction != '' & 
                                stimuli$wrapup_format == wrapup_format, 
                              'conjunction'],
        
        wrapup_adverb = stimuli[stimuli$wrapup_adverb != '' & 
                                  stimuli$wrapup_format == wrapup_format, 
                                'wrapup_adverb'],
        
        article_wrapup_noun = 
          stimuli[stimuli$article != '' &
                    stimuli$gender == combinations[i, 'wrapup_noun_gender'] %>% pull & 
                    stimuli$number == combinations[i, 'number'] %>% pull, 
                  'article']
      )
    }
    
    # In Mini-Norwegian, append articles to nouns
    if(language == 'Mini-Norwegian') {
      combinations = combinations %>%
        mutate(noun1_with_article = paste0(noun1, article_noun1),
               wrapup_noun_with_article = paste0(wrapup_noun, article_wrapup_noun))
    }
    
    # Get DOM morpheme
    DOM_morpheme = stimuli[stimuli$DOM_morpheme != '', 'DOM_morpheme']
    
    # Based on languages and conditions, store location of the 
    # target word in the sentence, and compose the sentence.
    
    combinations$target_word_location = NA
    combinations$sentence = NA
    
    for(i in 1:nrow(combinations)) {
      
      # Tailor the position of the adverb in the wrap-up sentence: 
      # additive 'too' appears after the object.
      if(combinations[i, 'wrapup_format'] == 'additive') {
        
        if(language == 'Mini-Norwegian') {  # for Mini-Norwegian only
          
          combinations[i,] = combinations[i,] %>% mutate( 
            
            # Time-lock triggers to the onset of the first word after the DOM morpheme, 
            # namely, the noun+article. In the DOM violation condition, which lacks the 
            # DOM morpheme, the trigger is also time-locked to the noun+article.
            
            target_word_location = case_when(
              grammaticality == 'grammatical' ~ 'word4',
              grammaticality == 'DOM violation' ~ 'word3',
              grammaticality == 'article location violation' ~ 'word4',
              .default = NA
            ),
            
            sentence = case_when(
              
              grammaticality == 'grammatical' ~
                paste0(person, ' ', verb, ' ', DOM_morpheme, ' ', noun1_with_article, ' ', 
                       conjunction, ' ', DOM_morpheme, ' ', wrapup_noun_with_article, ' ', 
                       wrapup_adverb, '.') %>% 
                str_to_sentence(),  # Make first letter in sentence uppercase
              
              grammaticality == 'DOM violation' ~
                paste0(person, ' ', verb, ' ', noun1_with_article, ' ', conjunction, ' ', 
                       wrapup_noun_with_article, ' ', wrapup_adverb, '.') %>% 
                str_to_sentence(),  # Make first letter in sentence uppercase
              
              grammaticality == 'article location violation' ~ 
                paste0(person, ' ', verb, ' ', DOM_morpheme, ' ', article_noun1, noun1, ' ',  
                       conjunction, ' ', DOM_morpheme, ' ', article_wrapup_noun, wrapup_noun, 
                       ' ', wrapup_adverb, '.') %>% 
                str_to_sentence(),  # Make first letter in sentence uppercase
              
              .default = NA
            )
          )
          
        } else {  # for other languages
          
          combinations[i,] = combinations[i,] %>% mutate(
            
            # Time-lock triggers to the onset of the first word after the DOM morpheme, 
            # namely, the article. In the DOM violation condition, which lacks the DOM
            # morpheme, the trigger is also time-locked to the article. 
            
            target_word_location = case_when(
              grammaticality == 'grammatical' ~ 'word4',
              grammaticality == 'DOM violation' ~ 'word3',
              grammaticality == 'article location violation' ~ 'word4',
              .default = NA
            ),
            
            sentence = case_when(
              
              grammaticality == 'grammatical' ~
                paste0(person, ' ', verb, ' ', DOM_morpheme, ' ', article_noun1, ' ', 
                       noun1, ' ', conjunction, ' ', DOM_morpheme, ' ', 
                       article_wrapup_noun, ' ', wrapup_noun, ' ', wrapup_adverb, '.') %>% 
                str_to_sentence(),  # Make first letter in sentence uppercase
              
              grammaticality == 'DOM violation' ~
                paste0(person, ' ', verb, ' ', article_noun1, ' ', noun1, ' ', conjunction, 
                       ' ', article_wrapup_noun, ' ', wrapup_noun, ' ', wrapup_adverb, '.') %>% 
                str_to_sentence(),  # Make first letter in sentence uppercase
              
              grammaticality == 'article location violation' ~ 
                paste0(person, ' ', verb, ' ', DOM_morpheme, ' ', article_noun1, noun1, ' ', 
                       conjunction, ' ', DOM_morpheme, ' ', article_wrapup_noun, wrapup_noun, 
                       ' ', wrapup_adverb, '.') %>% 
                str_to_sentence(),  # Make first letter in sentence uppercase
              
              .default = NA
            )
          )
        }
        
        # Tailor the position of the adverb in the wrap-up sentence: 
        # adversative 'not' appears before the object.
      } else if(combinations[i, 'wrapup_format'] == 'adversative') {
        
        if(language == 'Mini-Norwegian') {  # for Mini-Norwegian only
          
          combinations[i,] = combinations[i,] %>% mutate( 
            
            # Time-lock triggers to the onset of the first word after the DOM morpheme, 
            # namely, the noun+article. In the DOM violation condition, which lacks the 
            # DOM morpheme, the trigger is also time-locked to the noun+article.
            
            target_word_location = case_when(
              grammaticality == 'grammatical' ~ 'word4',
              grammaticality == 'DOM violation' ~ 'word3',
              grammaticality == 'article location violation' ~ 'word4',
              .default = NA
            ),
            
            sentence = case_when(
              
              grammaticality == 'grammatical' ~
                paste0(person, ' ', verb, ' ', DOM_morpheme, ' ', noun1_with_article, ' ', 
                       conjunction, ' ', wrapup_adverb, ' ', DOM_morpheme, ' ', 
                       wrapup_noun_with_article, '.') %>% 
                str_to_sentence(),  # Make first letter in sentence uppercase
              
              grammaticality == 'DOM violation' ~
                paste0(person, ' ', verb, ' ', noun1_with_article, ' ', conjunction, ' ', 
                       wrapup_adverb, ' ', wrapup_noun_with_article, '.') %>% 
                str_to_sentence(),  # Make first letter in sentence uppercase
              
              grammaticality == 'article location violation' ~ 
                paste0(person, ' ', verb, ' ', DOM_morpheme, ' ', article_noun1, noun1, ' ', 
                       conjunction, ' ', wrapup_adverb, ' ', DOM_morpheme, ' ', 
                       article_wrapup_noun, wrapup_noun, '.') %>% 
                str_to_sentence(),  # Make first letter in sentence uppercase
              
              .default = NA
            )
          )
          
        } else {  # for other languages
          
          combinations[i,] = combinations[i,] %>% mutate(
            
            # Time-lock triggers to the onset of the first word after the DOM morpheme, 
            # namely, the article. In the DOM violation condition, which lacks the DOM
            # morpheme, the trigger is also time-locked to the article. 
            
            target_word_location = case_when(
              grammaticality == 'grammatical' ~ 'word4',
              grammaticality == 'DOM violation' ~ 'word3',
              grammaticality == 'article location violation' ~ 'word4',
              .default = NA
            ),
            
            sentence = case_when(
              
              grammaticality == 'grammatical' ~
                paste0(person, ' ', verb, ' ', DOM_morpheme, ' ', article_noun1, ' ', noun1, ' ', 
                       conjunction, ' ', wrapup_adverb, ' ', DOM_morpheme, ' ', 
                       article_wrapup_noun, ' ', wrapup_noun, '.') %>% 
                str_to_sentence(),  # Make first letter in sentence uppercase
              
              grammaticality == 'DOM violation' ~
                paste0(person, ' ', verb, ' ', article_noun1, ' ', noun1, ' ', conjunction, ' ', 
                       wrapup_adverb, ' ', article_wrapup_noun, ' ', wrapup_noun, '.') %>% 
                str_to_sentence(),  # Make first letter in sentence uppercase
              
              grammaticality == 'article location violation' ~ 
                paste0(person, ' ', verb, ' ', DOM_morpheme, ' ', article_noun1, noun1, ' ', 
                       conjunction, ' ', wrapup_adverb, ' ', DOM_morpheme, ' ', 
                       article_wrapup_noun, wrapup_noun, '.') %>% 
                str_to_sentence(),  # Make first letter in sentence uppercase
              
              .default = NA
            )
          )
        }
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
      for(i_noun1 in seq_along(unique(list_combinations$noun1))) {
        
        list_noun1_combinations = list_combinations %>%
          filter(noun1 == unique(noun1)[i_noun1]) %>%
          mutate(target_word_trigger = i_noun1 + 39)  # Add 39 so that the triggers start from 40
        
        temp_combinations = rbind(temp_combinations, list_noun1_combinations)
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
    
    columns_to_check = c('noun1_gender', 'number', 'person', 'verb', 'noun1', 
                         'wrapup_noun')
    
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
      select(materials_version, language, list, number, verb_noun_ID, grammaticality, 
             correct_response, grammatical_property, grammatical_property_trigger, 
             grammaticality_trigger, sentence, sentence_trigger, session, 
             target_word_location, target_word_trigger, word1, word1_duration, 
             word2, word2_duration, word3, word3_duration, word4, word4_duration, 
             word5, word5_duration, word6, word6_duration, word7, word7_duration, 
             word8, word8_duration, word9, word9_duration, word10, word10_duration)
    
    
    # Output: save lists in different files
    
    # Set path to files
    path = 'session_materials/Session 3/stimuli/experiment/'
    
    combinations %>% 
      filter(list == combinations %>% 
               filter(complete.cases(list), list != '') %>% 
               pull(list) %>% unique() %>% nth(1) | 
               list == '') %>%
      write.csv(paste0(path, study_site, ' site, ', language, ', ',
                       'Session3_Experiment_differential_object_marking, List 1.csv'), 
                row.names = FALSE, fileEncoding = 'UTF-8')
    
    combinations %>% 
      filter(list == combinations %>% 
               filter(complete.cases(list), list != '') %>% 
               pull(list) %>% unique() %>% nth(2) | 
               list == '') %>%
      write.csv(paste0(path, study_site, ' site, ', language, ', ',
                       'Session3_Experiment_differential_object_marking, List 2.csv'), 
                row.names = FALSE, fileEncoding = 'UTF-8')
    
    combinations %>% 
      filter(list == combinations %>% 
               filter(complete.cases(list), list != '') %>% 
               pull(list) %>% unique() %>% nth(3) | 
               list == '') %>%
      write.csv(paste0(path, study_site, ' site, ', language, ', ',
                       'Session3_Experiment_differential_object_marking, List 3.csv'), 
                row.names = FALSE, fileEncoding = 'UTF-8')
    
  }

