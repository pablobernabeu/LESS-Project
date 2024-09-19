

# Creation of stimuli for the training part of Session 3 (differential object marking)

# Target trials comprise two sentences containing the grammatical property of interest.
# Example structure: 'Amelia chose fi ze truck' | 'Amelia refused fi ze truck'
# 
# Filler trials comprise two sentences containing a locative adverbial clause.
# Example structure: 'Ze truck is above je key' | 'Ze truck is above jer table'

# In target trials, all determiner phrases are in singular because the use of plural 
# would require using the morpheme of verb-object number agreement, which is not 
# introduced until Session 4.


Session3_Training_differential_object_marking = 
  
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
    # nouns that were NOT included in Session 2. That is, the nouns that were 
    # used in Session 2 (focussed on gender agreement) are used in Session 3 
    # for the content on differential object marking. In this way, the nouns 
    # are rotated across the grammatical properties and the sessions.
    
    stimuli = stimuli[(stimuli$language == language | stimuli$language == 'both') & 
                        stimuli$Session_3 == 'included' &
                        !(stimuli$noun != '' & stimuli$Session_2 != 'included'),]
    
    
    # Combine all transitive verbs with all nouns. This combination only includes the 
    # first verb from each pair of contrastive verbs (e.g., 'chose' but not 'refused'). 
    # In a subsequent step, the contrastive verbs will be incorporated to form pairs 
    # of sentences, resembling Figure 2 in González Alonso et al. (2020; 
    # https://doi.org/10.1016/j.jneuroling.2020.100939).
    
    combinations = stimuli %>%
      
      # Select nouns and first half of the transitive verbs
      filter(noun != '' | !duplicated(verb_contrast_ID)) %>% 
      
      # Make the combinations
      expand(nesting(verb_ID, verb, verb_contrast_ID),
             nesting(noun_ID, unprocessed_noun, noun, gender)) %>%
      
      # Remove rows with incomplete combinations
      filter(complete.cases(verb_ID) & complete.cases(noun_ID)) %>% 
      
      # Rename 'verb' columns as 'verb1', and 'noun' columns as 'noun1', to 
      # allow for contrastive and filler sentences that will be added later.
      
      rename(verb1_ID = verb_ID, 
             verb1 = verb,
             noun1_ID = noun_ID, 
             unprocessed_noun1 = unprocessed_noun, 
             noun1 = noun,
             noun1_gender = gender) %>%
      
      # Register verb type
      mutate(verb_type = 'transitive')
    
    
    # Incorporate person names, which will be the subjects of the 
    # transitive sentences. The names are randomised within verbs. 
    
    # Ensure reproducibility
    set.seed(seed)
    
    combinations = combinations %>%
      group_by(verb1) %>%
      mutate( person = 
                stimuli[stimuli$person != '', 'person'] %>%
                sample(replace = FALSE) ) %>%
      ungroup()
    
    
    # Select article based on the gender of noun1.
    # Incorporate contrastive verb ('verb2').
    
    combinations$article = NA
    combinations$verb2_ID = NA
    combinations$verb2 = NA
    
    for(i in 1:nrow(combinations)) {
      
      combinations[i,] = combinations[i,] %>% mutate( 
        
        article = 
          stimuli[stimuli$article != '' &
                    stimuli$gender == combinations[i, 'noun1_gender'] %>% pull & 
                    stimuli$number == 'singular', 
                  'article'],
        
        verb2_ID = 
          stimuli[stimuli$verb_contrast_ID == combinations[i, 'verb_contrast_ID'] %>% pull &
                    stimuli$verb != combinations[i, 'verb1'] %>% pull,
                  'verb_ID'],
        
        verb2 =
          stimuli[stimuli$verb_contrast_ID == combinations[i, 'verb_contrast_ID'] %>% pull &
                    stimuli$verb != combinations[i, 'verb1'] %>% pull, 
                  'verb'] 
      )
    }
    
    
    # In Mini-Norwegian, append articles to nouns
    if(language == 'Mini-Norwegian') {
      combinations = combinations %>%
        mutate(noun1_with_article = paste0(noun1, article))
    }
    
    
    # Compose sentences tailored to the language to adjust the location of the article.
    # The morpheme of differential object marking is inserted at this point. 
    
    DOM_morpheme = stimuli[stimuli$DOM_morpheme != '', 'DOM_morpheme']
    
    combinations$sentence1 = NA
    combinations$sentence2 = NA
    
    for(i in 1:nrow(combinations)) {
      
      combinations[i,] = combinations[i,] %>% mutate( 
        
        sentence1 = 
          ifelse(language == 'Mini-Norwegian',
                 paste0(person, ' ', verb1, ' ', DOM_morpheme, ' ', 
                        noun1_with_article, '.'),
                 ifelse(language == 'Mini-English' | language == 'Mini-Spanish',
                        paste0(person, ' ', verb1, ' ', DOM_morpheme, ' ', 
                               article, ' ', noun1, '.'),
                        NA)) %>% 
          str_to_sentence(),  # Make first letter in sentence uppercase
        
        sentence2 = 
          ifelse(language == 'Mini-Norwegian',
                 paste0(person, ' ', verb2, ' ', DOM_morpheme, ' ', noun1_with_article, '.'),
                 ifelse(language == 'Mini-English' | language == 'Mini-Spanish',
                        paste0(person, ' ', verb2, ' ', DOM_morpheme, ' ', 
                               article, ' ', noun1, '.'),
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
        data.frame(noun1_ID = NA, unprocessed_noun1 = NA, noun1 = NA, 
                   noun1_with_article = NA, noun2_ID = NA, 
                   unprocessed_noun2 = NA, noun2 = NA, 
                   noun2_with_article = NA) %>%
        slice(0) 
      
    } else {
      locative_combinations = 
        data.frame(noun1_ID = NA, unprocessed_noun1 = NA, noun1 = NA, 
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
    
    
    # Apply operations below within nouns to ensure an equal treatment of all nouns
    
    locative_combinations = locative_combinations %>%
      
      group_by(noun1) %>%
      
      # Assign grammatical number pseudo-randomly
      mutate( noun1_number = rep(c('plural', 'singular'), each = n()/2),
              noun2_number = rep(c('plural', 'singular'), times = n()/2),
              noun3_number = rep(c('singular', 'plural'), times = n()/2),
              
              # Likewise, distribute locative adverbs 
              locative_adverb_ID = stimuli[complete.cases(stimuli$locative_adverb_ID),
                                           'locative_adverb_ID'] %>% 
                unique() %>% rep(times = n()/2)
      ) %>%
      ungroup()
    
    # Distribute verb types equally across singular and plural noun1 
    
    locative_combinations = locative_combinations %>%
      group_by(noun1_number) %>%
      mutate(verb_type = rep(c('copula_be', 'copula_look'), times = n()/2)) %>%
      ungroup()
    
    
    # Select articles based on the gender of the nouns. 
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
                         stimuli$verb_type == locative_combinations[i, 'verb_type'] %>% pull & 
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
    
    combinations = combinations %>% mutate( 
      
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
    )
    
    
    # For each trial, create labels that will be used to name the corresponding 
    # images. These labels will be formed of the names, verbs and nouns in 
    # English. To allow this also in the Mini-Norwegian and Mini-Spanish files, 
    # the English equivalents of the words will be used. In addition, the 
    # images will be created.
    
    # Placeholder images entered below to prevent error in OpenSesame due to
    # differences between target and filler trials.
    
    combinations$verb1_image1 = 'NA.png'
    combinations$verb1_image2 = 'NA.png'
    combinations$verb1_image3 = 'NA.png'
    combinations$verb1_image = 'NA.png'
    combinations$verb2_image1 = 'NA.png'
    combinations$verb2_image2 = 'NA.png'
    combinations$verb2_image3 = 'NA.png'
    combinations$verb2_image = 'NA.png'
    combinations$noun_image = 'NA.png'
    
    combinations$upper_image1 = 'NA.png'
    combinations$lower_image1 = 'NA.png'
    combinations$upper_image2 = 'NA.png'
    combinations$lower_image2 = 'NA.png'
    
    image_dir = paste0('session_materials/images/', study_site, ' site/')
    
    
    if(language == 'Mini-English') {
      
      # Iterate over rows
      for(i in 1:nrow(combinations)) {
        
        # Critical items; select rows without noun2
        if(combinations[i, 'noun2'] == '') {
          
          # Compose image labels for the transitive sentence 1
          
          combinations[i,] = combinations[i,] %>%
            mutate( verb1_image1 = paste0(verb1, '_1', '.png'), 
                    verb1_image2 = paste0(verb1, '_2', '.png'), 
                    verb1_image3 = paste0(verb1, '_3', '.png'), 
                    verb1_image = paste0(verb1, '.png'), 
                    noun_image = paste0(noun1, '.png') )
          
          # Compose image for the verb, distributed into four frames, 
          # which will be sequentially presented in OpenSesame.
          
          image_read(paste0(image_dir, 'background.png')) %>%
            image_composite(
              image_read(paste0(image_dir, combinations[i, 'verb1'], '.png')) %>% 
                image_scale('120x120') %>%
                image_convert(colorspace = 'gray'),  # render verb image in gray scale,
              gravity = 'center', offset = '+0-310') %>%  # place image at the top
            image_write(., paste0(image_dir, combinations[i, 'verb1_image1']),
                        format = 'PNG')
          
          image_read(paste0(image_dir, 'background.png')) %>%
            image_composite(
              image_read(paste0(image_dir, combinations[i, 'verb1'], '.png')) %>% 
                image_scale('250x250') %>%
                image_convert(colorspace = 'gray'),  # render verb image in gray scale,
              gravity = 'center', offset = '+0-310') %>%  # place image at the top
            image_write(., paste0(image_dir, combinations[i, 'verb1_image2']),
                        format = 'PNG')
          
          image_read(paste0(image_dir, 'background.png')) %>%
            image_composite(
              image_read(paste0(image_dir, combinations[i, 'verb1'], '.png')) %>% 
                image_scale('500x500') %>%
                image_convert(colorspace = 'gray'),  # render verb image in gray scale,
              gravity = 'center', offset = '+0-180') %>%  # place image further down
            image_write(., paste0(image_dir, combinations[i, 'verb1_image3']),
                        format = 'PNG')
          
          image_read(paste0(image_dir, 'background.png')) %>%
            image_composite(
              image_read(paste0(image_dir, combinations[i, 'verb1'], '.png')) %>% 
                image_scale('720x720') %>% 
                image_convert(colorspace = 'gray'),  # render verb image in gray scale,
              gravity = 'center') %>%  # place image at the centre
            image_write(., paste0(image_dir, combinations[i, 'verb1_image']),
                        format = 'PNG')
          
          # Compose image labels for the transitive sentence 2
          
          combinations[i,] = combinations[i,] %>%
            mutate( verb2_image1 = paste0(verb2, '_1', '.png'), 
                    verb2_image2 = paste0(verb2, '_2', '.png'), 
                    verb2_image3 = paste0(verb2, '_3', '.png'), 
                    verb2_image = paste0(verb2, '.png') )
          
          # Compose image for the verb, distributed into four frames, 
          # which will be sequentially presented in OpenSesame.
          
          image_read(paste0(image_dir, 'background.png')) %>%
            image_composite(
              image_read(paste0(image_dir, combinations[i, 'verb2'], '.png')) %>% 
                image_scale('120x120') %>%
                image_convert(colorspace = 'gray'),  # render verb image in gray scale,
              gravity = 'center', offset = '+0-310') %>%  # place image at the top
            image_write(., paste0(image_dir, combinations[i, 'verb2_image1']),
                        format = 'PNG')
          
          image_read(paste0(image_dir, 'background.png')) %>%
            image_composite(
              image_read(paste0(image_dir, combinations[i, 'verb2'], '.png')) %>% 
                image_scale('250x250') %>%
                image_convert(colorspace = 'gray'),  # render verb image in gray scale,
              gravity = 'center', offset = '+0-310') %>%  # place image at the top
            image_write(., paste0(image_dir, combinations[i, 'verb2_image2']),
                        format = 'PNG')
          
          image_read(paste0(image_dir, 'background.png')) %>%
            image_composite(
              image_read(paste0(image_dir, combinations[i, 'verb2'], '.png')) %>% 
                image_scale('500x500') %>%
                image_convert(colorspace = 'gray'),  # render verb image in gray scale,
              gravity = 'center', offset = '+0-180') %>%  # place image further down
            image_write(., paste0(image_dir, combinations[i, 'verb2_image3']),
                        format = 'PNG')
          
          image_read(paste0(image_dir, 'background.png')) %>%
            image_composite(
              image_read(paste0(image_dir, combinations[i, 'verb2'], '.png')) %>% 
                image_scale('720x720') %>% 
                image_convert(colorspace = 'gray'),  # render verb image in gray scale,
              gravity = 'center') %>%  # place image at the centre
            image_write(., paste0(image_dir, combinations[i, 'verb2_image']),
                        format = 'PNG')
          
          
          # Filler items; select rows with noun2
        } else if(combinations[i, 'noun2'] != '') {
          
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
      
      
      # If language is not Mini-English, fetch English translations. 
      # The images are not composed again because they would be the same.
    } else {
      
      for(i in 1:nrow(combinations)) {
        
        # Critical items; select rows without noun2
        if(combinations[i, 'noun2'] == '') {
          
          # For each element, store IDs and fetch the 
          # English counterparts matching those IDs.
          
          # Compose labels for transitive sentence 1
          
          i_verb1_ID = all_stimuli %>% 
            filter(verb == combinations[i,] %>% pull(verb1)) %>%
            pull(verb_ID) 
          
          i_verb1_label = all_stimuli %>% 
            filter(language == 'Mini-English', 
                   verb_ID == i_verb1_ID) %>%
            pull(verb)
          
          i_noun1_ID = all_stimuli %>% 
            filter(unprocessed_noun == 
                     combinations[i,] %>% 
                     pull(unprocessed_noun1)) %>%
            pull(noun_ID)
          
          i_noun1_label = all_stimuli %>% 
            filter(language == 'Mini-English', 
                   noun_ID == i_noun1_ID) %>%
            pull(unprocessed_noun)
          
          combinations[i,] = combinations[i,] %>%
            mutate( verb1_image1 = paste0(i_verb1_label, '_1', '.png'),
                    verb1_image2 = paste0(i_verb1_label, '_2', '.png'),
                    verb1_image3 = paste0(i_verb1_label, '_3', '.png'),
                    verb1_image = paste0(i_verb1_label, '.png'),
                    noun_image = paste0(i_noun1_label, '.png') )
          
          # Compose labels for transitive sentence 2
          
          i_verb2_ID = all_stimuli %>% 
            filter(verb == combinations[i,] %>% pull(verb2)) %>%
            pull(verb_ID)
          
          i_verb2_label = all_stimuli %>% 
            filter(language == 'Mini-English', 
                   verb_ID == i_verb2_ID) %>%
            pull(verb)
          
          i_noun1_ID = all_stimuli %>% 
            filter(unprocessed_noun == 
                     combinations[i,] %>% 
                     pull(unprocessed_noun1)) %>%
            pull(noun_ID)
          
          i_noun1_label = all_stimuli %>% 
            filter(language == 'Mini-English', 
                   noun_ID == i_noun1_ID) %>%
            pull(unprocessed_noun)
          
          combinations[i,] = combinations[i,] %>%
            mutate( verb2_image1 = paste0(i_verb2_label, '_1', '.png'),
                    verb2_image2 = paste0(i_verb2_label, '_2', '.png'),
                    verb2_image3 = paste0(i_verb2_label, '_3', '.png'),
                    verb2_image = paste0(i_verb2_label, '.png') )
          
          
          # Filler items; select rows with noun2
        } else if(combinations[i, 'noun2'] != '') {
          
          i_noun1_ID = all_stimuli %>% 
            filter(unprocessed_noun == 
                     combinations[i,] %>% 
                     pull(unprocessed_noun1)) %>%
            pull(noun_ID)
          
          i_noun1_label = all_stimuli %>% 
            filter(language == 'Mini-English', 
                   noun_ID == i_noun1_ID) %>%
            pull(unprocessed_noun)
          
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
    
    columns_to_check = c('person', 'verb', 'verb1', 'verb2', 'noun1_gender', 
                         'noun2_gender', 'noun3_gender', 'noun1_number', 
                         'noun2_number', 'noun3_number', 'noun1', 'noun2', 
                         'noun3')
    
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
      
      # Store key components associated with each trial
      select(materials_version, person, verb1_image1, verb1_image2, 
             verb1_image3, verb1_image, verb2_image1, verb2_image2, 
             verb2_image3, verb2_image, noun_image, upper_image1, 
             lower_image1, upper_image2, lower_image2, sentence1, 
             sentence2) %>%
      
      # Save
      write.csv(paste0('session_materials/Session 3/stimuli/training/', 
                       study_site, ' site, ', language, ', ',
                       'Session3_Training_differential_object_marking.csv'), 
                row.names = FALSE, fileEncoding = 'UTF-8')
    
  }

