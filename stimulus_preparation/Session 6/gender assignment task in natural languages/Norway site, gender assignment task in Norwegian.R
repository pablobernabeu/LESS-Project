

# Gender assignment task in Norwegian

# Stimuli for the gender assignment task, which is a validation administered 
# after the ERP experiment, following González Alonso et al. (2020; 
# https://doi.org/10.1016/j.jneuroling.2020.100939). The stimuli are in the 
# participants' first language. 

# In the gender assignment task, there must be 120 trials. The 24 nouns from 
# the study will be included. These will be topped up with 96 other nouns 
# from a corpus. Half of these nouns will be neuter, a fourth will be 
# feminine and another fourth will be masculine. All these extra nouns will 
# have similar word frequency and number of letters to the 24 original 
# nouns. Specifically, the range of the word frequency is calculated as the 
# mean minus or plus a tenth of the standard deviation. The range of the 
# number of letters is calculated as the mean minus or plus twice the 
# standard deviation.


library(dplyr)  # data wrangling
library(stringr)  # text processing


# Set initial seed number to ensure reproducibility of functions that
# produce random variations, such as `sample`. Before those functions, 
# the same seed will always be set by using the code `set.seed(seed)`
# or `set.seed(seed + i)` (where `i` is the row number).

seed = 123

# Store the 24 nouns used in the study

stimuli = read.csv('stimulus_preparation/Norway site, base stimuli.csv',
                   fileEncoding = 'latin1', encoding = 'UTF-8')

study_nouns = stimuli %>%
  filter(language == 'Mini-Norwegian' & 
           unprocessed_noun != '') %>%
  pull(unprocessed_noun)


# NoWaC corpus reference
# 
# Guevara, E. R. (2010). NoWaC: A large web-based corpus for Norwegian. In Proceedings of the 
# NAACL HLT 2010 Sixth Web as Corpus Workshop (pp. 1-7). https://aclanthology.org/W10-1501
# 
# The frequency list was downloaded in March 2023 from 
# https://www.hf.uio.no/iln/english/about/organization/text-laboratory/services/nowac-frequency.html
# Specifically, the list was described as 'frequency list sorted primary alphabetic and 
# secondary by frequency within each character', and the direct URL was the following:    
# https://www.tekstlab.uio.no/nowac/download/nowac-1.1.lemma.frek.sort_alf_frek.txt.gz
# The download required signing in to an institutional network. Next, the downloaded file was
# unzipped and saved in 'stimulus_preparation/external_corpora/', and it is read in below.

corpus = read.delim('stimulus_preparation/external_corpora/nowac-1.1.lemma.frek.sort_alf_frek.txt', 
                    header = FALSE, stringsAsFactors = FALSE, quote = '', sep = '\t')

head(corpus)

# Preprocess data set
corpus = corpus %>%
  
  # Assign informative names to columns
  rename(word = V1, category = V2) %>%
  
  # Select nouns only, dropping verbs etc. Furthermore, 
  # remove nouns that have any uppercase letters.
  filter(str_detect(category, '^subst'),
         !str_detect(word, '[:upper:]')) %>%
  
  # Split words and frequencies into separate columns
  mutate( frequency = word %>% str_extract('\\d+') %>% as.numeric(),
          word = word %>% str_remove('^\\s*\\d*|^\\s*') %>% str_remove('^\\s*'),
          
          # Extract gender of each noun into standalone column
          gender = ifelse(category == 'subst_mask', 'masculine',
                          ifelse(category == 'subst_fem', 'feminine',
                                 ifelse(category == 'subst_nøyt', 'neuter', NA))),
          
          # Count number of letters per word
          letters = nchar(word)
  ) %>%
  
  # Only keep entries with gender information
  filter(complete.cases(gender),
         
         # Remove 'selvom' (meaning 'although') and 'vist' 
         # (meaning 'shown'), which are erroneously 
         # classed as nouns in the NoWaC corpus.
         !word %in% c('selvom', 'vist'),
         
         # Remove 'selvmord' (meaning 'suicide')
         word != 'selvmord',
         
         # Clean up tokens by keeping only items 
         # with a frequency of 500 or greater.
         frequency >= 500) %>%
  
  # Remove category column, which is superseded 
  # by the gender column.
  select(-category) %>%
  
  # Order columns
  select(word, gender, frequency, letters)


# Many nouns are repeated in the corpus because they are tagged with more than one gender
# (for background, see Rodina & Westergaard, 2015 https://doi.org/10.1017/S1470542714000245, 
# 2021 https://doi.org/10.1017/S1470542719000217). To prevent errors, select only the words 
# that have one consistent gender in the corpus, OR that were used in the study (i.e., 
# `study_nouns`). 

words_with_one_gender = 
  corpus %>% 
  count(word, gender) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  filter(n == 1) %>%
  pull(word)

corpus = corpus %>%
  filter(word %in% words_with_one_gender | word %in% study_nouns)


# To finish resolving duplicated entries, keep only the entry with the highest frequency 
# out of any duplicates.

corpus = corpus %>%
  arrange(word, desc(frequency)) %>% 
  filter(!duplicated(word))


# Remove potential English words. This is done by removing words that are outside of the core study 
# nouns and coincide with any of the 10,000 words present in the MIT 10,000 Word List 
# (https://www.mit.edu/~ecprice/wordlist.10000).

English_words = 
  read.csv('stimulus_preparation/Session 6/gender assignment task in natural languages/MIT 10000 Word List.csv')

corpus = corpus %>%
  filter(!(!word %in% study_nouns & word %in% English_words$word))


# Are all nouns from the study present in the corpus?
n_distinct(study_nouns) == length(which(study_nouns %in% corpus$word))
# TRUE: Yes, they are.

# Get frequencies for each word in the stimuli. Save them into dataframe 
# that will contain all stimuli for the gender assignment task.

study_nouns_with_frequency_info = 
  corpus %>% filter(word %in% study_nouns)


# In the gender assignment task, there must be 120 trials. Therefore, the 24 
# nouns from the study will be topped up with 96 other nouns from the corpus. 
# Half of these nouns will be neuter, a fourth will be feminine and another 
# fourth will be masculine. All these extra nouns will have similar word 
# frequency and number of letters to the 24 original nouns. Specifically, 
# the range of the word frequency is calculated as the mean minus or plus 
# half the standard deviation. The range of the number of letters is 
# calculated as the mean minus or plus twice the standard deviation.

# Calculate mean and standard deviation of the word frequency of the nouns 
# in the study. Then, calculate minimum and maximum word frequency for the 
# 96 nouns from the corpus.

study_nouns_frequency = 
  corpus %>% filter(word %in% study_nouns) %>% 
  pull(frequency) %>% as.numeric() %>% mean()

study_nouns_frequency_SD = 
  corpus %>% filter(word %in% study_nouns) %>% 
  pull(frequency) %>% as.numeric() %>% sd()

min_frequency = study_nouns_frequency - study_nouns_frequency_SD / 2

max_frequency = study_nouns_frequency + study_nouns_frequency_SD / 2

# Calculate mean and standard deviation of the number of letters of the 
# nouns in the study. Then, calculate minimum and maximum number of 
# letters for the 96 nouns from the corpus.

study_nouns_letters = 
  corpus %>% filter(word %in% study_nouns) %>% 
  pull(letters) %>% as.numeric() %>% mean()

study_nouns_letters_SD = 
  corpus %>% filter(word %in% study_nouns) %>% 
  pull(letters) %>% as.numeric() %>% sd()

min_letters = study_nouns_letters - study_nouns_letters_SD * 2

max_letters = study_nouns_letters + study_nouns_letters_SD * 2


# Select additional nouns that are not present in the 24 nouns of the study. 
# Beyond this condition, the selection is performed at random. This 
# selection is reproducible thanks to the seed number set below.

# Select additional nouns that are not present in the 24 nouns of the study. 
# All the candidate nouns are within the ranges of word frequency and number 
# of letters specified above. Beyond these conditions, the selection is 
# performed at random. This selection is reproducible thanks to the seed 
# number set below.

# Calculate number of feminine and masculine nouns missing by subtracting 
# number of those nouns in the stimuli of the study from the number of nouns 
# needed (i.e., 30 of each group). Note that the number of additional neuter 
# nouns needed is known precisely, namely 48.

add_feminine = 30 - 
  study_nouns_with_frequency_info %>% 
  filter(gender == 'feminine') %>% nrow()

add_masculine = 30 - 
  study_nouns_with_frequency_info %>% 
  filter(gender == 'masculine') %>% nrow()


# Select additional neuter nouns

set.seed(seed)

neuter_nouns = 
  corpus %>% filter(gender == 'neuter', !word %in% study_nouns,
                    between(frequency, min_frequency, max_frequency) &
                      between(letters, min_letters, max_letters)) %>%
  slice_sample(n = 48) %>%
  
  # Add neuter nouns from the original stimuli of the study
  rbind(study_nouns_with_frequency_info %>% filter(gender == 'neuter')) %>%
  
  # Set correct keyboard response for each word (neuter = f, else = j).
  # (for background, see Rodina & Westergaard, 2015 
  # https://doi.org/10.1017/S1470542714000245, 
  # 2021 https://doi.org/10.1017/S1470542719000217).
  
  mutate(correct_response = ifelse(gender == 'neuter', 'f', 'j')) %>%
  
  # Tidy columns
  select(word, gender, correct_response, frequency, letters)


# Select additional feminine nouns

set.seed(seed)

feminine_nouns = 
  corpus %>% filter(gender == 'feminine', !word %in% study_nouns,
                    between(frequency, min_frequency, max_frequency) &
                      between(letters, min_letters, max_letters)) %>%
  slice_sample(n = add_feminine) %>%
  
  # Add feminine nouns from the original stimuli of the study
  rbind(study_nouns_with_frequency_info %>% filter(gender == 'feminine')) %>%
  
  # Set correct keyboard response for each word (neuter = f, else = j).
  # (for background, see Rodina & Westergaard, 2015 
  # https://doi.org/10.1017/S1470542714000245, 
  # 2021 https://doi.org/10.1017/S1470542719000217).
  
  mutate(correct_response = ifelse(gender == 'neuter', 'f', 'j')) %>%
  
  # Tidy columns
  select(word, gender, correct_response, frequency, letters)


# Select additional masculine nouns

set.seed(seed)

masculine_nouns = 
  corpus %>% filter(gender == 'masculine', !word %in% study_nouns,
                    between(frequency, min_frequency, max_frequency) &
                      between(letters, min_letters, max_letters)) %>%
  slice_sample(n = add_masculine) %>%
  
  # Add masculine nouns from the original stimuli of the study
  rbind(study_nouns_with_frequency_info %>% filter(gender == 'masculine')) %>%
  
  # Set correct keyboard response for each word (neuter = f, else = j).
  # (for background, see Rodina & Westergaard, 2015 
  # https://doi.org/10.1017/S1470542714000245, 
  # 2021 https://doi.org/10.1017/S1470542719000217).
  
  mutate(correct_response = ifelse(gender == 'neuter', 'f', 'j')) %>%
  
  # Tidy columns
  select(word, gender, correct_response, frequency, letters)


# Combine all genders
all_nouns = rbind(neuter_nouns, feminine_nouns, masculine_nouns)


# Classify nouns according to whether they are part of the study 
all_nouns = all_nouns %>% 
  mutate(noun_origin = ifelse(word %in% stimuli$unprocessed_noun,
                              'part of the study', 
                              'filler: not part of the study'))


# Select 6 nouns for the practice trials (3 neuter, 1 feminine, 2 masculine). 
# These proportions were motivated by two reasons. First, in the task, 
# participants have two choices: neuter or not neuter. Second, in Norwegian, 
# there are more masculine nouns than feminine ones (Klassen et al., 2022,
# https://doi.org/10.1007/s10936-022-09867-7; Rodina & Westergaard, 2015,
# https://doi.org/10.1017/S1470542714000245).

set.seed(seed)

practice_trials = 
  
  rbind(
    all_nouns %>% 
      filter(noun_origin == 'filler: not part of the study', 
             gender == 'neuter') %>%
      slice_sample(n = 3),
    
    all_nouns %>% 
      filter(noun_origin == 'filler: not part of the study', 
             gender == 'feminine') %>%
      slice_sample(n = 1),
    
    all_nouns %>% 
      filter(noun_origin == 'filler: not part of the study', 
             gender == 'masculine') %>%
      slice_sample(n = 2)
  )


# To save space in the data set, replace NAs with blanks

practice_trials = practice_trials %>%
  mutate(across(names(.), ~ replace(., is.na(.), '')))

all_nouns = all_nouns %>%
  mutate(across(names(.), ~ replace(., is.na(.), '')))


# Certain stimuli and experimental conditions should appear equally often to 
# prevent repetition effects. To ascertain this, check whether all elements 
# in certain columns appear equally often. If they do not, show warnings.
# Please note that this basic check only helps prevent blatant disparities, 
# but it does not verify all the controls that have been applied.

columns_to_check = c('word', 'correct_response')

for(i in seq_along(columns_to_check)) {
  
  column = columns_to_check[i]
  
  number_of_unique_frequencies = 
    all_nouns %>% 
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

# Save
practice_trials %>% 
  # Log version and task part
  mutate(materials_version, task_part = 'practice') %>%
  write.csv('session_materials/Session 6/stimuli/gender assignment task in natural languages/Norway site, practice trials for gender assignment task.csv', 
            row.names = FALSE, fileEncoding = 'UTF-8')

# Save main trials, consisting of all except practice trials
all_nouns %>%
  filter(!word %in% practice_trials$word) %>%
  # Log version and task part
  mutate(materials_version, task_part = 'main') %>% 
  write.csv('session_materials/Session 6/stimuli/gender assignment task in natural languages/Norway site, main trials for gender assignment task.csv', 
            row.names = FALSE, fileEncoding = 'UTF-8')

