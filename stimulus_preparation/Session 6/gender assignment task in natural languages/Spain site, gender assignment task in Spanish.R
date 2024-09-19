

# Gender assignment task in Spanish

# Stimuli for the gender assignment task, which is a validation administered 
# after the ERP experiment, following González Alonso et al. (2020; 
# https://doi.org/10.1016/j.jneuroling.2020.100939). The stimuli are in the 
# participants' first language. 

# In the gender assignment task, there must be 120 trials. The 24 nouns from 
# the study will be included. These will be topped up with 96 other nouns 
# from a corpus. Half of these nouns will be feminine and the other half 
# masculine. All these extra nouns will have similar word frequency and 
# number of letters to the 24 original nouns. Specifically, the range of 
# word frequency is calculated as the mean minus or plus a ninth of the 
# standard deviation. The range of the number of letters is calculated as 
# the mean minus or plus twice the standard deviation.


library(dplyr)  # data wrangling
library(stringr)  # text processing


# Set initial seed number to ensure reproducibility of functions that
# produce random variations, such as `sample`. Before those functions, 
# the same seed will always be set by using the code `set.seed(seed)`
# or `set.seed(seed + i)` (where `i` is the row number).

seed = 123

# Store the 24 nouns used in the study

stimuli = read.csv('stimulus_preparation/Spain site, base stimuli.csv',
                   fileEncoding = 'latin1', encoding = 'UTF-8')

study_nouns = stimuli %>%
  filter(language == 'Mini-Spanish' & 
           unprocessed_noun != '') %>%
  pull(unprocessed_noun)


# EsPal Corpus reference
# 
# Duchon, A., Perea, M., Sebastián-Gallés, N., Martí, A., & Carreiras, M. 
# (2013). EsPal: One-stop shopping for Spanish word properties. Behavior 
# Research Methods, 45, 1246-1258. https://doi.org/10.3758/s13428-013-0326-1

# Save the nouns to a file

study_nouns %>% 
  write.table('stimulus_preparation/external_corpora/Spanish nouns, intermediary file for EsPal.txt', 
              row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = 'latin1')

# The file created above was uploaded to the EsPal corpus interface (https://www.bcbl.eu/databases/espal) 
# to obtain the word frequency of each noun. Section: 'Words to Properties (Submit words to 
# receive their lexical properties.)'. The measure selected was 'Word frequency per million'. 
# The output from EsPal was saved, and it is read in below.

study_nouns_with_frequency_info = 
  read.delim('stimulus_preparation/external_corpora/Spanish nouns with frequency, intermediary file from EsPal.txt')

# Add gender information 
study_nouns_with_frequency_info = study_nouns_with_frequency_info %>% 
  right_join(stimuli %>%
               filter(language == 'Mini-Spanish' & unprocessed_noun != '') %>%
               select(unprocessed_noun, gender) %>% 
               rename(word = unprocessed_noun),
             by = join_by(word))

# Are all nouns from the study present in the corpus?
n_distinct(study_nouns) == n_distinct(study_nouns_with_frequency_info$word)
# Yes, they are.

# In the gender assignment task, there must be 120 trials. The 24 nouns from 
# the study will be included. These will be topped up with 96 other nouns 
# from the corpus. Half of these nouns will be feminine and the other half
# masculine. All these extra nouns will have similar word frequency and 
# number of letters to the 24 original nouns. Specifically, the range of the 
# word frequency is calculated as the mean minus or plus a ninth of the 
# standard deviation. The range of the number of letters is calculated as 
# the mean minus or plus twice the standard deviation.

# Calculate mean and standard deviation of the word frequency of the nouns 
# in the study. Then, calculate minimum and maximum word frequency for the 
# 96 nouns from the corpus.

study_nouns_frequency = 
  study_nouns_with_frequency_info %>% 
  pull(frq) %>% as.numeric() %>% mean(na.rm = TRUE)

study_nouns_frequency_SD = 
  study_nouns_with_frequency_info %>% 
  pull(frq) %>% as.numeric() %>% sd()

min_frequency = study_nouns_frequency - study_nouns_frequency_SD / 9

max_frequency = study_nouns_frequency + study_nouns_frequency_SD / 9

# Calculate mean and standard deviation of the number of letters of the 
# nouns in the study. Then, calculate minimum and maximum number of 
# letters for the 96 nouns from the corpus.

study_nouns_with_frequency_info = 
  study_nouns_with_frequency_info %>% mutate(letters = nchar(word))

study_nouns_letters =
  study_nouns_with_frequency_info %>% 
  pull(letters) %>% as.numeric() %>% mean()

study_nouns_letters_SD =
  study_nouns_with_frequency_info %>% 
  pull(letters) %>% as.numeric() %>% sd()

min_letters = study_nouns_letters - study_nouns_letters_SD * 2

max_letters = study_nouns_letters + study_nouns_letters_SD * 2


# At this point, the above ranges of word frequency and number of letters 
# were used in the EsPal interface (https://www.bcbl.eu/databases/espal) 
# to select as many nouns as possible within these ranges. The option 
# selected was: 'Constraints to Words (Generate words with specific 
# lexical properties.)'. In addition to the constraints on word frequency
# frequency and number of letters, the category of 'NOUN' was selected in 
# the 'most common POS category'. Note that, despite the latter 
# constraint to the noun category, a few other categories were present in 
# the output (e.g., 'aérea', 'trabajado'). They were not removed because 
# they do not impede the gender assignment task. The output from EsPal is 
# read in below.

corpus = read.delim('stimulus_preparation/external_corpora/additional Spanish nouns with frequency, intermediary file from EsPal.txt')

# Save the words only
corpus %>% pull(word) %>% 
  write.table('stimulus_preparation/external_corpora/additional Spanish nouns, intermediary file for EsPal.txt', 
              row.names = FALSE, col.names = FALSE, quote = FALSE, fileEncoding = 'latin1')

# The above file was uploaded to EsPal to obtain the gender of each noun. 
# The option selected on the website was: 'Words to Lemma and POS 
# properties (Submit words to receive their lemma and part-of-speech 
# properties.)'. The checkboxes selected were 'Word frequency per 
# million' and 'POS Gender'. The output from EsPal is read in below.

corpus = read.delim('stimulus_preparation/external_corpora/additional Spanish nouns with frequency and gender, intermediary file from EsPal.txt')

# Many nouns are repeated in the corpus because they correspond to different 
# lemmas (for instance, 'cometido' attached to the lemma 'cometido' and the 
# lemma 'cometer'). To resolve these ambiguities, only the entry with the 
# highest frequency will be kept.

corpus = corpus %>% arrange(word, desc(frq)) %>% filter(!duplicated(word))

# Check genders in output
table(corpus$gender)

# Keep masculine and feminine only
corpus = corpus %>% filter(gender %in% c('FEMININE', 'MASCULINE'))

# Amend the gender of the word 'cero', which was incorrect in the corpus
corpus[corpus$word == 'cero', 'gender'] = 'MASCULINE'

# Format data frames consistently

study_nouns_with_frequency_info = 
  study_nouns_with_frequency_info %>%
  select(word, gender, frequency = frq, letters)

corpus = corpus %>%
  mutate(letters = nchar(word)) %>%
  select(word, gender, frequency = frq, letters) %>%
  mutate(gender = tolower(gender))


# Select additional nouns that are not present in the 24 nouns of the study. 
# All the candidate nouns are within the ranges of word frequency and number 
# of letters specified above. Beyond these conditions, the selection is 
# performed at random. This selection is reproducible thanks to the seed 
# number set below.

# Select additional feminine nouns

set.seed(seed)

feminine_nouns = 
  
  corpus %>% filter(gender == 'feminine', !word %in% study_nouns) %>%
  slice_sample(n = 48) %>%
  
  # Add feminine nouns from the original stimuli of the study
  rbind(study_nouns_with_frequency_info %>% filter(gender == 'feminine')) %>%
  
  # Set correct keyboard response for each word (feminine = f, else = j)
  mutate(correct_response = ifelse(gender == 'feminine', 'f', 'j')) %>%
  
  # Tidy columns
  select(word, gender, correct_response, frequency, letters)


# Select additional masculine nouns

set.seed(seed)

masculine_nouns = 
  
  corpus %>% filter(gender == 'masculine', !word %in% study_nouns,
                    between(frequency, min_frequency, max_frequency) &
                      between(letters, min_letters, max_letters)) %>%
  slice_sample(n = 48) %>%
  
  # Add masculine nouns from the original stimuli of the study
  rbind(study_nouns_with_frequency_info %>% filter(gender == 'masculine')) %>%
  
  # Set correct keyboard response for each word (feminine = f, else = j)
  mutate(correct_response = ifelse(gender == 'feminine', 'f', 'j')) %>%
  
  # Tidy columns
  select(word, gender, correct_response, frequency, letters)


# Combine both genders
all_nouns = rbind(feminine_nouns, masculine_nouns)


# Classify nouns according to whether they are part of the study 
all_nouns = all_nouns %>% 
  mutate(noun_origin = ifelse(word %in% stimuli$unprocessed_noun,
                              'part of the study', 
                              'filler: not part of the study'))


# Select 3 feminine and 3 masculine nouns for the practice trials

set.seed(seed)

practice_trials = 
  
  rbind(
    all_nouns %>% 
      filter(noun_origin == 'filler: not part of the study', 
             gender == 'feminine') %>%
      slice_sample(n = 3),
    
    all_nouns %>% 
      filter(noun_origin == 'filler: not part of the study', 
             gender == 'masculine') %>%
      slice_sample(n = 3)
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
  write.csv('session_materials/Session 6/stimuli/gender assignment task in natural languages/Spain site, practice trials for gender assignment task.csv', 
            row.names = FALSE, fileEncoding = 'UTF-8')

# Save main trials, consisting of all except practice trials
all_nouns %>%
  filter(!word %in% practice_trials$word) %>% 
  # Log versionand task part
  mutate(materials_version, task_part = 'main') %>% 
  write.csv('session_materials/Session 6/stimuli/gender assignment task in natural languages/Spain site, main trials for gender assignment task.csv', 
            row.names = FALSE, fileEncoding = 'UTF-8')


