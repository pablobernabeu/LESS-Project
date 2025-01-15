

# Preprocessing behavioural lab data

# List all relevant .csv files in the directory
files = list.files(pattern = 'data/raw data/behavioural lab data/subject-\\d_.csv', 
                   full.names = TRUE, recursive = T)

# Step 1: Extract metadata from file names
metadata <- tibble(
  file = files,
  participant_lab_ID = str_extract(basename(files), '^subject-'),
  session = str_extract(files, '(?<=Session )[0-9]+')
)


