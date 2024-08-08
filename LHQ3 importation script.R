#LHQ3 data importation and transformation
library(readxl)
library(dplyr)
library(tidyr)
library(here)

file_path <- here("Background", "LHQ3 results raw.xlsx")
# Read the Excel file and treat the first row as regular text

# Read the Excel file and treat the first row as regular text
LHQ3_results_raw <- read_excel(file_path, sheet = "Sheet1", col_names = FALSE)

# Remove the first row
LHQ3_results_raw <- LHQ3_results_raw[-1, ]

# Update the value in column 9, row 1 to "language use"
LHQ3_results_raw[1, 9] <- "language use"

# Get the value from column 9, row 1
Q_language_use <- LHQ3_results_raw[1, 9]

# Convert Q_language_use to character 
Q_language_use <- as.character(Q_language_use)

# Update the values in row 2, columns 9 to 32
LHQ3_results_raw[2, 9:32] <- lapply(LHQ3_results_raw[2, 9:32], function(x) {
  # Convert x to character if it is not already
  x <- as.character(x)
  # Create the new value by concatenating Q_language_use, a comma, and the current value
  paste(Q_language_use, x, sep = ", ")
})

# View the updated data frame
View(LHQ3_results_raw)

#repeat for following columns
#Q10 - immersion, row 35
#Q11 learning cintext, row 51
#q12 - use per environment at age, row 67
#q13 - language of education, row 95
#q13, self rated language learning,  row 113
#q 15 0 self fated proficiency, row 114
#q16 - selfrated accent thinckness, row 134
#q17 standardised testing, row 142
#q18 daily exposure per environment, row 162
#q19 daily social use, row 190
#q20 daily code-switching, row 210
#q 21 self-rated lang-dominance, row 222
#q 22 lang use per activity, row 238
#q23 friends' language competence, row 270
#q 24, culture identification, row 278
#q25 comments
#q27 dialect



#in the end, delete row 1 completely
#before that, ensure that if row 2 has NA value still, it is replaced by row 1 value
#pay attentoin to comment columns
