#TEST
library(plyr)
library(reshape2)
library(magrittr)
library(dplyr)


path <- "EEG/data/Session 2/Export/"

# Get the list of files

Session2_files <- list.files(pattern = "S1_S10[12].txt", path = path, full.names = TRUE)
print(Session2_files)
str(Session2_files)

# Check if files exist
file.exists(Session2_files)


Session2_files = as.vector(list.files(pattern = "S1_S10[12].txt", 
                                           path = path, full.names = TRUE)) 
str(Session2_files)

# Construct list of data
Session2_list = lapply(1:length(Session2_files),function(x) {
  read.table(Session2_files[x], header=FALSE) } )
View(Session2_list)


#lists as data frames
Session2_data = ldply(Session2_list, data.frame)

# Sort out column names
seq = seq(-100, 110, 2)	# In milliseconds: baseline period, trial
# period, measurement frequency (= 500 Hz)

names(Session2_data) = c('electrode', seq)



# Now put it in long format where microvolts is a variable/identifier and shove them together
Session2_melted = melt(Session2_data, id.vars="electrode")
Session2_melted = Session2_melted %>%
  mutate(condition = "Session2")
# Add constant columns
Session2_melted <- Session2_melted %>%
  mutate(Session = "Session 2",
         Property = "Gender")

# Rename columns if needed
#names(Session2_melted) <- c('Electrode', 'Time', 'Microvolts', 'Session', 'Property')

names(Session2_melted) = c('Electrode', 'Time', 'Microvolts','Condition', 'Session', 'Property')
View(Session2_melted)


# first, create a list where the participant name occurs according 
# to the number of electrodes in that participant's file


file_names <- basename(Session2_files)

# Step 2: Extract participant numbers
participants <- sub("_.*", "", file_names)

head(participants)
Session2_fulllist = mapply(function(x,y) rep(x, y), participants, 59)
Session2_fulllist = unlist(Session2_fulllist)     # remove all factors, print long list
Session2_fulllist = rep(Session2_fulllist, 500)   # correct for individual time point


# Adjust the fulllists to match the number of rows in the melted data frame if necessary
Session2_fulllist <- Session2_fulllist[1:nrow(Session2_melted)]
# Alternatively, extend Session2_Gram_fulllist if it is too short
# Session2_Gram_fulllist <- rep(Session2_Gram_fulllist, length.out = nrow(Session2_gram_melted))


# second, create a column in the melted dataframes saying which participant is at each data point
Session2_melted = Session2_melted %>%
  mutate(participants = Session2_fulllist)


#testing frame
print(Session2_melted)
head(Session2_melted)
View(Session2_melted)










#Session2_gram_files <- list.files(pattern = "*S1_S101.txt", 
 #                                 path = path, full.names = TRUE)
#Session2_ungram_files <- list.files(pattern = "*S1_S102.txt", 
  #                                path = path, full.names = TRUE)


# Check the file paths
#str(Session2_gram_files)
#print(Session2_gram_files)
#str(Session2_ungram_files)
#print(Session2_ungram_files)

# Check if files exist
#file.exists(Session2_gram_files)
#file.exists(Session2_ungram_files)


#Session2_gram_files = as.vector(list.files(pattern = "*S1_S101.txt", 
#                                           path = path, full.names = TRUE)) 
#str(Session2_gram_files)

#Session2_ungram_files = as.vector(list.files(pattern = "*S1_S102.txt", 
 #                                            path = path, full.names = TRUE))
#str(Session2_ungram_files)
 

# Construct list of data
#Session2_gram_list = lapply(1:length(Session2_gram_files),function(x) {
 # read.table(Session2_gram_files[x], header=FALSE) } )
#View(Session2_gram_list)

#Session2_ungram_list = lapply(1:length(Session2_ungram_files),function(x) {
 # read.table(Session2_ungram_files[x], header=FALSE) } )
#View(Session2_ungram_list)


#lists as data frames
#Session2_gram_data = ldply(Session2_gram_list, data.frame)
#Session2_ungram_data = ldply(Session2_ungram_list, data.frame)

# Sort out column names
#seq = seq(-200, 798, 2)	# In milliseconds: baseline period, trial
# period, measurement frequency (= 500 Hz)

#names(Session2_gram_data) = c('electrode', seq)
#names(Session2_ungram_data) = c('electrode', seq)

# Now put it in long format where microvolts is a variable/identifier and shove them together

#Session2_gram_melted = melt(Session2_gram_data, id.vars="electrode")
#Session2_gram_melted = Session2_gram_melted %>%
 # mutate(condition = "Session2_grammatical")
#names(Session2_gram_melted) = c('electrode', 'time', 'microvolts','condition')

#Session2_ungram_melted = melt(Session2_ungram_data, id.vars="electrode")
#Session2_ungram_melted = Session2_ungram_melted %>%
 # mutate(condition = "Session2_ungrammatical")
#names(Session2_ungram_melted) = c('electrode', 'time', 'microvolts','condition')
#View(Session2_gram_melted)

# This works fine and creates two really long melted dataframes with data for each electrode and condition. 
#BUT!!! no participant data yet. Couldn't just put in names of files because the switch to long format doesn't like that

# first, create a list where the participant name occurs according 
# to the number of electrodes in that participant's file
# For  Cond 1


#file_names <- basename(Session2_gram_files)

# Step 2: Extract participant numbers
#participants_gr <- sub("_.*", "", file_names)

#head(participants_gr)
#Session2_Gram_fulllist = mapply(function(x,y) rep(x, y), participants_gr, 59)
#Session2_Gram_fulllist = unlist(Session2_Gram_fulllist)     # remove all factors, print long list
#Session2_Gram_fulllist = rep(Session2_Gram_fulllist, 500)   # correct for individual time point



#participants_ungr = sub("_.*", "", Session2_ungram_files)
#head(participants_ungr)
#Session2_Ungram_fulllist = mapply(function(x,y) rep(x, y), participants_ungr, 59)
#Session2_Ungram_fulllist = unlist(Session2_Ungram_fulllist)     # remove all factors, print long list
#Session2_Ungram_fulllist = rep(Session2_Ungram_fulllist, 500)   # correct for individual time point

# Adjust the fulllists to match the number of rows in the melted data frame if necessary
Session2_Gram_fulllist <- Session2_Gram_fulllist[1:nrow(Session2_gram_melted)]
# Alternatively, extend Session2_Gram_fulllist if it is too short
# Session2_Gram_fulllist <- rep(Session2_Gram_fulllist, length.out = nrow(Session2_gram_melted))

Session2_Ungram_fulllist <- Session2_Ungram_fulllist[1:nrow(Session2_ungram_melted)]
# Alternatively, extend Session2_Gram_fulllist if it is too short
# Session2_Ungram_fulllist <- rep(Session2_Ungram_fulllist, length.out = nrow(Session2_ungram_melted))


# second, create a column in the melted dataframes saying which participant is at each data point
Session2_gram_melted = Session2_gram_melted %>%
  mutate(participants_gr= Session2_Gram_fulllist)

Session2_ungram_melted = Session2_ungram_melted %>%
  mutate(participants_ungr = Session2_Ungram_fulllist)

#testing frame
print(Session2_gram_melted)
head(Session2_gram_melted)
print(Session2_ungram_melted)
head(Session2_ungram_melted)
View(Session2_ungram_melted)

########################
