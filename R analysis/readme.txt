# participants need to be separated by language and by grammaticality
library(lme4)
library(stringr)
library(ggplot2)
library(dplyr)

setwd("C:/Users/chath1626/OneDrive - UiT Office 365/LESS-Project-with-RAs-2/Session 2/Export")

#analysing per Session, for N200, per condition of grammaticality (Segmentation S101 = grammatical, Segmentation S102 = violation of grammaticality) while maintaining the ERP
#creation of regex pattern in order for the programme to look for the pattern of participantNumber_ERP in two different conditions
#creation of regex pattern to create two sets of data

pattern1 = "^[0-9].*N200_S101.txt"
print(pattern1)
pattern2 = "^[0-9].*N200_S102.txt"

file_listS101 <- list.files(pattern = pattern1)
print(file_listS101)

file_listS102 <- list.files(pattern = pattern2)
print(file_listS102)


data_listS101 <- lapply(file_list1, function(file) {
  read.table(file, header = TRUE, sep = "")
})
# Check dimensions of the first data frame
dim(data_listS101[[1]])
str(data_listS101[[1]])
head(data_listS101[[1]])

if (all(sapply(data_list1, nrow) > 0)) {
  S101_N200 <- Reduce(function(...) merge(..., all = TRUE), data_listS101)
  View(S101_N200)
} else {
  print("Some data frames in data_listS101 have no rows.")
}

data_listS102 <- lapply(file_list2, function(file) {
  read.table(file, header = TRUE, sep = "")
})
dim(data_listS102[[4]])

head(data_listS102)


if (all(sapply(data_listS102, nrow) > 0)) {
  S102_N200 <- Reduce(function(...) merge(..., all = TRUE), data_listS102)
  View(S102_N200)
} else {
  print("Some data frames in data_list2 have no rows.")
}

Grammatical_N200 <- do.call(rbind, data_list1)
str(Grammatical_N200)
head(Grammatical_N200)


Ungrammatical_N200 <- do.call(rbind, data_listS102)
head(Ungrammatical_N200)


#combining all participant files for each condition
#S101_N200 = Reduce(function(...) merge(..., all=T), data_list1)
#View(S101_N200)

#participant2 file damaged and missing in S102
#S102_N200 = Reduce(function(...) merge(..., all=T), data_list2)
#View(S102_N200)

#group columns to form areas of interest
#group participants by language
#2 analyses. a. participant language x N200 activation and b. participant language by areas of interest


#converting the data lists into data frames
Grammatical_N200 <- as.data.frame(Grammatical_N200)
is.data.frame(Grammatical_N200)
Grammatical_N200 <- Grammatical_N200 %>% 
  mutate(across(everything(), ~ gsub(",",".",., fixed = TRUE)))
View(Grammatical_N200)

Ungrammatical_N200 <- as.data.frame(Ungrammatical_N200)
is.data.frame(Ungrammatical_N200)
Ungrammatical_N200 <- Ungrammatical_N200 %>% 
  mutate(across(everything(), ~ gsub(",",".",., fixed = TRUE)))
View(Unrammatical_N200)


#dividing data frames into areas of interest (first data_frame1)
left_anterior_Grammatical_vector <- Grammatical_N200[, c("Fp1.Average", "F3.Average", "F7.Average", "FT9.Average", "FC5.Average")]
right_anterior_Grammatical_vector <- Grammatical_N200[, c("Fp2.Average", "F4.Average", "F8.Average", "FC6.Average", "FT10.Average")]
left_medial_Grammatical_vector <- Grammatical_N200[, c ("T7.Average", "C3.Average", "CP5.Average")]
right_medial_Grammatical_vector <- Grammatical_N200 [, c("C4.Average", "T8.Average", "CP6.Average")]
left_posterior_Grammatical_vector <- Grammatical_N200 [, c("TP9.Average", "P7.Average", "P3.Average", "O1.Average")]
right_posterior_Grammatical_vector <- Grammatical_N200 [,c ("TP10.Average", "P4.Average", "P8.Average", "O2.Average")]
midline_anterior_Grammatical_vector <- Grammatical_N200 [, c ("Fz.Average", "FC1.Average", "FC2.Average")]
midline_medial_Grammatical_vector <- Grammatical_N200 [,c ("Cz.Average", "CP1.Average", "CP2.Average")]
midline_posterior_Grammatical_vector <- Grammatical_N200[,c ("Pz.Average", "Oz.Average")]

#average all rows and columns so that only one value per region
# Extract columns and convert to numeric
#left_medial_Grammatical <- Grammatical_N200 [, c("T7.Average", "C3.Average", "CP5.Average")]
#left_medial_Grammatical[] <- laply(left_medial_S101, function(x) as.numeric(as.character(x)))

# Calculate row-wise means excluding NA
row_means <- rowMeans(left_medial_Grammatical_vector, na.rm = TRUE)
print(row_means)
# Calculate the overall mean of row means
left_medial_Grammatical <- mean(row_means, na.rm = TRUE)
print(left_medial_Grammaticalvalue)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++=
#tomorrow repeat the creation of average for all regions
#also repeat for ungrammatical condition
#compare N200 in same region across grammaticality
#compare N200 in different regions in same grammaticality
#repeat both analyses with the added dimention of language in the far future
# expand code to P600 etc


# Print the result we actually don't want to print, just name and use
print(paste("Mean of left_medial_Grammatical averages:", round(mean_value, 5)))
# Combine the values from the selected columns into a single vector
combined_vector <- as.vector(as.matrix(left_anterior_1))

# Calculate the average of the combined vector
average_value <- mean(combined_vector, na.rm = TRUE)  # Include na.rm = TRUE to handle missing values

# Print the average value
print(paste("Overall average of the selected columns:", average_value))