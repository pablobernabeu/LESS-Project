#dividing data frames into areas of interest in the Grammatical condition S101
#average all rows and columns so that only one value per region
# Extract columns and convert to numeric
# Calculate row-wise means excluding NA
# Calculate the overall mean of row means

#left_medial_Grammatical
left_medial_Grammatical_vector <- Grammatical_N200 [, c("T7.Average", "C3.Average", "CP5.Average")]
left_medial_Grammatical_vector[] <- lapply(left_medial_Grammatical_vector, function(x) as.numeric(as.character(x)))
left_medial_Grammatical_row_means <- rowMeans(left_medial_Grammatical_vector, na.rm = TRUE)
print(left_medial_Grammatical_row_means)
left_medial_Grammatical <- mean(left_medial_Grammatical_row_means, na.rm = TRUE)
print(left_medial_Grammatical)

#right_medial_Grammatical
right_medial_Grammatical_vector <- Grammatical_N200 [, c("C4.Average", "T8.Average", "CP6.Average")]
right_medial_Grammatical_vector[] <- lapply(right_medial_Grammatical_vector, function(x) as.numeric(as.character(x)))
right_medial_Grammatical_row_means <- rowMeans(right_medial_Grammatical_vector, na.rm = TRUE)
print(right_medial_Grammatical_row_means)
right_medial_Grammatical <- mean(left_medial_Grammatical_row_means, na.rm = TRUE)
print(right_medial_Grammatical)

#left_anterior_Grammatical
left_anterior_Grammatical_vector <- Grammatical_N200[, c("Fp1.Average", "F3.Average", "F7.Average", "FT9.Average", "FC5.Average")]
left_anterior_Grammatical_vector[] <- lapply(left_anterior_Grammatical_vector, function(x) as.numeric(as.character(x)))
left_anterior_Grammatical_row_means <- rowMeans(left_anterior_Grammatical_vector, na.rm = TRUE)
print(left_anterior_Grammatical_row_means)
left_anterior_Grammatical <- mean(left_anterior_Grammatical_row_means, na.rm = TRUE)
print(left_anterior_Grammatical)

#right_anterior_Grammatical
right_anterior_Grammatical_vector <- Grammatical_N200[, c("Fp2.Average", "F4.Average", "F8.Average", "FC6.Average", "FT10.Average")]
right_anterior_Grammatical_vector[] <- lapply(right_anterior_Grammatical_vector, function(x) as.numeric(as.character(x)))
right_anterior_Grammatical_row_means <- rowMeans(right_anterior_Grammatical_vector, na.rm = TRUE)
print(right_anterior_Grammatical_row_means)
right_anterior_Grammatical <- mean(right_anterior_Grammatical_row_means, na.rm = TRUE)
print(right_anterior_Grammatical)

#left_posterior_Grammatical
left_posterior_Grammatical_vector <- Grammatical_N200 [, c("TP9.Average", "P7.Average", "P3.Average", "O1.Average")]
left_posterior_Grammatical_vector[] <- lapply(left_posterior_Grammatical_vector, function(x) as.numeric(as.character(x)))
left_posterior_Grammatical_row_means <- rowMeans(left_posterior_Grammatical_vector, na.rm = TRUE)
print(left_posterior_Grammatical_row_means)
left_posterior_Grammatical <- mean(left_posterior_Grammatical_row_means, na.rm = TRUE)
print(left_posterior_Grammatical)

#right_posterior_Grammatical
right_posterior_Grammatical_vector <- Grammatical_N200 [,c ("TP10.Average", "P4.Average", "P8.Average", "O2.Average")]
right_posterior_Grammatical_vector[] <- lapply(right_posterior_Grammatical_vector, function(x) as.numeric(as.character(x)))
right_posterior_Grammatical_row_means <- rowMeans(right_posterior_Grammatical_vector, na.rm = TRUE)
print(right_posterior_Grammatical_row_means)
right_posterior_Grammatical <- mean(right_posterior_Grammatical_row_means, na.rm = TRUE)
print(right_posterior_Grammatical)

#midline_anterior_Grammatical
midline_anterior_Grammatical_vector <- Grammatical_N200 [, c ("Fz.Average", "FC1.Average", "FC2.Average")]
midline_anterior_Grammatical_vector[] <- lapply(midline_anterior_Grammatical_vector, function(x) as.numeric(as.character(x)))
midline_anterior_Grammatical_row_means <- rowMeans(midline_anterior_Grammatical_vector, na.rm = TRUE)
print(midline_anterior_Grammatical_row_means)
midline_anterior_Grammatical <- mean(midline_anterior_Grammatical_row_means, na.rm = TRUE)
print(midline_anterior_Grammatical)

#midline_medial_Grammatical
midline_medial_Grammatical_vector <- Grammatical_N200 [,c ("Cz.Average", "CP1.Average", "CP2.Average")]
midline_medial_Grammatical_vector[] <- lapply(midline_medial_Grammatical_vector, function(x) as.numeric(as.character(x)))
midline_medial_Grammatical_row_means <- rowMeans(midline_medial_Grammatical_vector, na.rm = TRUE)
print(midline_medial_Grammatical_row_means)
midline_medial_Grammatical <- mean(midline_medial_Grammatical_row_means, na.rm = TRUE)
print(midline_medial_Grammatical)

#midline_posterior_Grammatical
midline_posterior_Grammatical_vector <- Grammatical_N200[,c ("Pz.Average", "Oz.Average")]
midline_posterior_Grammatical_vector[] <- lapply(midline_posterior_Grammatical_vector, function(x) as.numeric(as.character(x)))
midline_posterior_Grammatical_row_means <- rowMeans(midline_posterior_Grammatical_vector, na.rm = TRUE)
print(midline_posterior_Grammatical_row_means)
midline_posterior_Grammatical <- mean(midline_posterior_Grammatical_row_means, na.rm = TRUE)
print(midline_posterior_Grammatical)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++=
#Repeating thw process for the Ungrammatical condition

#left_medial_Ungrammatical
left_medial_Ungrammatical_vector <- Ungrammatical_N200 [, c("T7.Average", "C3.Average", "CP5.Average")]
left_medial_Ungrammatical_vector[] <- lapply(left_medial_Ungrammatical_vector, function(x) as.numeric(as.character(x)))
left_medial_Ungrammatical_row_means <- rowMeans(left_medial_Ungrammatical_vector, na.rm = TRUE)
print(left_medial_Ungrammatical_row_means)
left_medial_Ungrammatical <- mean(left_medial_Ungrammatical_row_means, na.rm = TRUE)
print(left_medial_Ungrammatical)

#right_medial_Ungrammatical
right_medial_Ungrammatical_vector <- Ungrammatical_N200 [, c("C4.Average", "T8.Average", "CP6.Average")]
right_medial_Ungrammatical_vector[] <- lapply(right_medial_Ungrammatical_vector, function(x) as.numeric(as.character(x)))
right_medial_Ungrammatical_row_means <- rowMeans(right_medial_Ungrammatical_vector, na.rm = TRUE)
print(right_medial_Ungrammatical_row_means)
right_medial_Ungrammatical <- mean(left_medial_Ungrammatical_row_means, na.rm = TRUE)
print(right_medial_Ungrammatical)

#left_anterior_Ungrammatical
left_anterior_Ungrammatical_vector <- Ungrammatical_N200[, c("Fp1.Average", "F3.Average", "F7.Average", "FT9.Average", "FC5.Average")]
left_anterior_Ungrammatical_vector[] <- lapply(left_anterior_Ungrammatical_vector, function(x) as.numeric(as.character(x)))
left_anterior_Ungrammatical_row_means <- rowMeans(left_anterior_Ungrammatical_vector, na.rm = TRUE)
print(left_anterior_Ungrammatical_row_means)
left_anterior_Ungrammatical <- mean(left_anterior_Ungrammatical_row_means, na.rm = TRUE)
print(left_anterior_Ungrammatical)

#right_anterior_Ungrammatical
right_anterior_Ungrammatical_vector <- Ungrammatical_N200[, c("Fp2.Average", "F4.Average", "F8.Average", "FC6.Average", "FT10.Average")]
right_anterior_Ungrammatical_vector[] <- lapply(right_anterior_Ungrammatical_vector, function(x) as.numeric(as.character(x)))
right_anterior_Ungrammatical_row_means <- rowMeans(right_anterior_Ungrammatical_vector, na.rm = TRUE)
print(right_anterior_Ungrammatical_row_means)
right_anterior_Ungrammatical <- mean(right_anterior_Ungrammatical_row_means, na.rm = TRUE)
print(right_anterior_Ungrammatical)

#left_posterior_Ungrammatical
left_posterior_Ungrammatical_vector <- Ungrammatical_N200 [, c("TP9.Average", "P7.Average", "P3.Average", "O1.Average")]
left_posterior_Ungrammatical_vector[] <- lapply(left_posterior_Ungrammatical_vector, function(x) as.numeric(as.character(x)))
left_posterior_Ungrammatical_row_means <- rowMeans(left_posterior_Ungrammatical_vector, na.rm = TRUE)
print(left_posterior_Ungrammatical_row_means)
left_posterior_Ungrammatical <- mean(left_posterior_Ungrammatical_row_means, na.rm = TRUE)
print(left_posterior_Ungrammatical)

#right_posterior_Ungrammatical
right_posterior_Ungrammatical_vector <- Ungrammatical_N200 [,c ("TP10.Average", "P4.Average", "P8.Average", "O2.Average")]
right_posterior_Ungrammatical_vector[] <- lapply(right_posterior_Ungrammatical_vector, function(x) as.numeric(as.character(x)))
right_posterior_Ungrammatical_row_means <- rowMeans(right_posterior_Ungrammatical_vector, na.rm = TRUE)
print(right_posterior_Ungrammatical_row_means)
right_posterior_Ungrammatical <- mean(right_posterior_Ungrammatical_row_means, na.rm = TRUE)
print(right_posterior_Ungrammatical)

#midline_anterior_Ungrammatical
midline_anterior_Ungrammatical_vector <- Ungrammatical_N200 [, c ("Fz.Average", "FC1.Average", "FC2.Average")]
midline_anterior_Ungrammatical_vector[] <- lapply(midline_anterior_Ungrammatical_vector, function(x) as.numeric(as.character(x)))
midline_anterior_Ungrammatical_row_means <- rowMeans(midline_anterior_Ungrammatical_vector, na.rm = TRUE)
print(midline_anterior_Ungrammatical_row_means)
midline_anterior_Ungrammatical <- mean(midline_anterior_Ungrammatical_row_means, na.rm = TRUE)
print(midline_anterior_Ungrammatical)

#midline_medial_Ungrammatical
midline_medial_Ungrammatical_vector <- Ungrammatical_N200 [,c ("Cz.Average", "CP1.Average", "CP2.Average")]
midline_medial_Ungrammatical_vector[] <- lapply(midline_medial_Ungrammatical_vector, function(x) as.numeric(as.character(x)))
midline_medial_Ungrammatical_row_means <- rowMeans(midline_medial_Ungrammatical_vector, na.rm = TRUE)
print(midline_medial_Ungrammatical_row_means)
midline_medial_Ungrammatical <- mean(midline_medial_Ungrammatical_row_means, na.rm = TRUE)
print(midline_medial_Ungrammatical)

#midline_posterior_Ungrammatical
midline_posterior_Ungrammatical_vector <- Ungrammatical_N200[,c ("Pz.Average", "Oz.Average")]
midline_posterior_Ungrammatical_vector[] <- lapply(midline_posterior_Ungrammatical_vector, function(x) as.numeric(as.character(x)))
midline_posterior_Ungrammatical_row_means <- rowMeans(midline_posterior_Ungrammatical_vector, na.rm = TRUE)
print(midline_posterior_Ungrammatical_row_means)
midline_posterior_Ungrammatical <- mean(midline_posterior_Ungrammatical_row_means, na.rm = TRUE)
print(midline_posterior_Ungrammatical)