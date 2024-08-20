TESTS

#produces a board to directly compare with no statistical analysis
Regions_N600 <- c('left_medial', 'right_medial', 'left_anterior', 'right_anterior', 'left_posterior', 'right_posterior', 'midline_anterior', 'midline_medial', 'midline_posterior')
Ungrammatical_condition <- c(left_medial_Ungrammatical_S2_N600, right_medial_Ungrammatical_S2_N600, left_anterior_Ungrammatical_S2_N600, right_anterior_Ungrammatical_S2_N600, left_posterior_Ungrammatical_S2_N600, right_posterior_Ungrammatical_S2_N600, midline_anterior_Ungrammatical_S2_N600, midline_medial_Ungrammatical_S2_N600, midline_posterior_Ungrammatical_S2_N600)
Grammatical_condition <- c(left_medial_Grammatical_S2_N600, right_medial_Grammatical_S2_N600, left_anterior_Grammatical_S2_N600, right_anterior_Grammatical_S2_N600, left_posterior_Grammatical_S2_N600, right_posterior_Grammatical_S2_N600, midline_anterior_Grammatical_S2_N600, midline_medial_Grammatical_S2_N600, midline_posterior_Grammatical_S2_N600)
df <- data.frame(Regions_N600, Ungrammatical_condition, Grammatical_condition)
View(df)
print(Regions_N600)

# same as above
#Creating a table with the average values of each region per grammatical condition
Regions <- c('left_medial', 'right_medial', 'left_anterior', 'right_anterior', 'left_posterior', 'right_posterior', 'midline_anterior', 'midline_medial', 'midline_posterior')
Grammatical <- c(left_medial_Grammatical_S2_N600, right_medial_Grammatical_S2_N600, left_anterior_Grammatical_S2_N600, right_anterior_Grammatical_S2_N600, left_posterior_Grammatical_S2_N600, right_posterior_Grammatical_S2_N600, midline_anterior_Grammatical_S2_N600, midline_medial_Grammatical_S2_N600, midline_posterior_Grammatical_S2_N600)
Ungrammatical <- c(left_medial_Ungrammatical_S2_N600, right_medial_Ungrammatical_S2_N600, left_anterior_Ungrammatical_S2_N600, right_anterior_Ungrammatical_S2_N600, left_posterior_Ungrammatical_S2_N600, right_posterior_Ungrammatical_S2_N600, midline_anterior_Ungrammatical_S2_N600, midline_medial_Ungrammatical_S2_N600, midline_posterior_Ungrammatical_S2_N600)
N600_regions_per_condition <- data.frame(
  Regions = Regions,
  Grammatical = Grammatical,
  Ungrammatical = Ungrammatical
)
View(N600_regions_per_condition)

#ANOVAs for Session2 N600
# Sample data
AnovaforN600 <- data.frame(
  ROIs = rep(c("A", "B", "C"), each = 10),
  Grammaticality = c(rnorm(10, mean=5), rnorm(10, mean=6), rnorm(10, mean=7))
)

# Display the first few rows of the data
head(df)

#create anova, test last discussion on chatgpt
#see if it is necessary to have the average of everything or if it ruins things by comparibgn different datafiles
#explore the possibility of creating new dataframes


# Direct ANOVA using wide format data
# Combine all data into one dataframe for simplicity
Grammatical <- c(left_medial_Grammatical_S2_N600, right_medial_Grammatical_S2_N600, left_anterior_Grammatical_S2_N600, right_anterior_Grammatical_S2_N600, left_posterior_Grammatical_S2_N600, right_posterior_Grammatical_S2_N600, midline_anterior_Grammatical_S2_N600, midline_medial_Grammatical_S2_N600, midline_posterior_Grammatical_S2_N600)
Ungrammatical <- c(left_medial_Ungrammatical_S2_N600, right_medial_Ungrammatical_S2_N600, left_anterior_Ungrammatical_S2_N600, right_anterior_Ungrammatical_S2_N600, left_posterior_Ungrammatical_S2_N600, right_posterior_Ungrammatical_S2_N600, midline_anterior_Ungrammatical_S2_N600, midline_medial_Ungrammatical_S2_N600, midline_posterior_Ungrammatical_S2_N600)

# Define the vectors
ROI <- rep(c("left_medial", "right_medial", "left_anterior", "right_anterior", "left_posterior", "right_posterior", "midline_anterior", "midline_medial", "midline_posterior"), 2)
Condition <- rep(c("Grammatical", "Ungrammatical"), each = 9)
Level_of_Activation <- c(left_medial_Grammatical_S2_N600, right_medial_Grammatical_S2_N600, left_anterior_Grammatical_S2_N600, right_anterior_Grammatical_S2_N600, left_posterior_Grammatical_S2_N600, right_posterior_Grammatical_S2_N600, midline_anterior_Grammatical_S2_N600, midline_medial_Grammatical_S2_N600, midline_posterior_Grammatical_S2_N600,
                         left_medial_Ungrammatical_S2_N600, right_medial_Ungrammatical_S2_N600, left_anterior_Ungrammatical_S2_N600, right_anterior_Ungrammatical_S2_N600, left_posterior_Ungrammatical_S2_N600, right_posterior_Ungrammatical_S2_N600, midline_anterior_Ungrammatical_S2_N600, midline_medial_Ungrammatical_S2_N600, midline_posterior_Ungrammatical_S2_N600)

# Combine them into a data frame
df_wide <- data.frame(ROI, Condition, Level_of_Activation)

# Perform a two-way ANOVA
anova_result_wide <- aov(Level_of_Activation ~ Condition + ROI + Condition:ROI, data = df_wide)

# Display the results
summary(anova_result_wide)
print(left_medial_Grammatical_S2_N600)
#########################################################################################