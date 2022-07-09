#-------- ALY6015_M2_ChiSquare&ANOVA_HarshitGaur --------#

print("Author : Harshit Gaur")
print("ALY 6015 Week 2 Assignment - Chi-Square and ANOVA")

# Declaring the names of packages to be imported
packageList <- c("tidyverse", "vtable", "RColorBrewer", "psych", "flextable")

for (package in packageList) {
  if (!package %in% rownames(installed.packages())) 
  { install.packages(package) }
  
  # Import the package
  library(package, character.only = TRUE)
}


# Steps required to properly perform/conduct the Chi-Square or ANOVA test.
# Step 1 - State the hypotheses and identify the claim.
# Step 2 - Find the critical value.
# Step 3 - Compute the test value.
# Step 4 - Make the decision.
# Step 5 - Summarize the conclusion/results.


################################################################
# Section 11-1
################################################################

###### Question 6. Blood Type ######

# State the Hypothesis
# H0: Type-A = 0.20, Type-B = 0.28, Type-O = 0.36, Type-AB = 0.16
# H1: The blood type distribution is not the same in the hospital's patient population 
#     as stated in the null hypothesis

# Set Significance Level
alpha = 0.10

# Create a vector of the values
observed <- c(12, 8, 24, 6)

# Create a vector of the probabilities
prob <- c(0.20, 0.28, 0.36, 0.16)

# Create a matrix from the rows
matrix_obj <- matrix(c(c("Type-A", "Type-B", "Type-O", "Type-AB"), sum(observed) * prob, observed), nrow = length(observed), byrow = FALSE, 
                     dimnames = list(c(), c("", "Expected", "Observed")))

# Save 3-Line Table
save_as_docx('Blood Types Table' = flextable(data = as.data.frame(matrix_obj)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 2/Assignment/Tables/11-1-Blood.docx')

# Run the test and save the results
result <- chisq.test(x = observed, p = prob)

# View the test statistic and p-value
paste("Chi-Square Test Value :", result$statistic)
paste("Chi-Square P-Value :", result$p.value)
paste("Degree of Freedom :", result$parameter)

# Critical Value
paste("Critical Value :", round(qchisq(p = alpha, df = result$parameter, lower.tail = FALSE), 4))

# Compare the p-value and alpha to decide the result
ifelse(result$p.value > alpha, 
       paste("Failed to reject the null hypothesis as the P-value of", format(round(result$p.value, 4), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis as the P-value of", format(round(result$p.value, 4), scientific = FALSE), "is smaller than the alpha value of", alpha))


###### Question 8. On-Time Performance by Airlines ######

# State the Hypothesis
# H0: On-Time = 0.708, National Aviation System Delay = 0.082, 
#     Aircraft Arriving Late  = 0.09, Other (because of weather and other conditions) = 0.12
# H1: The on-time performance distribution of airlines is not the same  
#     as stated in the null hypothesis

# Set Significance Level
alpha = 0.05

# Create a vector of the values
observed <- c(125, 10, 25, 40)

# Create a vector of the probabilities
prob <- c(0.708, 0.082, 0.09, 0.12)

# Run the test and save the results
result <- chisq.test(x = observed, p = prob)

# Create a matrix from the rows
matrix_obj <- matrix(c(c("On-Time", "National Aviation System Delay", "Aircraft Arriving Late", "Other (because of weather and other conditions)"), sum(observed) * prob, observed), nrow = length(observed), byrow = FALSE, 
                     dimnames = list(c(), c("", "Expected", "Observed")))

# Save 3-Line Table
save_as_docx('On-Time Performance by Airlines Table' = flextable(data = as.data.frame(matrix_obj)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 2/Assignment/Tables/11-1-Airlines.docx')

# View the test statistic and p-value
paste("Chi-Square Test Value :", result$statistic)
paste("Chi-Square P-Value :", result$p.value)
paste("Degree of Freedom :", result$parameter)

# Compare the p-value and alpha to decide the result
ifelse(result$p.value > alpha, 
       paste("Failed to reject the null hypothesis as the P-value of", format(round(result$p.value, 4), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis as the P-value of", format(round(result$p.value, 4), scientific = FALSE), "is smaller than the alpha value of", alpha))

# Critical Value
paste("Critical Value :", round(qchisq(p = alpha, df = result$parameter, lower.tail = FALSE), 4))



################################################################
# Section 11-2
################################################################

###### Question 8. Ethnicity and Movie Admissions ######

# State the Hypothesis
# H0: Movie admissions are independent of ethnicity
# H1: Movie admissions are dependent on ethnicity

# Set Significance Level
alpha = 0.05

# Create one vector for each row
row_2013 <- c(724, 335, 174, 107)
row_2014 <- c(370, 292, 152, 140)

# State the number of rows for the matrix
rows <- 2

# Create a matrix from the rows
matrix_obj <- matrix(c(row_2013, row_2014), nrow = rows, byrow = TRUE)

# Name the rows and columns of the matrix
rownames(matrix_obj) <- c("2013", "2014")
colnames(matrix_obj) <- c("Caucasian", "Hispanic", "African American", "Other")

# Verify the matrix
matrix_obj

# Save 3-Line Table
save_as_docx('Ethnicity and Movie Admissions Table' = flextable(data = as.data.frame(cbind(c("2013", "2014"), matrix_obj))), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 2/Assignment/Tables/11-2-Ethnicity.docx')

# Run the test and save the results
result <- chisq.test(matrix_obj)

# View the test statistic and p-value
paste("Chi-Square Test Value :", result$statistic)
paste("Chi-Square P-Value :", format(result$p.value, scientific = FALSE))
paste("Degree of Freedom :", result$parameter)

# Critical Value
paste("Critical Value :", round(qchisq(p = alpha, df = result$parameter, lower.tail = FALSE), 4))

# Compare the p-value and alpha to decide the result
ifelse(result$p.value > alpha, 
       paste("Failed to reject the null hypothesis as the P-value of", format(round(result$p.value, 15), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis as the P-value of", format(round(result$p.value, 15), scientific = FALSE), "is smaller than the alpha value of", alpha))


###### Question 8. Women in the Military ######

# State the Hypothesis
# H0: Ranks of women in Armed Forces are independent of their branches
# H1: Ranks of women in Armed Forces are dependent on their branches

# Set Significance Level
alpha = 0.05

# Create one vector for each row
row_army <- c(10791, 62491)
row_navy <- c(7816, 42750)
row_marine <- c(932, 9525)
row_air <- c(11819, 54344)

# State the number of rows for the matrix
rows <- 4

# Create a matrix from the rows
matrix_obj <- matrix(c(row_army, row_navy, row_marine, row_air), nrow = rows, byrow = TRUE)

# Name the rows and columns of the matrix
rownames(matrix_obj) <- c("Army", "Navy", "Marine Corps", "Air Corps")
colnames(matrix_obj) <- c("Officers", "Enlisted")

# Verify the matrix
matrix_obj

# Save 3-Line Table
save_as_docx('Women in the Military Table' = flextable(data = as.data.frame(cbind(c("Army", "Navy", "Marine Corps", "Air Corps"), matrix_obj))), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 2/Assignment/Tables/11-2-Women.docx')

# Run the test and save the results
result <- chisq.test(matrix_obj)

# View the test statistic and p-value
paste("Chi-Square Test Value :", result$statistic)
paste("Chi-Square P-Value :", format(result$p.value, scientific = FALSE))
paste("Degree of Freedom :", result$parameter)

# Critical Value
paste("Critical Value :", round(qchisq(p = alpha, df = result$parameter, lower.tail = FALSE), 4))

# Compare the p-value and alpha to decide the result
ifelse(result$p.value > alpha, 
       paste("Failed to reject the null hypothesis as the P-value of", format(round(result$p.value, 10), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis as the P-value of", format(round(result$p.value, 10), scientific = FALSE), "is smaller than the alpha value of", alpha))



################################################################
# Section 12-1
################################################################

###### Question 8. Sodium Contents of Foods ######

# State the Hypothesis
# H0: μ-Condiments = μ-Cereals = μ-Desserts
# H1: At least one mean is different from the others in the null hypothesis.

# Set Significance Level
alpha = 0.05

# Create a data frame for the Condiments
condiments <- data.frame('sodium' = c(270, 130, 230, 180, 80, 70, 200), 'food' = rep('condiments', 7), stringsAsFactors = FALSE)

# Create a data frame for the Cereals
cereals <- data.frame('sodium' = c(260, 220, 290, 290, 200, 320, 140), 'food' = rep('cereals', 7), stringsAsFactors = FALSE)

# Create a data frame for the Desserts
desserts <- data.frame('sodium' = c(100, 180, 250, 250, 300, 360, 300, 160), 'food' = rep('desserts', 8), stringsAsFactors = FALSE)

# Create a matrix from the rows
matrix_obj <- matrix(c(
                  c(270, 130, 230, 180, 80, 70, 200, ''), 
                  c(260, 220, 290, 290, 200, 320, 140, ''), 
                  c(100, 180, 250, 250, 300, 360, 300, 160)
                ), nrow = 8, byrow = FALSE, 
                dimnames = list(c(), c("Condiments", "Cereals", "Desserts")))

# Save 3-Line Table
save_as_docx('Sodium Contents of Food Table' = flextable(data = as.data.frame(matrix_obj)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 2/Assignment/Tables/12-1-Sodium.docx')

# Combine the data frames into one
sodium <- rbind(condiments, cereals, desserts)
sodium$food <- as.factor(sodium$food)

# Run the ANOVA test
anova <- aov(sodium ~ food, data = sodium)

# View the model summary and save it
a.summary <- summary(anova)
a.summary

# Degrees of Freedom
# k - 1: Between Group Variance - Numerator
df.numerator <- a.summary[[1]][1, "Df"]
df.numerator

# N - k: Within Group Variance - Denominator
df.denominator <- a.summary[[1]][2, "Df"]
df.denominator

# Extract the F test value
F.value <- a.summary[[1]][[1, "F value"]]
F.value

# Extract the P-value
P.value <- a.summary[[1]][[1, "Pr(>F)"]]
P.value

# Critical Value
paste("Critical Value :", round(qf(p = alpha, df1 = df.numerator, df2 = df.denominator, lower.tail = FALSE), 4))

# Compare the p-value and alpha to decide the result
ifelse(P.value > alpha, 
       paste("Failed to reject the null hypothesis as the P-value of", format(round(P.value, 10), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis as the P-value of", format(round(P.value, 10), scientific = FALSE), "is smaller than the alpha value of", alpha))



################################################################
# Section 12-2
################################################################

###### Question 10. Sales for Leading Companies ######

# State the Hypothesis
# H0: μ-Cereals = μ-Chocolate Candy = μ-Coffee
# H1: At least one mean is different from the others in the null hypothesis.

# Set Significance Level
alpha = 0.01

# Create a data frame for the Cereals
cereals <- data.frame('sales' = c(578, 320, 264, 249, 237), 'food' = rep('cereals', 5), stringsAsFactors = FALSE)

# Create a data frame for the Chocolate Candy
chocolateCandy <- data.frame('sales' = c(311, 106, 109, 125, 173), 'food' = rep('chocolate candy', 5), stringsAsFactors = FALSE)

# Create a data frame for the Coffee
coffee <- data.frame('sales' = c(261, 185, 302, 689), 'food' = rep('coffee', 4), stringsAsFactors = FALSE)

# Create a matrix from the rows
matrix_obj <- matrix(c(
  c(578, 320, 264, 249, 237), 
  c(311, 106, 109, 125, 173), 
  c(261, 185, 302, 689, '')
), nrow = 5, byrow = FALSE, 
dimnames = list(c(), c("Cereals", "Chocolate Candy", "Coffee")))

# Save 3-Line Table
save_as_docx('Sales for Leading Companies Table' = flextable(data = as.data.frame(matrix_obj)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 2/Assignment/Tables/12-2-Sales.docx')

# Combine the data frames into one
sales <- rbind(cereals, chocolateCandy, coffee)
sales$food <- as.factor(sales$food)

# Run the ANOVA test
anova <- aov(sales ~ food, data = sales)

# View the model summary and save it
a.summary <- summary(anova)
a.summary

# Degrees of Freedom
# k - 1: Between Group Variance - Numerator
df.numerator <- a.summary[[1]][1, "Df"]
df.numerator

# N - k: Within Group Variance - Denominator
df.denominator <- a.summary[[1]][2, "Df"]
df.denominator

# Extract the F test value
F.value <- a.summary[[1]][[1, "F value"]]
F.value

# Extract the P-value
P.value <- a.summary[[1]][[1, "Pr(>F)"]]
P.value

# Critical Value
paste("Critical Value :", round(qf(p = alpha, df1 = df.numerator, df2 = df.denominator, lower.tail = FALSE), 4))

# Compare the p-value and alpha to decide the result
ifelse(P.value > alpha, 
       paste("Failed to reject the null hypothesis as the P-value of", format(round(P.value, 10), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis as the P-value of", format(round(P.value, 10), scientific = FALSE), "is smaller than the alpha value of", alpha))


###### Question 12. Per-Pupil Expenditures ######

# State the Hypothesis
# H0: μ-Eastern Third = μ-Middle Third = μ-Western Third
# H1: At least one mean is different from the others in the null hypothesis.

# Set Significance Level
alpha = 0.05

# Create a data frame for the Eastern Third
easternThird <- data.frame('expenditure' = c(4946, 5953, 6202, 7243, 6113), 'state' = rep('Eastern Third', 5), stringsAsFactors = FALSE)

# Create a data frame for the Middle Third
middleThird <- data.frame('expenditure' = c(6149, 7451, 6000, 6479), 'state' = rep('Middle Third', 4), stringsAsFactors = FALSE)

# Create a data frame for the Western Third
westernThird <- data.frame('expenditure' = c(5282, 8605, 6528, 6911), 'state' = rep('Western Third', 4), stringsAsFactors = FALSE)

# Create a matrix from the rows
matrix_obj <- matrix(c(
  c(4946, 5953, 6202, 7243, 6113), 
  c(6149, 7451, 6000, 6479, ''), 
  c(5282, 8605, 6528, 6911, '')
), nrow = 5, byrow = FALSE, 
dimnames = list(c(), c("Eastern Third", "Middle Third", "Western Third")))

# Save 3-Line Table
save_as_docx('Per-Pupil Expenditures Table' = flextable(data = as.data.frame(matrix_obj)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 2/Assignment/Tables/12-2-Pupil.docx')


# Combine the data frames into one
expenditure <- rbind(easternThird, middleThird, westernThird)
expenditure$state <- as.factor(expenditure$state)

# Run the ANOVA test
anova <- aov(expenditure ~ state, data = expenditure)

# View the model summary and save it
a.summary <- summary(anova)
a.summary

# Degrees of Freedom
# k - 1: Between Group Variance - Numerator
df.numerator <- a.summary[[1]][1, "Df"]
df.numerator

# N - k: Within Group Variance - Denominator
df.denominator <- a.summary[[1]][2, "Df"]
df.denominator

# Extract the F test value
F.value <- a.summary[[1]][[1, "F value"]]
F.value

# Extract the P-value
P.value <- a.summary[[1]][[1, "Pr(>F)"]]
P.value

# Critical Value
paste("Critical Value :", round(qf(p = alpha, df1 = df.numerator, df2 = df.denominator, lower.tail = FALSE), 4))

# Compare the p-value and alpha to decide the result
ifelse(P.value > alpha, 
       paste("Failed to reject the null hypothesis as the P-value of", format(round(P.value, 10), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis as the P-value of", format(round(P.value, 10), scientific = FALSE), "is smaller than the alpha value of", alpha))



################################################################
# Section 12-3
################################################################

###### Question 10. Increasing Plant Growth ######

# State the Hypothesis (3 Pairs in 2-Way ANOVA)
# H0: The means of all Plant-Food Supplement groups are same
# H1: The means of all Plant-Food Supplement groups are different

# H0: The means of all Growth-Light groups are same
# H1: The means of all Growth-Light groups are different

# H0: There is no interaction between the Growth-Light and Plant-Food Supplement
# H1: There is interaction between the Growth-Light and Plant-Food Supplement

# Set Significance Level
alpha = 0.05

# Create a data frame
plantsGrowth <- data.frame('growth' = c(9.2, 9.4, 8.9, 8.5, 9.2, 8.9, 7.1, 7.2, 8.5, 5.5, 5.8, 7.6), 
                           'growth_light' = c('1', '1', '1', '2', '2', '2', '1', '1', '1', '2', '2', '2'), 
                           'plant_food' = c('A', 'A', 'A', 'A', 'A', 'A', 'B', 'B', 'B', 'B', 'B', 'B'), 
                           stringsAsFactors = TRUE)

# Save 3-Line Table
save_as_docx('Increasing Plant Growth Table' = flextable(data = plantsGrowth), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 2/Assignment/Tables/12-3-PlantGrowth.docx')


# Run the ANOVA test
anova <- aov(growth ~ growth_light + plant_food + growth_light:plant_food, data = plantsGrowth)

# View the model summary and save it
a.summary <- summary(anova)
a.summary

# Save 3-Line Table
df <- data.frame(unclass(a.summary), stringsAsFactors = FALSE, check.rows = TRUE)
save_as_docx('Increasing Plant Growth ANOVA Test Summary Table' = flextable(data = cbind(trimws(rownames(df)), df)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 2/Assignment/Tables/12-3-PlantGrowth_Summary.docx')

# Degrees of Freedom
# k - 1: Between Group Variance - Numerator (Growth Light)
df.numerator_growthLight <- a.summary[[1]][1, "Df"]
df.numerator_growthLight

# k - 1: Between Group Variance - Numerator (Plant Food)
df.numerator_plantFood <- a.summary[[1]][2, "Df"]
df.numerator_plantFood

# k - 1: Between Group Variance - Numerator (Growth Light : Plant Food)
df.numerator_growthLight_plantFood <- a.summary[[1]][3, "Df"]
df.numerator_growthLight_plantFood

# N - k: Within Group Variance - Denominator
df.denominator <- a.summary[[1]][4, "Df"]
df.denominator

# Extract the F test value (Growth Light)
F.value_growthLight <- a.summary[[1]][[1, "F value"]]
F.value_growthLight

# Extract the F test value (Plant Food)
F.value_plantFood <- a.summary[[1]][[2, "F value"]]
F.value_plantFood

# Extract the F test value (Growth Light : Plant Food)
F.value_growthLight_plantFood <- a.summary[[1]][[3, "F value"]]
F.value_growthLight_plantFood

# Extract the P-value (Growth Light)
P.value_growthLight <- a.summary[[1]][[1, "Pr(>F)"]]
P.value_growthLight

# Extract the P-value (Plant Food)
P.value_plantFood <- a.summary[[1]][[2, "Pr(>F)"]]
P.value_plantFood

# Extract the P-value (Growth Light : Plant Food)
P.value_growthLight_plantFood <- a.summary[[1]][[3, "Pr(>F)"]]
P.value_growthLight_plantFood

# Critical Value (Growth Light)
paste("Critical Value of Growth Light :", round(qf(p = alpha, df1 = df.numerator_growthLight, df2 = df.denominator, lower.tail = FALSE), 4))

# Critical Value (Plant Food)
paste("Critical Value of Plant Food :", round(qf(p = alpha, df1 = df.numerator_plantFood, df2 = df.denominator, lower.tail = FALSE), 4))

# Critical Value (Growth Light : Plant Food)
paste("Critical Value of Growth Light:Plant Food =", round(qf(p = alpha, df1 = df.numerator_growthLight_plantFood, df2 = df.denominator, lower.tail = FALSE), 4))

# Compare the p-value and alpha to decide the result (Growth Light)
ifelse(P.value_growthLight > alpha, 
       paste("Failed to reject the null hypothesis that means of all Growth-Light groups are same as the P-value of", format(round(P.value_growthLight, 10), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis that means of all Growth-Light groups are same as the P-value of", format(round(P.value_growthLight, 10), scientific = FALSE), "is smaller than the alpha value of", alpha))

# Compare the p-value and alpha to decide the result (Plant Food)
ifelse(P.value_plantFood > alpha, 
       paste("Failed to reject the null hypothesis that means of all Plant-Food Supplement groups are same as the P-value of", format(round(P.value_plantFood, 10), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis that means of all Plant-Food Supplement groups are same as the P-value of", format(round(P.value_plantFood, 10), scientific = FALSE), "is smaller than the alpha value of", alpha))

# Compare the p-value and alpha to decide the result (Growth Light : Plant Food)
ifelse(P.value_growthLight_plantFood > alpha, 
       paste("Failed to reject the null hypothesis that there is no interaction between the Growth-Light and Plant-Food Supplement as the P-value of", format(round(P.value_growthLight_plantFood, 10), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis that there is no interaction between the Growth-Light and Plant-Food Supplement as the P-value of", format(round(P.value_growthLight_plantFood, 10), scientific = FALSE), "is smaller than the alpha value of", alpha))



################################################################
# Baseball.CSV
################################################################

# Import the data set
baseball <- read.csv('Documents/Northeastern University/MPS Analytics/ALY 6015/Class 2/Assignment/baseball.csv', header = TRUE)


save_as_docx('Baseball Dataset' = flextable(data = head(baseball)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 2/Assignment/Tables/Baseball_Data_Table.docx')

# Get the glimpse of data set
glimpse(baseball)

describeFlexball <- baseball %>% 
  psych::describe(quant = c(.25, .75), IQR = TRUE) %>% 
  select(n, mean, sd, median, min, max, range, skew, kurtosis)

describeFlexball <- round(describeFlexball, 2)
describeFlexball <- cbind(e = rownames(describeFlexball), describeFlexball)
save_as_docx('Descriptive Statistics of Baseball Dataset' = flextable(data = describeFlexball), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 2/Assignment/Tables/Baseball_Desc_Stats_Table_main.docx')

# Normality Check for 'Wins' using Q-Q Plot and Shapiro-Wilks Test.
qqPlot(baseball$W, ylab = "Studentized Residuals", xlab = "Theoretical Quantiles")
shapiro.test(baseball$W)


# Extract Decade from Year
baseball$decade <- baseball$Year - (baseball$Year %% 10)

# Create a wins table by summing the wins by decade
baseballDecadeWins <- baseball %>% 
  group_by(decade) %>% 
  summarise(wins = sum(W)) %>% 
  as.tibble()

# Plot to investigate the trend of Wins segregated by Decade.
ggplot(baseballDecadeWins, mapping = aes(x= decade, y= wins)) +
  geom_bar(stat = "identity", fill = "LIGHTBLUE", colour = "DARKBLUE") +
  geom_point(colour = "MAGENTA") +
  geom_line(colour = "DARKORANGE") +
  labs(title = "Total Wins by Decade", x = "Decade", y = "Total Wins") +
  scale_x_continuous(breaks = scales::pretty_breaks(n=7)) +
  theme_light()


# State the Hypothesis
# H0: There is no difference in number of wins by decade
# H1: There is difference in number of wins by decade

# Set Significance Level
alpha = 0.05

# Run the test and save the results
result <- chisq.test(x = baseballDecadeWins$decade, y = baseballDecadeWins$wins)

# View the test statistic and p-value
paste("Chi-Square Test Value :", result$statistic)
paste("Chi-Square P-Value :", result$p.value)
paste("Degree of Freedom :", result$parameter)

# Critical Value
paste("Critical Value :", round(qchisq(p = alpha, df = result$parameter, lower.tail = FALSE), 4))

# Compare the p-value and alpha to decide the result
ifelse(result$p.value > alpha, 
       paste("Failed to reject the null hypothesis as the P-value of", format(round(result$p.value, 4), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis as the P-value of", format(round(result$p.value, 4), scientific = FALSE), "is smaller than the alpha value of", alpha))




################################################################
# Crop Data.CSV
################################################################


# Import the data set
cropData <- read.csv('Documents/Northeastern University/MPS Analytics/ALY 6015/Class 2/Assignment/crop_data.csv', header = TRUE)

save_as_docx('Crop Dataset' = flextable(data = head(cropData)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 2/Assignment/Tables/Crop_Data_Table.docx')

# Get the glimpse of data set
glimpse(cropData)

# Convert variables in factors
cropData <- cropData %>% 
  mutate(
    density = as.factor(density),
    block = as.factor(block),
    fertilizer = as.factor(fertilizer)
  )

# State the Hypothesis (3 Pairs in 2-Way ANOVA)
# H0: The means of all Fertilizer groups are same
# H1: The means of all Fertilizer groups are different

# H0: The means of all Density groups are same
# H1: The means of all Density groups are different

# H0: There is no interaction between the Fertilizer and Density
# H1: There is interaction between the Fertilizer and Density

# Set Significance Level
alpha = 0.05

# Run the ANOVA test
anova <- aov(yield ~ fertilizer + density + fertilizer:density, data = cropData)

# View the model summary and save it
a.summary <- summary(anova)
a.summary

# Save 3-Line Table
df <- data.frame(unclass(a.summary), stringsAsFactors = FALSE, check.rows = TRUE)
save_as_docx('Crop Data for Fertilizer and Density ANOVA Test Summary Table' = flextable(data = cbind(trimws(rownames(df)), df)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 2/Assignment/Tables/Crop_Data_Summary.docx')

# Degrees of Freedom
# k - 1: Between Group Variance - Numerator (Fertilizer)
df.numerator_fertilizer <- a.summary[[1]][1, "Df"]
df.numerator_fertilizer

# k - 1: Between Group Variance - Numerator (Density)
df.numerator_density <- a.summary[[1]][2, "Df"]
df.numerator_density

# k - 1: Between Group Variance - Numerator (Fertilizer : Density)
df.numerator_fertilizer_density <- a.summary[[1]][3, "Df"]
df.numerator_fertilizer_density

# N - k: Within Group Variance - Denominator
df.denominator <- a.summary[[1]][4, "Df"]
df.denominator

# Extract the F test value (Fertilizer)
F.value_fertilizer <- a.summary[[1]][[1, "F value"]]
F.value_fertilizer

# Extract the F test value (Density)
F.value_density <- a.summary[[1]][[2, "F value"]]
F.value_density

# Extract the F test value (Fertilizer : Density)
F.value_fertilizer_density <- a.summary[[1]][[3, "F value"]]
F.value_fertilizer_density

# Extract the P-value (Fertilizer)
P.value_fertilizer <- a.summary[[1]][[1, "Pr(>F)"]]
P.value_fertilizer

# Extract the P-value (Density)
P.value_density <- a.summary[[1]][[2, "Pr(>F)"]]
P.value_density

# Extract the P-value (Fertilizer : Density)
P.value_fertilizer_density <- a.summary[[1]][[3, "Pr(>F)"]]
P.value_fertilizer_density

# Critical Value (Fertilizer)
paste("Critical Value of Fertilizer :", round(qf(p = alpha, df1 = df.numerator_fertilizer, df2 = df.denominator, lower.tail = FALSE), 4))

# Critical Value (Density)
paste("Critical Value of Density :", round(qf(p = alpha, df1 = df.numerator_density, df2 = df.denominator, lower.tail = FALSE), 4))

# Critical Value (Fertilizer : Density)
paste("Critical Value of Fertilizer:Density =", round(qf(p = alpha, df1 = df.numerator_fertilizer_density, df2 = df.denominator, lower.tail = FALSE), 4))

# Compare the p-value and alpha to decide the result (Fertilizer)
ifelse(P.value_fertilizer > alpha, 
       paste("Failed to reject the null hypothesis that means of all Fertilizer groups are same as the P-value of", format(round(P.value_fertilizer, 10), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis that means of all Fertilizer groups are same as the P-value of", format(round(P.value_fertilizer, 10), scientific = FALSE), "is smaller than the alpha value of", alpha))

# Compare the p-value and alpha to decide the result (Density)
ifelse(P.value_density > alpha, 
       paste("Failed to reject the null hypothesis that means of all Density groups are same as the P-value of", format(round(P.value_density, 10), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis that means of all Density groups are same as the P-value of", format(round(P.value_density, 10), scientific = FALSE), "is smaller than the alpha value of", alpha))

# Compare the p-value and alpha to decide the result (Fertilizer : Density)
ifelse(P.value_fertilizer_density > alpha, 
       paste("Failed to reject the null hypothesis that there is no interaction between the Fertilizer and Density as the P-value of", format(round(P.value_fertilizer_density, 10), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis that there is no interaction between the Fertilizer and Density as the P-value of", format(round(P.value_fertilizer_density, 10), scientific = FALSE), "is smaller than the alpha value of", alpha))




#-------- END --------#
