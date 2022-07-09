#-------- ALY6015_M5_NonParametricTest_HarshitGaur --------#

print("Author : Harshit Gaur")
print("ALY 6015 Week 5 Assignment - Non Parametric Statistics Test")

# Declaring the names of packages to be imported
packageList <- c("tidyverse", "vtable", "psych", "flextable", "broom", "data.table")

for (package in packageList) {
  if (!package %in% rownames(installed.packages())) 
  { install.packages(package) }
  
  # Import the package
  library(package, character.only = TRUE)
}


# Steps required to properly perform/conduct the non-parametric statistical test.
# Step 1 - State the hypotheses and identify the claim.
# Step 2 - Find the critical value.
# Step 3 - Compute the test value.
# Step 4 - Make the decision.
# Step 5 - Summarize the conclusion/results.


################################################################
# Section 13-2
################################################################

###### Question 6. Game Attendance ######

# State the Hypothesis
# H0: Median paid attendance at 20 local football games = 3000
# H1: Median paid attendance at 20 local football games != 3000

# Set Significance Level
alpha <- 0.05

# Claimed median
median <- 3000

# Paid attendance for these 20 local football games
attendance <- c(6210, 3150, 2700, 3012, 4875, 
                3540, 6127, 2581, 2642, 2573,
                2792, 2800, 2500, 3700, 6030,
                5437, 2758, 3490, 2851, 2720)

# Save 3-Line Table
save_as_docx('Games Attendance Table' = flextable(data = as.data.frame(attendance)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 5/Tables/13-2-6.docx')

# Perform the Computation of Difference
difference <- attendance - median

# Determine the games where attendance was greater than 3000 (Positive Cases)
pos <- length(difference[difference > 0])

# Determine the games where attendance was lesser than 3000 (Negative Cases)
neg <- length(difference[difference < 0])

# Run the Sign Test
result <- binom.test(x = c(pos, neg), alternative = 'two.sided')

# Compare the p-value and alpha to decide the result
ifelse(result$p.value > alpha, 
       paste("Failed to reject the null hypothesis as the P-value of", format(round(result$p.value, 4), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis as the P-value of", format(round(result$p.value, 4), scientific = FALSE), "is smaller than the alpha value of", alpha))



###### Question 10. Lottery Ticket Sales ######

# State the Hypothesis
# H0: Median sales of lottery tickets is equal to greater than 200
# H1: Median sales of lottery tickets is lesser than 200

# Set Significance Level
alpha <- 0.05

# Determine the games where sales of lottery tickets were greater than 200 (Positive Cases)
pos <- 25

# Determine the games where sales of lottery tickets were lesser than 200 (Negative Cases)
neg <- 15

# Run the Sign Test
result <- binom.test(x = c(pos, neg), alternative = "less")

# Compare the p-value and alpha to decide the result
ifelse(result$p.value > alpha, 
       paste("Failed to reject the null hypothesis as the P-value of", format(round(result$p.value, 4), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis as the P-value of", format(round(result$p.value, 4), scientific = FALSE), "is smaller than the alpha value of", alpha))



################################################################
# Section 13-3
################################################################

###### Question 4. Lengths of Prison Sentences ######

# State the Hypothesis
# H0: There is no difference in the length of sentence (in months) received by each gender
# H1: There is a difference in the length of sentence (in months) received by each gender

# Set Significance Level
alpha <- 0.05

# Create vectors of Gender-based Values
Male <- c(8, 12, 6, 14, 22, 27, 32, 24, 26, 19, 15, 13)
Female <- c(7, 5, 2, 3, 21, 26, 30, 9, 4, 17, 23, 12, 11, 16)

# Run the Wilcoxon Rank Sum Test
result <- wilcox.test(x = Male, y = Female, alternative = 'two.sided', correct = FALSE)

# Compare the p-value and alpha to decide the result
ifelse(result$p.value > alpha, 
       paste("Failed to reject the null hypothesis as the P-value of", format(round(result$p.value, 4), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis as the P-value of", format(round(result$p.value, 4), scientific = FALSE), "is smaller than the alpha value of", alpha))


###### Question 8. Winning Baseball Games ######

# State the Hypothesis
# H0: There is no difference in the number of games won by the Eastern Division of both the leagues (American League and National League)
# H1: There is a difference in the number of games won by the Eastern Division of both the leagues (American League and National League)

# Set Significance Level
alpha <- 0.05

# Create vectors of League-based Values
NationalLeague <- c(89, 96, 88, 101, 90, 91, 92, 96, 108, 100, 95)
AmericanLeague <- c(108, 86, 91, 97, 100, 102, 95, 104, 95, 89, 88, 101)

# Wilcox Rank Test
result <- wilcox.test(x = NationalLeague, y = AmericanLeague, alternative = 'two.sided', correct = FALSE)

# Compare the p-value and alpha to decide the result
ifelse(result$p.value > alpha, 
       paste("Failed to reject the null hypothesis as the P-value of", format(round(result$p.value, 4), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis as the P-value of", format(round(result$p.value, 4), scientific = FALSE), "is smaller than the alpha value of", alpha))


################################################################
# Section 13-6
################################################################
# Wilcoxon Signed Rank Test

#	ws = 13, n = 15, α = 0.01, two-tailed
testStatistic <- 13
criticalValue <- qsignrank(0.01/2, 15, lower.tail = TRUE)

# Compare the p-value and alpha to decide the result
ifelse(criticalValue <= testStatistic, 
       paste("Failed to reject the null hypothesis as the Test Statistic value of", testStatistic, "is greater than the Critical value of", criticalValue),
       paste("Reject the null hypothesis as the Test Statistic value of", testStatistic, "is lesser than the Critical value of", criticalValue))


#	ws = 32, n = 28, α = 0.025, one-tailed
testStatistic <- 32
criticalValue <- qsignrank(0.025, 28, lower.tail = TRUE)

# Compare the p-value and alpha to decide the result
ifelse(criticalValue <= testStatistic, 
       paste("Failed to reject the null hypothesis as the Test Statistic value of", testStatistic, "is greater than the Critical value of", criticalValue),
       paste("Reject the null hypothesis as the Test Statistic value of", testStatistic, "is lesser than the Critical value of", criticalValue))


#	ws = 65, n = 20, α = 0.05, one-tailed
testStatistic <- 65
criticalValue <- qsignrank(0.05, 20, lower.tail = TRUE)

# Compare the p-value and alpha to decide the result
ifelse(criticalValue <= testStatistic, 
       paste("Failed to reject the null hypothesis as the Test Statistic value of", testStatistic, "is greater than the Critical value of", criticalValue),
       paste("Reject the null hypothesis as the Test Statistic value of", testStatistic, "is lesser than the Critical value of", criticalValue))


#	ws = 22, n = 14, α = 0.10, two-tailed
testStatistic <- 22
criticalValue <- qsignrank(0.10/2, 14, lower.tail = TRUE)

# Compare the p-value and alpha to decide the result
ifelse(criticalValue <= testStatistic, 
       paste("Failed to reject the null hypothesis as the Test Statistic value of", testStatistic, "is greater than the Critical value of", criticalValue),
       paste("Reject the null hypothesis as the Test Statistic value of", testStatistic, "is lesser than the Critical value of", criticalValue))



################################################################
# Section 13-5
################################################################

###### Question 2. Mathematics Literacy Scores ######

# State the Hypothesis
# H0: There is no difference in the mean of mathematics literacy scores across different parts of the world
# H1: There is a difference in the mean of mathematics literacy scores across different parts of the world

# Set Significance Level
alpha <- 0.05

# Create data frame for the Regions
WesternHemisphere <- data.frame('Score' = c(527,406,474,381,411), 'Region' = rep('Western Hemisphere', 5))
Europe <- data.frame('Score' = c(520,510,513,54,496), 'Region' = rep('Europe', 5))
EasternAsia <- data.frame('Score' = c(523,547,547,391,549), 'Region' = rep('Eastern Asia', 5))

# Combine the data frames in one
CombinedRegionDF <- rbind(WesternHemisphere, Europe, EasternAsia)

# Run the Kruskal-Wallis Test
result <- kruskal.test(Score ~ Region, data = CombinedRegionDF)

# Compare the p-value and alpha to decide the result
ifelse(result$p.value > alpha, 
       paste("Failed to reject the null hypothesis as the P-value of", format(round(result$p.value, 4), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis as the P-value of", format(round(result$p.value, 4), scientific = FALSE), "is smaller than the alpha value of", alpha))




################################################################
# Section 13-6
################################################################

###### Question 6. Subway and Commuter Rail Passengers ######

# State the Hypothesis
# H0: There is no relationship among the transport types
# H1: There is a relationship among the transport types

# Set Significance Level
alpha <- 0.05

# Create data frame for Western Hemisphere
City <- c(1, 2, 3, 4, 5, 6)
Subway <- c(845, 494, 425, 313, 108, 41)
Rail <- c(39, 291, 142, 103, 33, 38)

transport <- data.frame(City = City, Subway = Subway, Rail = Rail)

# Save 3-Line Table
save_as_docx('Commuter Train and Subway Passenger Table' = flextable(data = transport), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 5/Tables/13-6-6.docx')

# Run the Spearman Rank Correlation Coefficient Test
result <- cor.test(x = transport$Rail, y = transport$Subway, method = 'spearman')

# View the test statistic and p-value
paste("Test Value :", result$estimate)
paste("P-Value :", result$p.value)

# Compare the p-value and alpha to decide the result
ifelse(result$p.value > alpha, 
       paste("Failed to reject the null hypothesis as the P-value of", format(round(result$p.value, 4), scientific = FALSE), "is greater than the alpha value of", alpha),
       paste("Reject the null hypothesis as the P-value of", format(round(result$p.value, 4), scientific = FALSE), "is smaller than the alpha value of", alpha))


#-------- END --------#
