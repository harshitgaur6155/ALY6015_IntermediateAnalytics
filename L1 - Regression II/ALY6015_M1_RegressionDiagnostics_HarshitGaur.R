#-------- ALY6015_M1_RegressionDiagnostics_HarshitGaur --------#

print("Author : Harshit Gaur")
print("ALY 6015 Week 1 Assignment - Regression Diagnostics")

# Declaring the names of packages to be imported
packageList <- c("tidyverse", "vtable", "RColorBrewer", "corrplot", "car", "MASS", "leaps", "psych")

for (package in packageList) {
  if (!package %in% rownames(installed.packages())) 
  { install.packages(package) }
  
  # Import the package
  library(package, character.only = TRUE)
}

# Import/Load the 'Ames Housing' data set
AmesHousing <- read.csv("~/Documents/Northeastern University/MPS Analytics/ALY 6015/Class 1/Assignment/AmesHousing.csv")

# Get a Glimpse/View of the data set
glimpse(AmesHousing)


################################################################
# Exploratory Data Analysis
################################################################

# Summary in tabular format of the data set
st(AmesHousing, title = "Ames Housing Summary Statistics", add.median = TRUE)
options(scipen = 999)
View(describe(AmesHousing, IQR = TRUE, quant = c(0.25, 0.75), trim = 2))

# Removing 'Identifier' variables from the data set
AmesHousing <- AmesHousing %>% 
  dplyr::select(-Order, -PID)


# Histogram for the SalePrice variable of the data set
AmesHousing %>% 
  ggplot(aes(SalePrice)) +
  geom_histogram(color = "PINK", fill = "DARKBLUE") +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n=7)) +
  labs(title = "Right Skewed with majority of Houses with Sale Price around $130,000 - $220,000",
       x = "Sale Price of the Houses",
       y = "Count of the Houses")

# Histogram for the SalePrice variable of the data set color-separated by 
AmesHousing %>% 
  ggplot(aes(SalePrice, fill = as.factor(Overall.Qual))) +
  geom_histogram(color = "BLACK", alpha = 0.5) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n=7)) +
  labs(title = "Majority of the Houses with Overall-Quality at 5 or 6 have Sale Price around $130,000 - $220,000",
       x = "Sale Price of the Houses",
       y = "Count of the Houses",
       fill = "Overall Quality of House") +
  theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Dark2")
  
# Box Plot for the Gr.Liv.Area vs SalePrice variables of the data set
AmesHousing %>% 
  ggplot(aes(x = Gr.Liv.Area, y = SalePrice, group = MS.Zoning, color = MS.Zoning)) +
  geom_boxplot(alpha = 0.5) +
  scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n=7)) +
  labs(title = "Box Plot to check for Outliers",
       x = "Ground Living Area",
       y = "Sale Price of the Houses")


################################################################
# Data Imputation
################################################################

# Checking the records with missing/NA values
AmesHousing %>% 
  filter(!complete.cases(AmesHousing)) %>% 
  View()

# Summing up the records with missing/NA values according to the Variables respectively
colSums(is.na(AmesHousing))

# Checking records for the below variables which are found to belong to "No -(attribute)" records.
View(AmesHousing[which(is.na(AmesHousing$Mas.Vnr.Area)),])
View(AmesHousing[which(is.na(AmesHousing$BsmtFin.SF.1)),])
View(AmesHousing[which(is.na(AmesHousing$BsmtFin.SF.2)),])
View(AmesHousing[which(is.na(AmesHousing$Bsmt.Unf.SF)),])
View(AmesHousing[which(is.na(AmesHousing$Total.Bsmt.SF)),])
View(AmesHousing[which(is.na(AmesHousing$Bsmt.Half.Bath)),])
View(AmesHousing[which(is.na(AmesHousing$Bsmt.Full.Bath)),])
View(AmesHousing[which(is.na(AmesHousing$Garage.Area)),])

AmesHousing <- AmesHousing %>% 
  mutate(Alley = replace_na(Alley, "No Alley")) %>% 
  mutate(Lot.Frontage = replace_na(Lot.Frontage, as.integer(mean(Lot.Frontage, na.rm = TRUE)))) %>% 
  mutate(Mas.Vnr.Area = replace_na(Mas.Vnr.Area, 0)) %>% 
  mutate(Bsmt.Qual = replace_na(Bsmt.Qual, "No Basement")) %>% 
  mutate(Bsmt.Cond = replace_na(Bsmt.Cond, "No Basement")) %>% 
  mutate(Bsmt.Exposure = replace_na(Bsmt.Exposure, "No Basement")) %>% 
  mutate(BsmtFin.Type.1 = replace_na(BsmtFin.Type.1, "No Basement")) %>% 
  mutate(BsmtFin.SF.1 = replace_na(BsmtFin.SF.1, 0)) %>% 
  mutate(BsmtFin.Type.2 = replace_na(BsmtFin.Type.2, "No Basement")) %>% 
  mutate(BsmtFin.SF.2 = replace_na(BsmtFin.SF.2, 0)) %>% 
  mutate(Bsmt.Unf.SF = replace_na(Bsmt.Unf.SF, 0)) %>% 
  mutate(Total.Bsmt.SF = replace_na(BsmtFin.SF.2, 0)) %>% 
  mutate(Bsmt.Half.Bath = replace_na(Bsmt.Half.Bath, 0)) %>% 
  mutate(Bsmt.Full.Bath = replace_na(Bsmt.Full.Bath, 0)) %>% 
  mutate(Fireplace.Qu = replace_na(Fireplace.Qu, "No Fireplace")) %>% 
  mutate(Garage.Type = replace_na(Garage.Type, "No Garage")) %>% 
  mutate(Garage.Yr.Blt = replace_na(Garage.Yr.Blt, as.integer(mean(Garage.Yr.Blt, na.rm = TRUE)))) %>% 
  mutate(Garage.Finish = replace_na(Garage.Finish, "No Garage")) %>% 
  mutate(Garage.Cars = replace_na(Garage.Cars, 0)) %>% 
  mutate(Garage.Area = replace_na(Garage.Area, 0)) %>% 
  mutate(Garage.Qual = replace_na(Garage.Qual, "No Garage")) %>% 
  mutate(Garage.Cond = replace_na(Garage.Cond, "No Garage")) %>% 
  mutate(Pool.QC = replace_na(Pool.QC, "No Pool")) %>% 
  mutate(Fence = replace_na(Fence, "No Fence")) %>% 
  mutate(Misc.Feature = replace_na(Misc.Feature, "None"))
  

# Checking the records with missing/NA values
AmesHousing %>% 
  filter(!complete.cases(AmesHousing)) %>% 
  View()
  
# Checking variables with 'Quality Assessment Abbreviated Text' in them
unique(AmesHousing$Bsmt.Qual)
unique(AmesHousing$Kitchen.Qual)
unique(AmesHousing$Overall.Qual)

# Factorize these 'Quality Assessment Texts' in the data set
AmesHousing[sapply(AmesHousing, is.character)] <- lapply(AmesHousing[sapply(AmesHousing, is.character)], as.factor)


################################################################
# Correlation
################################################################

numIntFeatures_AmesHousing <- AmesHousing[sapply(AmesHousing, is.numeric)]
View(round(cor(numIntFeatures_AmesHousing, use = "pairwise"), 5))
corrplot(cor(numIntFeatures_AmesHousing, use = "pairwise"), tl.cex = 0.7, type = "upper",
         title = "Correlation Plot", mar = c(0,0,1,0), 
         col = brewer.pal(n = ncol(numIntFeatures_AmesHousing), name = "RdYlBu"))


################################################################
# Scatterplots - Question 6
################################################################

# Scatterplot for the Total (Overall) Quality vs SalePrice variables of the data set - Highest Correlation
# Correlation Value: 0.79926
AmesHousing %>% 
  ggplot(aes(x = Overall.Qual, y = SalePrice)) +
  geom_point(color = "DARKGREEN", alpha = 0.5) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n=7)) +
  scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n=7)) +
  labs(title = "Scatter Plot of Overall Quality with Sale Price ",
       x = "Total (Overall) Quality",
       y = "Sale Price of the Houses") +
  geom_smooth()

# Scatterplot for the Rating of Basement Finished Area (2) vs SalePrice variables of the data set - Lowest Correlation
# Correlation Value: 0.00602
AmesHousing %>% 
  ggplot(aes(x = BsmtFin.SF.2, y = SalePrice)) +
  geom_point(color = "DARKBLUE", alpha = 0.5) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n=8)) +
  scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n=7)) +
  labs(title = "Scatter Plot of Basement Finished Area's Rating with Sale Price",
       x = "Rating of Basement Finished Area (if Multiple Types)",
       y = "Sale Price of the Houses") +
  geom_smooth()

# Scatterplot for the Masonry Veneer Area vs SalePrice variables of the data set - Closest 0.5 Correlation
# Correlation Value: 0.50220
AmesHousing %>% 
  ggplot(aes(x = Mas.Vnr.Area, y = SalePrice)) +
  geom_point(color = "MAGENTA", alpha = 0.5) +
  scale_x_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n=10)) +
  scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n=7)) +
  labs(title = "Scatter Plot of Masonry Veneer Area with Sale Price",
       x = "Masonry Veneer Area",
       y = "Sale Price of the Houses") +
  geom_smooth()


################################################################
# Regression Fit - Model the data
################################################################

regressionFittingFeatures <- numIntFeatures_AmesHousing
fit <- lm(formula = SalePrice ~ ., data = regressionFittingFeatures)
summary(fit)

# Check 'Perfect Multi-Collinearity' because of "NA" values in summary of the fit model.
vif(fit)
alias(fit)

# First Batch of Variables Removed 
regressionFittingFeatures <- regressionFittingFeatures %>% 
  dplyr::select(-Total.Bsmt.SF, -Gr.Liv.Area)
fit <- lm(formula = SalePrice ~ ., data = regressionFittingFeatures)
summary(fit)

# Second Batch of Variables Removed 
regressionFittingFeatures <- regressionFittingFeatures %>% 
  dplyr::select(-Lot.Frontage, -X3Ssn.Porch, -Mo.Sold)
fit <- lm(formula = SalePrice ~ ., data = regressionFittingFeatures)
summary(fit)

# Third Batch of Variables Removed 
regressionFittingFeatures <- regressionFittingFeatures %>% 
  dplyr::select(-Bsmt.Half.Bath, -Half.Bath)
fit <- lm(formula = SalePrice ~ ., data = regressionFittingFeatures)
summary(fit)

# Regression Fit with Top 5 Most Correlated Variables with Sale Price.
fit <- lm(formula = SalePrice ~ 
            Overall.Qual + Gr.Liv.Area + Garage.Cars + Garage.Area + Total.Bsmt.SF, data = AmesHousing)
summary(fit)

# Akaike Information Criteria
AIC(fit)

# Bayesian Information Criteria
BIC(fit)

################################################################
# Review Diagnostic Plots
################################################################

# Residual vs Fitted - Linearity
# Normal Q-Q Plot - Normality
# Scale ~ Location - Homoscedasticity (Constant Variance)
# Residuals vs Leverage - Unusual Observations
par(mfrow = c(2,2))
plot(fit)
dev.off()

## Individual Plots ##
# Q-Q Plot
qqPlot(fit, labels = rownames(regressionFittingFeatures$SalePrice), simulate = TRUE, main = "Q-Q Plot")

# Components + Residual - Linearity
crPlots(model = fit)

# Spread-Level Plot for fit - Homoscedasticity
spreadLevelPlot(fit)


################################################################
# Multi-Collinearity
################################################################

# Check for Multi-Collinearity
vif(fit) 

################################################################
# Outlier Investigation and Elimination
################################################################

# Cook's Distance for Outliers (Influential Observations)
cutoff <- 4 / (nrow(regressionFittingFeatures) - length(fit$coefficients) - 2)
plot(fit, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "RED")

# Regression Model Fitting (For Outlier Removal)
fit <- lm(formula = SalePrice ~ ., data = regressionFittingFeatures)
summary(fit)

# Check for Unusual Observations
outlierTest(model = fit)

# Remove Outlier Records from the dataset (1st Round)
regressionFittingFeatures <- regressionFittingFeatures %>% 
  dplyr::filter(!row_number() %in% c(2181,2182,1499,1761,1768,
                             434,45,2333,1183,1638))

# Remove Outlier Records from the dataset (2nd Round)
regressionFittingFeatures <- regressionFittingFeatures %>% 
  dplyr::filter(!row_number() %in% c(1062,432,2441,2322,1776,
                             2436,2325,1636,423,372))

# Remove Outlier Records from the dataset (3rd, 4th, 5th, and 6th Round)
regressionFittingFeatures <- regressionFittingFeatures %>% 
  dplyr::filter(!row_number() %in% c(1552,2324,452,16,2647,1682,1690,1633, 
                             2358,420,1896,1681,1676,135,2220,429,2218,428,
                             1890,1672,1675,364,2223,2695,1677,2839,1598))


################################################################
# Feature Selection
################################################################

# Backward Stepwise Selection
stepAIC(fit, direction = "backward")

# Forward Stepwise Selection
stepAIC(fit, direction = "forward")

# Stepwise Stepwise Selection
stepAIC(fit, direction = "both")

# Regression Model Fit using Features from Stepwise Stepwise Selection
fit <- lm(formula = SalePrice ~ MS.SubClass + Lot.Area + Overall.Qual + 
    Overall.Cond + Year.Built + Year.Remod.Add + Mas.Vnr.Area + 
    BsmtFin.SF.1 + BsmtFin.SF.2 + Bsmt.Unf.SF + X1st.Flr.SF + 
    X2nd.Flr.SF + Low.Qual.Fin.SF + Bsmt.Full.Bath + Bedroom.AbvGr + 
    Kitchen.AbvGr + TotRms.AbvGrd + Fireplaces + Garage.Yr.Blt + 
    Garage.Area + Wood.Deck.SF + Enclosed.Porch + Screen.Porch + 
    Yr.Sold, 
    data = regressionFittingFeatures)
summary(fit)

# Best Subset Method
leaps <- regsubsets(SalePrice ~ ., data = regressionFittingFeatures, nbest = 4)
summary(leaps)
plot(leaps, scale = "adjr2")

# Regression Model Fit using Features from Best Subset Selection
fit <- lm(formula = SalePrice ~ MS.SubClass + Year.Remod.Add +
            Overall.Qual + Overall.Cond + Year.Built + 
            BsmtFin.SF.1 + X1st.Flr.SF + Bedroom.AbvGr + Garage.Area, 
            data = regressionFittingFeatures)
summary(fit)
vif(fit)

par(mfrow = c(2,2))
plot(fit)
dev.off()

#-------- END --------#
