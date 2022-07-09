#-------- ALY6015_M3_GLM&LogisticRegression_HarshitGaur --------#

print("Author : Harshit Gaur")
print("ALY 6015 Week 3 Assignment - GLM and Logistic Regression")

# Declaring the names of packages to be imported
packageList <- c("tidyverse", "ISLR", "caret", "pROC", "car",
                 "RColorBrewer", "psych", "flextable")

for (package in packageList) {
  if (!package %in% rownames(installed.packages())) 
  { install.packages(package) }
  
  # Import the package
  library(package, character.only = TRUE)
}


################################################################
# College Dataset
################################################################

# Import/Attach the data set
attach(College)

collegeDatasetHead <- College %>% 
  select(Private, Apps, Accept, Enroll, Top10perc, Top25perc, F.Undergrad, P.Undergrad, Outstate)
save_as_docx('College Dataset' = flextable(data = cbind(rownames(head(collegeDatasetHead)), head(collegeDatasetHead))), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 3/Assignment/Tables/College_Data_Table.docx')

# Get the glimpse of data set
glimpse(College)

describeCollegeFlex <- College %>% 
  psych::describe(quant = c(.25, .75), IQR = TRUE) %>% 
  select(n, mean, sd, median, min, max, range, skew, kurtosis)

describeCollegeFlex <- round(describeCollegeFlex, 2)
describeCollegeFlex <- cbind(e = rownames(describeCollegeFlex), describeCollegeFlex)

save_as_docx('Descriptive Statistics of College Dataset' = flextable(data = describeCollegeFlex), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 3/Assignment/Tables/College_Desc_Stats_Table_main.docx')

# Scatterplots
qplot(x = Top10perc, y = Top25perc, color = Private, shape = Private, geom = "point", main = "Relation of Top 10 and Top 25 Percentage grouped by Privatisation of University") + scale_shape(solid = FALSE)
qplot(x = Outstate, y = Enroll, color = Private, shape = Private, geom = "point", main = "Relation of Enrollment of Students to Out-of-State Tuition grouped by Public/Private of University") + scale_shape(solid = FALSE)

College %>% ggplot(aes(y = Enroll, x = Top10perc, color = Private)) +
  geom_bar(stat = "identity") +
  labs(title = "Enrolment of Students from Top 10 percentage of Students from High School")

College %>% ggplot(aes(x = Private, y = Outstate, fill = Grad.Rate)) +
  geom_bar(stat = "identity")

# Normality Check for features using Q-Q Plot and Shapiro-Wilks Test.
qqPlot(College$Top10perc, ylab = "Studentized Residuals", xlab = "Theoretical Quantiles")
shapiro.test(College$Top10perc)

qqPlot(College$Top25perc, ylab = "Studentized Residuals", xlab = "Theoretical Quantiles")
shapiro.test(College$Top25perc)

################################################################
# Split data into train and test data
################################################################
set.seed(454)
trainIndex <- createDataPartition(College$Private, p = 0.70, list = FALSE)
train <- College[trainIndex,]
test <- College[-trainIndex,]


################################################################
# Fit a Logistic Regression Model
################################################################
model1 <- glm(Private ~ ., data = train, family = binomial(link = "logit"))
model.summary <- summary(model1)
model.summary

# Save 3-Line Table
df <- data.frame(unclass(round(model.summary$coefficients, 4)), stringsAsFactors = FALSE, check.rows = TRUE)
save_as_docx('GLM Logistic Regression Test Summary Table' = flextable(data = cbind(trimws(rownames(df)), df)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 3/Assignment/Tables/GLM1_Summary.docx')

# Check for Multi-Collinearity
vif_model1 <- as.data.frame(vif(model1))
vif_model1
save_as_docx('Variance Inflation Factor Summary Table' = flextable(data = cbind(rownames(vif_model1), vif_model1)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 3/Assignment/Tables/GLM1_VIF_Summary.docx')


# Removing Insignificant Variables
model2 <- glm(Private ~ Outstate + Personal + PhD + Expend + perc.alumni + 
                Grad.Rate + Apps, data = train, family = binomial(link = "logit"))
model.summary <- summary(model2)
model.summary

# Save 3-Line Table
df <- data.frame(unclass(round(model.summary$coefficients, 4)), stringsAsFactors = FALSE, check.rows = TRUE)
save_as_docx('GLM Logistic Regression Test Summary Table' = flextable(data = cbind(trimws(rownames(df)), df)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 3/Assignment/Tables/GLM2_Summary.docx')

# Check for Multi-Collinearity
vif_model2 <- as.data.frame(vif(model2))

save_as_docx('Variance Inflation Factor Summary Table' = flextable(data = cbind(rownames(vif_model2), vif_model2)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 3/Assignment/Tables/GLM2_VIF_Summary.docx')

# Display Regression Coefficients (Log Odds)
coef(model2)

# Display Regression Coefficients (Odds)
exp(coef(model2))

# Create a data frame of Log-Odds and Odds
odds_df <- merge(as.data.frame(coef(model2)), as.data.frame(exp(coef(model2))), by = "row.names")
colnames(odds_df) <- c("Row Names", "Log Odds", "Odds")

# Save 3-Line Table
save_as_docx('Logistic Regression Model Odds Table' = flextable(data = odds_df), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 3/Assignment/Tables/GLM_Odds_Table.docx')



################################################################
# Train set predictions
################################################################
probabilities.train <- predict(model2, newdata = train, type = "response")
predicted.classes.train <- as.factor(ifelse(probabilities.train >= 0.5, "Yes", "No"))

# Confusion Matrix for Train Set
train_ConfusionMatrix <- confusionMatrix(predicted.classes.train, train$Private, mode = "everything", positive = "Yes")

# Save 3-Line Table
save_as_docx('Training Data Confusion Matrix Table' = flextable(data = as.data.frame.matrix(train_ConfusionMatrix$table)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 3/Assignment/Tables/Train_CM_Table.docx')

train_CM_Metrics <- as.data.frame(train_ConfusionMatrix$byClass)
# Save 3-Line Table
save_as_docx('Training Data Confusion Matrix Table' = flextable(data = cbind(rownames(train_CM_Metrics), train_CM_Metrics)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 3/Assignment/Tables/Train_CM_Metrics_Table.docx')


################################################################
# Test set predictions
################################################################
probabilities.test <- predict(model2, newdata = test, type = "response")
predicted.classes.test <- as.factor(ifelse(probabilities.test >= 0.5, "Yes", "No"))

# Confusion Matrix for Test Set
test_ConfusionMatrix <- confusionMatrix(predicted.classes.test, test$Private, mode = "everything", positive = "Yes")

# Save 3-Line Table
save_as_docx('Testing Data Confusion Matrix Table' = flextable(data = as.data.frame.matrix(test_ConfusionMatrix$table)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 3/Assignment/Tables/Test_CM_Table.docx')

test_CM_Metrics <- as.data.frame(test_ConfusionMatrix$byClass)
# Save 3-Line Table
save_as_docx('Testing Data Confusion Matrix Table' = flextable(data = cbind(rownames(test_CM_Metrics), test_CM_Metrics)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 3/Assignment/Tables/Test_CM_Metrics_Table.docx')


################################################################
# ROC and AUC Curve
################################################################

# Plot the Receiver Operating Characteristic Curve
ROC <- roc(test$Private, probabilities.test)
plot(ROC, col = "DARKBLUE", ylab = "Sensitivity - TruePositive Rate", xlab = "Sensitivity - FalsePositive Rate")

# Plot the Area Under the ROC Curve
auc <- auc(ROC)
auc

#-------- END --------#
