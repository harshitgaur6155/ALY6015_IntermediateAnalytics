#-------- ALY6015_M4_Lasso&RidgeRegularization_HarshitGaur --------#

print("Author : Harshit Gaur")
print("ALY 6015 Week 4 Assignment - Lasso and Ridge Regularization")

# Declaring the names of packages to be imported
packageList <- c("tidyverse", "ISLR", "caret", "car", "glmnet", "Metrics",
                 "RColorBrewer", "psych", "flextable", "MASS")

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
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 4/Assignment/Tables/College_Data_Table.docx')

# Get the glimpse of data set
glimpse(College)

describeCollegeFlex <- College %>% 
  psych::describe(quant = c(.25, .75), IQR = TRUE) %>% 
  select(n, mean, sd, median, min, max, range, skew, kurtosis)

describeCollegeFlex <- round(describeCollegeFlex, 2)
describeCollegeFlex <- cbind(e = rownames(describeCollegeFlex), describeCollegeFlex)

save_as_docx('Descriptive Statistics of College Dataset' = flextable(data = describeCollegeFlex), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 3/Assignment/Tables/College_Desc_Stats_Table_main.docx')


# Normality Check for features using Q-Q Plot and Shapiro-Wilks Test.
qqPlot(College$Top10perc, ylab = "Studentized Residuals", xlab = "Theoretical Quantiles")
shapiro.test(College$Top10perc)

qqPlot(College$Top25perc, ylab = "Studentized Residuals", xlab = "Theoretical Quantiles")
shapiro.test(College$Top25perc)


################################################################
# Adding Interaction Variables and Square Variables of the features
################################################################
interactionSquareFeaturesCollege <- College[, which(!names(College) %in% c("Private", "Grad.Rate"))]

# Adding Square Variables
squareTemp <- setNames(as.data.frame(
  cbind(interactionSquareFeaturesCollege,interactionSquareFeaturesCollege^2))
  , c(names(interactionSquareFeaturesCollege),paste0(names(interactionSquareFeaturesCollege),'_2')))

squareTemp <- squareTemp[, -which(names(squareTemp) %in% names(College))]
save_as_docx('Introduction of Square Variables of features of College Dataset' = flextable(data = head(squareTemp)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 4/Assignment/Tables/Sqaure_Variables_Table.docx')

# Adding Interaction Variables
interactionTemp <- as.data.frame.matrix(model.matrix(~ . ^2, data = interactionSquareFeaturesCollege))
interactionTemp <- interactionTemp[, which(!names(interactionTemp) %in% c("(Intercept)", names(College)))]
save_as_docx('Table 5: Introduction of Interaction Variables of features of College Dataset' = flextable(data = head(interactionTemp[,1:10])), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 4/Assignment/Tables/Interaction_Variables_Table.docx')

# Merge all data sets
College <- cbind(College, squareTemp, interactionTemp[, 1:15])



################################################################
# Split data into train and test data
################################################################
set.seed(454)
trainIndex <- createDataPartition(College$Grad.Rate, p = 0.80, list = FALSE)
train <- College[trainIndex,]
test <- College[-trainIndex,]

train_x <- model.matrix(Grad.Rate ~ ., train)[, -which(colnames(College) == "Grad.Rate")]
test_x <- model.matrix(Grad.Rate ~ ., test)[, -which(colnames(College) == "Grad.Rate")]

train_y <- train$Grad.Rate
test_y <- test$Grad.Rate


################################################################
# Find best value of Lambda using Cross-Validation
################################################################
set.seed(454)
cv.lasso_ridge <- cv.glmnet(train_x, train_y, nfolds = 10)
plot(cv.lasso_ridge)
cv.lasso_ridge
save_as_docx('Table 6: Estimating Lambda using Cross Validation' = flextable(data = model.matrix(cv.lasso_ridge)), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 4/Assignment/Tables/Estimate_Lambda_Table.docx')


################################################################
# Optimal Value of Lambda; Minimizes the Prediction Error
# Lambda Min - Minimizes out of sample loss
# Lambda 1SE - Largest value of Lambda within 1 Standard Error of Lambda Min.
################################################################
log(cv.lasso_ridge$lambda.min)
log(cv.lasso_ridge$lambda.1se)


################################################################
# Fit the Ridge Regularization Model on Training Set based on Lambdas
# alpha = 1 for Lasso (L2 Regularization)
# alpha = 0 for Ridge (L1 Regularization)
################################################################

########## RIDGE Regularization ##########

# Fit the model on training set using lambda.min 
model.ridge.train.min <- glmnet(train_x, train_y, alpha = 0, lambda = cv.lasso_ridge$lambda.min)

# Display Regression Coefficients
coef(model.ridge.train.min)
plot(coef(model.ridge.train.min))


# Fit the model on training set using lambda.1se
model.ridge.train.1se <- glmnet(train_x, train_y, alpha = 0, lambda = cv.lasso_ridge$lambda.1se)

# Display Regression Coefficients
coef(model.ridge.train.1se)

save_as_docx('Table 7: Ridge Regression on Training Data set using Lambda at 1 Standard Error' = flextable(data = cbind(rownames(coef(model.ridge.train.1se)), as.data.frame.matrix(coef(model.ridge.train.1se)))), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 4/Assignment/Tables/Ridge_Training_1SE_Table.docx')

# Display coefficients of OLS model with no regularization
ols_train <- lm(Grad.Rate ~ ., data = train)
coef(ols_train)

# View RMSE of the full model
predict.ridge.train.ols <- predict(ols_train, new = test)
rmse(test$Grad.Rate, predict.ridge.train.ols)


################################################################
# Make Prediction on the Training Data
################################################################
predict.ridge.train.1se <- predict(model.ridge.train.1se, newx = train_x)
ridge.train.rmse <- rmse(train_y, predict.ridge.train.1se)


################################################################
# Make Prediction on the Testing Data
################################################################
predict.ridge.test.1se <- predict(model.ridge.train.1se, newx = test_x)
ridge.test.rmse <- rmse(test_y, predict.ridge.test.1se)

ridge.train.rmse
ridge.test.rmse



########## LASSO Regularization ##########

# Fit the model on training set using lambda.min 
model.lasso.train.min <- glmnet(train_x, train_y, alpha = 1, lambda = cv.lasso_ridge$lambda.min)

# Display Regression Coefficients
coef(model.lasso.train.min)

# Fit the model on training set using lambda.1se
model.lasso.train.1se <- glmnet(train_x, train_y, alpha = 1, lambda = cv.lasso_ridge$lambda.1se)

# Display Regression Coefficients
coef(model.lasso.train.1se)

save_as_docx('Table 7: Lasso Regularization on Training Data set using Lambda at 1 Standard Error' = flextable(data = cbind(rownames(coef(model.lasso.train.1se)), as.data.frame.matrix(coef(model.lasso.train.1se)))), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 4/Assignment/Tables/Lasso_Training_1SE_Table.docx')

# Display coefficients of OLS model with no regularization
ols_train <- lm(Grad.Rate ~ ., data = train)
coef(ols_train)

# View RMSE of the full model
predict.lasso.train.ols <- predict(ols_train, new = test)
rmse(test$Grad.Rate, predict.lasso.train.ols)


################################################################
# Make Prediction on the Training Data
################################################################
predict.lasso.train.1se <- predict(model.lasso.train.1se, newx = train_x)
lasso.train.rmse <- rmse(train_y, predict.lasso.train.1se)


################################################################
# Make Prediction on the Testing Data
################################################################
predict.lasso.test.1se <- predict(model.lasso.train.1se, newx = test_x)
lasso.test.rmse <- rmse(test_y, predict.lasso.test.1se)

predict.lasso.test.1se

lasso.train.rmse
lasso.test.rmse



################################################################
# Stepwise Stepwise Selection (Training Data)
################################################################

######### Linear Regression for Model on Training Data #########
regressionFittingFeatures <-  train[sapply(train, is.numeric)]
fit <- lm(formula = Grad.Rate ~ ., data = regressionFittingFeatures)

# Stepwise Stepwise Selection
stepAIC(fit, direction = "both")

# Regression Model Fit using Features from Stepwise Stepwise Selection on Training Data
fit <- lm(formula = Grad.Rate ~ Accept + Enroll + Top10perc + Top25perc + 
            P.Undergrad + Outstate + Books + Personal + Terminal + perc.alumni + 
            Expend + Accept_2 + Top25perc_2 + Outstate_2 + Room.Board_2 + 
            Books_2 + Personal_2 + S.F.Ratio_2 + Expend_2 + `Apps:Room.Board` + 
            `Apps:PhD` + `Apps:perc.alumni`, data = regressionFittingFeatures)

lm.summary <- summary(fit)

save_as_docx('Table 7: Linear Regression Model on Training Data set using features from Stepwise Selection' = flextable(data = cbind(rownames(lm.summary$coefficients), as.data.frame(round(lm.summary$coefficients, 4)))), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 4/Assignment/Tables/LR_train_Table.docx')
# RMSE of Linear Regression Model
rmse_stepwise_train <- sqrt(mean(lm.summary$residuals^2))


################################################################
# Stepwise Stepwise Selection (Testing Data)
################################################################

######### Linear Regression for Model on Testing Data #########
regressionFittingFeatures <-  test[sapply(test, is.numeric)]
fit <- lm(formula = Grad.Rate ~ ., data = regressionFittingFeatures)
lm.summary <- summary(fit)

save_as_docx('Table 8: Linear Regression Model on Testing Data set using features from Stepwise Selection' = flextable(data = cbind(rownames(lm.summary$coefficients), as.data.frame(round(lm.summary$coefficients, 4)))), 
             path = 'Documents/Northeastern University/MPS Analytics/ALY 6015/Class 4/Assignment/Tables/LR_test_Table.docx')

# Stepwise Stepwise Selection
stepAIC(fit, direction = "both")

# Regression Model Fit using Features from Stepwise Stepwise Selection on Training Data
fit <- lm(formula = Grad.Rate ~ Apps + Accept + Enroll + F.Undergrad + 
            P.Undergrad + Outstate + PhD + S.F.Ratio + Expend + Accept_2 + 
            Top25perc_2 + F.Undergrad_2 + Books_2 + PhD_2 + Terminal_2 + 
            S.F.Ratio_2 + perc.alumni_2 + Expend_2 + `Apps:Enroll` + 
            `Apps:Top10perc` + `Apps:Top25perc` + `Apps:F.Undergrad` + 
            `Apps:P.Undergrad` + `Apps:Room.Board` + `Apps:Books` + `Apps:Terminal` + 
            `Apps:S.F.Ratio` + `Apps:Expend`, data = regressionFittingFeatures)

lm.summary <- summary(fit)

# RMSE of Linear Regression Model
rmse_stepwise_test <- sqrt(mean(lm.summary$residuals^2))



#-------- END --------#
