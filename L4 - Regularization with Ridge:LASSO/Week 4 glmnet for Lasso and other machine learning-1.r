

rm(list=ls())
while (!is.null(dev.list()))  dev.off() 


### You need to embellish these basic codes

### important to create your own quadratic/cubic and interactions terms prior to Lasso
### at least 5x or 50 of them
https://stackoverflow.com/questions/31905221/r-generate-all-possible-interaction-variables


## glmnet and lars
if (is.null(dev.off()==FALSE)) {dev.off()} # close plots
rm(list=ls()) # wipe environment
install.packages("lars", dependencies = TRUE)


library(lars)
#install.packages("glmnet")
library(glmnet)
data(diabetes)

attach(diabetes) # can directly access variable
head(diabetes)
View(diabetes)
summary(diabetes)
write.csv(diabetes,"see_diabetes.csv") # see the data set in Excel


# to pop up charts with zoom, Go to R studio menu bar and Tools->Global options->R Mark down
# In that phase select "window" and "popup" from that list in the "show output preview in:" then apply
par(mar=c(1,1,1,1)) # set the margins
par(mfrow=c(2,5)) # 2 x 5 matrix
for(i in 1:10){
  plot(x[,i], y)  # plot 10 regressions on the variables 1 thru 10
  abline(lm(y~x[,i]))
}


dev.off() # you must close plots before proceeding
model_ols <- lm(y ~ x2) # prefix x.
summary(model_ols)

library(glmnet)
model_lasso <- glmnet(x	,  y) # LASSO with 20 variables
#plot.glmnet(model_lasso, xvar = "norm", label = TRUE) # the very small numbers on the right are the variables 1 thru 10
plot(model_lasso)
plot(model_lasso,xvar="lambda",label=TRUE)
# fit3=glmnet(x	,  y,family="multinomial")
# plot(fit3,pch=19)
summary(model_lasso)


coef(model_lasso)

predict.glmnet(model_lasso,x)



cv_fit <- cv.glmnet(x=x, y=y, alpha = 1, nlambda = 1000) #cross validate
plot(cv_fit)

cv_fit$lambda.min

fit <- glmnet(x=x, y=y, alpha = 1, lambda=cv_fit$lambda.min)
fit$beta

cv_fit$lambda.1se

fit <- glmnet(x=x, y=y, alpha = 1, lambda=cv_fit$lambda.1se)
fit$beta


str(fit)





### how to replace X matrix
library(glmnet)
data(diabetes)
attach(diabetes) # can directly access variable
x                  # see that x is a matrix

# compares LASSO/glmnet vs. linear regression
attach(mtcars) # attach your data set here
x <- as.matrix(cbind(mtcars$cyl, mtcars$disp, mtcars$hp))
y <- as.matrix(mtcars$mpg)

model_ols <- lm(y ~ x) # x prefix works now
summary(model_ols)

summary(lm(mpg~ cyl+disp+hp)) #exactly similar output by hand


model_lasso <- glmnet(x, y) # LASSO with variables
plot(model_lasso, xvar = "norm", label = TRUE) # the very small numbers on the right are the variables 1 thru 10
str(model_lasso)
coef(model_lasso)
cv_fit <- cv.glmnet(x=x, y=y, alpha = 1, nlambda = 1000) #cross validate
plot(cv_fit)
cv_fit$lambda.min
fit <- glmnet(x=x, y=y, alpha = 1, lambda=cv_fit$lambda.min)
fit$beta
cv_fit$lambda.1se
fit <- glmnet(x=x, y=y, alpha = 1, lambda=cv_fit$lambda.1se)
fit$beta

# compared model_ols and model_lasso
# adj-R2, AIC, BIC, Cp



## PCA
View(mtcars)
mycars <-mtcars
head(mycars)
carspca<-princomp(mycars[-1]) # conduct PCA on all variables except the first variable
summary(carspca) # it shows the first component captured 92.73% of the variation
carspca$loadings
carspca$scores

# components are arranged from the strongest to the weakest
mycars$pca <- carspca$scores[,1:2] # merged the first 2 components to the data set
head(mycars)
pca_predict1 <- lm(mpg~pca, data=mycars ) # regressed with 2 components
summary(pca_predict1)
mycars$yhat <- predict.lm(pca_predict1) # predicted with 2 components
head(mycars)


lm_predict <- lm(mpg~wt+ cyl +disp, data=mycars )
mycars$yhat2 <- predict.lm(lm_predict) # predicted with 3 regular variables
head(mycars)
View(mycars)


# suggest making a chart here (possibly the predicted values)


# Calculate RMSE, AIC or any suitable metric



### confusion matrix
#install.packages("caret")
require(caret)
attach(mydata)
new_linear_setota<- as.factor(linear_setota)
new_logit_setota<- as.factor(logit_setota)
new_setosa <- as.factor(setosa)
confusionMatrix(new_linear_setota, new_setosa)
confusionMatrix(new_logit_setota, new_setosa)

class(new_fitted_logit)
attributes(new_fitted_logit)


# reshape( )
# add fips to state information
# merge on state and year
DF0 <- merge(DF1, DF2, by=c("year","state"))





### How to use factors in regression
summary(lm(mpg~gear, data=mtcars)) # 3 levels

# pretend gears were not ordinal (example: white, black, Asian)
table(mtcars$gear)
summary(lm(mpg~as.factor(gear), data=mtcars))




# K-Means Cluster Analysis
mydata <- na.omit(mtcars) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables
fit <- kmeans(mydata, 5) # 5 cluster solution
aggregate(mydata,by=list(fit$cluster),FUN=mean) # get cluster means 
mydata <- data.frame(mydata, fit$cluster) # append cluster assignment
View(mydata)

# exploratory
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) # Determine number of clusters
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")





# Ward Hierarchical Clustering
mydata <- na.omit(mtcars) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")




# Random Forest prediction of Kyphosis data
#install.packages("randomForest")
install.packages("randomForest")
library(randomForest)
data(mtcars)
mtcars.rf <- randomForest(mpg ~ ., data=mtcars, ntree=1000,
                          keep.forest=FALSE, importance=TRUE)
print(mtcars.rf)
importance(mtcars.rf)
importance(mtcars.rf, type=1)
plot(randomForest(mpg ~ ., mtcars, keep.forest=FALSE, ntree=100), log="y")












