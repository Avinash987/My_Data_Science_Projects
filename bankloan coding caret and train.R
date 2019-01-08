#Classification and Regression Tree
library(caret)


bankloan<-read.csv("C:.../bankloan.csv",header = T)

head(bankloan)

#create a list of 80%of the rows in the original dataset we can use for training
validation_index <- createDataPartition(bankloan$ed, p=0.80, list=FALSE)
head(validation_index)
length(validation_index)
ncol(validation_index)
dim(validation_index)

# select 20% of the data for validation
validation <- bankloan[-validation_index, ]
data.frame(validation)
length(validation)


# use the remaining 80% of data to training and testing the models
bankloan <- bankloan[validation_index,]
head(bankloan)
length(bankloan)

#dimension of dataset
dim(bankloan)
dim(validation)

#Types of attributes
sapply(bankloan,class)

head(bankloan)
table(bankloan$default)

# summarize the class distribution
percentage <- prop.table(table(bankloan$ed)) * 100
# Express table entries as fraction of marginal table
percentage
cbind(freq=table(bankloan$ed), percentage=percentage)

summary(bankloan)

cor(bankloan)

# assuming relationship is linear between income and debt(all)
# insert heading
ggplot(data = bankloan, aes(x = income, y = debtinc)) +
  geom_point(col="red",main ="Plot")

ggplot(data = bankloan, aes(x = income, y = othdebt)) +
  geom_point()

ggplot(data = bankloan, aes(x = income, y = creddebt)) +
  geom_point()

# assuming relationship is linear between age and creddebt
ggplot(data = bankloan, aes(x = age, y = creddebt)) +
  geom_point()

regagecred<-lm(creddebt~age,data=bankloan)
summary(regagecred)

plot(bankloan$age,bankloan$creddebt)
abline(regagecred,col="red")

reginccred<-lm(creddebt~income,data=bankloan)
summary(reginccred)

plot(bankloan$income,bankloan$creddebt,main="Income and Credi debt")
abline(reginccred,col="red")

cor(bankloan$income,bankloan$creddebt)


#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build model using train() using lm method
#~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# When training a model using train(), remember
# - The dataset you're working with
# - The target variable you're trying to predict (e.g., the credit debt variable)
# - The input variable (e.g., the income variable)
# - The machine learning method you want to use (in this case "linear regression")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


model.bankloan_lm <- train(creddebt~income,data = bankloan,method = "lm")
model.bankloan_lm
summary(model.bankloan_lm)

#Train function sets up a grid of tuning parameters for a number of classification
#and regression routines, fits each model and calculates a resampling based
#performance measure.
# R-squared is a relative measure of fit, RMSE is an absolute measure of fit.


# like above codes we can get the relation between any two variable

#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Retrieve coefficients for
#  - slope
#  - intercept
#~~~~~~~~~~~~~~~~~~~~~~~~~~

coef.icept <- coef(model.bankloan_lm$finalModel)[1]
coef.icept
coef.slope <- coef(model.bankloan_lm$finalModel)[2]
coef.slope

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot scatterplot and regression line
#  using ggplot()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot(data = bankloan, aes(x = income, y = creddebt)) +
  geom_point(col="blue")+
  geom_abline(slope = coef.slope, intercept = coef.icept, color = "red")



#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build model using train() using knn method
#~~~~~~~~~~~~~~~~~~~~~~~~~~

model.bankloan_knn <- train(creddebt~income,data = bankloan,method = "knn")
model.bankloan_knn
summary(model.bankloan_knn)

###### KNN interpretation ############

reg_oth_credit<-lm(creddebt~.,data=bankloan)
summary(reg_oth_credit)

model.bankloan_knn1 <- train(creddebt~.,data = bankloan,method = "knn")
model.bankloan_knn1
summary(model.bankloan_knn1)


# so credit data have linear relationship with employ, income, debtinc, othdebt and default
# higher the R2 bettter the model, here rsquare is 0.6292 which can be more with other model.
# lower the RMSE has better fit
library(rpart)

control <- trainControl(method="cv", number=10)

control
metric <- "Accuracy"

# a) linear algorithms
set.seed(7)
# random number
fit.lda <- train(creddebt~., data=bankloan, method="lm", trControl=control)
fit.lda
# b) nonlinear algorithms
# CART
# kNN

fit.knn <- train(creddebt~., data=bankloan, method="knn", trControl=control,na.rm=T)
fit.knn
# c) advanced algorithms
# SVM
library(kernlab)
set.seed(7)
fit.svm <- train(creddebt~., data=bankloan, method="svmRadial", trControl=control)
# Random Forest
library(randomForest)
set.seed(7)
fit.rf <- train(creddebt~., data=bankloan, method="rf", trControl=control)

#We now have 5 models and accuracy estimations for each. We need to compare
#the models to each other and select the most accurate.
# summarize accuracy of models
results <- resamples(list(lda=fit.lda, knn=fit.knn, svm=fit.svm, rf=fit.rf))

summary(results)
# compare accuracy of models

dotplot(results)
print(fit.rf)
# so we can say that rf is the best model and most accurate model
#Select the model which have highest accuracy(highest Rsquared mean in rf).

# estimate skill of KNN on the validation dataset
predictions <- predict(fit.knn, validation)

predictions

nrow(bankloan)
ncol(validation)
ncol(bankloan)
test<-validation[1,]
confusionMatrix(table(predictions,validation$creddebt))
validation$income
table(predictions,validation$creddebt)
factor(predictions,validation$creddebt)
table(factor(bankloan, levels=min(validation):max(validation)), factor(validation, levels=min(validation):max(validation)))
