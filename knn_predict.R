library(readr)
bankloan <- read_csv(".../bankloan.csv")
View(bankloan)
#Classification and Regression Tree
library(caret)
dim(bankloan)
head(bankloan)

#create a list of 80%of the rows in the
#original dataset we can use for training
validation_index <-
    createDataPartition(bankloan$ed, p=0.80, list=FALSE)
head(validation_index)
dim(validation_index) # 560 1 ; 560 rows selected randomly

# select 20% of the data for validation
bankloantest <- bankloan[-validation_index, ]
data.frame(bankloantest)
dim(bankloantest) # 140 9

# use the remaining 80% of data for training

bankloantrain <- bankloan[validation_index,]
head(bankloantrain)
dim(bankloantrain) # 560 9

# ----------Linear Regression---------

reg_credit<-lm(creddebt~.,data=bankloantrain)
reg_credit
summary(reg_credit)

predict_reg<- predict(reg_credit, bankloantest)
head(predict_reg)
length(predict_reg)

e_reg<- bankloantest$creddebt - predict_reg
head(e_reg, 3)
se_reg<- e_reg^2
head(se_reg, 3)
sse_reg<- sum(se_reg)
head(sse_reg, 3)
msse_reg<- sse_reg/nrow(bankloantest)
msse_reg
rmse_reg<- sqrt(msse_reg)
rmse_reg # 1.159 ; R Square = 0.6251

# lets plot predicted vs actual of Linear Regression
plot(predict_reg~bankloantest$creddebt, col = "blue", lwd = 4, main = "Linear Regression: Predicted vs Actual")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build model using train() using knn method
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trctrl <- trainControl(method = "repeatedcv")

# It controls the computational nuances
# of the train() method.
# method is repeatedcv means repeated
# cross-validation for removing overfitting

set.seed(3333)
knn_fit <- train(creddebt ~., data = bankloantrain, method = "knn",
 trControl=trctrl,
 preProcess = c("center", "scale"),
 tuneLength = 10)

# parameter �center� & �scale� help for
# centering and scaling the data.
# After preProcessing these convert our
# training data with mean
# value as approximately �0� and standard deviation as �1�.
# The �tuneLength� parameter holds an integer value
# This is for tuning our algorithm.

knn_fit
plot(knn_fit,col="red",lwd=2,main="RMSE", pch = 2)

######------KNN interpretation-----############
# now we can see that MACHINE has learned
# that for k=7 this model gives better value
# here for 7 RMSE is lowest and R square is highest.

# now as MACHINE learned best k value,
# we can apply it to test data
test_pred <- predict(knn_fit, newdata = bankloantest)
head(test_pred)

e_knn<- bankloantest$creddebt - test_pred
head(e_knn, 3)
head(bankloantest$creddebt, 3)
se_knn<- e_knn^2
sse_knn<- sum(e_knn^2)
sse_knn
msse_knn<- sse_knn/nrow(bankloantest)
msse_knn
rmse_knn<- sqrt(msse_knn)
rmse_knn

# predicted vs observed LR & knn
plot(predict_reg~bankloantest$creddebt,
     col = "blue", lwd = 4,
     main = "Linear Regression: Predicted vs Actual")

plot(test_pred~bankloantest$creddebt,
     col = "red", lwd = 4,
     main = "knn: Predicted vs Actual")
