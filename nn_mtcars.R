library(readr)
mtcars <- read_csv("..../mtcars.csv")
install.packages("neuralnet")
library(neuralnet)
str(mtcars)
dim(mtcars)

trainset<- mtcars[1:20,]
testset<- mtcars[21:32,]
dim(trainset)
dim(testset)

# linear regression model
mod<- lm(mpg~disp+hp+wt+drat, trainset)
mod
results_lm<- predict(mod, testset)
results_lm

e_lm<- testset$mpg-results_lm
head(e_lm)
e_lmSQUARE<- e_lm^2
head(e_lmSQUARE, 3)
sse_lm<- sum(e_lmSQUARE)
sse_lm
msse_lm<- sse_lm/nrow(testset)
msse_lm
rmse_lm<- sqrt(msse_lm)
rmse_lm


# Neural Network Model------------
mtcarsnet<- neuralnet(mpg~disp+hp+wt+drat, trainset, hidden = 3, lifesign = "minimal")
plot(mtcarsnet, rep = "best")

# Predict
temp_test<- subset(testset, select = c("disp", "hp", "wt", "drat"))
mtcarsnet.results<- compute(mtcarsnet, temp_test)
results<- data.frame(actual = testset$mpg, prediction = mtcarsnet.results$net.result)
results

e= results$actual - results$prediction
head(e)
se<- e^2
sse<- sum(se)
sse
msse<-sse/nrow(testset)
msse
rmse_nn<- sqrt(msse)
rmse_nn # 5.27047195 against linear model = 2.68
