library(readr)
student <- read_csv(".../DataSets/student.csv")
dim(student)
str(student)
table(student$Grade)
nrow(student)
ncol(student)
student$Grade<- as.factor(student$Grade)
student$Gender<- as.factor(student$Gender)
student$Program<- as.factor(student$Program)
student$`Subject Code`<- as.factor(student$`Subject Code`)
student$Semester<- as.factor(student$Semester)
class(student)
str(student)

# training & testing data set

select_rows<- sample(1:nrow(student), round(0.2*nrow(student)), replace = F)
length(select_rows)
stuTest<- student[select_rows,]
stuTrain<- student[-(select_rows),]
install.packages("tree")
library(tree)

#------------------ regression tree

modelRegTree<- tree(Mark~Motivation+Gender+Age, data = stuTrain)
plot(modelRegTree)
text(modelRegTree, pretty = 0, cex = 0.75)

# prediction of Mark
pred<- predict(modelRegTree, newdata = stuTest)
head(pred, 5)

# accuracy of prediction

e<-(stuTest$Mark-pred)
head(e, 3) # 9.306061 -2.179167 -1.354839
se<- e^2 # 86.602764 4.748767 1.835588
head(se,3)
sse<- sum(se)
sse # 2127.685
msse<- sse/nrow(stuTest)
msse # 53.19214
rmse<-sqrt(msse)
rmse # 7.293294

#-----------classification tree

install.packages("caTools")
library(caTools)
library(tree)
set.seed(1)
split<- sample.split(student, SplitRatio = 0.70)
studentTrain<- subset(student, split == TRUE)
studentTest<- subset(student, split == FALSE)

str(student) #???
str(studentTrain) #??
str(studentTest)#???
student$Grade<- as.factor(student$Grade)

table(student$Grade)
table(studentTrain$Grade)
table(studentTest$Grade)

prop.table(table(student$Grade))
prop.table(table(studentTrain$Grade))
prop.table(table(studentTest$Grade))

modelClassTree<- tree(Grade~Motivation+Age+Gender, data = studentTrain)
plot(modelClassTree)
text(modelClassTree, pretty = 0, cex = 0.75)

#--------prediction of groups

pred<- predict(modelClassTree, newdata = studentTest, type = "class")
conf<- table(studentTest$Grade, pred)
conf
OAA<- (conf[1,1]+conf[2,2]+conf[3,3]+conf[4,4]+conf[5,5]+conf[6,6])/sum(conf)
OAA


library(tree)
t=tree(mtcars$mpg~mtcars$disp+mtcars$hp)
summary(t)
t
plot(t);text(t)
