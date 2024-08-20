library(tidyverse)
library(caret)
library(class)
library(rpart)
library(rpart.plot)
library(rattle)

mydata <- read.csv('wisconsin.csv', header = TRUE)
str(mydata)
mydata <- mydata[-1]
str(mydata)

mydata$Diagnosis <- factor(mydata$Diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))
str(mydata)

table(mydata$Diagnosis)
round(prop.table(table(mydata$Diagnosis)) * 100, digits = 1)

set.seed(456)
mydata_index <- sample(1:nrow(mydata), size = nrow(mydata) * 0.8, replace = FALSE)
mydata_train <- mydata[mydata_index,]
mydata_test <- mydata[-mydata_index,]
str(mydata_train)
str(mydata_test)

dectree <- rpart(Diagnosis ~ worst_concave_points + worst_area + area_se, data = mydata_train)
fancyRpartPlot(dectree, caption = NULL)

dectree_pred_train <- predict(dectree, mydata_train[,-1], type = "class")
confusionMatrix(dectree_pred_train, mydata_train$Diagnosis)

#eval.on.test.data <- table(predicted = dectree_pred_train, actual = mydata_train$Diagnosis)
#eval.on.test.data
#((eval.on.test.data[1,1] + eval.on.test.data[2,2]) / nrow(mydata_train))

dectree_pred_test <- predict(dectree, mydata_test[,-1], type = "class")
confusionMatrix(dectree_pred_test, mydata_test$Diagnosis)

process <- preProcess(as.data.frame(mydata), method=c("range"))
myattr <- predict(process, as.data.frame(mydata))
myattr <- myattr[,-1]
#myattr <- scale(mydata[,-1])
mylabs <- mydata[,1]
mylabs

train <- 1:455
train.attr <- myattr[train,]
test.attr <- myattr[-train,]
train.lab <- mylabs[train]
test.lab <- mylabs[-train]

dim(train.attr)
dim(test.attr)
length(train.lab)
length(test.lab)

onenn_test <- knn(train.attr, test.attr, train.lab, k=1)
tb1 <- table(onenn_test, test.lab)
tb1
test_acc1 <- (tb1[1,1] + tb1[2,2]) / nrow(test.attr)
test_acc1

onenn_train <- knn(train.attr, train.attr, train.lab, k=1)
tb11 <- table(onenn_train, train.lab)
tb11
train_acc1 <- (tb11[1,1] + tb11[2,2]) / nrow(train.attr)
train_acc1

threenn_test <- knn(train.attr, test.attr, train.lab, k=3)
tb2 <- table(threenn_test, test.lab)
tb2
test_acc3 <- (tb2[1,1] + tb2[2,2]) / nrow(test.attr)
test_acc3

threenn_train <- knn(train.attr, train.attr, train.lab, k=3)
tb22 <- table(threenn_train, train.lab)
tb22
train_acc3 <- (tb22[1,1] + tb22[2,2]) / nrow(train.attr)
train_acc3

fivenn_test <- knn(train.attr, test.attr, train.lab, k=5)
tb3 <- table(fivenn_test, test.lab)
tb3
test_acc5 <- (tb3[1,1] + tb3[2,2]) / nrow(test.attr)
test_acc5

fivenn_train <- knn(train.attr, train.attr, train.lab, k=5)
tb33 <- table(fivenn_train, train.lab)
tb33
train_acc5 <- (tb33[1,1] + tb33[2,2]) / nrow(train.attr)
train_acc5

twenty_onenn_test <- knn(train.attr, test.attr, train.lab, k=21)
tb4 <- table(twenty_onenn_test, test.lab)
tb4
test_acc21 <- (tb4[1,1] + tb4[2,2]) / nrow(test.attr)
test_acc21

twenty_onenn_train <- knn(train.attr, train.attr, train.lab, k=21)
tb44 <- table(twenty_onenn_train, train.lab)
tb44
train_acc21 <- (tb44[1,1] + tb44[2,2]) / nrow(train.attr)
train_acc21

acc.nn.models <- data.frame(num_neighbours = c(1,3,5,21), train_acc = c(train_acc1, train_acc3, train_acc5, train_acc21)*100, test_acc = c(test_acc1, test_acc3, test_acc5, test_acc21)*100)
acc.nn.models
