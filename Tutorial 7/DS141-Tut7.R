library(tidyverse)
library(caret)
library(class)

mydata <- read.csv('wisconsin.csv', header = TRUE)
str(mydata)
mydata <- mydata[-1]
str(mydata)

mydata$Diagnosis <- factor(mydata$Diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))
str(mydata)

table(mydata$Diagnosis)
round(prop.table(table(mydata$Diagnosis)) * 100, digits = 1)

meanRadius <- summary(mydata$mean_radius)
meanArea <- summary(mydata$mean_area)
meanSmooth <- summary(mydata$mean_smoothness)
summary_stats <- cbind(meanRadius,meanArea,meanSmooth)
summary_stats

# summary(mydata[c("mean_radius", "mean_area", "mean_smoothness")])

process <- preProcess(as.data.frame(mydata), method=c("range"))
myattr <- predict(process, as.data.frame(mydata))
myattr <- myattr[,-1]
str(myattr)
summary(myattr$mean_area)
summary(myattr[c("mean_radius", "mean_area", "mean_smoothness")])

mylabs <- mydata[,1]
mylabs
train <- 1:469
train.attr <- myattr[train,]
train.lab <- mylabs[train]
test.attr <- myattr[-train,]
test.lab <- mylabs[-train]

dim(train.attr)
length(train.lab)
dim(test.attr)
length(test.lab)

set.seed(7)

onenn <- knn(train.attr, test.attr, train.lab, k=1)
table(onenn, test.lab)

fivenn <- knn(train.attr, test.attr, train.lab, k=5)
table(fivenn, test.lab)

twenty_fivenn <- knn(train.attr, test.attr, train.lab, k=25)
table(twenty_fivenn, test.lab)

# For 1NN:
# (72 + 21) = 93 of test cases were correctly classified, giving accuracy of 93%.
# There were 2 false negatives and 5 false positives.

# For 5NN:
# (74 + 23) = 97 of test cases were correctly classified, giving accuracy of 97%.
# There were no false negatives, but 3 false positives.

# For 25NN:
# (77 + 21) = 98 of test cases were correctly classified, giving accuracy of 98%.
# There were 2 false negatives and no false positives.

# While the accuracy of the 5NN and 25NN models are almost similar, there is a difference
# in the number of false positives and the number of false negatives.  
# In choosing between these models, you would have to consider the "cost" of the incorrect predictions.  
# False negatives can be very costly, as it will mean that a patient does not receive treatment when he/she 
# actually has cancer. However, false positives are also undesirable, since it will lead to unnecessary 
# treatment and stress, but perhaps, in this context, preferable to false negative results.
