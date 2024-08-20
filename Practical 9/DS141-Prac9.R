library(tidyverse)
library(rpart)
library(caret)

head(iris)
str(iris)
summary(iris)

myiris <- iris %>% filter(Species != "setosa") %>% select(Sepal.Width, Petal.Width, Species)
str(myiris)
myiris$Species <- factor(myiris$Species)
str(myiris)

ggplot(data = myiris, mapping = aes(x = Sepal.Width, y = Petal.Width, color = Species)) + geom_point(position = "jitter")

dectree <- rpart(Species~., data = myiris)

dectree.pred <- predict(dectree, myiris[,-3], type = "class")

dectree.conf <- confusionMatrix(dectree.pred, myiris$Species)
dectree.conf

onenn <- knn3(Species~., data = myiris, k = 1)

onenn.pred <- predict(onenn, myiris[,-3], type = "class")

onenn.conf <- confusionMatrix(onenn.pred, myiris$Species)
onenn.conf

fivenn <- knn3(Species~., data = myiris, k = 5)

fivenn.pred <- predict(fivenn, myiris[,-3], type = "class")

fivenn.conf <- confusionMatrix(fivenn.pred, myiris$Species)
fivenn.conf