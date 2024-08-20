library(tidyverse)
set.seed(2)
mydata <- matrix(rnorm(100), ncol = 2)
colnames(mydata) <- c("X","Y")
mydata <- data.frame(mydata)
mydata

ggplot(data = mydata, mapping = aes(x = X, y = Y)) + geom_point()
mydata[1:25, 1] <- mydata[1:25, 1] + 3
mydata[1:25, 2] <- mydata[1:25, 2] - 4
ggplot(data = mydata, mapping = aes(x = X, y = Y)) + geom_point()

clus <- as.factor(c(rep(1,25), rep(2,25)))

mydata <- cbind(mydata, clus)

ggplot(data = mydata, mapping = aes(x = X, y = Y, colour = clus, shape = clus)) + geom_point()

two_km <- kmeans(x = mydata[,-3], centers = 2, nstart = 20)
two_km

twodat <- data.frame(cbind(mydata, predclus = as.factor(two_km$cluster)))

table(twodat$clus, twodat$predclus)
ggplot(data = twodat, mapping = aes(x = X, y = Y, colour = predclus, shape = clus)) + geom_point()

set.seed(4)
three_km <- kmeans(x = mydata[,-3], centers = 3, nstart = 20)
three_km

threedat <- data.frame(cbind(mydata, predclus = as.factor(three_km$cluster)))
table(threedat$clus, threedat$predclus)
ggplot(data = threedat, mapping = aes(x = X, y = Y, colour = predclus, shape = clus)) + geom_point()

## Hierarchical clustering

hiercom <- hclust(dist(mydata[,-3]), method = "complete")
plot(hiercom, main = "Complete Linkage", xlab = "", sub = "")

compclus <- cutree(hiercom, k = 2)
compclus

compdat <- data.frame(cbind(mydata, predclus = as.factor(compclus)))
table(compdat$clus, compdat$predclus)
ggplot(data = compdat, mapping = aes(x = X, y = Y, colour = predclus, shape = clus)) + geom_point()

hieravg <- hclust(dist(mydata[,-3]), method = "average")

plot(hieravg, main = "Average Linkage", xlab = "", sub = "")

avgclus <- cutree(hieravg, k = 2)
avgclus

avgdat <- data.frame(cbind(mydata, predclus = as.factor(avgclus)))
table(avgdat$clus, avgdat$predclus)
ggplot(data = avgdat, mapping = aes(x = X, y = Y, colour = predclus, shape = clus)) + geom_point()

hiersin <- hclust(dist(mydata[,-3]), method = "single")
plot(hiersin, main = "Single Linkage", xlab = "", sub = "")
sinclus <- cutree(hiersin, k = 2)
sinclus
sindat <- data.frame(cbind(mydata, predclus = as.factor(sinclus)))
table(sindat$clus, sindat$predclus)
ggplot(data = sindat, mapping = aes(x = X, y = Y, colour = predclus, shape = clus)) + geom_point()
