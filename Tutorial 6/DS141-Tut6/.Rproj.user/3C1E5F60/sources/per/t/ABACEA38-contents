library(tidyverse)
library(gridExtra)
library(ISLR)

ggplot(data = diamonds) + geom_boxplot(mapping = aes(x = clarity, y = price, fill = clarity))

ggplot(data = diamonds) + geom_point(mapping = aes(x = carat, y = price, color = clarity))

ggplot(data = diamonds) + geom_point(mapping = aes(x = x, y = y))

ggplot(data = diamonds) + geom_point(mapping = aes(x = x, y = z))

ggplot(data = diamonds) + geom_point(mapping = aes(x = y, y = z))

cleandiamonds <- filter(diamonds, x>0,y>0,z>0,x<=30,y<=30,z<=30)

sum <- 0
for (i in cleandiamonds$x){if(i==0){sum = sum + 1}}
for (i in cleandiamonds$y){if(i==0){sum = sum + 1}}
for (i in cleandiamonds$z){if(i==0){sum = sum + 1}}
sum

sum2 <- 0
for (i in cleandiamonds$x){if(i>30){sum2 = sum2 + 1}}
for (i in cleandiamonds$y){if(i>30){sum2 = sum2 + 1}}
for (i in cleandiamonds$z){if(i>30){sum2 = sum2 + 1}}
sum2

summary(cleandiamonds)
str(cleandiamonds)

cleandiamonds_numeric <- select(cleandiamonds, carat, depth, table, x, y, z)

cor(cleandiamonds$price, cleandiamonds_numeric[,])
