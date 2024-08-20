library(tidyverse)
# h <- ggplot(diamonds, aes(carat, price))
# h + geom_bin2d(binwidth = c(0.25, 500))
# h + geom_density_2d()
# h + geom_hex()
avg_petal_length <- mean(iris$Petal.Length)
avg_petal_width <- mean(iris$Petal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Width)
cor(iris$Petal.Length,iris$Petal.Width)
?iris
ggplot(data = iris, mapping = aes(x = Petal.Length, y = Petal.Width)) + geom_point(aes(color = Species), position = "jitter") + geom_hline(aes(yintercept = avg_petal_width)) + geom_vline(aes(xintercept = avg_petal_length))

ggplot(data = diamonds, aes(price)) + geom_histogram(bins = 12)

ggplot(data = diamonds, aes(cut)) + geom_bar()
ggplot(data = diamonds, aes(color)) + geom_bar()
ggplot(data = diamonds, aes(clarity)) + geom_bar()

ggplot(data = diamonds, aes(carat)) + geom_histogram(binwidth = 0.01)

ggplot(data = diamonds, aes(x = price, y = color)) + geom_boxplot()
ggplot(data = diamonds, aes(x = color, y = price)) + geom_boxplot()

# Relationship between carat, price and colour of diamonds
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + geom_point(aes(color = color), position = "jitter") + labs(title = "Relationship between carat, price and colour of diamonds", x = "Carat", y = "Price", color = "Color of diamond") + theme(plot.title = element_text(hjust = 0.5)) + geom_smooth()
# Relationship between carat and price of diamonds
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + geom_hex() + labs(title = "Relationship between carat and price of diamonds", x = "Carat", y = "Price") + theme(plot.title = element_text(hjust = 0.5))
