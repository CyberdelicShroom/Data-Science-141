library(tidyverse)
# ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + geom_point(aes(color = cut)) + labs(title = "Relationship between carat and price of diamonds", x = "Carat", y = "Price", color = "Cut of diamond") + theme(plot.title = element_text(hjust = 0.5)) + geom_smooth()

# Redraw the scatterplot from (1.2), but this time use a different shape for each
# different type of car. (In other words, add a shape aesthetic for the variable class):

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class)) + scale_shape_manual(values = c(8,9,15,16,23,25,0))

# Draw a scatterplot showing city miles per gallon on the x-axis and highway miles
# per gallon on the y-axis. Use different sized points for engines with different engine
# displacement. (In other words, add a size aesthetic for the variable displ):

ggplot(data = mpg) + geom_point(mapping = aes(x = cty, y = hwy, size = displ))

# Local mapping vs. global mapping:

# Local:
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + geom_smooth(mapping = aes(x = displ, y = hwy))
# Global:
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point() + geom_smooth()

# Draw a bar chart showing the type of car:
ggplot(data = mpg) + geom_bar(mapping = aes(x = class))

# Changing color by class:
ggplot(data = mpg) + geom_bar(mapping = aes(x = class, color = class))
ggplot(data = mpg) + geom_bar(mapping = aes(x = class, fill = class))

# Adding labels, title and subtitle:
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class)) + labs(title = "Engine displacement and fuel efficiency", subtitle = "Including type of car comparison", x = "Engine displacement (in litres)", y = "Highway miles per gallon", color = "Type of car") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
