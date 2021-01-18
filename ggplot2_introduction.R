gapminder_2007 <- gapminder %>%
  filter(year == 2007)

gapminder_2007

install.packages("ggplot2")
library(ggplot2)

#Ejemplo, gdpPercap vs expectativa de vida
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp)) + geom_point() 

# Create gapminder_1952
gapminder_1952 <- gapminder%>%
  filter(year == 1952)

# Change to put pop on the x-axis and gdpPercap on the y-axis
ggplot(gapminder_1952, aes(x = pop, y = gdpPercap)) +
  geom_point()

# Create a scatter plot with pop on the x-axis and lifeExp on the y-axis
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) + geom_point()

#aplicar la escala logaritmica
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point() + scale_x_log10()

# Change this plot to put the x-axis on a log scale
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point() + scale_x_log10()

# Scatter plot comparing pop and gdpPercap, with both axes on a log scale
ggplot(gapminder_1952, aes(x = pop, y = gdpPercap)) + geom_point() +
  scale_x_log10() + scale_y_log10()

#Mas aesthetics
ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, color = continent)) + 
  geom_point() + scale_x_log10()

ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, color = continent, 
  size = pop)) + geom_point() + scale_x_log10()

# Scatter plot comparing pop and lifeExp, with color representing continent
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent)) +
  geom_point() + scale_x_log10()

# Add the size aesthetic to represent a country's gdpPercap
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent, 
  size = gdpPercap)) + geom_point() + scale_x_log10()

#separate by variable 
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent, 
  size = gdpPercap)) + geom_point() + scale_x_log10() + facet_wrap(~ continent)

# Scatter plot comparing pop and lifeExp, faceted by continent
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) + 
  geom_point() + scale_x_log10() + facet_wrap(~ continent)

# Scatter plot comparing gdpPercap and lifeExp, with color representing continent
# and size representing population, faceted by year
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent,
  size = pop)) + geom_point() + scale_x_log10() + facet_wrap(~ year) 




