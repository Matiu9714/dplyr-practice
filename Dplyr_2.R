library(gapminder)

#sacar la media de la esperanza de vida de todos los paises 
gapminder %>%
  summarize(meanLifeExp = mean(lifeExp))

#Para un año en especifico y el total de poblacion
gapminder %>%
  filter(year == 2007) %>%
  summarize(meanLifeExp = mean(lifeExp), totalpop = sum(pop)) 

# Summarize to find the median life expectancy
gapminder %>%
  summarize(medianLifeExp = median(lifeExp))

# Summarize to find the median life expectancy for a year
gapminder %>%
  filter(year == 1957) %>%
  summarize(medianLifeExp = median(lifeExp))

# Summarize to find the median life expectancy and max gdp for a year 
gapminder %>%
  filter(year == 1957) %>%
  summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

gapminder %>%
  filter(year == 2007) %>%
  summarize(meanLifeExp = mean(lifeExp), totalPop = sum(pop)) 

#Agrupar por años
gapminder %>%
  group_by(year) %>%
  summarize(meanLifeExp = mean(lifeExp), totalPop = sum(pop))

gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(meanLifeExp = mean(lifeExp), totalPop = sum(pop))

#Agrupar por varios años 
gapminder %>%
  group_by(continent, year) %>%
  summarize(meanLifeExp = mean(lifeExp), totalPop = sum(pop))
  
gapminder %>%
  group_by(year, continent) %>%
  summarize(meanLifeExp = mean(lifeExp), totalPop = sum(pop))

# Find median life expectancy and maximum GDP per capita in each year
gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

# Find median life expectancy and maximum GDP per capita in each continent in 1957
gapminder %>%   
  filter(year == 1957) %>%
  group_by(continent) %>%
  summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

# Find median life expectancy and maximum GDP per capita in each continent/year combination
gapminder %>%
  group_by(continent, year) %>%
  summarize(medianLifeExp = median(lifeExp), maxGdpPercap = max(gdpPercap))

#Graficar 
by_year <- gapminder %>%
  group_by(year) %>%
  summarize(totalPop = sum(pop), meanLifeExp = mean(lifeExp))
by_year

ggplot(by_year, aes(x = year, y = totalPop)) + geom_point()

#Expandir hasta 0 
ggplot(by_year, aes(x = year, y = totalPop)) + geom_point() + 
  expand_limits( y = 0)

#Mas grupos 
by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(totalPop = sum(pop), meanLifeExp = mean(lifeExp))
by_year_continent

ggplot(by_year_continent, aes(x = year, y = totalPop, color = continent)) +
  geom_point() + expand_limits(y = 0)

# Create a scatter plot showing the change in medianLifeExp over time
ggplot(by_year, aes(x = year, y = medianLifeExp)) +
  geom_point() + expand_limits(y = 0)

# Summarize medianGdpPercap within each continent within each year: by_year_continent
by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Plot the change in medianGdpPercap in each continent over time
ggplot(by_year_continent, aes(x = year, y = medianGdpPercap, 
  color = continent)) + geom_point() + expand_limits(y = 0)

# Summarize the median GDP and median life expectancy per continent in 2007
by_continent_2007 <- gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(medianLifeExp = median(lifeExp), medianGdpPercap = median(gdpPercap))


# Use a scatter plot to compare the median GDP and median life expectancy
ggplot(by_continent_2007, aes(x = medianGdpPercap, y = medianLifeExp, 
  color = continent)) + geom_point()


