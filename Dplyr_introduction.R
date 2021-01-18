
install.packages("gapminder")
install.packages("dplyr")
library(gapminder)
library(dplyr)
gapminder

gapminder %>%
  filter(year == 2007) #Filtrar para solo año 2007 

gapminder %>%
  filter(country == "United States") #Filtrar para un solo pais 

gapminder %>%
  filter(year == 2007, country == "United States") #Filtrar para varias condiciones.

# Filter the gapminder dataset for the year 1957
gapminder %>%
  filter(year == 1957)

# Filter for China in 2002
gapminder %>%
  filter(year == 2002, country == "China")

#Ordenar por valor cuantitativo (De menor a mayor)
gapminder %>% 
  arrange(gdpPercap)

#Ordenar (de mayor a menor)
gapminder %>%
  arrange(desc(gdpPercap))

#Combinar filtros 
gapminder %>%
  filter(year == 2007) %>%
  arrange(desc(gdpPercap))

# Sort in ascending order of lifeExp
gapminder %>%
  arrange(lifeExp)


# Sort in descending order of lifeExp
gapminder %>%
  arrange(desc(lifeExp))

# Filter for the year 1957, then arrange in descending order of population
gapminder %>%
  filter(year == 1957) %>%
  arrange(desc(pop))

#crear nueva variable o modificar variable
gapminder %>%
  mutate(pop = pop / 1000000 )

#nueva variable 
gapminder %>%
  mutate(gdp = gdpPercap * pop)

#combinando 
gapminder %>%
  mutate(gdp = gdpPercap * pop) %>%
  filter(year == 2007) %>% 
  arrange(desc(gdp))

# Use mutate to change lifeExp to be in months
gapminder %>%   
  mutate(lifeExp = lifeExp * 12)

# Use mutate to create a new column called lifeExpMonths
gapminder %>%
  mutate(lifeExpMonths = lifeExp * 12)

# Filter, mutate, and arrange the gapminder dataset
gapminder %>%
  filter(year == 2007) %>%
  mutate(lifeExpMonths = 12 * lifeExp) %>%
  arrange(desc(lifeExpMonths))






