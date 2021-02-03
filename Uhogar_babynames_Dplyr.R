library(dplyr)
Uhogar <- read_dta("C:/Users/juanp/Downloads/Trabajo de grado/Urbano 2010/Bases/Uhogar.dta")
View(Uhogar)
glimpse(Uhogar)

ingresos <- Uhogar %>%
  select(region, estrato, ing_trabajo)

library(ggplot2)

ing_bogota <- Uhogar %>%
  filter(region == 5)

ggplot(ing_bogota, aes(x = estrato, y = ing_trabajo, group = estrato)) + 
  geom_boxplot() 

##USANDO EL BABYNAMES DATASET 

# Filter for the names Steven, Thomas, and Matthew 
selected_names <- babynames %>%
  filter(name %in% c("Steven", "Thomas", "Matthew"))

# Plot the names using a different color for each name
ggplot(selected_names, aes(x = year, y = number, color = name)) +
  geom_line()

# Calculate the fraction of people born each year with the same name
babynames %>%
  group_by(year) %>%
  mutate(year_total = sum(number)) %>%
  ungroup() %>%
  mutate(fraction = number / year_total) %>%
  # Find the year each name is most common
  group_by(name) %>%
  top_n(1, fraction)

babynames %>%
  group_by(name) %>%
  mutate(name_total = sum(number),
         name_max = max(number)) %>%
  # Ungroup the table 
  ungroup() %>%
  # Add the fraction_max column containing the number by the name maximum 
  mutate(fraction_max = number / name_max)

# Filter for the names Steven, Thomas, and Matthew
names_filtered <- names_normalized %>%
  filter(name %in% c("Steven", "Thomas", "Matthew"))

# Visualize these names over time
ggplot(names_filtered, aes(x = year, y = fraction_max, color = name)) + 
  geom_line()

babynames_fraction %>%
  # Arrange the data in order of name, then year 
  arrange(name, year) %>%
  # Group the data by name
  group_by(name) %>%
  # Add a ratio column that contains the ratio between each year 
  mutate(ratio = fraction / lag(fraction))

babynames_ratios_filtered %>%
  # Extract the largest ratio from each name 
  top_n(1, ratio) %>%
  # Sort the ratio column in descending order 
  arrange(desc(ratio)) %>%
  # Filter for fractions greater than or equal to 0.001
  filter(fraction >= 0.001)