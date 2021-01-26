library(dplyr)
Uhogar <- read_dta("C:/Users/juanp/Downloads/Trabajo de grado/Urbano 2010/Bases/Uhogar.dta")
View(Uhogar)
glimpse(Uhogar)

Uhogar %>%
  select(zona, region, estrato, t_personas)

ingresos_trabajo <- Uhogar %>%
  select(estrato, region, ing_trabajo)

ingresos_trabajo %>%
  arrange(desc(ing_trabajo))
  
ingresos_trabajo %>%
  arrange(desc(ing_trabajo)) %>%
  filter(region == 5)

ingresos_trabajo %>%
  arrange(desc(ing_trabajo)) %>%
  filter(ing_trabajo < 1200000, region == 1)

ingresos_trabajo <- Uhogar %>%
  select(estrato, region, ing_trabajo, t_personas)

ingresos_trabajo %>%
  mutate(ingresoPercap = ing_trabajo/t_personas) %>% #Crear variable
  arrange(desc(ingresoPercap))

Uhogar %>%
  count()

Uhogar %>%
  count(region)

Uhogar %>%
  count(region, sort = TRUE)

Uhogar %>%
  count(region, id_mpio, sort = TRUE)

Uhogar %>%
  count(region, wt = ing_trabajo,  sort = TRUE) #Añadir weight

Uhogar %>%
  summarize(total_ingreso = sum(ing_trabajo, na.rm = TRUE))

Uhogar %>%
  summarize(total_ingreso = sum(ing_trabajo, na.rm = TRUE),
            media_ingreso = mean(ing_trabajo, na.rm = TRUE))

Uhogar %>%
  group_by(region) %>%
  summarize(total_ingreso = sum(ing_trabajo, na.rm = TRUE),
            media_ingreso = mean(ing_trabajo, na.rm = TRUE))

Uhogar %>%
  group_by(region) %>%
  summarize(total_ingreso = sum(ing_trabajo, na.rm = TRUE),
            media_ingreso = mean(ing_trabajo, na.rm = TRUE)) %>%
  arrange(desc(total_ingreso))

Uhogar %>%
  group_by(region, estrato) %>%
  summarize(total_ingreso = sum(ing_trabajo, na.rm = TRUE),
            media_ingreso = mean(ing_trabajo, na.rm = TRUE)) %>%
  arrange(desc(region))

Uhogar %>%
  select(region, estrato, ing_trabajo:ing_otros_nrem) %>%
  arrange(ing_trabajo)

Uhogar %>%
  select(region, contains("ing"))

Uhogar %>%
  select(region, starts_with("ing"))

Uhogar %>%
  select(region, ends_with("trabajo"))

ingresos_trabajo %>%
  rename(trabajo_ing = ing_trabajo)

Uhogar %>%
  select(region, estrato, trabajo_ing = ing_trabajo)

Uhogar %>%
  transmute(region, estrato, ingresoPercap = ing_trabajo / t_hogar)

