sets <- readRDS("C:/Users/juanp/Downloads/Curso de R/Lego Dataset/sets.rds")
themes <- readRDS("C:/Users/juanp/Downloads/Curso de R/Lego Dataset/themes.rds")
colors <- readRDS("C:/Users/juanp/Downloads/Curso de R/Lego Dataset/colors.rds")
inventories <- readRDS("C:/Users/juanp/Downloads/Curso de R/Lego Dataset/inventories.rds")
inventory_parts <- readRDS("C:/Users/juanp/Downloads/Curso de R/Lego Dataset/inventory_parts.rds")
part_categories <- readRDS("C:/Users/juanp/Downloads/Curso de R/Lego Dataset/part_categories.rds")
parts <- readRDS("C:/Users/juanp/Downloads/Curso de R/Lego Dataset/parts.rds")
question_tags <- readRDS("C:/Users/juanp/Downloads/Curso de R/Lego Dataset/question_tags.rds")
questions <- readRDS("C:/Users/juanp/Downloads/Curso de R/Lego Dataset/questions.rds")
tags <- readRDS("C:/Users/juanp/Downloads/Curso de R/Lego Dataset/tags.rds")
answers <- readRDS("C:/Users/juanp/Downloads/Curso de R/Lego Dataset/answers.rds")

sets %>%
  inner_join(themes, by = c("theme_id" = "id"))  

sets %>%
  inner_join(themes, by = c("theme_id" = "id"), suffix = c("_set", "_theme"))  %>%
  count(name_theme, sort = TRUE)

# Use the suffix argument to replace .x and .y suffixes
parts %>% 
  inner_join(part_categories, by = c("part_cat_id" = "id"),
             suffix = c("_part", "_category"))

sets %>%
  inner_join(inventories, by = "set_num")

# Combine the parts and inventory_parts tables
parts %>%
  inner_join(inventory_parts, by = "part_num")

# Combine the parts and inventory_parts tables
inventory_parts %>%
  inner_join(parts, by = "part_num")

sets %>%
  inner_join(inventories, by = "set_num") %>%
  inner_join(themes, by = c("theme_id" = "id"), suffix = c("_set", "_theme" ))

sets %>%
  # Add inventories using an inner join 
  inner_join(inventories, by = "set_num") %>%
  # Add inventory_parts using an inner join 
  inner_join(inventory_parts, by = c("id" = "inventory_id"))

# Add an inner join for the colors table
sets %>%
  inner_join(inventories, by = "set_num") %>%
  inner_join(inventory_parts, by = c("id" = "inventory_id")) %>%
  inner_join(colors, by = c("color_id" = "id"),
             suffix = c("_set", "_color")) 

# Count the number of colors and sort
sets %>%
  inner_join(inventories, by = "set_num") %>%
  inner_join(inventory_parts, by = c("id" = "inventory_id")) %>%
  inner_join(colors, by = c("color_id" = "id"), 
             suffix = c("_set", "_color")) %>%
  count(name_color) %>%
  arrange(desc(n))

inventory_parts_joined <- inventories %>%
  inner_join(inventory_parts, by = c("id" = "inventory_id")) %>%
  select(-id, -version) %>%
  arrange(desc(quantity))
inventory_parts_joined

batmobile <- inventory_parts_joined %>%
  filter(set_num == "7784-1") %>%
  select(-set_num)

batwing <- inventory_parts_joined %>%
  filter(set_num == "70916-1") %>%
  select(-set_num)

batmobile %>%
  inner_join(batwing, by = c("part_num", "color_id"), 
             suffix = c("_batmobile", "_batwing"))

batmobile %>%
  left_join(batwing, by = c("part_num", "color_id"), 
             suffix = c("_batmobile", "_batwing"))

millennium_falcon <- inventory_parts_joined %>%
  filter(set_num == "7965-1")

star_destroyer <- inventory_parts_joined %>%
  filter(set_num == "75190-1")

# Combine the star_destroyer and millennium_falcon tables
millennium_falcon %>%
  left_join(star_destroyer, by = c("part_num", "color_id"), 
            suffix = c("_falcon", "_star_destroyer"))

# Aggregate Millennium Falcon for the total quantity in each part
millennium_falcon_colors <- millennium_falcon %>%
  group_by(color_id) %>%
  summarize(total_quantity = sum(quantity))

# Aggregate Star Destroyer for the total quantity in each part
star_destroyer_colors <- star_destroyer %>%
  group_by(color_id) %>%
  summarize(total_quantity = sum(quantity))

# Left join the Millennium Falcon colors to the Star Destroyer colors
millennium_falcon_colors %>%
  left_join(star_destroyer_colors, by = "color_id", 
            suffix =c("_falcon", "_star_destroyer"))

inventory_version_1 <- inventories %>%
  filter(version == 1)

# Join versions to sets
sets %>%
  left_join(inventory_version_1, by = "set_num") %>%
  # Filter for where version is na
  filter(is.na(version))

parts %>%
  count(part_cat_id) %>%
  right_join(part_categories, by = c("part_cat_id" = "id")) %>%
  # Filter for NA
  filter(is.na(n))

parts %>%
  count(part_cat_id) %>%
  right_join(part_categories, by = c("part_cat_id" = "id")) %>%
  # Use replace_na to replace missing values in the n column
  replace_na(list(n = 0))

themes %>% 
  # Inner join the themes table
  inner_join(themes, by = c("id" = "parent_id"), 
             suffix = c("_parent", "_child")) %>%
  # Filter for the "Harry Potter" parent name 
  filter(name_parent == "Harry Potter")

# Join themes to itself again to find the grandchild relationships
themes %>% 
  inner_join(themes, by = c("id" = "parent_id"),
             suffix = c("_parent", "_child")) %>%
  inner_join(themes, by = c("id_child" = "parent_id"),
             suffix = c("_parent", "_grandchild"))

themes %>% 
  # Left join the themes table to its own children
  left_join(themes, by = c("id" = "parent_id"), 
            suffix = c("_parent", "_child")) %>%
  # Filter for themes that have no child themes
  filter(is.na(id_child))

# Start with inventory_parts_joined table
inventory_parts_joined %>%
  # Combine with the sets table 
  inner_join(sets, by = "set_num") %>%
  # Combine with the themes table 
  inner_join(themes, by = c("theme_id" = "id"), 
             suffix = c("_set", "_theme"))

# Count the part number and color id, weight by quantity
batman %>% 
  count(part_num, color_id, wt = quantity)
star_wars %>%
  count(part_num, color_id, wt = quantity)

batman_parts %>%
  # Combine the star_wars_parts table 
  full_join(star_wars_parts, by = c("part_num", "color_id"), 
            suffix = c("_batman", "_star_wars")) %>%
  
  # Replace NAs with 0s in the n_batman and n_star_wars columns 
  replace_na(list(n_star_wars = 0))

parts_joined %>%
  # Sort the number of star wars pieces in descending order 
  arrange(desc(n_star_wars)) %>%
  # Join the colors table to the parts_joined table
  full_join(colors, by = c("color_id" = "id")) %>%
  # Join the parts table to the previous join 
  full_join(parts, by = "part_num", 
            suffix = c("_color", "_part"))

parts_joined %>%
  # Sort the number of star wars pieces in descending order 
  arrange(desc(n_star_wars)) %>%
  # Join the colors table to the parts_joined table
  full_join(colors, by = c("color_id" = "id")) %>%
  # Join the parts table to the previous join 
  inner_join(parts, by = "part_num", 
             suffix = c("_color", "_part"))

# Filter the batwing set for parts that are also in the batmobile set
batwing %>%
  semi_join(batmobile, by = "part_num")

# Filter the batwing set for parts that aren't in the batmobile set
batwing %>%
  anti_join(batmobile, by = "part_num")

# Use inventory_parts to find colors included in at least one set
colors %>%
  semi_join(inventory_parts, by = c("id" = "color_id"))

# Use filter() to extract version 1 
version_1_inventories <- inventories %>%
  filter(version == 1)

# Use anti_join() to find which set is missing a version 1
sets %>%
  anti_join(version_1_inventories, by = "set_num")

batman_colors <- inventory_parts_themes %>%
  # Filter the inventory_parts_themes table for the Batman theme
  filter(name_theme == "Batman") %>%
  group_by(color_id) %>%
  summarize(total = sum(quantity)) %>%
  # Add a percent column of the total divided by the sum of the total 
  mutate(percent = total / sum(total))

# Filter and aggregate the Star Wars set data; add a percent column
star_wars_colors <- inventory_parts_themes %>%
  filter(name_theme == "Star Wars") %>%
  group_by(color_id) %>%
  summarize(total = sum(quantity)) %>%
  # Add a percent column of the total divided by the sum of the total 
  mutate(percent = total / sum(total))

batman_colors %>%
  full_join(star_wars_colors, by = "color_id", suffix = c("_batman", "_star_wars")) %>%
  replace_na(list(total_batman = 0, total_star_wars = 0)) %>%
  inner_join(colors, by = c("color_id" = "id")) %>%
  # Create the difference and total columns
  mutate(difference = percent_batman - percent_star_wars,
         total = total_batman + total_star_wars) %>%
  # Filter for totals greater than 200
  filter(total > 200)

# Create a bar plot using colors_joined and the name and difference columns
ggplot(colors_joined, aes(x = name, y = difference, fill = name)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = color_palette, guide = FALSE) +
  labs(y = "Difference: Batman - Star Wars")




