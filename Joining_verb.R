question_tags <- readRDS("C:/Users/juanp/Downloads/Curso de R/Data Camp/Lego Dataset/question_tags.rds")
questions <- readRDS("C:/Users/juanp/Downloads/Curso de R/Data Camp/Lego Dataset/questions.rds")
tags <- readRDS("C:/Users/juanp/Downloads/Curso de R/Data Camp/Lego Dataset/tags.rds")
answers <- readRDS("C:/Users/juanp/Downloads/Curso de R/Data Camp/Lego Dataset/answers.rds")

library(dplyr)

questions_with_tags <- questions %>%
  inner_join(question_tags, by = c("id" = "question_id")) %>%
  inner_join(tags,  by = c("tag_id" = "id" ))

questions_with_tags

questions_with_tags %>%
  count(tag_name, sort = TRUE)

# Replace the NAs in the tag_name column
questions %>%
  left_join(question_tags, by = c("id" = "question_id")) %>%
  left_join(tags, by = c("tag_id" = "id")) %>%
  replace_na(list(tag_name = "only-r"))

questions_with_tags %>%
  # Group by tag_name
  group_by(tag_name) %>%
  # Get mean score and num_questions
  summarize(score = mean(score),
            num_questions = n()) %>%
  # Sort num_questions in descending order
  arrange(desc(num_questions))

# Using a join, filter for tags that are never on an R question
tags %>%
  anti_join(question_tags, by = c("id" = "tag_id"))

questions %>%
  # Inner join questions and answers with proper suffixes
  inner_join(answers, by = c("id" = "question_id"),
             suffix = c("_question", "_answer")) %>%
  # Subtract creation_date_question from creation_date_answer to create gap
  mutate(gap = as.integer(creation_date_answer - creation_date_question))

# Count and sort the question id column in the answers table
answer_counts <- answers %>%
  count(question_id) %>%
  arrange(n)


# Combine the answer_counts and questions tables
questions %>% 
  left_join(answer_counts, by = c("id" = "question_id")) %>%
  
  # Replace the NAs in the n column
  replace_na(list(n = 0))

question_answer_counts %>%
  # Join the question_tags tables
  inner_join(question_tags, by = c("id" = "question_id")) %>%
  # Join the tags table
  inner_join(tags, by = c("tag_id" = "id"))

tagged_answers %>%
  # Aggregate by tag_name
  group_by(tag_name) %>%
  # Summarize questions and average_answers
  summarize(questions = n(),
            average_answers = mean(n)) %>%
  # Sort the questions in descending order
  arrange(desc(questions))

# Inner join the question_tags and tags tables with the questions table
questions %>%
  inner_join(question_tags, by = c("id" = "question_id")) %>%
  inner_join(tags, by = c("tag_id" = "id"))

# Inner join the question_tags and tags tables with the answers table
answers %>%
  inner_join(question_tags, by = "question_id") %>%
  inner_join(tags, by = c("tag_id" = "id"))

# Combine the two tables into posts_with_tags
posts_with_tags <- bind_rows(questions_with_tags %>% mutate(type = "question"),
                             answers_with_tags %>% mutate(type = "answer"))


# Add a year column, then aggregate by type, year, and tag_name
posts_with_tags %>%
  mutate(year = year(creation_date)) %>%
  count(type, year, tag_name)

# Filter for the dplyr and ggplot2 tag names 
by_type_year_tag_filtered <- by_type_year_tag %>%
  filter(tag_name %in% c("dplyr", "ggplot2"))

# Create a line plot faceted by the tag name 
ggplot(by_type_year_tag_filtered, aes(year, n, color = type)) +
  geom_line() +
  facet_wrap(~ tag_name)