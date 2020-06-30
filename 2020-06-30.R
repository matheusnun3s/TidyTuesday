library(tidyverse)
library(ggthemes)

character_visualization <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/character_visualization.csv'
  )


fantasia <- character_visualization %>%
  group_by(character, costume) %>%
  summarize_if(is.numeric, sum) %>%
  filter(character != "Editor narration",
         character != "Omnipresent narration") %>%
  mutate(hero_name = str_remove(character, "\\=.*")) %>%
  mutate(hero_name = str_remove(hero_name, "\\(.*")) %>%
  mutate(depicted = if_else(costume == "Non-Costume",
                            depicted,
                            -1 * depicted)) %>% 
  arrange(desc(depicted), costume)

break_values <- c(-2000, -1000, 0, 1000, 2000)

p <- fantasia %>%
  ggplot(mapping = aes(x = hero_name, y = depicted)) +
  geom_bar(stat = 'identity', aes(fill = costume)) +
  scale_y_continuous(labels = abs(break_values)) +
  scale_fill_manual(values = c('#EAF92C', '#0A0A0A')) +
  coord_flip() +
  labs(
    title = "X-men's costume appearances",
    x = 'Character',
    y = 'Number of appearances',
    capition = "Source:  Claremont Run Project ") 
  


p + theme_wsj()
