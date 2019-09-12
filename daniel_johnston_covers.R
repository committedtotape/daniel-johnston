library(rvest)
library(tidyverse)
library(glue)
library(extrafont)
library(ggtext)

covers_page <- read_html("http://www.rejectedunknown.com/covers/")

covers_table_scrape <- covers_page %>% 
  html_nodes(xpath = '//*[(@id = "AutoNumber5")]//table') %>% 
  html_table() %>% 
  .[[1]]

covers_tidy <- covers_table_scrape %>% 
  filter(row_number() != 1) %>% 
  rename(song = X1, performer = X2) %>% 
  mutate(song = na_if(song, "")) %>% 
  fill(song) %>% 
  filter(performer != "")

most_covered <- covers_tidy %>% 
  count(song, sort = TRUE) %>% 
  filter(n >= 9) %>% 
  mutate(position = row_number(),
         label_times = if_else(position == 1, glue("Covered
                                                   {n}
                                                   times"), 
                               glue("{n}"))) 

artists_pick <- most_covered %>% 
  filter(position <= 3) %>% 
  add_column(artists = c("Cover artists include: Wilco, Beck and Spiritualized",
                         "Pearl Jam, TV on the Radio",
                         "Bright Eyes"))

overall <- covers_tidy %>% 
  summarise(songs = n_distinct(song), covers = n())

ggplot(most_covered, aes(x = position, y = n)) +
  scale_x_reverse() +
  geom_linerange(aes(x = position + 0.1, ymin = 0, ymax = n), 
               size = 1.5) +
  geom_linerange(aes(x = position - 0.1, ymin = 0, ymax = n),  
                 size = 1.5) +
  geom_point(shape = 21, size = 8, fill = "white", colour = "black", stroke = 2) +
  geom_point(size = 2, colour = "black") +
  geom_text(aes(y = 0, label = song), hjust = 0, nudge_x = 0.4, fontface = "bold", family = "Gaegu") +
  geom_text(aes(label = label_times, y = n + 3), fontface = "bold", size = 5, family = "Gaegu") +
  geom_text(data = artists_pick,
            aes(x = position, label = artists, y = n - 2), hjust = 1, nudge_x = -0.3,
            size = 3.5, family = "Gaegu") +
  geom_line(aes(y=-0.5), position = position_jitter(h = 0.1), colour="black", size = 2) +
  geom_rich_text(aes(label = "<img src='hi-how-are-you.png'
    width='300'  />", x = 8.5, y = 60), colour = NA) +
  annotate("text", label = glue("{overall$songs} songs covered"), 
           x = 5, y = 52, fontface = "bold", size = 5, family = "Gaegu") +
  annotate("text", label = glue("{overall$covers} times"), 
           x = 5, y = 65, fontface = "bold", size = 5, family = "Gaegu") +
  coord_flip() +
  labs(title = "...you'll find out just who was your friend",
       subtitle = "Daniel Johnston's Most Covered Songs",
       caption = glue("Source: rejectedunknown.com
                      Graphic: @committedtotape")) +
  theme_void(base_family = "Gaegu") +
  theme(
        plot.title = element_text(hjust = 0.1, size = 24, face = "bold"),
        plot.subtitle = element_text(hjust = 0.06, size = 16),
        plot.caption = element_text(size = 12),
        plot.margin = margin(10,10,10,5))

ggsave("dj_most_covered.png", width = 11, height = 7.5)
