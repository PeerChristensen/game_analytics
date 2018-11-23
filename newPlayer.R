#  Data analysis: daily installs

library(tidyverse)
library(bigrquery)
library(scales)

billing <- "testproj-223217" # replace this with your project ID 
sql <- "SELECT eventName, eventDate 
        FROM `tactile-external.interview.events` 
        WHERE eventName = 'newPlayer'"

tb <- bq_project_query(billing, sql)

df <- bq_table_download(tb)

write_csv(df, "newPlayer.csv")

df %>% 
  group_by(eventDate) %>%
  count() %>%
  ggplot(aes(eventDate,n)) + 
  geom_line(size=1) +
  geom_point(size=2) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_y_continuous(labels = function(n) {
    trans = n / 1000
    paste0(trans, "K")
  })

ggsave("newPlayer.png")

df %>% 
  +     group_by(eventDate) %>%
  +     count() %>% summary(n)
