# Daily Active users (DAU)

library(tidyverse)
library(bigrquery)

billing <- "testproj-223217"
sql <- "SELECT userid, eventDate
        FROM `tactile-external.interview.events`
        WHERE eventName = 'gameStarted'"

#tb <- bq_project_query(billing, sql)

#df <- bq_table_download(tb)

#write_csv(df,"daily_users.csv")

##################################

df <- read_csv("daily_users.csv")

df %>% 
  group_by(eventDate) %>%
  summarise(n = length(unique(userid))) %>%
  ggplot(aes(x=eventDate,y=n)) +
  geom_line(size=1) +
  geom_point(size=1.5) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_y_continuous(labels = function(n) {
    trans = n / 1000
    paste0(trans, "K")
  })

# average daily users: 82171
df %>% 
  group_by(eventDate) %>%
  summarise(n = length(unique(userid))) %>%
  summarise(mean(n), sd(n))

