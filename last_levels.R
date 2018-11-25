# last levels

library(tidyverse)
library(bigrquery)
library(magrittr)

# #billing <- "testproj-223217"
# #sql <- "SELECT userid, userLevel, eventName
#         FROM `tactile-external.interview.events`
#         WHERE eventName = 'missionStarted'"

# tb <- bq_project_query(billing, sql)
# 
# df <- bq_table_download(tb)
# 
# write_csv(df,"level_difficulty.csv")

#################################

df <- read_csv("level_difficulty.csv")

df %<>%
  group_by(userid) %>%
  filter(userLevel == max(userLevel)) 
  
df %>%
  ggplot(aes(x=userLevel)) +
  geom_density()

# bars

df %>%
  group_by(userid) %>%
  filter(userLevel < 200) %>%
  group_by(userLevel) %>%
  summarise(n = length(unique((userid)))) %>%
  ggplot(aes(x=userLevel, y = n)) +
  geom_col()

top3 <- df %>%
  group_by(userLevel) %>%
  summarise(n = length(unique((userid)))) %>%
  top_n(3,n)

df %>%
  group_by(userid) %>%
  filter(userLevel < 200) %>%
  group_by(userLevel) %>%
  summarise(n = length(unique((userid)))) %>%
  ggplot(aes(x=userLevel, y = n)) +
  geom_col(fill="#C83488") +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  annotate("label", x = top3$userLevel+5, y = top3$n, label = top3$userLevel, size = 6)

ggsave("last_levels.png")
