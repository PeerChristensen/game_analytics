# level duration

library(tidyverse)
library(bigrquery)

billing <- "testproj-223217"
sql <- "SELECT levelId, missionSecondsPlayed
FROM `tactile-external.interview.events`"

#tb <- bq_project_query(billing, sql)

#df <- bq_table_download(tb)

#write_csv(df,"level_duration.csv")

#########################################

df <- read_csv("level_duration.csv")

df %>% 
  group_by(levelId) %>%
  summarise(m = mean(missionSecondsPlayed)) %>%
  filter(!levelId== 0 & !levelId== 291 & !levelId== 292) 

duration <- df %>%
  group_by(levelId) %>%
  summarise(m = mean(missionSecondsPlayed)) %>%
  filter(!levelId== 0 & !levelId== 291 & !levelId== 292) %>%
  mutate(part = factor(case_when(levelId >= 1   & levelId <= 50  ~ "1-50",
                                 levelId >= 51  & levelId <= 100 ~ "51-100",
                                 levelId >= 101 & levelId <= 150 ~ "101-150",
                                 levelId >= 151 & levelId <= 200 ~ "151-200",
                                 levelId >= 201 & levelId <= 250 ~ "201-250",
                                 levelId >= 251 & levelId <= 290 ~ "251-290")))

duration$part <- fct_relevel(duration$part, "1-50","51-100","101-150","151-200","201-250","251-290")

#1-50
duration %>%
  filter(part=="1-50") %>%
  ggplot(aes(x=levelId,y=m)) +
  geom_point() +
  geom_line() + 
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  labs(y="Probability of completion") 

ggsave("duration1.png",width= 18,height = 10)

#51-100
duration %>%
  filter(part=="51-100") %>%
  ggplot(aes(x=levelId,y=m)) +
  geom_point() +
  geom_line() + 
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  labs(y="Probability of completion") 

ggsave("duration2.png",width= 18,height = 10)

#101-150
duration %>%
  filter(part=="101-150") %>%
  ggplot(aes(x=levelId,y=m)) +
  geom_point() +
  geom_line() + 
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  labs(y="Probability of completion") 

ggsave("duration3.png", width= 18,height = 10)