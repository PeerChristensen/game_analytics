# mission abandoned

library(tidyverse)
library(bigrquery)

billing <- "testproj-223217"
sql <- "SELECT levelId, eventName
        FROM `tactile-external.interview.events`
        WHERE eventName = 'missionAbandoned' OR eventName = 'missionStarted'"

# tb <- bq_project_query(billing, sql)
# 
# df <- bq_table_download(tb)
# 
# write_csv(df,"mission_abandoned.csv")

######################################

df <- read_csv("mission_abandoned.csv")

abandoned <- df %>%
  group_by(levelId) %>%
  summarise(starts = length(eventName),abandon=length(eventName[eventName=="missionAbandoned"])) %>%
  filter(starts > 1000) %>%
  mutate(probAbandon = abandon/starts) %>%
  mutate(error = (sqrt(probAbandon* (1 - probAbandon) / starts))*100) %>%
  filter(!levelId== 0 & !levelId== 291 & !levelId== 292) %>%
  mutate(part = factor(case_when(levelId >= 1   & levelId <= 50  ~ "1-50",
                                 levelId >= 51  & levelId <= 100 ~ "51-100",
                                 levelId >= 101 & levelId <= 150 ~ "101-150",
                                 levelId >= 151 & levelId <= 200 ~ "151-200",
                                 levelId >= 201 & levelId <= 250 ~ "201-250",
                                 levelId >= 251 & levelId <= 290 ~ "251-290")))


#1-50
abandoned %>%
  filter(part=="1-50") %>%
  ggplot(aes(x=levelId,y=probAbandon)) +
  geom_errorbar(aes(ymin=probAbandon-error,ymax=probAbandon+error)) +
  geom_point() +
  geom_line() + 
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  labs(y="Probability of abandonment") 

ggsave("abandoned1.png",width= 18,height = 10)

#51-100
abandoned %>%
  filter(part=="51-100") %>%
  ggplot(aes(x=levelId,y=probAbandon)) +
  geom_errorbar(aes(ymin=probAbandon-error,ymax=probAbandon+error)) +
  geom_point() +
  geom_line() + 
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  labs(y="Probability of abandonment") 

ggsave("abandoned2.png",width= 18,height = 10)

#101-150
abandoned %>%
  filter(part=="101-150") %>%
  ggplot(aes(x=levelId,y=probAbandon)) +
  geom_errorbar(aes(ymin=probAbandon-error,ymax=probAbandon+error)) +
  geom_point() +
  geom_line() + 
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  labs(y="Probability of abandonment") 

ggsave("abandoned3.png", width= 18,height = 10)

  