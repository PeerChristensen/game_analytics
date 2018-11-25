# level difficulty

# NOTE THAT EVENT LEVEL WAS EMPTY

library(tidyverse)
library(bigrquery)

billing <- "testproj-223217"
sql <- "SELECT userid, levelId, eventName
        FROM `tactile-external.interview.events`
        WHERE eventName = 'missionCompleted' OR eventName = 'missionFailed'"
 
#tb <- bq_project_query(billing, sql)
 
#df <- bq_table_download(tb)

#write_csv(df,"level_difficulty.csv")

#################################

df <- read_csv("level_difficulty.csv")

difficulty <- df %>%
  group_by(levelId) %>%
  summarise(attempts = length(eventName),wins=length(eventName[eventName=="missionCompleted"])) %>%
  mutate(probWin = wins/attempts) %>%
  mutate(error = (sqrt(probWin * (1 - probWin) / attempts))*100) %>%
  filter(!levelId== 0 & !levelId== 291 & !levelId== 292) %>%
  mutate(part = factor(case_when(levelId >= 1   & levelId <= 50  ~ "1-50",
                                 levelId >= 51  & levelId <= 100 ~ "51-100",
                                 levelId >= 101 & levelId <= 150 ~ "101-150",
                                 levelId >= 151 & levelId <= 200 ~ "151-200",
                                 levelId >= 201 & levelId <= 250 ~ "201-250",
                                 levelId >= 251 & levelId <= 290 ~ "251-290")))

difficulty$part <- fct_relevel(difficulty$part, "1-50","51-100","101-150","151-200","201-250","251-290")

difficulty %>%
  filter(part=="1-50" | part=="51-100" | part=="101-150") %>%
  ggplot(aes(x=levelId,y=probWin)) +
  geom_errorbar(aes(ymin=probWin-error,ymax=probWin+error)) +
  geom_point() +
  geom_line() + 
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  labs(y="Probability of completion") +
  facet_wrap(~part, ncol=1, scales = "free_x")

#1-50
difficulty %>%
  filter(part=="1-50") %>%
  ggplot(aes(x=levelId,y=probWin)) +
  geom_errorbar(aes(ymin=probWin-error,ymax=probWin+error)) +
  geom_point() +
  geom_line() + 
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  labs(y="Probability of completion") 

ggsave("difficulty1.png",width= 18,height = 10)

#51-100
difficulty %>%
  filter(part=="51-100") %>%
  ggplot(aes(x=levelId,y=probWin)) +
  geom_errorbar(aes(ymin=probWin-error,ymax=probWin+error)) +
  geom_point() +
  geom_line() + 
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  labs(y="Probability of completion") 
  
ggsave("difficulty2.png",width= 18,height = 10)

#101-150
difficulty %>%
  filter(part=="101-150") %>%
  ggplot(aes(x=levelId,y=probWin)) +
  geom_errorbar(aes(ymin=probWin-error,ymax=probWin+error)) +
  geom_point() +
  geom_line() + 
  scale_y_continuous(labels = scales::percent) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  labs(y="Probability of completion") 

ggsave("difficulty3.png", width= 18,height = 10)
