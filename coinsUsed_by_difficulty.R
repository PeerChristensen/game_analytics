# coinsUsed by level difficulty

library(tidyverse)
library(bigrquery)

billing <- "testproj-223217"
sql <- "SELECT levelId, eventName
        FROM `tactile-external.interview.events`
        WHERE eventName = 'missionCompleted' OR eventName = 'missionFailed' OR eventName = 'coinsUsed'"

#tb <- bq_project_query(billing, sql)

#df <- bq_table_download(tb)
 
#write_csv(df,"coinsUsed_by_difficulty.csv")

#################################

df <- read_csv("coinsUsed_by_difficulty.csv")

difficulty <- df %>%
  group_by(levelId) %>%
  summarise(attempts  = length(eventName[eventName=="missionCompleted" |eventName=="missionFailed" ]),
            wins      = length(eventName[eventName=="missionCompleted"]),
            coinsUsed = length(eventName[eventName=="coinsUsed"])) %>%
  filter(attempts > 5000) %>%
  mutate(probWin = wins/attempts,
         probCoinUse = coinsUsed/attempts) %>%
  mutate(part = factor(case_when(levelId >= 1   & levelId <= 50  ~ "1-50",
                                 levelId >= 51  & levelId <= 100 ~ "51-100",
                                 levelId >= 101 & levelId <= 150 ~ "101-150",
                                 levelId >= 151 & levelId <= 200 ~ "151-200",
                                 levelId >= 201 & levelId <= 250 ~ "201-250",
                                 levelId >= 251 & levelId <= 290 ~ "251-290")))

difficulty$part <- fct_relevel(difficulty$part, "1-50","51-100","101-150","151-200","201-250","251-290")

# plot by level
difficulty %>%
  filter(levelId < 150) %>%
  ggplot(aes(levelId,coinsUsed)) +
  geom_col() +
  geom_col(fill="#C83488") +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16))

# plot by difficulty
difficulty %>%
  filter(levelId < 150) %>%
  ggplot(aes(probWin,probCoinUse)) +
  geom_point(size=5, alpha = .8) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16))

ggsave("coins_by_difficulty.png")

# plot by difficulty with trend
difficulty %>%
  filter(levelId < 150) %>%
  ggplot(aes(probWin,probCoinUse)) +
  geom_point(size=5, alpha = .8) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  geom_smooth(colour="snow", fill="#C83488")

ggsave("coins_by_difficulty_trend.png")


fit <- lm(probCoinUse~probWin,data=difficulty)
summary(fit)

# regression 
fit <- lm(probCoinUse~probWin,data=difficulty)
summary(fit)


