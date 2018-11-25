# conversion by level

library(tidyverse)
library(bigrquery)

billing <- "testproj-223217"
sql <- "SELECT userid, levelId, eventDate, eventName
FROM `tactile-external.interview.events`
WHERE eventName = 'transaction'"

#tb <- bq_project_query(billing, sql)

#df <- bq_table_download(tb)

#write_csv(df,"conversion_level.csv")

#################################

df <- read_csv("conversion_level.csv")


# density
df %>%
  group_by(userid) %>%
  filter(userLevel == min(userLevel)) %>%
  ggplot(aes(x=userLevel)) +
  geom_density()

# bars

#find top 3 levels

top3 <- df %>%
  group_by(userid) %>%
  filter(userLevel == min(userLevel)) %>%
  group_by(userLevel) %>%
  summarise(n = length(unique((userid)))) %>%
  top_n(3,n)

#     userLevel     n
#           <int> <int>
#   1        10  1236
#   2        30  1428
#   3        61   335

df %>%
  group_by(userid) %>%
  filter(userLevel == min(userLevel) & userLevel < 150) %>%
  group_by(userLevel) %>%
  summarise(n = length(unique((userid)))) %>%
  ggplot(aes(x=userLevel, y = n)) +
  geom_col(fill="#C83488") +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  annotate("label", x = top3$userLevel+5, y = top3$n, label = top3$userLevel, size = 6)

ggsave("conversion_level.png")

# median and mode

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

df %>%
  group_by(userid) %>%
  filter(userLevel == min(userLevel)) %>%
  ungroup() %>%
  summarise(median = median(userLevel),
            mode   = get_mode(userLevel))
