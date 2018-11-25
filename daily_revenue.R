# daily revenue

library(tidyverse)
library(bigrquery)

billing <- "testproj-223217"
sql <- "SELECT eventName, eventDate, productAmount
        FROM `tactile-external.interview.events`
        WHERE eventName = 'transaction'"

#tb <- bq_project_query(billing, sql)

#df <- bq_table_download(tb)

#write_csv(df,"daily_revenue.csv")

#################################

df <- read_csv("daily_revenue.csv")

df %>%
  group_by(eventDate) %>%
  summarise(dayRevenue = sum(productAmount,na.rm=T)) %>%
  ggplot(aes(x=eventDate,y=dayRevenue)) +
  geom_line(size=1) +
  geom_point(size=1.5) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_y_continuous(labels = function(n) {
    trans = n / 1000
    paste0(trans, "K")
  })

ggsave("daily_revenue.png")

range(df$d)
