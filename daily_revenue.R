# daily revenue

library(tidyverse)
library(bigrquery)

billing <- "testproj-223217"
sql <- "SELECT eventName, eventDate, productAmount
        FROM `tactile-external.interview.events`
        WHERE eventName = 'transaction'"

tb <- bq_project_query(billing, sql)

df <- bq_table_download(tb)

write_csv(df,"daily_revenue.csv")

#################################

df %>%
  group_by(eventDate) %>%
  summarise(dayRevenue = sum(productAmount,na.rm=T)) %>%
  ggplot(aes(x=eventDate,y=dayRevenue)) + geom_line()
