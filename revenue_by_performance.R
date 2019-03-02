# revenue by performance

library(tidyverse)
library(bigrquery)

billing <- "testproj-223217"
sql <- "SELECT userid, eventName
        FROM `tactile-external.interview.events`
        WHERE eventName = 'missionStarted' OR eventName = 'missionFailed' OR eventName = 'coinsUsed'"

tb <- bq_project_query(billing, sql)
 
df <- bq_table_download(tb)

write_csv(df,"retention.csv")
