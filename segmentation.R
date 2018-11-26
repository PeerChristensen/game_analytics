# segmentation

library(tidyverse)
library(bigrquery)

# 
# billing <- "testproj-223217"
# sql <- "SELECT userid, eventDate, eventName
# FROM `tactile-external.interview.events`
# WHERE eventName = 'newPlayer' OR eventName = 'gameStarted'"
# 
# tb <- bq_project_query(billing, sql)
# 
# df <- bq_table_download(tb)
# 
# write_csv(df,"retention.csv")

#################################