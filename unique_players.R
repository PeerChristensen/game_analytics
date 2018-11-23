# N unique players

library(tidyverse)
library(bigrquery)
# 
# billing <- "testproj-223217"
# sql <- "SELECT DISTINCT userid
#         FROM `tactile-external.interview.events`"
# 
# tb <- bq_project_query(billing, sql)
# 
# df <- bq_table_download(tb)
# 
# write_csv(df,"unique_players.csv")

#####################################

df <- read_csv("unique_players.csv")

dim(df) # 399465 unique players
