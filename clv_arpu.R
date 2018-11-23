# CLV and Average Revenue Per User (ARPU) and Average Revenue Per Paying User (ARPPU)

library(tidyverse)
library(bigrquery)

billing <- "testproj-223217"
sql <- "SELECT userid, productAmount, productCategory, itemAmount, itemType
         FROM `tactile-external.interview.events`
         WHERE eventName = 'newPlayer'"

tb <- bq_project_query(billing, sql)

df <- bq_table_download(tb)

