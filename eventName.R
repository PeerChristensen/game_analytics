#  Event name

library(tidyverse)
library(bigrquery)

billing <- "testproj-223217" # replace this with your project ID 
sql <- "SELECT eventName FROM `tactile-external.interview.events`"

#tb <- bq_project_query(billing, sql)
#> Auto-refreshing stale OAuth token.
#df <- bq_table_download(tb)
#write_csv(df, "eventNames.csv")

df <- read_csv("eventNames.csv")

df %>% 
  group_by(eventName) %>% 
  count() %>% 
  arrange(n) %>% 
  ungroup() %>% 
  mutate(order = row_number()) %>%
  ggplot(aes(x=reorder(eventName,order),y=n, fill = order)) + 
  geom_col() +
  coord_flip() +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  labs(x = "Event name", y="N") +
  #scale_fill_viridis_d("A") +
  scale_fill_gradient(low= "black", high = "#C83488",guide=F) +
  scale_y_continuous(labels = function(n) {
    trans = n / 1000
    paste0(trans, "K")
  })

ggsave("eventNames.png")
