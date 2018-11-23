# CLV and Average Revenue Per User (ARPU) and Average Revenue Per Paying User (ARPPU)

library(tidyverse)
library(bigrquery)

billing <- "testproj-223217"
sql <- "SELECT userid, productAmount, productCategory, itemAmount, itemType
        FROM `tactile-external.interview.events`
        WHERE eventName = 'transaction'"

#tb <- bq_project_query(billing, sql)

#df <- bq_table_download(tb)

#write_csv(df,"clv_arpu.csv")

#################################

df <- read_csv("clv_arpu.csv")

length(unique(df$userid))
#   8464 paying players
# 399465 total players
# percentage = 8464/399465*100 = 2.1

df %<>% filter(productCategory=="REAL_CURRENCY") %>%
  select(-itemAmount, -itemType, -productCategory)

df$productAmount <- as.numeric(df$productAmount)


# ARPPU
arppu <- df %>%
  group_by(userid) %>%
  summarise(total = sum(productAmount))

arppu %>% 
  ggplot(aes(x=total)) +
  geom_density(fill="#9E3896") +
  xlim(c(0,5000)) +
  theme_light() +
  labs(x="Total revenue", y="") +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16))

ggsave("arppu.png")

summary(arppu$total)

# ARPU
sum(arppu$total) / 399465
# ARPU = 93.9

