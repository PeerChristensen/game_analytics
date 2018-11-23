# peak hour and peak hours by country

library(tidyverse)
library(bigrquery)
library(scales)
library(magrittr)
library(maps)
library(lubridate)
library(ggridges)

billing <- "testproj-223217" # replace this with your project ID
sql <- "SELECT eventName, eventTimestamp, userCountry
        FROM `tactile-external.interview.events`
        WHERE eventName = 'gameStarted'"

#tb <- bq_project_query(billing, sql)

#df <- bq_table_download(tb)

#write_csv(df, "peak_hours.csv")

##################################################

df <- read_csv("peak_hours.csv")

df$eventTimestamp <- hms::hms(second(df$eventTimestamp), minute(df$eventTimestamp), hour(df$eventTimestamp))  

#df2 = df[1:100,]
#df2$eventTimestamp = hms::hms(second(df2$eventTimestamp), minute(df2$eventTimestamp), hour(df2$eventTimestamp))  

#df %<>% mutate(time = as.POSIXct(strptime(df$eventTimestamp, "%Y/%m/%d %H:%M",  tz = "UTC")))


# peak hours: game start
p <-df[1:1000,] %>%
  ggplot(aes(x=eventTimestamp)) +
  geom_density(fill="#9E3896") 

# find peak
densities <- ggplot_build(p)[[1]][[1]]
x = densities %>%filter(density==max(density)) %>% select(x)
x = as.numeric(x)
xAsTime = hms::hms(66791)

p + 
  geom_vline(xintercept=x, linetype="dashed", size = 1.5) +
  annotate("text", x = x-25000, y = 1.574253e-05, label = glue::glue("Peak: {xAsTime} (UTC)"),
           size=7) +
  labs(x="UTC Time", y="Game starts") +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16))

ggsave("game_starts_time.png")

# find peak by country
countries <- read_csv("top20_countries.csv")

countryPeaks <- left_join(countries,df)

countryPeaks %>%
  ggplot(aes(x=eventTimestamp,y=region)) +
  geom_density_ridges(fill="#9F3896") +
  labs(x="UTC Time", y="") +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16))

ggsave("peak_hours_country.png")
  
