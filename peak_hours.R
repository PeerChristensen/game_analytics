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

df$eventTimestampMin <- hms::hms(second(df$eventTimestamp), minute(df$eventTimestamp), hour(df$eventTimestamp)) 

#df2 = df[1:10,]
#df2$eventTimestamp2 <- hms::hms(df2$eventTimestamp)
#df2$min <- format(df2$eventTimestamp, "%H:%M:%S")
#df2$min2 <- as.POSIXct(df2$eventTimestamp, format="%H:%M")
#df2 = df[1:100,]
#df2$eventTimestamp = hms::hms(second(df2$eventTimestamp), minute(df2$eventTimestamp), hour(df2$eventTimestamp))  

#df %<>% mutate(time = as.POSIXct(strptime(df$eventTimestamp, "%Y/%m/%d %H:%M",  tz = "UTC")))


# peak hours: game start
p <- df %>%
  ggplot(aes(x=eventTimestampMin)) +
  geom_density(fill="#9E3896") 

# find peak
densities <- ggplot_build(p)[[1]][[1]]
x = densities %>%filter(density==max(density)) %>% select(x)
x = as.numeric(round(x))
xAsTime = hms::hms(x)

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

# with Hong Kong
cols <- scales::seq_gradient_pal("#C83488", "#9F3896", "Lab")(seq(0,1,length.out=length(unique(countryPeaks$region))))

countryPeaks %>% 
  ggplot(aes(x=eventTimestampMin,y=region,fill=region)) +
  geom_density_ridges(colour="gray70") +
  labs(x="UTC Time", y="") +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_fill_manual(values=cols, guide=F)

ggsave("peak_hours_country_wHK.png")


# without Hong Kong
cols <- scales::seq_gradient_pal("#C83488", "#9F3896", "Lab")(seq(0,1,length.out=length(unique(countryPeaks$region))-1))

countryPeaks %>%
  filter(userCountry!="HK") %>%
  ggplot(aes(x=eventTimestampMin,y=region, fill=region)) +
  geom_density_ridges(colour="gray70") +
  labs(x="UTC Time", y="") +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_fill_manual(values=cols, guide=F)

ggsave("peak_hours_country.png")
  
