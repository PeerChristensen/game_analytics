#  Tactile data analysis: daily installs

library(tidyverse)
library(bigrquery)
library(scales)
library(magrittr)
library(maps)

billing <- "testproj-223217" # replace this with your project ID 
sql <- "SELECT eventName, eventDate, advertisingId, acquisitionChannel, userCountry
        FROM `tactile-external.interview.events` 
        WHERE eventName = 'newPlayer'"

#tb <- bq_project_query(billing, sql)

# NOTE that advertisingId and acquisitionChannel are empty variables
#df <- bq_table_download(tb)

# 2919 NAs
#table(is.na(df$userCountry)) 

#df %<>% 
#  filter(!is.na(userCountry), userCountry != "-1") %>%
#  select(-acquisitionChannel, -advertisingId) 

#write_csv(df, "newPlayer_by_country.csv")

##############################################

df <- read_csv("newPlayer_by_country.csv")

worldMapData   <- map_data("world")
countryCodes <- read_csv("country-codes.csv") %>%
  mutate(region      = `English short name lower case`,
         userCountry = `Alpha-2 code`) %>%
  select(region,userCountry)

countryCodes$region[countryCodes$userCountry=="US"] = "USA"
countryCodes$region[countryCodes$userCountry=="GB"] = "UK"

mapData <- left_join(worldMapData,countryCodes)



# world map
mapData <- df %>%
  group_by(userCountry) %>%
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  #top_n(20) %>%
  mutate(order = row_number()) %>%
  left_join(mapData, by = "userCountry") 

ggplot()                           +
  geom_map(data = worldMapData, map = worldMapData, 
           aes(map_id = region,x = long, y = lat),
           fill = "gray50", colour = "gray97", size = 0.3)   +
  geom_map(data = mapData, map = mapData, 
           aes(map_id = region,fill=log(n)))      +
  theme_void() +
  scale_fill_viridis_c(option="B",guide=F, direction = -1)   +
  #coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))          +
  coord_equal(ylim = c(-55,90)) 

ggsave("newPlayersByCountry.png")

# world map, proportion of population


# top 20 countries
mapData %>%
  select(region,userCountry,n) %>%
  distinct() %>%
  mutate(region=replace(region, userCountry=="HK", "Hong Kong")) %>%
  top_n(20) %>%
  mutate(order = row_number()) %>%
  ggplot(aes(x=reorder(region,n),y=n, fill = order)) +
  geom_col() +
  coord_flip() +
  labs(x = "country", y = "n")

# top 20 countries by population


