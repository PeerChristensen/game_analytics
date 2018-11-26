# segmentation

library(tidyverse)
library(bigrquery)
library(factoextra)

 
billing <- "testproj-223217"
sql <- "SELECT userid, userCountry, eventName
        FROM `tactile-external.interview.events`
        WHERE eventName = 'coinsUsed' OR eventName = 'missionFailed' OR eventName = 'missionCompleted'
        AND userCountry = 'DK'"

 
t#b <- bq_project_query(billing, sql)
 
#df <- bq_table_download(tb)
 
#write_csv(df,"user_segmentation.csv")

#################################

df <- read_csv("user_segmentation.csv")
df <- subset(df, userCountry=="DK")

users <- df %>%
  group_by(userid) %>%
  summarise(attempts  = length(eventName[eventName=="missionCompleted" |eventName=="missionFailed" ]),
            wins      = length(eventName[eventName=="missionCompleted"]),
            coinsUsed = length(eventName[eventName=="coinsUsed"])) %>%
  mutate(probWin = wins/attempts,
         probCoinUse = coinsUsed/attempts)

# Distance
dfScale <- users %>%
  dplyr::select(probWin,probCoinUse) %>%
  mutate(probWin = scale(probWin),
                   probCoinUse = scale(probCoinUse)) %>%
  data.frame()

rownames(dfScale) <- as.character(seq(1,nrow(dfScale)))


# K-MEANS
# n clusters
fviz_nbclust(dfScale, kmeans)

set.seed(324789)
km.res <- kmeans(dfScale, 5, nstart = 25)

fviz_cluster(km.res, data = dfScale,
             ellipse.type = "convex",
             geom = "point",
             #palette = inferno(10)[c(4,8)],
             ggtheme = theme_minimal(),
             main = "K-means Clustering") 


