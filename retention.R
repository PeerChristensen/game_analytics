# Retention

library(tidyverse)
library(bigrquery)
library(reshape2)
library(gmodels)

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

df <- read_csv("retention.csv")

cohorts <- df %>%
  group_by(userid) %>%
  mutate(first = min(eventDate)) %>%
  group_by(first, eventDate) %>% 
  summarise(users = n()) %>%
  spread(eventDate, users) %>%
  ungroup() %>%
  mutate(first = as.character(seq(1,31))) 

cohortsDF <- cohorts %>% data.frame()

# shift columns

cohortsShift <- cohortsDF #create new data frame
totcols <- ncol(cohortsShift) #count number of columns in data set
for (i in 1:nrow(cohortsShift)) { #for loop for shifting each row
  df <- cohortsShift[i,] #select row from data frame
  df <- df[ , !is.na(df[])] #remove columns with zeros
  partcols <- ncol(df) #count number of columns in row (w/o zeros)
  #fill columns after values by zeros
  if (partcols < totcols) df[, c((partcols+1):totcols)] <- 0
  cohortsShift[i,] <- df #replace initial row by new one
}

# percentages

x <- cohortsShift[,c(2:ncol(cohortsShift))]
y <- cohortsShift[,2]

retentionRate <- apply(x, 2, function(x) round(x/y * 100,1))
retentionRate <- data.frame(cohort=cohorts$first, retentionRate)
retentionRate <- retentionRate[-31,-2]
retentionRate = as_tibble(retentionRate)
retentionRate$cohort <- seq(1,30)

names(retentionRate)[2:ncol(retentionRate)] <- seq(1:(ncol(retentionRate)-1))

retentionRate <- melt(retentionRate, id.vars = 'cohort')
names(retentionRate ) <- c('cohort', 'day', 'retention')

retentionRate <- subset(retentionRate, retentionRate$retention != 0)  

# plot all-in-one

retentionRate %>% 
  ggplot(aes(x=day,y=retention, colour = factor(cohort), group=cohort)) +
  geom_line(size=1.5) +
  geom_point(size=1.5) +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  scale_colour_viridis_d(option="A", alpha=.8)

ggsave("retention_rate_all.png")

# facets
# first nine cohorts

retentionRate %>% 
  filter(!cohort >9) %>%
  ggplot(aes(x=day,y=retention, colour = factor(cohort), group=cohort)) +
  geom_line(size=1.5) +
  geom_point(size=1.5) +
  theme_minimal() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text.x = element_blank()) +
  scale_colour_viridis_d(option="A", end=.9) +
  facet_wrap(~cohort)

ggsave("retention_rate_facets.png")

# bars 1, 7 day retention

# conf. intervals

retentionBars <- retentionRate %>%
  filter(day == "1" | day == "7") %>%
  group_by(day) %>%
  summarise(m  = mean(retention),
            lowCI = ci(retention)[2],
            hiCI  = ci(retention)[3])

retentionBars %>%
  ggplot(aes(x=day,y=m)) +
  geom_bar(stat="identity", fill = "#9F3896", width=.7) +
  geom_errorbar(aes(ymin=lowCI,ymax=hiCI),width=.2) +
  labs(y="retention") +
  theme_light() +
  theme(axis.text  = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  annotate("text", x =c(1,2), y = 10,label = c("53.9%", "20.7%"),size=12, colour = "snow")
  
ggsave("retention_rate_bars.png")

