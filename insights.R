rm(list=ls())
setwd("/home/tauro/Projects/quizziz")

library(jsonlite)
library(lubridate)
library(dplyr)
library(ggplot2)
#library(reshape2)
#library(purrr)

data <- stream_in(file("data_role_assignment.json"))

#params <- data$params

params <- lapply(data$params, "[[", 2)
params <- setNames(do.call(rbind.data.frame, params), 
                       c("sessionId", "page", "userId"))

data$params <- NULL

data <- cbind(data, params)

rm(params)

###########################

data$serverTime <- ymd_hms(data$serverTime, tz="UTC")

data$day <- day(data$serverTime) 
data$month <- month(data$serverTime)
data$hour <- hour(data$serverTime)
data$wday <- wday(data$serverTime)

#data$serverTime <- NULL

categorical_cols <- c("version", "eventId", "sessionId", "page", "userId", "day",
                      "month", "hour", "wday")

data[categorical_cols] <- lapply(data[categorical_cols], factor)


###############################

#Events per session across both versions
eventsPerSessionAB <- data %>% select(everything()) %>% group_by(sessionId, version) %>%
  summarise("n_events" = length(eventId)) %>% arrange(desc(n_events)) 

#Average events per session across both versions
eventsPerSessionAB_avg <- data %>% select(everything()) %>% group_by(sessionId, version) %>%
  summarise("n_events" = length(eventId)) %>% group_by(version) %>% 
  summarise("avg_n_events" = mean(n_events))

ggplot(eventsPerSessionAB_avg, aes(x=version, y=avg_n_events, fill=version)) + geom_bar(stat="identity")


#############################

#Amount of time per session across both versions
timePerSessionAB <- data %>% select(sessionId, version, serverTime) %>% 
  arrange(serverTime) %>%
  group_by(sessionId, version) %>% 
  #summarise(last(serverTime), first(serverTime))
  summarise("session_duration" = last(serverTime) - first(serverTime))
