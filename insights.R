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

#Number of events per session across both versions
eventsPerSessionAB <- data %>% select(everything()) %>% group_by(sessionId, version) %>%
  summarise("n_events" = length(eventId)) %>% arrange(desc(n_events)) 

#Mean number of events per session across both versions (including bounces)
mean_eventsPerSessionAB <- eventsPerSessionAB  %>% group_by(version) %>% 
  summarise("mean_n_events" = mean(n_events))

ggplot(mean_eventsPerSessionAB, aes(x=version, y=mean_n_events, fill=version)) + geom_bar(stat="identity")


#Mean number of events per session across both versions (excluding bounces)
mean_eventsPerSessionAB <- eventsPerSessionAB %>% filter(n_events > 1) %>% 
  group_by(version) %>% summarise("mean_n_events" = mean(n_events))

ggplot(mean_eventsPerSessionAB, aes(x=version, y=mean_n_events, fill=version)) + geom_bar(stat="identity")

#Mean IQR number of events per session across both versions 
miqr_eventsPerSessionAB <- eventsPerSessionAB %>% group_by(version) %>% 
  filter(n_events >= quantile(n_events, 0.05) & n_events <= quantile(n_events, 0.95)) %>%
  summarise("mean_IQR_n_events" = mean(n_events))


################################

#Which version has a higher bounce rate?

bounce_rate <- eventsPerSessionAB %>% filter(n_events == 1) %>% group_by(version) %>%
  summarise("bounce_rate" = 100 * (length(n_events)/length(eventsPerSessionAB$n_events)))

ggplot(bounce_rate, aes(x=version, y=bounce_rate, fill=version)) + 
  geom_bar(stat="identity")

#############################

#Amount of time per session across both versions
timePerSessionAB <- data %>% select(sessionId, version, serverTime) %>% 
  arrange(serverTime) %>%
  group_by(sessionId, version) %>% 
  #summarise(last(serverTime), first(serverTime))
  summarise("session_duration" = as.numeric(last(serverTime) - first(serverTime),
                                            units = "mins"))

#Mean amount of time per session across both versions (including bounces)
mean_timePerSessionAB <- timePerSessionAB %>% group_by(version) %>% 
  summarise("mean_session_duration" = mean(session_duration))


ggplot(mean_timePerSessionAB, aes(x=version, y=mean_session_duration, fill=version)) +
  geom_bar(stat="identity")

#Mean amount of time per session across both versions (excluding bounces)
mean_timePerSessionAB <- timePerSessionAB %>% filter(session_duration > 0) %>%
  group_by(version) %>% summarise("mean_session_duration" = mean(session_duration))



ggplot(mean_timePerSessionAB, aes(x=version, y=mean_session_duration, fill=version)) +
  geom_bar(stat="identity")

#Mean IQR amount of time per session across both versions
mean_IQRtimePerSessionAB <- timePerSessionAB %>% group_by(version) %>%
  filter(session_duration >= quantile(session_duration, 0.05) &
           session_duration <= quantile(session_duration, 0.95)) %>%
  summarise("mean_IQR_session_duration" = median(session_duration))


