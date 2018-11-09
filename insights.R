rm(list=ls())
setwd("/home/tauro/Projects/quizziz")

library(jsonlite)
library(lubridate)
library(dplyr)
library(ggplot2)
#library(reshape2)
#library(purrr)

options(scipen=999)


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
data$wday <- wday(data$serverTime, label=TRUE)

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

ggplot(timePerSessionAB, aes(x=session_duration, fill=version)) + 
  geom_histogram(binwidth = 1, position = "dodge") + xlim(c(0, 20)) + ylim(0, 2000) +
  scale_x_reverse()


##
timePerSessionABgrt300 <- timePerSessionAB %>% filter(session_duration > 50)

ggplot(timePerSessionABgrt300, aes(x=session_duration, fill=version)) + 
  geom_histogram(binwidth = 1, position = "dodge") + xlim(c(0, 4000)) + ylim(0, 5)


#Mean IQR amount of time per session across both versions
mean_IQRtimePerSessionAB <- timePerSessionAB %>% group_by(version) %>%
  filter(session_duration >= quantile(session_duration, 0.05) &
           session_duration <= quantile(session_duration, 0.95)) %>%
  summarise("mean_IQR_session_duration" = median(session_duration))







###########################
#Total userId
users_total <- length(data$userId[!(is.na(data$userId))])
users_unique_total <- length(unique(data$userId[!(is.na(data$userId))])) 

#Number of registered users per version
numUsersAB <- data %>% filter(!(is.na(userId))) %>% group_by(version) %>% 
  summarise("n_users" = length(userId), "percentage" = 100 * n_users/users_total)

#Number of unique registered users per version
numUniqueUsersAB <- data %>% filter(!(is.na(userId))) %>% group_by(version) %>%
  summarise("n_users" = length(unique(userId)), "percentage" = 100 * n_users/users_unique_total)




##########################
#Events driven per page across both versions
eventsPerPage <- data %>% group_by(page, version) %>% 
  summarise("n_events" = length(eventId)) %>% group_by(version) %>% arrange(desc(n_events))

ggplot(eventsPerPage, aes(x=page, y=n_events, fill=version)) + 
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=45, hjust=1))


############################
#Bounces per page across both versions

bouncesPerPage <- data %>% group_by(sessionId, version, page) %>%
  summarise("n_events" = n()) %>% filter(n_events == 1) %>% 
  group_by(page, version) %>% summarise("n_bounces" = n()) %>% 
  group_by(version) %>% mutate("prop_bounce_for_version" = n_bounces/sum(n_bounces)) %>%
  arrange(desc(prop_bounce_for_version))
  



##################################
#Proportion of non-bounces (a user not bouncing is assumed to be a success here)




nonBouncePerVersion <- data %>% select(everything()) %>% group_by(sessionId, version) %>% 
  summarise("n_events" = length(eventId)) %>%
  group_by(version) %>% summarise("sessions_not_bounced" = length(sessionId[n_events > 1]),
                                  "sessions_bounced" = length(sessionId[n_events == 1]),
                                  "total_session" = length(sessionId))


n_trials_A <- subset(nonBouncePerVersion, version=="A")$total_session
n_trials_B <- subset(nonBouncePerVersion, version=="B")$total_session

n_success_A <- subset(nonBouncePerVersion, version=="A")$sessions_not_bounced
n_success_B <- subset(nonBouncePerVersion, version=="B")$sessions_not_bounced

pA <- n_success_A/n_trials_A
pB <- n_success_B/n_trials_B



x_a <- 1:25000
y_a <- dbinom(x_a, n_trials_A, pA)

x_b <- 1:25000
y_b <- dbinom(x_b, n_trials_B, pB)

df <- data.frame(x_a=x_a, y_a=y_a, x_b=x_b, y_b=y_b)


options(repr.plot.width=5, repr.plot.height=3)
cols = c("A"="green","B"="orange")

ggplot(data = df)+
  labs(x="Number of successes", y="Probability") + xlim(0, 25000) +
  geom_point(aes(x=x_a, y=y_a, colour="A")) +
  geom_point(aes(x=x_b, y=y_b, colour="B")) +
  scale_colour_manual(name="Variants", values=cols)



################################

x_a = seq(from=0.005, to=0.95, by=0.00001)
y_a = dnorm(x_a, mean = pA, sd = sqrt((pA * (1-pA))/25000))

x_b = seq(from=0.005, to=0.95, by=0.00001)
y_b = dnorm(x_b, mean = pB, sd = sqrt((pB * (1-pB))/25000))

df = data.frame(x_a=x_a, y_a=y_a, x_b=x_b, y_b=y_b)
options(repr.plot.width=7, repr.plot.height=3)
cols = c("A"="green","B"="orange")
ggplot(data = df)+
  labs(x="Proportions value", y="Probability Density Function") +
  geom_point(aes(x=x_a, y=y_a, colour="A")) + 
  geom_point(aes(x=x_b, y=y_b, colour="B")) + 
  scale_colour_manual(name="Variants", values=cols)


###############################

binom.test(n_success_B, 25000, p=pA, alternative="greater")

binom.test(n_success_A, 25000, p=pB, alternative="less")

alpha = 0.05
qbinom(1 - alpha, 25000, pA)

#############################

bounce_sessions <- data %>% select(everything()) %>% group_by(sessionId) %>%
  summarise("n_events" = length(eventId), "version" = first(version)) %>% 
  filter(n_events == 1)



#Do any sessions use both versions? 
bothversionSession <- subset(data, version=="A" & version=="B")
#No


##################################

bouncesPerWDay <- data %>% select(everything()) %>% 
  group_by(sessionId, wday) %>% summarise("n_events" = length(eventId)) %>% 
  filter(n_events == 1) %>% group_by(wday) %>% summarise("n_bounces" = length(n_events))
#bouncesPerWDay <- bouncesPerWDay[unique(bouncesPerWDay$sessionId),]

ggplot(bouncesPerWDay, aes(x=wday, y=n_bounces, fill=wday)) + geom_bar(stat="identity")


bounceRatePerWDay <- data %>% select(everything()) %>% 
  group_by(sessionId, wday, version) %>% summarise("n_events" = length(eventId)) %>% 
  group_by(wday, version) %>% 
  summarise("bounce_rate" = 100 *(length(n_events == 1)/sum(n_events)))


ggplot(bounceRatePerWDay, aes(x=wday, y=bounce_rate, fill=version)) + 
  geom_bar(stat="identity", position="dodge")
  

##################

bouncesPerDay <- data %>% select(everything()) %>% group_by(sessionId, day, wday, version) %>%
  summarise("n_events" = length(eventId)) %>% filter(n_events == 1) %>%
  group_by(day, wday, version) %>% summarise("n_bounces" = length(n_events))

ggplot(bouncesPerDay, aes(x=paste(day, "\n", wday), y=n_bounces, fill=version)) + 
  geom_bar(stat="identity", position="dodge") 


bounceRatePerDay <- data %>% select(everything()) %>% 
  group_by(sessionId, day, wday, version) %>% summarise("n_events" = length(eventId)) %>% 
  group_by(day, wday, version) %>% 
  summarise("bounce_rate" = 100 *(length(n_events == 1)/sum(n_events)))

ggplot(bounceRatePerDay, aes(x=paste(day, "\n", wday), y=bounce_rate, fill=version)) + 
  geom_bar(stat="identity", position="dodge") + xlab("Day")

