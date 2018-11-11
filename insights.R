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

data$wday <- as.factor(as.character(data$wday))

data[categorical_cols] <- lapply(data[categorical_cols], factor)

#Filling in missing userIds for same session
data <- data %>% group_by(sessionId) %>% mutate("userId" = first(userId))

data$is_reg <- as.factor(as.integer(is.na(data$userId)))


###############################

#Number of events per session across both versions
eventsPerSessionAB <- data %>% select(everything()) %>% 
  group_by(sessionId, version, is_reg) %>% summarise("n_events" = length(eventId)) %>% 
  arrange(desc(n_events)) 


wilcox.test(n_events ~ version, data=eventsPerSessionAB)
#The number of events for both versions are not identically distributed

wilcox.test(n_events ~ is_reg, data=eventsPerSessionAB)
#The number of events for registered and non_registered sessions are not identically distributed


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

#Has a session bounced or not? 

bounced_yesno <- data %>% select(everything()) %>% 
  group_by(sessionId, version, is_reg) %>% summarise("n_events" = length(eventId)) %>% 
  mutate("bounced" = as.integer(!n_events == 1)) %>% ungroup() %>%
  select(sessionId, bounced)

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

session_duration_A <- c(timePerSessionAB[timePerSessionAB$version=="A",]$sessionId)
session_duration_B <- c(timePerSessionAB[timePerSessionAB$version=="B",]$sessionId)


shapiro.test(sample(session_duration_A, 3000))
#Cannot assume normality, hence not idea to perform t-test directly

wilcox.test(session_duration_A, session_duration_B, alternative="greater")

t.test(session_duration_A, session_duration_B, alternative="greater")


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
  
ggplot(bouncesPerPage, aes(x=page, y=prop_bounce_for_version, fill=version)) +
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=45, hjust=1))


##################################
#Proportion of non-bounces (a user not bouncing is assumed to be a success here)




nonBouncePerVersion <- data %>% select(everything()) %>% group_by(sessionId, version) %>% 
  summarise("n_events" = length(eventId)) %>%
  group_by(version) %>% 
  summarise("sessions_not_bounced" = length(sessionId[n_events > 1]), 
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
  group_by(sessionId, wday, version) %>% summarise("n_events" = length(eventId)) %>% 
  filter(n_events == 1) %>% group_by(wday, version) %>% 
  summarise("n_bounces" = length(n_events))
#bouncesPerWDay <- bouncesPerWDay[unique(bouncesPerWDay$sessionId),]

ggplot(bouncesPerWDay, aes(x=wday, y=n_bounces, fill=version)) + 
  geom_bar(stat="identity", position="dodge")


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

#Time spent per page
timeSpentPerPage <- data %>% group_by(sessionId, page, version, is_reg) %>% 
  arrange(serverTime) %>%
  summarise("time_spent_mins" = abs(as.numeric(first(serverTime) - last(serverTime),
                                               units="mins")))

mean_timeSpentPerPage <- timeSpentPerPage %>% group_by(page, version) %>% 
  summarise("mean_time_spent_mins" = mean(time_spent_mins))

ggplot(mean_timeSpentPerPage, aes(x=page, y=mean_time_spent_mins, fill=version)) +
  geom_bar(stat="identity") +theme(axis.text.x = element_text(angle=45, hjust=1))



############SESSION MASTER#############
session_data <- left_join(eventsPerSessionAB, timePerSessionAB, 
                          by=c("sessionId", "version"))

session_data <- left_join(session_data, bounced_yesno, by="sessionId")

session_data <- merge(session_data, data[,c("sessionId", "wday", "day")], by="sessionId")

session_data$bounced <- as.factor(session_data$bounced)
session_data$is_reg <- as.factor(session_data$is_reg)

library(polycor)

#hetcor(session_data[,- which(names(session_data) %in% c("sessionId", "day"))])

hetcor(session_data$bounced, session_data$n_events)
#hetcor(session_data$bounced, session_data$session_duration)

hetcor(session_data$is_reg, session_data$n_events)
hetcor(session_data$version, session_data$n_events)

#hetcor(session_data$is_reg, session_data$session_duration)

summary(aov(n_events ~ wday, data = session_data))
#There are statistically significant differences in groups of wday wrt n_events

summary(aov(session_duration ~ wday, data = session_data))
#There are statistically significant differences in groups of wday wrt session_duration


summary(aov(n_events ~ is_reg, data = session_data))
#There are statistically significant differences in groups of is_reg wrt n_events

summary(aov(session_duration ~ is_reg, data = session_data))
#There are statistically significant differences in groups of is_reg wrt session_duration


ggplot(session_data, aes(group=bounced, y=n_events)) + geom_boxplot()



