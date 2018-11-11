#Do registered and unregistered users have different behaviours? 

data_reg <- data %>% filter(!is.na(userId))

data_not_reg <- data %>% filter(is.na(userId))


#Events per session

eventsPerSession_reg <- data_reg %>% select(everything()) %>% 
  group_by(sessionId, version) %>% summarise("n_events" = length(eventId))


wilcox.test(n_events ~ version, data=eventsPerSession_reg)
#The number of events of version A and version B sessions are two different populations.
#There is some statistically significant variation between the two. 

eventsPerSession_not_reg <- data_not_reg %>% select(everything()) %>% 
  group_by(sessionId, version) %>% summarise("n_events" = length(eventId))


wilcox.test(n_events ~ version, data=eventsPerSession_not_reg)



