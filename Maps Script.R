#MAPS


#get latitude and longitude of each city in tables
require(zipcode)
data(zipcode)
colnames(zipcode) <- c("ZIP5", "CITY", "STATE", "latitude", "longitude")


#combine latitude and longitude into fatalres tables by zipcode of the city

fatal07 <- fatalres_07 %>% select(STREETNAME, CITY, STATE, ZIP5, CAUSE_IGNITION)
fatal07zip <- subset(zipcode, zip = fatal07$ZIP5)
fatal07 <- left_join(fatal07,fatal07zip)

fatal08 <- fatalres_08 %>% select(STREETNAME, CITY, STATE, ZIP5, CAUSE_IGNITION)
fatal08zip <- subset(zipcode, zip = fatal08$ZIP5)
fatal08 <- left_join(fatal08,fatal08zip)

fatal09 <- fatalres_09 %>% select(STREETNAME, CITY, STATE, ZIP5, CAUSE_IGNITION)
fatal09zip <- subset(zipcode, zip = fatal09$ZIP5)
fatal09 <- left_join(fatal09,fatal09zip)

fatal10 <- fatalres_10 %>% select(STREETNAME, CITY, STATE, ZIP5, CAUSE_IGNITION)
fatal10zip <- subset(zipcode, zip = fatal10$ZIP5)
fatal10 <- left_join(fatal10,fatal10zip)

fatal11 <- fatalres_11 %>% select(STREETNAME, CITY, STATE, ZIP5, CAUSE_IGNITION)
fatal11zip <- subset(zipcode, zip = fatal11$ZIP5)
fatal11 <- left_join(fatal11,fatal11zip)

fatal12 <- fatalres_12 %>% select(STREETNAME, CITY, STATE, ZIP5, CAUSE_IGNITION)
fatal12zip <- subset(zipcode, zip = fatal12$ZIP5)
fatal12 <- left_join(fatal12,fatal12zip)

fatal13 <- fatalres_13 %>% select(STREETNAME, CITY, STATE, ZIP5, CAUSE_IGNITION)
fatal13zip <- subset(zipcode, zip = fatal13$ZIP5)
fatal13 <- left_join(fatal13,fatal11zip)

fatal14 <- fatalres_14 %>% select(STREETNAME, CITY, STATE, ZIP5, CAUSE_IGNITION)
fatal14zip <- subset(zipcode, zip = fatal14$ZIP5)
fatal14 <- left_join(fatal14,fatal14zip)


#get the map of United States
map <- get_map(location = "United States", zoom=6, maptype="watercolor",
               source='stamen')

ggmap(map)


#draw cause of ignition in terms of city on maps

map07 <- ggmap(map) +
  geom_point(aes(x= longitude, y= latitude, color = fatal07$CAUSE_IGNITION), data = fatal07)

map08 <- ggmap(map) +
  geom_point(aes(x= longitude, y= latitude, color = fatal08$CAUSE_IGNITION), data = fatal08)

map09 <- ggmap(map) +
  geom_point(aes(x= longitude, y= latitude, color = fatal09$CAUSE_IGNITION), data = fatal09)

map10 <- ggmap(map) +
  geom_point(aes(x= longitude, y= latitude, color = fatal10$CAUSE_IGNITION), data = fatal10)

map11 <- ggmap(map) +
  geom_point(aes(x= longitude, y= latitude, color = fatal11$CAUSE_IGNITION), data = fatal11)

map12 <- ggmap(map) +
  geom_point(aes(x= longitude, y= latitude, color = fatal12$CAUSE_IGNITION), data = fatal12)

map13 <- ggmap(map) +
  geom_point(aes(x= longitude, y= latitude, color = fatal13$CAUSE_IGNITION), data = fatal13)

map14 <- ggmap(map) +
  geom_point(aes(x= longitude, y= latitude, color = fatal14$CAUSE_IGNITION), data = fatal14)