library(foreign)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(shiny)
library(data.table)


#After read through "Data ANalysis Guidelines and Issues", I find out that fireincident and basicincident is related to the topic
#read tables that has relation with “residential fires with fatalities” 

incident_07 <- read.dbf("NFIRS_2007/fireincident.dbf")
basic_07 <- read.dbf("NFIRS_2007/basicincident.dbf")
incident_08 <- read.dbf("NFIRS_2008/fireincident.dbf")
basic_08 <- read.dbf("NFIRS_2008/basicincident.dbf")
incident_09 <- read.dbf("NFIRS_2009/fireincident.dbf")
basic_09 <- read.dbf("NFIRS_2009/basicincident.dbf")
incident_10 <- read.dbf("NFIRS_2010/fireincident.dbf")
basic_10 <- read.dbf("NFIRS_2010/basicincident.dbf")
incident_11 <- read.dbf("NFIRS_2011/fireincident.dbf")
basic_11 <- read.dbf("NFIRS_2011/basicincident.dbf")
incident_12 <- fread( "NFIRS_2012/fireincident.txt", sep = "^")
basic_12 <- fread( "NFIRS_2012/basicincident.txt", sep = "^")
incident_13 <- fread( "NFIRS_2013/fireincident.txt", sep = "^")
basic_13 <- fread( "NFIRS_2013/basicincident.txt", sep = "^")
incident_14 <- fread( "NFIRS_2014/fireincident.txt", sep = "^")
basic_14 <- fread( "NFIRS_2014/basicincident.txt", sep = "^")



#from "Public Data Release Format" document I leanred that civilian death is severity = 5 in Civilian Cas. table
#read civiliancausitytable
civilian_07 <- read.dbf("NFIRS_2007/civiliancasualty.dbf")



#TABLES
#Clean Data

#since column "Not_RES" shows whether the fire incident is in resident or not
#delete data that are not in residential

incident_07 = incident_07[incident_07$NOT_RES == "N",]
incident_08 = incident_08[incident_08$NOT_RES == "N",]
incident_09 = incident_09[incident_09$NOT_RES == "N",]
incident_10 = incident_10[incident_10$NOT_RES == "N",]
incident_11 = incident_11[incident_11$NOT_RES == "N",]
incident_12 = incident_12[incident_12$NOT_RES == "N",]
incident_13 = incident_13[incident_13$NOT_RES == "N",]
incident_14 = incident_14[incident_14$NOT_RES == "N",]






#remove those data that is not fatality
basic_07 = basic_07[basic_07$FF_DEATH != 0 | basic_07$OTH_DEATH != 0, ]
basic_08 = basic_08[basic_08$FF_DEATH != 0 | basic_08$OTH_DEATH != 0, ]
basic_09 = basic_09[basic_09$FF_DEATH != 0 | basic_09$OTH_DEATH != 0, ]
basic_10 = basic_10[basic_11$FF_DEATH != 0 | basic_10$OTH_DEATH != 0, ]
basic_11 = basic_11[basic_11$FF_DEATH != 0 | basic_11$OTH_DEATH != 0, ]
basic_12 = basic_12[basic_12$FF_DEATH != 0 | basic_12$OTH_DEATH != 0, ]
basic_13 = basic_13[basic_13$FF_DEATH != 0 | basic_13$OTH_DEATH != 0, ]
basic_14 = basic_14[basic_14$FF_DEATH != 0 | basic_14$OTH_DEATH != 0, ]





#combine those tables
fatalres_07 <- inner_join(basic_07, incident_07)
fatalres_08 <- inner_join(basic_08, incident_08)
fatalres_09 <- inner_join(basic_09, incident_09)
fatalres_10 <- inner_join(basic_10, incident_10)
fatalres_11 <- inner_join(basic_11, incident_11)
fatalres_12 <- inner_join(basic_12, incident_12)
fatalres_13 <- inner_join(basic_13, incident_13)
fatalres_14 <- inner_join(basic_14, incident_14)






## Since I want to focus on cause of residential fire with fatalities,
## I think "STATE', "FDID", "INC_DATE", "AREA_ORIG", "HEAT_SOURC", "FIRST_IGN", "CAUSE_IGN" 


colnames(fatalres_07)

#keep column 1~4, 53, 55, 58

fatalres_07 <- fatalres_07[c(1:4,53:55,58)]
fatalres_08 <- fatalres_08[c(1:4,53:55,58)]
fatalres_09 <- fatalres_09[c(1:4,53:55,58)]
fatalres_10 <- fatalres_10[c(1:4,53:55,58)]
fatalres_11 <- fatalres_11[c(1:4,53:55,58)]
fatalres_12 <- fatalres_12[c(1:4,53:55,58)]
fatalres_13 <- fatalres_13[c(1:4,53:55,58)]
fatalres_14 <- fatalres_14[c(1:4,53:55,58)]



#Add specific address to the table
#read address tables
address_07 <- read.dbf("NFIRS_2007/incidentaddress.dbf")
address_08 <- read.dbf("NFIRS_2008/incidentaddress.dbf")
address_09 <- read.dbf("NFIRS_2009/incidentaddress.dbf")
address_10 <- read.dbf("NFIRS_2010/incidentaddress.dbf")
address_11 <- read.dbf("NFIRS_2011/incidentaddress.dbf")
address_12 <- fread( "NFIRS_2012/incidentaddress.txt", sep = "^")
address_13 <- read.table( "NFIRS_2013/incidentaddress.txt", sep = "^", header = TRUE)
address_14 <- fread( "NFIRS_2014/incidentaddress.txt", sep = "^")



#combine address and previous table

fatalres_07 <- inner_join(fatalres_07, address_07)
fatalres_08 <- inner_join(fatalres_08, address_08)
fatalres_09 <- inner_join(fatalres_09, address_09)
fatalres_10 <- inner_join(fatalres_10, address_10)
fatalres_11 <- inner_join(fatalres_11, address_11)
fatalres_12 <- inner_join(fatalres_12, address_12)
fatalres_13 <- inner_join(fatalres_13, address_13)
fatalres_14 <- inner_join(fatalres_14, address_14)



#decode the table
code_07 <- read.dbf("NFIRS_2007/codelookup.dbf")

#decode "original area"
code_area <- code_07[code_07$FIELDID == "AREA_ORIG",]
colnames(code_area) <- c("FIELDID", "AREA_ORIG", "ORIG_AREA")

#decode "HEAT_SOURC"
code_heat <- code_07[code_07$FIELDID == "HEAT_SOURC",]
colnames(code_heat) <- c("FIELDID1", "HEAT_SOURC", "HEAT_SOURCE")
#decoe "FIRST_IGN"
code_first <- code_07[code_07$FIELDID == "FIRST_IGN",]
colnames(code_first) <- c("FIELDID2", "FIRST_IGN", "FIRST_IGNITION")
#decode "CAUSE_IGN"
code_cause <- code_07[code_07$FIELDID == "CAUSE_IGN",]
colnames(code_cause) <- c("FIELDID3", "CAUSE_IGN", "CAUSE_IGNITION")


fatalres_07 <- left_join(fatalres_07, code_area) %>% left_join(., code_heat) %>%
  left_join(.,code_first) %>% left_join(.,code_cause)

fatalres_08 <- left_join(fatalres_08, code_area) %>% left_join(., code_heat) %>%
  left_join(., code_first) %>% left_join(.,code_cause)

fatalres_09 <- left_join(fatalres_09, code_area)%>% left_join(., code_heat) %>%
  left_join(.,code_first) %>% left_join(.,code_cause)


fatalres_10 <- left_join(fatalres_10, code_area)%>% left_join(., code_heat) %>%
  left_join(.,code_first) %>% left_join(.,code_cause)

fatalres_11 <- left_join(fatalres_11, code_area)%>% left_join(., code_heat) %>%
  left_join(.,code_first) %>% left_join(.,code_cause)

fatalres_12 <- left_join(fatalres_12, code_area)%>% left_join(., code_heat) %>%
  left_join(.,code_first) %>% left_join(.,code_cause)

fatalres_13 <- left_join(fatalres_13, code_area)%>% left_join(., code_heat) %>%
  left_join(.,code_first) %>% left_join(.,code_cause)

fatalres_14 <- left_join(fatalres_14, code_area)%>% left_join(., code_heat) %>%
  left_join(.,code_first) %>% left_join(.,code_cause)


#remove duplicated columns
colnames(fatalres_07)

fatalres_07 <- fatalres_07[,-c(5:9, 18, 20, 22, 24, 26, 28)]
fatalres_08 <- fatalres_08[,-c(5:9, 18, 20, 22, 24, 26, 28)]
fatalres_09 <- fatalres_09[,-c(5:9, 18, 20, 22, 24, 26, 28)]
fatalres_10 <- fatalres_10[,-c(5:9, 18, 20, 22, 24, 26, 28)]
fatalres_11 <- fatalres_11[,-c(5:9, 18, 20, 22, 24, 26, 28)]
fatalres_12 <- fatalres_12[,-c(5:9, 18, 20, 22, 24, 26, 28)]
fatalres_13 <- fatalres_13[,-c(5:9, 18, 20, 22, 24, 26, 28)]
fatalres_14 <- fatalres_14[,-c(5:9, 18, 20, 22, 24, 26, 28)]

#checked the data, in fatalres_08, there is a bad data, delete it
fatalres_08 <- fatalres_08[-c(107),]


#I made an shiny app in order to see those tables more clearly
# http://127.0.0.1:6356




#GRAPHS


#horizontal comparision

#Diane said that causes of fatal residential fires in the period 2011 to 2015 
#with the reported causes of fatal residential fires the period 2006 to 2010
#I want to figure out why

#make a table combine years and cause of factions

year07 = as.numeric(fatalres_07$INC_DATE)%%10^4
cause_07 <- data.table(year07,fatalres_07$CAUSE_IGNITION)
colnames(cause_07) <- c("year", "cause")

year08 = as.numeric(fatalres_08$INC_DATE)%%10^4
cause_08 <- data.table(year08,fatalres_08$CAUSE_IGNITION)
colnames(cause_08) <- c("year", "cause")


year10 = as.numeric(fatalres_10$INC_DATE)%%10^4
cause_10 <- data.table(year10,fatalres_10$CAUSE_IGNITION)
colnames(cause_10) <- c("year", "cause")

year11 = as.numeric(fatalres_11$INC_DATE)%%10^4
cause_11 <- data.table(year11,fatalres_11$CAUSE_IGNITION)
colnames(cause_11) <- c("year", "cause")

year12 = as.numeric(fatalres_12$INC_DATE)%%10^4
cause_12 <- data.table(year12,fatalres_12$CAUSE_IGNITION)
colnames(cause_12) <- c("year", "cause")

year13 = as.numeric(fatalres_13$INC_DATE)%%10^4
cause_13 <- data.table(year13,fatalres_13$CAUSE_IGNITION)
colnames(cause_13) <- c("year", "cause")

year14 = as.numeric(fatalres_14$INC_DATE)%%10^4
cause_14 <- data.table(year14,fatalres_14$CAUSE_IGNITION)
colnames(cause_14) <- c("year", "cause")

m1 <- full_join(cause_07, cause_08)%>%full_join(cause_10)%>%full_join(cause_09)%>%
  full_join(cause_11)%>%full_join(cause_12)%>%full_join(cause_13)%>%full_join(cause_14)

#bar graph of 
cause <- ggplot(m1, aes(x =m1$year)) +
  geom_bar(aes(fill = m1$cause), stat = "count")+
  geom_text(stat = 'count', aes(label=..count..),vjust=-1)+
  ggtitle("Cause of Residential Fire Incidents with Fatalities")
cause


#Analyze of barplots:

#From barplot of Cause of Residential Fire Incidents with Fatalities, we can see that the percentage of cause is similiar from 2007 to 2014.
#Diane finds out that the cuase "unknown" is increasing, I am going to have a further look of those incidents.

#table of unknown cause in fire accidents
unknown <- subset(m1, m1$cause == "Cause under investigation")
u1 <- subset(m1, m1$cause == "Cause undetermined after investigation")
unknown <- full_join(unknown, u1)
head(unknown)


#plot of unknown cause by year
uknownp <- ggplot(unknown, aes(x =unknown$year))+
  geom_text(stat = 'count', aes(label=..count..),vjust=-1)+
  geom_line( stat = "count")
  ggtitle("Unknown Cause of Residential Fire Incidents with Fatalities")
uknownp

#From the plot, we can see that year 2007 and 2013 has the lowest number of incident as cause "unknown"
#It could be explained by that in these two years, the number of fire incidents is also lower than other years.


#I decide to conduct a comparison t test to check if there is a difference between two time period


#1.make a table
prop1 <- c(523/1492, 628/1570, 650/1411, 502/1632)
prop2 <- c(688/1546, 634/1606, 581/1412, 658/1469)
t1 <- data.frame(cbind(prop1,prop2))
as.factor(t1$prop1)
as.factor(t1$prop2)
colnames(t1)<- c("prop of unknown 07-10", "prop of unknown 11-14")
t1

#comparison t test
t.test(t1$`prop of unknown 07-10`,t1$`prop of unknown 11-14`)

#RESULT

#Welch Two Sample t-test

#data:  t1$prop1 and t1$prop2
#t = -1.2738, df = 3.9105, p-value = 0.2732
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.14428248  0.05408976
#sample estimates:
#  mean of x   mean of y 
# 0.3797001    0.4247965 


#ANALYSIS
#Since the p-value = 0.2732, which is bigger than 0.05, I conclude that there si no difference between two group.
#The unknown of cause of fatal residential fires which is quite stable in ten years. Probably Diane  did not check the data nationally.






#MAPS(script in another file)



#Analyze from map:

#From the map, we can see that fire incidence with fatalities often happens in state CA, New York, Florida and Texas.
#It is understandable, because in those places, there are more people live there. Therefore, the occurance of residential fire incidence is higher than other states.

#Shiny APP http://127.0.0.1:6356


