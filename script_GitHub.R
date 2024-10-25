library(haven)
library(tidyverse)
library(ggplot2)
library(boot)
library(tidyr)

Sys.setlocale("LC_ALL","English")


#################################################################################################################################
##                                                                                                                             ##
##                    ALL CONSULTATIONS IN KUHR FOR SELECTED DIAGNOSES, BOTH GENDERS AND ALL AGES (6-19 YRS)                   ##
##                                                                                                                             ##
#################################################################################################################################

## Population counts: https://www.ssb.no/en/statbank/table/07459
## Population using API: https://data.ssb.no/api/v0/dataset/1082?lang=en

# Reading data for patients included in 2017 and follow-up in 2018 and 2019
KUHR <- read.csv("data/primaryhealthcare/kuhr/c19_antall_totalt_alle_aldre_anyment_2017.csv")
KUHR <- KUHR[c(«aar», «Week», «anyment»)]
KUHR2018 <- KUHR[which(KUHR$aar == "2017"),]
KUHR2017$RATE <- KUHR2017$anyment/(895958/100000)
KUHR2017opp01 <- KUHR[which(KUHR$aar == "2018"),] 
KUHR2017opp01$RATE <- KUHR2017opp01$anyment/(898292/100000)
KUHR2017opp02 <- KUHR[which(KUHR$aar == "2019"),] 
KUHR2017opp02$RATE <- KUHR2017opp02$anyment/(899318/100000)

# Reading data for patients included in 2018 and follow-up in 2019 and 2020
KUHR <- read.csv("data/primaryhealthcare/kuhr/c19_antall_totalt_alle_aldre_anyment_2018.csv")
KUHR <- KUHR[c(«aar», «Week», «anyment»)]
KUHR2018 <- KUHR[which(KUHR$aar == "2018"),]
KUHR2018$RATE <- KUHR2018$anyment/(898292/100000)
KUHR2018opp01 <- KUHR[which(KUHR$aar == "2019"),] 
KUHR2018opp01$RATE <- KUHR2018opp01$anyment/(899318/100000)
KUHR2018opp02 <- KUHR[which(KUHR$aar == "2020"),] 
KUHR2018opp02$RATE <- KUHR2018opp02$anyment/(897590/100000)

# Reading data for patients included in 2019 and follow-up in 2020 and 2021
KUHR <- read.csv("data/primaryhealthcare/kuhr/c19_antall_totalt_alle_aldre_anyment_2019.csv")
KUHR <- KUHR[c(«aar», «Week», «anyment»)]
KUHR2019<-KUHR[which(KUHR$aar == "2019"),]
KUHR2019$RATE <- KUHR2019$anyment/(899318/100000)
KUHR2019opp01 <- KUHR[which(KUHR$aar == "2020"),] 
KUHR2019opp01$RATE <- KUHR2019opp01$anyment/(897590/100000)
KUHR2019opp02 <- KUHR[which(KUHR$aar == "2021"),] 
KUHR2019opp02$RATE <- KUHR2019opp02$anyment/(894026/100000)

# Reading data for patients included in 2020 and follow-up in 2021 and 2022
KUHR <- read.csv("data/primaryhealthcare/kuhr/c19_antall_totalt_alle_aldre_anyment_2020.csv")
KUHR <- KUHR[c(«aar», «Week», «anyment»)]
KUHR2020 <- KUHR[which(KUHR$aar == "2020"),]
KUHR2020$RATE <- KUHR2020$anyment/(897590/100000)
KUHR2020opp01 <- KUHR[which(KUHR$aar == "2021"),] 
KUHR2020opp01$RATE <- KUHR2020opp01$anyment/(894026/100000)
KUHR2020opp02 <- KUHR[which(KUHR$aar == "2022"),] 
KUHR2020opp02$RATE <- KUHR2020opp02$anyment/(894041/100000)

# Correlation between count of observations in 2017 and each year with follow-up (2018 and 2019)
# This provides an estimate of the expected count in 2020 and 2021 (as a fraction of the observed count 
# in the inclusion year). Thus, we assume that the follow-up is similar for all patients included in a 
# specific year during the pandemic (or covering the pandemic): 2018, 2019 and 2020.

tot17 <- KUHR2017 %>% 
  group_by(Week) %>%
  summarise(total17 = sum(RATE))

tot18 <- KUHR2017opp01 %>% 
  group_by(Week) %>%
  summarise(total18 = sum(RATE))

tot19 <- KUHR2017opp02 %>% 
  group_by(Week) %>%
  summarise(total19 = sum(RATE))

totA <- merge(tot17, tot18, by = "Week")
totA <- select(totA, c(«total17», «total18»))
colnames(totA) <- c(«x», «y»)
totB <- merge(tot17, tot19, by = "Week")
totB <- select(totB, c(«total17», «total19»))
colnames(totB) <- c(«x», «y»)

ratioY01 <- boot(totA, statistic = function(totA, i) {cor(totA[i, «x»], totA[i, «y»], method = "pearson")}, R=10000)
## The correlation between Weekly counts in 2017 and 2018 based on 10000 bootstrapped draws.
ratioY01 <- ratioY01$t0

ratioY02 <- boot(totB, statistic = function(totB, i) {cor(totB[i, «x»], totB[i, «y»], method = "pearson")}, R=10000)
## The correlation between Weekly counts in 2017 and 2019 based on 10000 bootstrapped draws.
ratioY02 <- ratioY02$t0


#################################################################################################################################
##                                                                                                                             ##
##                            EXPECTED WEEKLY COUNT IN 2019 AND 2020 FOR PATIENTS INCLUDED IN 2018                             ##
##                                                                                                                             ##
#################################################################################################################################

## Patients included in 2018:
data2018 <- KUHR2018 %>% 
  group_by(Week) %>%
  summarise(Obs2018 = sum(RATE))
data2018$Pred2018<-data2018$Obs2018
data2018 <- select(data2018, c("Week","Obs2018","Pred2018"))
colnames(data2018) <- c(«Week», «Predicted», «Observed»)
data2018$aar <- 2018
data2018$aar_Week <- paste(data2018$aar, data2018$Week)
data2018$dato <- as.Date(paste(data2018$aar_Week,1), "%Y %W %u")

## For the first follow-up year (patients included in 2018): 2019
data2019 <- KUHR2018 %>% 
  group_by(Week) %>%
  summarise(Obs2018 = sum(RATE))
data2019$ratio <- ratioY01  # Average correlation 2017-2018.
data2019$Pred2019 <- data2019$Obs2018*data2019$ratio
tot19 <- KUHR2018opp01 %>% 
  group_by(Week) %>%
  summarise(Obs2019 = sum(RATE))
data2019 <- merge(data2019, tot19, by = "Week")
data2019 <- select(data2019, c(«Week»,»Pred2019», «Obs2019»))
colnames(data2019) <- c(«Week», «Predicted», «Observed»)
data2019$aar <- 2019
data2019$aar_Week <- paste(data2019$aar, data2019$Week)
data2019$dato <- as.Date(paste(data2019$aar_Week,1), "%Y %W %u")

## For the second follow-up year (patients included in 2018): 2020
data2020 <- KUHR2018 %>% 
  group_by(Week) %>%
  summarise(Obs2018 = sum(RATE))
data2020$ratio <- ratioY02  # Average correlation 2017-2019.
data2020$Pred2020 <- data2020$Obs2018*data2020$ratio
tot20 <- KUHR2018opp02 %>% 
  group_by(Week) %>%
  summarise(Obs2020 = sum(RATE))
data2020 <- merge(data2020, tot20, by = "Week")
data2020 <- select(data2020, c(«Week», «Pred2020», «Obs2020»))
colnames(data2020) <- c(«Week», «Predicted», «Observed»)
data2020$aar <- 2020
data2020$aar_Week <- paste(data2020$aar, data2020$Week)
data2020$dato <- as.Date(paste(data2020$aar_Week,1), "%Y %W %u")

## MERGING THE FILES TO VISUALIZE THE TIME SERIES (OBSERVERED AND EXPECTED):
analysetotal <- rbind(data2019,data2020)

# Figure for all consultations with KUHR.
# Periods with national restrictions in Norway during 2020-2021 is included as shaded areas.
# Individual observations per Week is turned off.

## FIGURE
figure_total_2019_2020 <- ggplot(data.frame(analysetotal), aes(dato)) + 
  geom_rect(data = analysetotal, aes(xmin = as.Date("2020-03-12"), xmax = as.Date("2020-05-11"),ymin = 0, ymax =400),alpha=0.1,fill="#D6EAF8") +
  geom_rect(data = analysetotal, aes(xmin = as.Date("2020-10-26"), xmax = as.Date("2020-12-31"),ymin = 0, ymax =400),alpha=0.1,fill="#D6EAF8") +
  geom_smooth(aes(y =Predicted), method = "loess", level = 0.99999, span = 0.17, color = "deepskyblue4", linetype = 2) +
  geom_smooth(aes(y = Observed), method = "loess", level = 0.99999, span = 0.17, color = "red", se = FALSE, linetype = 1) +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = 4, colour = "black") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
  ggtitle("Any mental disorder \n Total population (6-19 years)") +
  xlab("Year 2019-2020") +
  ylab("Rate per 100 000") +
  coord_cartesian(xlim = c(as.Date("2019-02-01"), as.Date("2020-11-30"))) +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=90))

ggsave(figure_total_2019_2020, file = "figures/figure_total_2019_2020.png", bg = 'white', type = "cairo", width = 30, height = 12, units = "cm", dpi = 1200)


#################################################################################################################################
##                                                                                                                             ##
##                            EXPECTED WEEKLY COUNT IN 2020 AND 2021 FOR PATIENTS INCLUDED IN 2019                             ##
##                                                                                                                             ##
#################################################################################################################################

## Patients included in 2019:
data2019 <- KUHR2019 %>% 
  group_by(Week) %>%
  summarise(Obs2019 = sum(RATE))
data2019$Pred2019<-data2019$Obs2019
data2019 <- select(data2019, c("Week","Obs2019","Pred2019"))
colnames(data2019) <- c(«Week», «Predicted», «Observed»)
data2019$aar <- 2019
data2019$aar_Week <- paste(data2019$aar, data2019$Week)
data2019$dato <- as.Date(paste(data2019$aar_Week,1), "%Y %W %u")

## For the first follow-up year (patients included in 2019): 2020
data2020 <- KUHR2019 %>% 
  group_by(Week) %>%
  summarise(Obs2019 = sum(RATE))
data2020$ratio <- ratioY01  # Average correlation 2017-2018.
data2020$Pred2020 <- data2020$Obs2019*data2020$ratio
tot20 <- KUHR2019opp01 %>% 
  group_by(Week) %>%
  summarise(Obs2020 = sum(RATE))
data2020 <- merge(data2020, tot20, by = "Week")
data2020 <- select(data2020, c(«Week», «Pred2020», «Obs2020»))
colnames(data2020) <- c(«Week», «Predicted», «Observed»)
data2020$aar <- 2020
data2020$aar_Week <- paste(data2020$aar, data2020$Week)
data2020$dato <- as.Date(paste(data2020$aar_Week,1), "%Y %W %u")

## For the second follow-up year (patients included in 2019): 2021
data2021 <- KUHR2019 %>% 
  group_by(Week) %>%
  summarise(Obs2019 = sum(RATE))
data2021$ratio <- ratioY02  # Average correlation 2017-2019.
data2021$Pred2021 <- data2021$Obs2019*data2021$ratio
tot21 <- KUHR2019opp02 %>% 
  group_by(Week) %>%
  summarise(Obs2021 = sum(RATE))
data2021 <- merge(data2021, tot21, by = "Week")
data2021 <- select(data2021, c("Week","Pred2021","Obs2021"))
colnames(data2021) <- c(«Week», «Predicted», «Observed»)
data2021$aar <- 2021
data2021$aar_Week <- paste(data2021$aar, data2021$Week)
data2021$dato <- as.Date(paste(data2021$aar_Week,1), "%Y %W %u")

## MERGING THE FILES TO VISUALIZE THE TIME SERIES (OBSERVERED AND EXPECTED):
analysetotal <- rbind(data2020,data2021)

# Figure for all consultations with KUHR.
# Periods with national restrictions in Norway during 2020-2021 is included as shaded areas.
# Individual observations per Week is turned off.

## FIGURE
figure_total_2020_2021 <- ggplot(data.frame(analysetotal), aes(dato)) + 
  geom_rect(data = analysetotal, aes(xmin = as.Date("2020-03-12"), xmax = as.Date("2020-05-11"),ymin = 0, ymax =400),alpha=0.1,fill="#D6EAF8") +
  geom_rect(data = analysetotal, aes(xmin = as.Date("2020-10-26"), xmax = as.Date("2021-05-27"),ymin = 0, ymax =400),alpha=0.1,fill="#D6EAF8") +
  geom_rect(data = analysetotal, aes(xmin = as.Date("2021-11-26"), xmax = as.Date("2022-02-01"),ymin = 0, ymax =400),alpha=0.1,fill="#D6EAF8") +
  geom_smooth(aes(y =Predicted), method = "loess", level = 0.99999, span = 0.17, color = "deepskyblue4", linetype = 2) +
  geom_smooth(aes(y = Observed), method = "loess", level = 0.99999, span = 0.17, color = "red", se = FALSE, linetype = 1) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype = 4, colour = "black") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
  ggtitle("Any mental disorder \n Total population (6-19 years)") +
  xlab("Year 2020-2021") +
  ylab("Rate per 100 000") +
  coord_cartesian(xlim = c(as.Date("2020-02-01"), as.Date("2021-11-30"))) +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=90))

ggsave(figure_total_2020_2021, file = "figures/figure_total_2020_2021.png", bg = 'white', type = "cairo", width = 30, height = 12, units = "cm", dpi = 1200)


#################################################################################################################################
##                                                                                                                             ##
##                            EXPECTED WEEKLY COUNT IN 2021 AND 2022 FOR PATIENTS INCLUDED IN 2020                             ##
##                                                                                                                             ##
#################################################################################################################################

## Patients included in 2020:
data2020 <- KUHR2020 %>% 
  group_by(Week) %>%
  summarise(Obs2020 = sum(RATE))
data2020$Pred2020<-data2020$Obs2020
data2020 <- select(data2020, c("Week","Obs2020","Pred2020"))
colnames(data2020) <- c(«Week», «Predicted», «Observed»)
data2020$aar <- 2020
data2020$aar_Week <- paste(data2020$aar, data2020$Week)
data2020$dato <- as.Date(paste(data2020$aar_Week,1), "%Y %W %u")

## For the first follow-up year (patients included in 2020): 2021
data2021 <- KUHR2020 %>% 
  group_by(Week) %>%
  summarise(Obs2020 = sum(RATE))
data2021$ratio <- ratioY01  # Average correlation 2017-2018.
data2021$Pred2021 <- data2021$Obs2020*data2021$ratio
tot21 <- KUHR2020opp01 %>% 
  group_by(Week) %>%
  summarise(Obs2021 = sum(RATE))
data2021 <- merge(data2021, tot21, by = "Week")
data2021 <- select(data2021, c("Week","Pred2021","Obs2021"))
colnames(data2021) <- c(«Week», «Predicted», «Observed»)
data2021$aar <- 2021
data2021$aar_Week <- paste(data2021$aar, data2020$Week)
data2021$dato <- as.Date(paste(data2021$aar_Week,1), "%Y %W %u")

## For the second follow-up year (patients included in 2020): 2022
data2022 <- KUHR2020 %>% 
  group_by(Week) %>%
  summarise(Obs2020 = sum(RATE))
data2022$ratio <- ratioY02  # Average correlation 2017-2019.
data2022$Pred2022 <- data2022$Obs2020*data2022$ratio
tot22 <- KUHR2020opp02 %>% 
  group_by(Week) %>%
  summarise(Obs2022 = sum(RATE))
data2022 <- merge(data2022, tot22, by = "Week")
data2022 <- select(data2022, c("Week","Pred2022","Obs2022"))
colnames(data2022) <- c(«Week», «Predicted», «Observed»)
data2022$aar <- 2022
data2022$aar_Week <- paste(data2022$aar, data2022$Week)
data2022$dato <- as.Date(paste(data2022$aar_Week,1), "%Y %W %u")

## MERGING THE FILES TO VISUALIZE THE TIME SERIES (OBSERVERED AND EXPECTED):
analysetotal <- rbind(data2021,data2022)

# Figure for all consultations with KUHR.
# Periods with national restrictions in Norway during 2020-2021 is included as shaded areas.
# Individual observations per Week is turned off.

## FIGURE
figure_total_2021_2022 <- ggplot(data.frame(analysetotal), aes(dato)) + 
  geom_rect(data = analysetotal, aes(xmin = as.Date("2020-10-26"), xmax = as.Date("2021-05-27"),ymin = 0, ymax =400),alpha=0.1,fill="#D6EAF8") +
  geom_rect(data = analysetotal, aes(xmin = as.Date("2021-11-26"), xmax = as.Date("2022-02-01"),ymin = 0, ymax =400),alpha=0.1,fill="#D6EAF8") +
  geom_smooth(aes(y =Predicted), method = "loess", level = 0.99999, span = 0.17, color = "deepskyblue4", linetype = 2) +
  geom_smooth(aes(y = Observed), method = "loess", level = 0.99999, span = 0.17, color = "red", se = FALSE, linetype = 1) +
  geom_vline(xintercept = as.Date("2022-01-01"), linetype = 4, colour = "black") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
  ggtitle("Any mental disorder \n Total population (6-19 years)") +
  xlab("Year 2021-2022") +
  ylab("Rate per 100 000") +
  coord_cartesian(xlim = c(as.Date("2021-02-01"), as.Date("2022-11-30"))) +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=90))

ggsave(figure_total_2021_2022, file = "figures/figure_total_2021_2022.png", bg = 'white', type = "cairo", width = 30, height = 12, units = "cm", dpi = 1200)


#################################################################################################################################
##                                                                                                                             ##
##                          ALL CONSULTATIONS IN KUHR FOR SELECTED DIAGNOSES, GIRLS ALL AGES (6-19 YRS)                        ##
##                                                                                                                             ##
#################################################################################################################################

# Reading data for patients included in 2017 and follow-up in 2018 and 2019
KUHR <- read.csv("data/primaryhealthcare/kuhr/c19_antall_kvinner_alle_aldre_anyment_2017.csv")
KUHR <- KUHR[c(«aar», «Week», «anyment»)]
KUHR2018 <- KUHR[which(KUHR$aar == "2017"),]
KUHR2017$RATE <- KUHR2017$anyment/(435628/100000)
KUHR2017opp01 <- KUHR[which(KUHR$aar == "2018"),] 
KUHR2017opp01$RATE <- KUHR2017opp01$anyment/(436670/100000)
KUHR2017opp02 <- KUHR[which(KUHR$aar == "2019"),] 
KUHR2017opp02$RATE <- KUHR2017opp02$anyment/(437251/100000)

# Reading data for patients included in 2018 and follow-up in 2019 and 2020
KUHR <- read.csv("data/primaryhealthcare/kuhr/c19_antall_kvinner_alle_aldre_anyment_2018.csv")
KUHR <- KUHR[c(«aar», «Week», «anyment»)]
KUHR2018 <- KUHR[which(KUHR$aar == "2018"),]
KUHR2018$RATE <- KUHR2018$anyment/(436670/100000)
KUHR2018opp01 <- KUHR[which(KUHR$aar == "2019"),] 
KUHR2018opp01$RATE <- KUHR2018opp01$anyment/(437251/100000)
KUHR2018opp02 <- KUHR[which(KUHR$aar == "2020"),] 
KUHR2018opp02$RATE <- KUHR2018opp02$anyment/(437080/100000)

# Reading data for patients included in 2019 and follow-up in 2020 and 2021
KUHR <- read.csv("data/primaryhealthcare/kuhr/c19_antall_kvinner_alle_aldre_anyment_2019.csv")
KUHR <- KUHR[c(«aar», «Week», «anyment»)]
KUHR2019<-KUHR[which(KUHR$aar == "2019"),]
KUHR2019$RATE <- KUHR2019$anyment/(437251/100000)
KUHR2019opp01 <- KUHR[which(KUHR$aar == "2020"),] 
KUHR2019opp01$RATE <- KUHR2019opp01$anyment/(437080/100000)
KUHR2019opp02 <- KUHR[which(KUHR$aar == "2021"),] 
KUHR2019opp02$RATE <- KUHR2019opp02$anyment/(435618/100000)


# Reading data for patients included in 2020 and follow-up in 2021 and 2022
KUHR <- read.csv("data/csv-nordic-080923/kuhr/c19_antall_kvinner_alle_aldre_anyment_2020.csv")
KUHR <- KUHR[c(«aar», «Week», «anyment»)]
KUHR2020 <- KUHR[which(KUHR$aar == "2020"),]
KUHR2020$RATE <- KUHR2020$anyment/(437080/100000)
KUHR2020opp01 <- KUHR[which(KUHR$aar == "2021"),] 
KUHR2020opp01$RATE <- KUHR2020opp01$anyment/(435618/100000)
KUHR2020opp02 <- KUHR[which(KUHR$aar == "2022"),] 
KUHR2020opp02$RATE <- KUHR2020opp02$anyment/(435577/100000)


# Correlation between count of observations in 2017 and each year with follow-up (2018 and 2019)
# This provides an estimate of the expected count in 2020 and 2021 (as a fraction of the observed count 
# in the inclusion year). Thus, we assume that the follow-up is similar for all patients included in a 
# specific year during the pandemic (or covering the pandemic): 2018, 2019 and 2020.

tot17 <- KUHR2017 %>% 
  group_by(Week) %>%
  summarise(total17 = sum(RATE))

tot18 <- KUHR2017opp01 %>% 
  group_by(Week) %>%
  summarise(total18 = sum(RATE))

tot19 <- KUHR2017opp02 %>% 
  group_by(Week) %>%
  summarise(total19 = sum(RATE))

totA <- merge(tot17, tot18, by = "Week")
totA <- select(totA, c(«total17», «total18»))
colnames(totA) <- c(«x», «y»)
totB <- merge(tot17, tot19, by = "Week")
totB <- select(totB, c(«total17», «total19»))
colnames(totB) <- c(«x», «y»)

ratioY01 <- boot(totA, statistic = function(totA, i) {cor(totA[i, «x»], totA[i, «y»], method = "pearson")}, R=10000)
## The correlation between Weekly counts in 2017 and 2018 based on 10000 bootstrapped draws.
ratioY01 <- ratioY01$t0

ratioY02 <- boot(totB, statistic = function(totB, i) {cor(totB[i, «x»], totB[i, «y»], method = "pearson")}, R=10000)
## The correlation between Weekly counts in 2017 and 2019 based on 10000 bootstrapped draws.
ratioY02 <- ratioY02$t0


#################################################################################################################################
##                                                                                                                             ##
##                            EXPECTED WEEKLY COUNT IN 2019 AND 2020 FOR PATIENTS INCLUDED IN 2018                             ##
##                                                                                                                             ##
#################################################################################################################################

## Patients included in 2018:
data2018 <- KUHR2018 %>% 
  group_by(Week) %>%
  summarise(Obs2018 = sum(RATE))
data2018$Pred2018<-data2018$Obs2018
data2018 <- select(data2018, c("Week","Obs2018","Pred2018"))
colnames(data2018) <- c(«Week», «Predicted», «Observed»)
data2018$aar <- 2018
data2018$aar_Week <- paste(data2018$aar, data2018$Week)
data2018$dato <- as.Date(paste(data2018$aar_Week,1), "%Y %W %u")

## For the first follow-up year (patients included in 2018): 2019
data2019 <- KUHR2018 %>% 
  group_by(Week) %>%
  summarise(Obs2018 = sum(RATE))
data2019$ratio <- ratioY01  # Average correlation 2017-2018.
data2019$Pred2019 <- data2019$Obs2018*data2019$ratio
tot19 <- KUHR2018opp01 %>% 
  group_by(Week) %>%
  summarise(Obs2019 = sum(RATE))
data2019 <- merge(data2019, tot19, by = "Week")
data2019 <- select(data2019, c(«Week»,»Pred2019», «Obs2019»))
colnames(data2019) <- c(«Week», «Predicted», «Observed»)
data2019$aar <- 2019
data2019$aar_Week <- paste(data2019$aar, data2019$Week)
data2019$dato <- as.Date(paste(data2019$aar_Week,1), "%Y %W %u")

## For the second follow-up year (patients included in 2018): 2020
data2020 <- KUHR2018 %>% 
  group_by(Week) %>%
  summarise(Obs2018 = sum(RATE))
data2020$ratio <- ratioY02  # Average correlation 2017-2019.
data2020$Pred2020 <- data2020$Obs2018*data2020$ratio
tot20 <- KUHR2018opp02 %>% 
  group_by(Week) %>%
  summarise(Obs2020 = sum(RATE))
data2020 <- merge(data2020, tot20, by = "Week")
data2020 <- select(data2020, c(«Week», «Pred2020», «Obs2020»))
colnames(data2020) <- c(«Week», «Predicted», «Observed»)
data2020$aar <- 2020
data2020$aar_Week <- paste(data2020$aar, data2020$Week)
data2020$dato <- as.Date(paste(data2020$aar_Week,1), "%Y %W %u")

## MERGING THE FILES TO VISUALIZE THE TIME SERIES (OBSERVERED AND EXPECTED):
analysegirls_2019_2020 <- rbind(data2019,data2020)

# Figure for all consultations with KUHR.
# Periods with national restrictions in Norway during 2020-2021 is included as shaded areas.
# Individual observations per Week is turned off.

## FIGURE
figure_girls_2019_2020 <- ggplot(data.frame(analysegirls_2019_2020), aes(dato)) + 
  geom_rect(data = analysegirls_2019_2020, aes(xmin = as.Date("2020-03-12"), xmax = as.Date("2020-05-11"),ymin = 0, ymax =450),alpha=0.1,fill="#D6EAF8") +
  geom_rect(data = analysegirls_2019_2020, aes(xmin = as.Date("2020-10-26"), xmax = as.Date("2020-12-31"),ymin = 0, ymax =450),alpha=0.1,fill="#D6EAF8") +
  geom_smooth(aes(y =Predicted), method = "loess", level = 0.99999, span = 0.17, color = "deepskyblue4", linetype = 2) +
  geom_smooth(aes(y = Observed), method = "loess", level = 0.99999, span = 0.17, color = "red", se = FALSE, linetype = 1) +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = 4, colour = "black") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
  ggtitle("Any mental disorder \n Girls (6-19 years)") +
  xlab("Year 2019-2020") +
  ylab("Rate per 100 000") +
  coord_cartesian(xlim = c(as.Date("2019-02-01"), as.Date("2020-11-30"))) +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=90))

ggsave(figure_girls_2019_2020, file = "figures/figure_girls_2019_2020.png", bg = 'white', type = "cairo", width = 30, height = 12, units = "cm", dpi = 1200)


#################################################################################################################################
##                                                                                                                             ##
##                            EXPECTED WEEKLY COUNT IN 2020 AND 2021 FOR PATIENTS INCLUDED IN 2019                             ##
##                                                                                                                             ##
#################################################################################################################################

## Patients included in 2019:
data2019 <- KUHR2019 %>% 
  group_by(Week) %>%
  summarise(Obs2019 = sum(RATE))
data2019$Pred2019<-data2019$Obs2019
data2019 <- select(data2019, c("Week","Obs2019","Pred2019"))
colnames(data2019) <- c(«Week», «Predicted», «Observed»)
data2019$aar <- 2019
data2019$aar_Week <- paste(data2019$aar, data2019$Week)
data2019$dato <- as.Date(paste(data2019$aar_Week,1), "%Y %W %u")

## For the first follow-up year (patients included in 2019): 2020
data2020 <- KUHR2019 %>% 
  group_by(Week) %>%
  summarise(Obs2019 = sum(RATE))
data2020$ratio <- ratioY01  # Average correlation 2017-2018.
data2020$Pred2020 <- data2020$Obs2019*data2020$ratio
tot20 <- KUHR2019opp01 %>% 
  group_by(Week) %>%
  summarise(Obs2020 = sum(RATE))
data2020 <- merge(data2020, tot20, by = "Week")
data2020 <- select(data2020, c(«Week», «Pred2020», «Obs2020»))
colnames(data2020) <- c(«Week», «Predicted», «Observed»)
data2020$aar <- 2020
data2020$aar_Week <- paste(data2020$aar, data2020$Week)
data2020$dato <- as.Date(paste(data2020$aar_Week,1), "%Y %W %u")

## For the second follow-up year (patients included in 2019): 2021
data2021 <- KUHR2019 %>% 
  group_by(Week) %>%
  summarise(Obs2019 = sum(RATE))
data2021$ratio <- ratioY02  # Average correlation 2017-2019.
data2021$Pred2021 <- data2021$Obs2019*data2021$ratio
tot21 <- KUHR2019opp02 %>% 
  group_by(Week) %>%
  summarise(Obs2021 = sum(RATE))
data2021 <- merge(data2021, tot21, by = "Week")
data2021 <- select(data2021, c("Week","Pred2021","Obs2021"))
colnames(data2021) <- c(«Week», «Predicted», «Observed»)
data2021$aar <- 2021
data2021$aar_Week <- paste(data2021$aar, data2021$Week)
data2021$dato <- as.Date(paste(data2021$aar_Week,1), "%Y %W %u")

## MERGING THE FILES TO VISUALIZE THE TIME SERIES (OBSERVERED AND EXPECTED):
analysegirls_2020_2021 <- rbind(data2020,data2021)

# Figure for all consultations with KUHR.
# Periods with national restrictions in Norway during 2020-2021 is included as shaded areas.
# Individual observations per Week is turned off.

## FIGURE
figure_girls_2020_2021 <- ggplot(data.frame(analysetotal), aes(dato)) + 
  geom_rect(data = analysegirls_2020_2021, aes(xmin = as.Date("2020-03-12"), xmax = as.Date("2020-05-11"),ymin = 0, ymax =450),alpha=0.1,fill="#D6EAF8") +
  geom_rect(data = analysegirls_2020_2021, aes(xmin = as.Date("2020-10-26"), xmax = as.Date("2021-05-27"),ymin = 0, ymax =450),alpha=0.1,fill="#D6EAF8") +
  geom_rect(data = analysegirls_2020_2021, aes(xmin = as.Date("2021-11-26"), xmax = as.Date("2022-02-01"),ymin = 0, ymax =450),alpha=0.1,fill="#D6EAF8") +
  geom_smooth(aes(y =Predicted), method = "loess", level = 0.99999, span = 0.17, color = "deepskyblue4", linetype = 2) +
  geom_smooth(aes(y = Observed), method = "loess", level = 0.99999, span = 0.17, color = "red", se = FALSE, linetype = 1) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype = 4, colour = "black") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
  ggtitle("Any mental disorder \n Girls (6-19 years)") +
  xlab("Year 2020-2021") +
  ylab("Rate per 100 000") +
  coord_cartesian(xlim = c(as.Date("2020-02-01"), as.Date("2021-11-30"))) +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=90))

ggsave(figure_girls_2020_2021, file = "figures/figure_girls_2020_2021.png", bg = 'white', type = "cairo", width = 30, height = 12, units = "cm", dpi = 1200)


#################################################################################################################################
##                                                                                                                             ##
##                            EXPECTED WEEKLY COUNT IN 2021 AND 2022 FOR PATIENTS INCLUDED IN 2020                             ##
##                                                                                                                             ##
#################################################################################################################################

## Patients included in 2020:
data2020 <- KUHR2020 %>% 
  group_by(Week) %>%
  summarise(Obs2020 = sum(RATE))
data2020$Pred2020<-data2020$Obs2020
data2020 <- select(data2020, c("Week","Obs2020","Pred2020"))
colnames(data2020) <- c(«Week», «Predicted», «Observed»)
data2020$aar <- 2020
data2020$aar_Week <- paste(data2020$aar, data2020$Week)
data2020$dato <- as.Date(paste(data2020$aar_Week,1), "%Y %W %u")

## For the first follow-up year (patients included in 2020): 2021
data2021 <- KUHR2020 %>% 
  group_by(Week) %>%
  summarise(Obs2020 = sum(RATE))
data2021$ratio <- ratioY01  # Average correlation 2017-2018.
data2021$Pred2021 <- data2021$Obs2020*data2021$ratio
tot21 <- KUHR2020opp01 %>% 
  group_by(Week) %>%
  summarise(Obs2021 = sum(RATE))
data2021 <- merge(data2021, tot21, by = "Week")
data2021 <- select(data2021, c("Week","Pred2021","Obs2021"))
colnames(data2021) <- c(«Week», «Predicted», «Observed»)
data2021$aar <- 2021
data2021$aar_Week <- paste(data2021$aar, data2020$Week)
data2021$dato <- as.Date(paste(data2021$aar_Week,1), "%Y %W %u")

## For the second follow-up year (patients included in 2020): 2022
data2022 <- KUHR2020 %>% 
  group_by(Week) %>%
  summarise(Obs2020 = sum(RATE))
data2022$ratio <- ratioY02  # Average correlation 2017-2019.
data2022$Pred2022 <- data2022$Obs2020*data2022$ratio
tot22 <- KUHR2020opp02 %>% 
  group_by(Week) %>%
  summarise(Obs2022 = sum(RATE))
data2022 <- merge(data2022, tot22, by = "Week")
data2022 <- select(data2022, c("Week","Pred2022","Obs2022"))
colnames(data2022) <- c(«Week», «Predicted», «Observed»)
data2022$aar <- 2022
data2022$aar_Week <- paste(data2022$aar, data2022$Week)
data2022$dato <- as.Date(paste(data2022$aar_Week,1), "%Y %W %u")

## MERGING THE FILES TO VISUALIZE THE TIME SERIES (OBSERVERED AND EXPECTED):
analysegirls_2021_2022 <- rbind(data2021,data2022)

# Figure for all consultations with KUHR.
# Periods with national restrictions in Norway during 2020-2021 is included as shaded areas.
# Individual observations per Week is turned off.

## FIGURE
figure_girls_2021_2022 <- ggplot(data.frame(analysegirls_2021_2022), aes(dato)) + 
  geom_rect(data = analysegirls_2021_2022, aes(xmin = as.Date("2020-10-26"), xmax = as.Date("2021-05-27"),ymin = 0, ymax =550),alpha=0.1,fill="#D6EAF8") +
  geom_rect(data = analysegirls_2021_2022, aes(xmin = as.Date("2021-11-26"), xmax = as.Date("2022-02-01"),ymin = 0, ymax =550),alpha=0.1,fill="#D6EAF8") +
  geom_smooth(aes(y =Predicted), method = "loess", level = 0.99999, span = 0.17, color = "deepskyblue4", linetype = 2) +
  geom_smooth(aes(y = Observed), method = "loess", level = 0.99999, span = 0.17, color = "red", se = FALSE, linetype = 1) +
  geom_vline(xintercept = as.Date("2022-01-01"), linetype = 4, colour = "black") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
  ggtitle("Any mental disorder \n Girls (6-19 years)") +
  xlab("Year 2021-2022") +
  ylab("Rate per 100 000") +
  coord_cartesian(xlim = c(as.Date("2021-02-01"), as.Date("2022-11-30"))) +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=90))

ggsave(figure_girls_2021_2022, file = "figures/figure_girls_2021_2022.png", bg = 'white', type = "cairo", width = 30, height = 12, units = "cm", dpi = 1200)


#################################################################################################################################
##                                                                                                                             ##
##                         ALL CONSULTATIONS IN KUHR FOR SELECTED DIAGNOSES, BOYS ALL AGES (6-19 YRS)                          ##
##                                                                                                                             ##
#################################################################################################################################

# Reading data for patients included in 2017 and follow-up in 2018 and 2019
KUHR <- read.csv("data/primaryhealthcare/kuhr/c19_antall_menn_alle_aldre_anyment_2017.csv")
KUHR <- KUHR[c(«aar», «Week», «anyment»)]
KUHR2018 <- KUHR[which(KUHR$aar == "2017"),]
KUHR2017$RATE <- KUHR2017$anyment/(460330/100000)
KUHR2017opp01 <- KUHR[which(KUHR$aar == "2018"),] 
KUHR2017opp01$RATE <- KUHR2017opp01$anyment/(461622/100000)
KUHR2017opp02 <- KUHR[which(KUHR$aar == "2019"),] 
KUHR2017opp02$RATE <- KUHR2017opp02$anyment/(462067/100000)

# Reading data for patients included in 2018 and follow-up in 2019 and 2020
KUHR <- read.csv("data/primaryhealthcare/kuhr/c19_antall_menn_alle_aldre_anyment_2018.csv")
KUHR <- KUHR[c(«aar», «Week», «anyment»)]
KUHR2018 <- KUHR[which(KUHR$aar == "2018"),]
KUHR2018$RATE <- KUHR2018$anyment/(461622/100000)
KUHR2018opp01 <- KUHR[which(KUHR$aar == "2019"),] 
KUHR2018opp01$RATE <- KUHR2018opp01$anyment/(462067/100000)
KUHR2018opp02 <- KUHR[which(KUHR$aar == "2020"),] 
KUHR2018opp02$RATE <- KUHR2018opp02$anyment/(460510/100000)

# Reading data for patients included in 2019 and follow-up in 2020 and 2021
KUHR <- read.csv("data/primaryhealthcare/kuhr/c19_antall_menn_alle_aldre_anyment_2019.csv")
KUHR <- KUHR[c(«aar», «Week», «anyment»)]
KUHR2019<-KUHR[which(KUHR$aar == "2019"),]
KUHR2019$RATE <- KUHR2019$anyment/(462067/100000)
KUHR2019opp01 <- KUHR[which(KUHR$aar == "2020"),] 
KUHR2019opp01$RATE <- KUHR2019opp01$anyment/(460510/100000)
KUHR2019opp02 <- KUHR[which(KUHR$aar == "2021"),] 
KUHR2019opp02$RATE <- KUHR2019opp02$anyment/(458408/100000)


# Reading data for patients included in 2020 and follow-up in 2021 and 2022
KUHR <- read.csv("data/primaryhealthcare/kuhr/c19_antall_menn_alle_aldre_anyment_2020.csv")
KUHR <- KUHR[c(«aar», «Week», «anyment»)]
KUHR2020 <- KUHR[which(KUHR$aar == "2020"),]
KUHR2020$RATE <- KUHR2020$anyment/(460510/100000)
KUHR2020opp01 <- KUHR[which(KUHR$aar == "2021"),] 
KUHR2020opp01$RATE <- KUHR2020opp01$anyment/(458408/100000)
KUHR2020opp02 <- KUHR[which(KUHR$aar == "2022"),] 
KUHR2020opp02$RATE <- KUHR2020opp02$anyment/(458464/100000)

# Correlation between count of observations in 2017 and each year with follow-up (2018 and 2019)
# This provides an estimate of the expected count in 2020 and 2021 (as a fraction of the observed count 
# in the inclusion year). Thus, we assume that the follow-up is similar for all patients included in a 
# specific year during the pandemic (or covering the pandemic): 2018, 2019 and 2020.

tot17 <- KUHR2017 %>% 
  group_by(Week) %>%
  summarise(total17 = sum(RATE))

tot18 <- KUHR2017opp01 %>% 
  group_by(Week) %>%
  summarise(total18 = sum(RATE))

tot19 <- KUHR2017opp02 %>% 
  group_by(Week) %>%
  summarise(total19 = sum(RATE))

totA <- merge(tot17, tot18, by = "Week")
totA <- select(totA, c(«total17», «total18»))
colnames(totA) <- c(«x», «y»)
totB <- merge(tot17, tot19, by = "Week")
totB <- select(totB, c(«total17», «total19»))
colnames(totB) <- c(«x», «y»)

ratioY01 <- boot(totA, statistic = function(totA, i) {cor(totA[i, «x»], totA[i, «y»], method = "pearson")}, R=10000)
## The correlation between Weekly counts in 2017 and 2018 based on 10000 bootstrapped draws.
ratioY01 <- ratioY01$t0

ratioY02 <- boot(totB, statistic = function(totB, i) {cor(totB[i, «x»], totB[i, «y»], method = "pearson")}, R=10000)
## The correlation between Weekly counts in 2017 and 2019 based on 10000 bootstrapped draws.
ratioY02 <- ratioY02$t0


#################################################################################################################################
##                                                                                                                             ##
##                            EXPECTED WEEKLY COUNT IN 2019 AND 2020 FOR PATIENTS INCLUDED IN 2018                             ##
##                                                                                                                             ##
#################################################################################################################################

## Patients included in 2018:
data2018 <- KUHR2018 %>% 
  group_by(Week) %>%
  summarise(Obs2018 = sum(RATE))
data2018$Pred2018<-data2018$Obs2018
data2018 <- select(data2018, c("Week","Obs2018","Pred2018"))
colnames(data2018) <- c(«Week», «Predicted», «Observed»)
data2018$aar <- 2018
data2018$aar_Week <- paste(data2018$aar, data2018$Week)
data2018$dato <- as.Date(paste(data2018$aar_Week,1), "%Y %W %u")

## For the first follow-up year (patients included in 2018): 2019
data2019 <- KUHR2018 %>% 
  group_by(Week) %>%
  summarise(Obs2018 = sum(RATE))
data2019$ratio <- ratioY01  # Average correlation 2017-2018.
data2019$Pred2019 <- data2019$Obs2018*data2019$ratio
tot19 <- KUHR2018opp01 %>% 
  group_by(Week) %>%
  summarise(Obs2019 = sum(RATE))
data2019 <- merge(data2019, tot19, by = "Week")
data2019 <- select(data2019, c(«Week»,»Pred2019», «Obs2019»))
colnames(data2019) <- c(«Week», «Predicted», «Observed»)
data2019$aar <- 2019
data2019$aar_Week <- paste(data2019$aar, data2019$Week)
data2019$dato <- as.Date(paste(data2019$aar_Week,1), "%Y %W %u")

## For the second follow-up year (patients included in 2018): 2020
data2020 <- KUHR2018 %>% 
  group_by(Week) %>%
  summarise(Obs2018 = sum(RATE))
data2020$ratio <- ratioY02  # Average correlation 2017-2019.
data2020$Pred2020 <- data2020$Obs2018*data2020$ratio
tot20 <- KUHR2018opp02 %>% 
  group_by(Week) %>%
  summarise(Obs2020 = sum(RATE))
data2020 <- merge(data2020, tot20, by = "Week")
data2020 <- select(data2020, c(«Week», «Pred2020», «Obs2020»))
colnames(data2020) <- c(«Week», «Predicted», «Observed»)
data2020$aar <- 2020
data2020$aar_Week <- paste(data2020$aar, data2020$Week)
data2020$dato <- as.Date(paste(data2020$aar_Week,1), "%Y %W %u")

## MERGING THE FILES TO VISUALIZE THE TIME SERIES (OBSERVERED AND EXPECTED):
analyseboys_2019_2020 <- rbind(data2019,data2020)

# Figure for all consultations with KUHR.
# Periods with national restrictions in Norway during 2020-2021 is included as shaded areas.
# Individual observations per Week is turned off.

## FIGURE
figure_boys_2019_2020 <- ggplot(data.frame(analyseboys_2019_2020), aes(dato)) + 
  geom_rect(data = analyseboys_2019_2020, aes(xmin = as.Date("2020-03-12"), xmax = as.Date("2020-05-11"),ymin = 0, ymax =400),alpha=0.1,fill="#D6EAF8") +
  geom_rect(data = analyseboys_2019_2020, aes(xmin = as.Date("2020-10-26"), xmax = as.Date("2020-12-31"),ymin = 0, ymax =400),alpha=0.1,fill="#D6EAF8") +
  geom_smooth(aes(y =Predicted), method = "loess", level = 0.99999, span = 0.17, color = "deepskyblue4", linetype = 2) +
  geom_smooth(aes(y = Observed), method = "loess", level = 0.99999, span = 0.17, color = "red", se = FALSE, linetype = 1) +
  geom_vline(xintercept = as.Date("2020-01-01"), linetype = 4, colour = "black") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
  ggtitle("Any mental disorder \n Boys (6-19 years)") +
  xlab("Year 2019-2020") +
  ylab("Rate per 100 000") +
  coord_cartesian(xlim = c(as.Date("2019-02-01"), as.Date("2020-11-30"))) +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=90))

ggsave(figure_boys_2019_2020, file = "figures/figure_boys_2019_2020.png", bg = 'white', type = "cairo", width = 30, height = 12, units = "cm", dpi = 1200)


#################################################################################################################################
##                                                                                                                             ##
##                            EXPECTED WEEKLY COUNT IN 2020 AND 2021 FOR PATIENTS INCLUDED IN 2019                             ##
##                                                                                                                             ##
#################################################################################################################################

## Patients included in 2019:
data2019 <- KUHR2019 %>% 
  group_by(Week) %>%
  summarise(Obs2019 = sum(RATE))
data2019$Pred2019<-data2019$Obs2019
data2019 <- select(data2019, c("Week","Obs2019","Pred2019"))
colnames(data2019) <- c(«Week», «Predicted», «Observed»)
data2019$aar <- 2019
data2019$aar_Week <- paste(data2019$aar, data2019$Week)
data2019$dato <- as.Date(paste(data2019$aar_Week,1), "%Y %W %u")

## For the first follow-up year (patients included in 2019): 2020
data2020 <- KUHR2019 %>% 
  group_by(Week) %>%
  summarise(Obs2019 = sum(RATE))
data2020$ratio <- ratioY01  # Average correlation 2017-2018.
data2020$Pred2020 <- data2020$Obs2019*data2020$ratio
tot20 <- KUHR2019opp01 %>% 
  group_by(Week) %>%
  summarise(Obs2020 = sum(RATE))
data2020 <- merge(data2020, tot20, by = "Week")
data2020 <- select(data2020, c(«Week», «Pred2020», «Obs2020»))
colnames(data2020) <- c(«Week», «Predicted», «Observed»)
data2020$aar <- 2020
data2020$aar_Week <- paste(data2020$aar, data2020$Week)
data2020$dato <- as.Date(paste(data2020$aar_Week,1), "%Y %W %u")

## For the second follow-up year (patients included in 2019): 2021
data2021 <- KUHR2019 %>% 
  group_by(Week) %>%
  summarise(Obs2019 = sum(RATE))
data2021$ratio <- ratioY02  # Average correlation 2017-2019.
data2021$Pred2021 <- data2021$Obs2019*data2021$ratio
tot21 <- KUHR2019opp02 %>% 
  group_by(Week) %>%
  summarise(Obs2021 = sum(RATE))
data2021 <- merge(data2021, tot21, by = "Week")
data2021 <- select(data2021, c("Week","Pred2021","Obs2021"))
colnames(data2021) <- c(«Week», «Predicted», «Observed»)
data2021$aar <- 2021
data2021$aar_Week <- paste(data2021$aar, data2021$Week)
data2021$dato <- as.Date(paste(data2021$aar_Week,1), "%Y %W %u")

## MERGING THE FILES TO VISUALIZE THE TIME SERIES (OBSERVERED AND EXPECTED):
analyseboys_2020_2021 <- rbind(data2020,data2021)

# Figure for all consultations with KUHR.
# Periods with national restrictions in Norway during 2020-2021 is included as shaded areas.
# Individual observations per Week is turned off.

## FIGURE
figure_boys_2020_2021 <- ggplot(data.frame(analyseboys_2020_2021), aes(dato)) + 
  geom_rect(data = analyseboys_2020_2021, aes(xmin = as.Date("2020-03-12"), xmax = as.Date("2020-05-11"),ymin = 0, ymax =400),alpha=0.1,fill="#D6EAF8") +
  geom_rect(data = analyseboys_2020_2021, aes(xmin = as.Date("2020-10-26"), xmax = as.Date("2021-05-27"),ymin = 0, ymax =400),alpha=0.1,fill="#D6EAF8") +
  geom_rect(data = analyseboys_2020_2021, aes(xmin = as.Date("2021-11-26"), xmax = as.Date("2022-02-01"),ymin = 0, ymax =400),alpha=0.1,fill="#D6EAF8") +
  geom_smooth(aes(y =Predicted), method = "loess", level = 0.99999, span = 0.17, color = "deepskyblue4", linetype = 2) +
  geom_smooth(aes(y = Observed), method = "loess", level = 0.99999, span = 0.17, color = "red", se = FALSE, linetype = 1) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype = 4, colour = "black") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
  ggtitle("Any mental disorder \n Boys (6-19 years)") +
  xlab("Year 2020-2021") +
  ylab("Rate per 100 000") +
  coord_cartesian(xlim = c(as.Date("2020-02-01"), as.Date("2021-11-30"))) +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=90))

ggsave(figure_boys_2020_2021, file = "figures/figure_boys_2020_2021.png", bg = 'white', type = "cairo", width = 30, height = 12, units = "cm", dpi = 1200)


#################################################################################################################################
##                                                                                                                             ##
##                            EXPECTED WEEKLY COUNT IN 2021 AND 2022 FOR PATIENTS INCLUDED IN 2020                             ##
##                                                                                                                             ##
#################################################################################################################################

## Patients included in 2020:
data2020 <- KUHR2020 %>% 
  group_by(Week) %>%
  summarise(Obs2020 = sum(RATE))
data2020$Pred2020<-data2020$Obs2020
data2020 <- select(data2020, c("Week","Obs2020","Pred2020"))
colnames(data2020) <- c(«Week», «Predicted», «Observed»)
data2020$aar <- 2020
data2020$aar_Week <- paste(data2020$aar, data2020$Week)
data2020$dato <- as.Date(paste(data2020$aar_Week,1), "%Y %W %u")

## For the first follow-up year (patients included in 2020): 2021
data2021 <- KUHR2020 %>% 
  group_by(Week) %>%
  summarise(Obs2020 = sum(RATE))
data2021$ratio <- ratioY01  # Average correlation 2017-2018.
data2021$Pred2021 <- data2021$Obs2020*data2021$ratio
tot21 <- KUHR2020opp01 %>% 
  group_by(Week) %>%
  summarise(Obs2021 = sum(RATE))
data2021 <- merge(data2021, tot21, by = "Week")
data2021 <- select(data2021, c("Week","Pred2021","Obs2021"))
colnames(data2021) <- c(«Week», «Predicted», «Observed»)
data2021$aar <- 2021
data2021$aar_Week <- paste(data2021$aar, data2020$Week)
data2021$dato <- as.Date(paste(data2021$aar_Week,1), "%Y %W %u")

## For the second follow-up year (patients included in 2020): 2022
data2022 <- KUHR2020 %>% 
  group_by(Week) %>%
  summarise(Obs2020 = sum(RATE))
data2022$ratio <- ratioY02  # Average correlation 2017-2019.
data2022$Pred2022 <- data2022$Obs2020*data2022$ratio
tot22 <- KUHR2020opp02 %>% 
  group_by(Week) %>%
  summarise(Obs2022 = sum(RATE))
data2022 <- merge(data2022, tot22, by = "Week")
data2022 <- select(data2022, c("Week","Pred2022","Obs2022"))
colnames(data2022) <- c(«Week», «Predicted», «Observed»)
data2022$aar <- 2022
data2022$aar_Week <- paste(data2022$aar, data2022$Week)
data2022$dato <- as.Date(paste(data2022$aar_Week,1), "%Y %W %u")


## MERGING THE FILES TO VISUALIZE THE TIME SERIES (OBSERVERED AND EXPECTED):
analyseboys_2021_2022 <- rbind(data2021,data2022)

# Figure for all consultations with KUHR.
# Periods with national restrictions in Norway during 2020-2021 is included as shaded areas.
# Individual observations per Week is turned off.

## FIGURE
figure_boys_2021_2022 <- ggplot(data.frame(analyseboys_2021_2022), aes(dato)) + 
  geom_rect(data = analyseboys_2021_2022, aes(xmin = as.Date("2020-10-26"), xmax = as.Date("2021-05-27"),ymin = 0, ymax =400),alpha=0.1,fill="#D6EAF8") +
  geom_rect(data = analyseboys_2021_2022, aes(xmin = as.Date("2021-11-26"), xmax = as.Date("2022-02-01"),ymin = 0, ymax =400),alpha=0.1,fill="#D6EAF8") +
  geom_smooth(aes(y =Predicted), method = "loess", level = 0.99999, span = 0.17, color = "deepskyblue4", linetype = 2) +
  geom_smooth(aes(y = Observed), method = "loess", level = 0.99999, span = 0.17, color = "red", se = FALSE, linetype = 1) +
  geom_vline(xintercept = as.Date("2022-01-01"), linetype = 4, colour = "black") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
  ggtitle("Any mental disorder \n Boys (6-19 years)") +
  xlab("Year 2021-2022") +
  ylab("Rate per 100 000") +
  coord_cartesian(xlim = c(as.Date("2021-02-01"), as.Date("2022-11-30"))) +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=90))

ggsave(figure_boys_2021_2022, file = "figures/figure_boys_2021_2022.png", bg = 'white', type = "cairo", width = 30, height = 12, units = "cm", dpi = 1200)


#################################################################################################################################
##                                                                                                                             ##
##                                                  Testing for linear trend                                                   ##
##                                                                                                                             ##
#################################################################################################################################


# 2019-2020 (both boys and girls)
analysegirls_2019_2020 <- analysegirls_2019_2020 %>% mutate(sex = "girls")
analyseboys_2019_2020 <- analyseboys_2019_2020 %>% mutate(sex = "boys")
analyse_2019_2020 <- rbind(analysegirls_2019_2020, analyseboys_2019_2020)
analyse_2019_2020_long <- analyse_2019_2020 %>%
  pivot_longer(cols = c(Observed, Predicted),
               names_to = "Type", 
               values_to = "Value")
analyse_2019_2020_long$Time <- with(analyse_2019_2020_long, (aar - 2019) * 52 + Week)

model_2019_2020 <- lm(Value ~ Time * Type * sex, data = analyse_2019_2020_long)
summary(model_2019_2020)

# 2020-2021 (both boys and girls)
analysegirls_2020_2021 <- analysegirls_2020_2021 %>% mutate(sex = "girls")
analyseboys_2020_2021 <- analyseboys_2020_2021 %>% mutate(sex = "boys")
analyse_2020_2021 <- rbind(analysegirls_2020_2021, analyseboys_2020_2021)
analyse_2020_2021_long <- analyse_2020_2021 %>%
  pivot_longer(cols = c(Observed, Predicted),
               names_to = "Type", 
               values_to = "Value")
analyse_2020_2021_long$Time <- with(analyse_2020_2021_long, (aar - 2020) * 52 + Week)

model_2020_2021 <- lm(Value ~ Time * Type * sex, data = analyse_2020_2021_long)
summary(model_2020_2021)


#################################################################################################################################
##                                                                                                                             ##
##                                     T-test comparing primary and specialist healthcare                                      ##
##                                                                                                                             ##
#################################################################################################################################

# Consultation rates per 100,000 for primary and specialist healthcare
primary_rate <- c(267, 145, 133, 273, 149, 139, 277, 154, 162)
specialist_rate <- c(960, 656, 454, 911, 666, 463, 944, 704, 322)

years <- c(2017, 2018, 2019, 2018, 2019, 2020, 2019, 2020, 2021)

# Paired t-test (since we are comparing the same time points for each group)
t_test_result <- t.test(primary_rate, specialist_rate, paired = TRUE)
t_test_result

data <- data.frame(
  Year = years,
  Primary = primary_rate,
  Specialist = specialist_rate
)

# Summary table with the difference and t-test result
summary_table <- data %>%
  mutate(Difference = Primary - Specialist) %>%
  summarise(
    Primary_Mean = mean(Primary),
    Specialist_Mean = mean(Specialist),
    Difference_Mean = mean(Difference),
    t_statistic = t_test_result$statistic,
    p_value = t_test_result$p.value
  )
summary_table
