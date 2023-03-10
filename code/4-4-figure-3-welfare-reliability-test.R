###################################################################
# Author: Lusi Xie, University of Alberta, 07/13/2020
# R script to test mean difference of estimates for 2017, 2018, 2019
# 
# Purpose: Figure 3 in the manuscript
# 
# Input: 
#       1. results/original/sp/2017-welfare-all-09-08.csv: 2017 welfare impacts of site closures (all simulations and all respondents)
#       2. results/original/sp/2018-welfare-all-09-08.csv: 2018 welfare impacts of site closures (all simulations and all respondents)
#       3. results/original/sp/2019-welfare-all-09-08.csv: 2019 welfare impacts of site closures (all simulations and all respondents)
#       4. data/2017-data-08-28.csv: 2017 survey SP data, used in estimation
#       5. data/2018-data-08-28.csv: 2018 survey SP data, used in estimation
#       6. data/2019-data-08-28.csv: 2019 survey SP data, used in estimation
#
# Output:
#       1. Frequency table of reliable pairs (prepared for Figure 3)
###################################################################
rm(list=ls(all=TRUE))

#-----------------------------------------------------------------#
#Load Packages 
#-----------------------------------------------------------------#
library(tidyverse)
library(reshape2)
library(data.table)
library(rstatix)

#############################################################################################
#WTP aggregated by site, using sim_id and id
#############################################################################################


#-----------------------------------------------------------------#
#Import data
#-----------------------------------------------------------------#

#Welfare
#wtp17 <- read_csv("results/original/sp/2017-welfare-all-09-08.csv")
#wtp18 <- read_csv("results/original/sp/2018-welfare-all-09-08.csv")
#wtp19 <- read_csv("results/original/sp/2019-welfare-all-09-08.csv")


#Trip
#trip17 <- read_csv("data/2017-data-08-28.csv")
#trip18 <- read_csv("data/2018-data-08-28.csv")
#trip19 <- read_csv("data/2019-data-08-28.csv")


#-----------------------------------------------------------------#
#Rearrange data
#-----------------------------------------------------------------#
#Trip data: only keep usable variables
trip17 <- trip17 %>% rename(choice=alt) %>% select(id, choice, quant)
trip18 <- trip18 %>% rename(choice=alt) %>% select(id, choice, quant)
trip19 <- trip19 %>% rename(choice=alt) %>% select(id, choice, quant)

#WTP: wide to long
wtplong17 <- reshape2::melt(wtp17, id.vars = c("id", "sim_id"), measure.vars = c(1:79), variable.name="choice", value.name="wtp") %>% 
  arrange(id)

wtplong18 <- reshape2::melt(wtp18, id.vars = c("id", "sim_id"), measure.vars = c(1:72), variable.name="choice", value.name="wtp") %>% 
  arrange(id)

wtplong19 <- melt(wtp19, id.vars = c("id", "sim_id"), measure.vars = c(1:79), variable.name="choice", value.name="wtp") %>% 
  arrange(id)


#WTP: create the choice variable to be combined with trip data
wtplong17 <- wtplong17 %>%
  mutate(choice = rep(rep(unique(trip17$choice), each=length(unique(wtplong17$sim_id))), length(unique(wtplong17$id))))

wtplong18 <- wtplong18 %>%
  mutate(choice = rep(rep(unique(trip18$choice), each=length(unique(wtplong18$sim_id))), length(unique(wtplong18$id))))

wtplong19 <- wtplong19 %>%
  mutate(choice = rep(rep(unique(trip19$choice), each=length(unique(wtplong19$sim_id))), length(unique(wtplong19$id))))


wtplong17$id <- as.numeric(wtplong17$id)
wtplong18$id <- as.numeric(wtplong18$id)
wtplong19$id <- as.numeric(wtplong19$id)


#Merge wtp and trip data
wtp17 <- left_join(wtplong17, trip17, by=c("id", "choice"))
wtp18 <- left_join(wtplong18, trip18, by=c("id", "choice"))
wtp19 <- left_join(wtplong19, trip19, by=c("id", "choice"))

#-----------------------------------------------------------------#
#Calculate WTP per trip
#-----------------------------------------------------------------#
wtp17 <- wtp17 %>%
  mutate(wtptrip = ifelse(quant!=0, wtp/quant, wtp))

wtp18 <- wtp18 %>%
  mutate(wtptrip = ifelse(quant!=0, wtp/quant, wtp))

wtp19 <- wtp19 %>%
  mutate(wtptrip = ifelse(quant!=0, wtp/quant, wtp))


#############################################################################################
#Reliability tests 
#############################################################################################
#-----------------------------------------------------------------#
#Using confidence intervals: aggregate by site (with 30 draws) by person
#-----------------------------------------------------------------#
wtptest17 <- wtp17 %>%  
  group_by(sim_id, choice) %>%
  mutate(wtptrip17=mean(wtptrip)) %>%
  ungroup() %>%
  distinct(sim_id, choice, wtptrip17) 

wtptest18  <- wtp18 %>%
  group_by(sim_id, choice) %>%
  mutate(wtptrip18=mean(wtptrip)) %>%
  ungroup() %>%
  distinct(sim_id, choice, wtptrip18)

wtptest19 <- wtp19 %>% 
  group_by(sim_id, choice) %>%
  mutate(wtptrip19=mean(wtptrip)) %>%
  ungroup() %>%
  distinct(sim_id, choice, wtptrip19)


data <- left_join(wtptest17, wtptest18, by=c("sim_id", "choice"))%>%
  left_join(., wtptest19, by=c("sim_id", "choice")) %>%
  mutate(d1=wtptrip18-wtptrip17, d2=wtptrip19-wtptrip17, d3=wtptrip19-wtptrip18)


#-----------------------------------------------------------------#
#90% CI
#-----------------------------------------------------------------#
ci <- data %>%
  group_by(choice) %>%
  mutate(d1_mean=mean(d1), d1_ci_low=quantile(d1, (1-0.90)/2, na.rm=TRUE), d1_ci_high=quantile(d1, 0.90+(1-0.90)/2, na.rm=TRUE),
         d2_mean=mean(d2), d2_ci_low=quantile(d2, (1-0.90)/2, na.rm=TRUE), d2_ci_high=quantile(d2, 0.90+(1-0.90)/2, na.rm=TRUE),
         d3_mean=mean(d3), d3_ci_low=quantile(d3, (1-0.90)/2, na.rm=TRUE), d3_ci_high=quantile(d3, 0.90+(1-0.90)/2, na.rm=TRUE)) %>%
  distinct(choice, d1_ci_low, d1_ci_high, d2_ci_low, d2_ci_high, d3_ci_low, d3_ci_high) %>%
  mutate(d1insign=ifelse(d1_ci_low<0 & d1_ci_high>0, 1, 0),
         d2insign=ifelse(d2_ci_low<0 & d2_ci_high>0, 1, 0),
         d3insign=ifelse(d3_ci_low<0 & d3_ci_high>0, 1, 0))

#Reliability across years: prepared for Figure 4
ci <- ci %>% mutate(sum=(d1insign+d2insign+d3insign))
as.data.frame(table(ci$sum)) %>%
  mutate(percent=Freq/72) %>%
  rename(Reliablepairs=Var1) #Number of reliable pairs (e.g. =3, are reliable across three pairs/years)

#-----------------------------------------------------------------#
#Using confidence intervals: aggregate by site (with 30 draws) by participant
#-----------------------------------------------------------------#
wtptest17sub <- wtp17 %>%  
  filter (quant!=0) %>%
  group_by(sim_id, choice) %>%
  mutate(wtptrip17=mean(wtptrip)) %>%
  ungroup() %>%
  distinct(sim_id, choice, wtptrip17) 

wtptest18sub  <- wtp18 %>%
  filter (quant!=0) %>%
  group_by(sim_id, choice) %>%
  mutate(wtptrip18=mean(wtptrip)) %>%
  ungroup() %>%
  distinct(sim_id, choice, wtptrip18)

wtptest19sub <- wtp19 %>% 
  filter (quant!=0) %>%
  group_by(sim_id, choice) %>%
  mutate(wtptrip19=mean(wtptrip)) %>%
  ungroup() %>%
  distinct(sim_id, choice, wtptrip19)

data <- left_join(wtptest17sub, wtptest18sub, by=c("sim_id", "choice"))%>%
  left_join(., wtptest19sub, by=c("sim_id", "choice")) %>%
  mutate(d1=wtptrip18-wtptrip17, d2=wtptrip19-wtptrip17, d3=wtptrip19-wtptrip18)

#-----------------------------------------------------------------#
#90% CI
#-----------------------------------------------------------------#
ci <- data %>%
  group_by(choice) %>%
  mutate(d1_mean=mean(d1), d1_ci_low=quantile(d1, (1-0.90)/2, na.rm=TRUE), d1_ci_high=quantile(d1, 0.90+(1-0.90)/2, na.rm=TRUE),
         d2_mean=mean(d2), d2_ci_low=quantile(d2, (1-0.90)/2, na.rm=TRUE), d2_ci_high=quantile(d2, 0.90+(1-0.90)/2, na.rm=TRUE),
         d3_mean=mean(d3), d3_ci_low=quantile(d3, (1-0.90)/2, na.rm=TRUE), d3_ci_high=quantile(d3, 0.90+(1-0.90)/2, na.rm=TRUE)) %>%
  distinct(choice, d1_ci_low, d1_ci_high, d2_ci_low, d2_ci_high, d3_ci_low, d3_ci_high) %>%
  mutate(d1insign=ifelse(d1_ci_low<0 & d1_ci_high>0, 1, 0),
         d2insign=ifelse(d2_ci_low<0 & d2_ci_high>0, 1, 0),
         d3insign=ifelse(d3_ci_low<0 & d3_ci_high>0, 1, 0))


#Reliability across years: prepared for Figure 4
ci <- ci %>% mutate(sum=(d1insign+d2insign+d3insign))
as.data.frame(table(ci$sum)) %>%
  mutate(percent=Freq/72) %>%
  rename(Reliablepairs=Var1) #Number of reliable pairs (e.g. =3, are reliable across three pairs/years)
