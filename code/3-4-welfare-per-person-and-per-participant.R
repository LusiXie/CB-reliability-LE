###################################################################
# Author: Lusi Xie, University of Alberta, 07/13/2020
# R script to test mean difference of welfares for 2017, 2018, 2019
# 
# Purpose: Appendix C
# 
# Input: 
#       1. results/original/sp/2017-welfare-all-09-08.csv: 2018 welfare impacts of site closures (all simulations and all respondents)
#       2. results/original/sp/2018-welfare-all-09-08.csv: 2019 welfare impacts of site closures (all simulations and all respondents)
#       3. results/original/sp/2019-welfare-all-09-08.csv: 2020 welfare impacts of site closures (all simulations and all respondents)
#       4. data/2017-data-08-28.csv: 2018 survey SP data, used in estimation
#       5. data/2018-data-08-28.csv: 2019 survey SP data, used in estimation
#       6. data/2019-data-08-28.csv: 2020 survey SP data, used in estimation
# Output:
#       1. results/wtp-by-site-per-person-per-trip.csv: welfare loss of site closures per person per trip
#       2. results/wtp-by-site-per-participant-per-trip.csv: welfare loss of site closures per participant (with positive trips) per trip
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
wtp17 <- read_csv("results/original/sp/2017-welfare-all-09-08.csv")
wtp18 <- read_csv("results/original/sp/2018-welfare-all-09-08.csv")
wtp19 <- read_csv("results/original/sp/2019-welfare-all-09-08.csv")


#Trip
trip17 <- read_csv("data/2017-data-08-28.csv")
trip18 <- read_csv("data/2018-data-08-28.csv")
trip19 <- read_csv("data/2019-data-08-28.csv")


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

#-----------------------------------------------------------------#
#Calculate average WTP per trip by site (all respondents)
#-----------------------------------------------------------------#
wtp17site <- wtp17 %>%
  group_by(sim_id, choice) %>%
  mutate(wtp17=mean(wtptrip)) %>%
  distinct(sim_id, choice, wtp17) %>%
  group_by(choice) %>%
  mutate(wtp17_mean=mean(wtp17), wtp17_sd=sd(wtp17), wtp17_ci_low=quantile(wtp17, (1-0.95)/2), wtp17_ci_high=quantile(wtp17, 0.95+(1-0.95)/2))%>%
  distinct(choice, wtp17_mean, wtp17_sd, wtp17_ci_low, wtp17_ci_high)

wtp18site <- wtp18 %>%
  group_by(sim_id, choice) %>%
  mutate(wtp18=mean(wtptrip)) %>%
  distinct(sim_id, choice, wtp18) %>%
  group_by(choice) %>%
  mutate(wtp18_mean=mean(wtp18), wtp18_sd=sd(wtp18), wtp18_ci_low=quantile(wtp18, (1-0.95)/2), wtp18_ci_high=quantile(wtp18, 0.95+(1-0.95)/2))%>%
  distinct(choice, wtp18_mean, wtp18_sd, wtp18_ci_low, wtp18_ci_high)


wtp19site <- wtp19 %>%
  group_by(sim_id, choice) %>%
  mutate(wtp19=mean(wtptrip)) %>%
  distinct(sim_id, choice, wtp19) %>%
  group_by(choice) %>%
  mutate(wtp19_mean=mean(wtp19), wtp19_sd=sd(wtp19), wtp19_ci_low=quantile(wtp19, (1-0.95)/2), wtp19_ci_high=quantile(wtp19, 0.95+(1-0.95)/2))%>%
  distinct(choice, wtp19_mean, wtp19_sd, wtp19_ci_low, wtp19_ci_high)

wtpsite <- left_join(wtp17site, wtp18site, by="choice") %>%
  left_join(., wtp19site, by="choice")

#-----------------------------------------------------------------#
#Calculate average WTP per trip by site for those who would have taken trips
#-----------------------------------------------------------------#
wtp17sitesub <- wtp17 %>%
  filter (quant!=0) %>%
  group_by(sim_id, choice) %>%
  mutate(wtp17=mean(wtptrip)) %>%
  distinct(sim_id, choice, wtp17) %>%
  group_by(choice) %>%
  mutate(wtp17_mean=mean(wtp17), wtp17_sd=sd(wtp17), wtp17_ci_low=quantile(wtp17, (1-0.95)/2), wtp17_ci_high=quantile(wtp17, 0.95+(1-0.95)/2))%>%
  distinct(choice, wtp17_mean, wtp17_sd, wtp17_ci_low, wtp17_ci_high) %>%
  arrange(choice)

wtp18sitesub <- wtp18 %>%
  filter (quant!=0) %>%
  group_by(sim_id, choice) %>%
  mutate(wtp18=mean(wtptrip)) %>%
  distinct(sim_id, choice, wtp18) %>%
  group_by(choice) %>%
  mutate(wtp18_mean=mean(wtp18), wtp18_sd=sd(wtp18), wtp18_ci_low=quantile(wtp18, (1-0.95)/2), wtp18_ci_high=quantile(wtp18, 0.95+(1-0.95)/2))%>%
  distinct(choice, wtp18_mean, wtp18_sd, wtp18_ci_low, wtp18_ci_high) %>%
  arrange(choice)


wtp19sitesub <- wtp19 %>%
  filter (quant!=0) %>%
  group_by(sim_id, choice) %>%
  mutate(wtp19=mean(wtptrip)) %>%
  distinct(sim_id, choice, wtp19) %>%
  group_by(choice) %>%
  mutate(wtp19_mean=mean(wtp19), wtp19_sd=sd(wtp19), wtp19_ci_low=quantile(wtp19, (1-0.95)/2), wtp19_ci_high=quantile(wtp19, 0.95+(1-0.95)/2))%>%
  distinct(choice, wtp19_mean, wtp19_sd, wtp19_ci_low, wtp19_ci_high) %>%
  arrange(choice)

wtpsitesub <- left_join(wtp17sitesub, wtp18sitesub, by="choice") %>%
  left_join(., wtp19sitesub, by="choice")

#-----------------------------------------------------------------#
#Export stats
#-----------------------------------------------------------------#
write.csv(wtpsite, "results/wtp-by-site-per-person-per-trip.csv", row.names=FALSE)
write.csv(wtpsitesub, "results/wtp-by-site-per-participant-per-trip.csv", row.names=FALSE)