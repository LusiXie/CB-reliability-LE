###################################################################
# Author: Lusi Xie, University of Alberta, 07/12/2020
# R script to show estimates with error bars in graphs
# 
# Purpose: Figure 5 in manuscript
# 
# Input: 
#       1. results/wtp-by-site-per-person-per-trip.csv: welfare loss of site closures per person per trip
#       2. results/wtp-by-site-per-participant-per-trip.csv: welfare loss of site closures per participant (with positive trips) per trip
#       3. data/2017-data-08-28.csv: 2018 survey SP data, used in estimation
#       4. data/2018-data-08-28.csv: 2019 survey SP data, used in estimation
#       5. data/2019-data-08-28.csv: 2020 survey SP data, used in estimation
# Output:
#       1. results/welfare.png
###################################################################

rm(list=ls(all=TRUE))

#-----------------------------------------------------------------#
#Load Packages 
#-----------------------------------------------------------------#
library(tidyverse)
library(gridExtra)
library(cowplot)

#-----------------------------------------------------------------#
#Import data
#-----------------------------------------------------------------#
#estimates
data17 <- read_csv("results/original/sp/2017-estimates-09-08.csv")
data18 <- read_csv("results/original/sp/2018-estimates-09-08.csv")
data19 <- read_csv("results/original/sp/2019-estimates-09-08.csv")

#Welfare
wtpperson <- read_csv("results/wtp-by-site-per-person-per-trip.csv")
wtpparticipant <- read_csv("results/wtp-by-site-per-participant-per-trip.csv")


#############################################################################################
#WTP per person
#
#Selected welfare estimates with error bars per participant per trip
#WMU 151: all scenarios apply, high CWD prevalence (doubled every year); 
#WMU 230: Season expansion applies, with CWD (increased every year, remains low);
#WMU 501: no scenarios apply, no CWD
#############################################################################################

#-----------------------------------------------------------------#
#Rearrange data for graphs
#-----------------------------------------------------------------#
selectwtp <- wtpperson %>%
  filter(choice==151 | choice==230 | choice==501) %>%
  mutate(choice=as.factor(choice)) 

wtp17 <- selectwtp %>% 
  select(choice, wtp17_mean, wtp17_ci_low, wtp17_ci_high) %>%
  rename(wtp=wtp17_mean, ci_low=wtp17_ci_low, ci_high=wtp17_ci_high) %>%
  mutate(year=2018)

wtp18 <- selectwtp %>% 
  select(choice, wtp18_mean, wtp18_ci_low, wtp18_ci_high) %>%
  rename(wtp=wtp18_mean, ci_low=wtp18_ci_low, ci_high=wtp18_ci_high) %>%
  mutate(year=2019)

wtp19 <- selectwtp %>% 
  select(choice, wtp19_mean, wtp19_ci_low, wtp19_ci_high) %>%
  rename(wtp=wtp19_mean, ci_low=wtp19_ci_low, ci_high=wtp19_ci_high) %>%
  mutate(year=2020)

selectwtp <- rbind(wtp17, wtp18, wtp19) %>%
  mutate(choice=paste("WMU", choice, sep=""))

#-----------------------------------------------------------------#
#Graph
#-----------------------------------------------------------------#
wtppersonplot <- ggplot(selectwtp, aes(x=choice, group=choice)) + 
  facet_wrap( ~ year, strip.position="bottom")+
  geom_point(aes(y=wtp, shape=choice), size=9) +
  labs(shape = "Hunting sites") +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=.3, size=2)+
  labs(title="Per Person", x="", y = "Welfare estimates (CAD$)" )+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  theme_classic(base_size=35) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

wtppersonplot

#############################################################################################
#WTP per participant
#
#Selected welfare estimates with error bars per participant per trip
#WMU 150: all scenarios apply, high CWD prevalence (doubled every year); 
#WMU 230: Season expansion applies, with CWD (increased every year, remains low);
#WMU 501: no scenarios apply, no CWD
#############################################################################################

#-----------------------------------------------------------------#
#Rearrange data for graphs
#-----------------------------------------------------------------#
selectwtp <- wtpparticipant %>%
  filter(choice==151 | choice==230 | choice==501) %>%
  mutate(choice=as.factor(choice)) 

wtp17 <- selectwtp %>% 
  select(choice, wtp17_mean, wtp17_ci_low, wtp17_ci_high) %>%
  rename(wtp=wtp17_mean, ci_low=wtp17_ci_low, ci_high=wtp17_ci_high) %>%
  mutate(year=2018)

wtp18 <- selectwtp %>% 
  select(choice, wtp18_mean, wtp18_ci_low, wtp18_ci_high) %>%
  rename(wtp=wtp18_mean, ci_low=wtp18_ci_low, ci_high=wtp18_ci_high) %>%
  mutate(year=2019)

wtp19 <- selectwtp %>% 
  select(choice, wtp19_mean, wtp19_ci_low, wtp19_ci_high) %>%
  rename(wtp=wtp19_mean, ci_low=wtp19_ci_low, ci_high=wtp19_ci_high) %>%
  mutate(year=2020)

selectwtp <- rbind(wtp17, wtp18, wtp19) %>%
  mutate(choice=paste("WMU", choice, sep=""))

#-----------------------------------------------------------------#
#Graph
#-----------------------------------------------------------------#
wtpparticipantplot <- ggplot(selectwtp, aes(x=choice, group=choice)) + 
  facet_wrap( ~ year, strip.position="bottom")+
  geom_point(aes(y=wtp, shape=choice), size=9) +
  labs(shape = "Hunting sites") +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high), width=.3, size=2)+
  labs(title="Per Participant", x="", y = "Welfare estimates (CAD$)" )+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  theme_classic(base_size=35) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

wtpparticipantplot

#-----------------------------------------------------------------#
#Export
#-----------------------------------------------------------------#
welfare <-arrangeGrob(wtppersonplot, wtpparticipantplot, ncol=1, nrow=2)
ggsave("results/welfare.png", welfare, width = 50, height = 60, units = "cm")
