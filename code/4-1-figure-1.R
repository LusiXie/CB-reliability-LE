###################################################################
# Author: Lusi Xie, University of Alberta, 07/12/2020
# 
# Purpose: Figure 4 in manuscript
# 
# Input: 
#       1. results/original/2017-estimates-09-08.csv: 2018 model estimates
#       2. results/original/2018-estimates-09-08.csv: 2019 model estimates
#       3. results/original/2019-estimates-09-08.csv: 2020 model estimates
# Output:
#       1. results/CBestimates.png
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

#############################################################################################
#Key estimates with error bars
#############################################################################################

#-----------------------------------------------------------------#
#CB scenarios
#-----------------------------------------------------------------#
estimates17 <- data17 %>%
  filter(parameter=="psi_oct" | parameter=="psi_dec" | parameter=="psi_tag" | parameter=="psi_gift") %>%
  separate(parameter, c("psi_", "parameter"), sep = 4) %>%
  mutate(year='2018', ci=1.96*Std.err) %>%
  select(parameter, Estimate, ci, year)

estimates18 <- data18 %>%
  filter(parameter=="psi_oct" | parameter=="psi_dec" | parameter=="psi_tag" | parameter=="psi_gift") %>%
  separate(parameter, c("psi_", "parameter"), sep = 4) %>%
  mutate(year='2019', ci=1.96*Std.err) %>%
  select(parameter, Estimate, ci, year)

estimates19 <- data19 %>%
  filter(parameter=="psi_oct" | parameter=="psi_dec" | parameter=="psi_tag" | parameter=="psi_gift") %>%
  separate(parameter, c("psi_", "parameter"), sep = 4) %>%
  mutate(year='2020', ci=1.96*Std.err) %>%
  select(parameter, Estimate, ci, year)

estimates <- rbind(estimates17, estimates18, estimates19) 

estimates$parameter[estimates$parameter=="dec"] <- "December"
estimates$parameter[estimates$parameter=="oct"] <- "October"
estimates$parameter[estimates$parameter=="tag"] <- "Extra tags"
estimates$parameter[estimates$parameter=="gift"] <- "Gift cards"


CBscenarios <- ggplot(estimates, aes(x=parameter, group=parameter)) + 
  facet_wrap( ~ year, strip.position="bottom")+
  geom_point(aes(y=Estimate, shape=parameter),size=9) +
  geom_errorbar(aes(ymin=Estimate-ci, ymax=Estimate+ci), width=.5, size=2)+
  labs(shape = "CB Scenarios") +
  labs(x="", y = "Psi parameter estimates")+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  theme_classic(base_size=40) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

CBscenarios

ggsave("results/CBestimates.png", width = 50, height = 30, units = "cm")