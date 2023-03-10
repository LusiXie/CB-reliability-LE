###################################################################
# Author: Lusi Xie, University of Delaware, 02/07/2022
# 
# Purpose: R script to test mean difference of cross price elasticities for 2018, 2019, 2020
# 
# Input: 
#       1. r-output/2017-sp-z1-demand: full simulation results for 2018 z1
#       2. r-output/2018-sp-z1-demand: full simulation results for 2019 z1
#       3. r-output/2019-sp-z1-demand: full simulation results for 2020 z1
#       4. data/separate/2017-data-sp.csv: 2018 survey CB data, cap max trips to around 30 by scenario
#       5. data/separate/2018-data-sp.csv: 2019 survey CB data, cap max trips to around 30 by scenario
#       6. data/separate/2019-data-sp.csv: 2020 survey CB data, cap max trips to around 30 by scenario
###################################################################

rm(list=ls(all=TRUE))

#-----------------------------------------------------------------#
#Load Packages 
#-----------------------------------------------------------------#
library(tidyverse)
library(reshape2)
library(data.table)
library(rstatix)
library(miscTools)

#-----------------------------------------------------------------#
#Import data
#-----------------------------------------------------------------#
#Original data to calculate old numeraire good
#data_17 <- read_csv("data/separate/2017-data-sp.csv")
#data_18 <- read_csv("data/separate/2018-data-sp.csv")
#data_19 <- read_csv("data/separate/2019-data-sp.csv")

#Simulated new numeraire
load("r-output/2017-sp-z1-demand")
summary_draw_17 <- summary_draw

load("r-output/2018-sp-z1-demand")
summary_draw_18 <- summary_draw

load("r-output/2019-sp-z1-demand")
summary_draw_19 <- summary_draw

rm(summary_draw)


#-----------------------------------------------------------------#
#Calculate old numeraire good: income-sum(trips*prices)
#-----------------------------------------------------------------#
z0_17 <- data_17 %>% 
  group_by(id) %>% 
  mutate(trips_sum=sum(trips*price)) %>%
  distinct(id, trips_sum, income) %>%
  mutate(z0=income-trips_sum) %>%
  ungroup() %>%
  mutate(id = row_number()) %>%
  select(id, z0)

z0_18 <- data_18 %>% 
  group_by(id) %>% 
  mutate(trips_sum=sum(trips*price)) %>%
  distinct(id, trips_sum, income) %>%
  mutate(z0=income-trips_sum) %>%
  ungroup() %>%
  mutate(id = row_number()) %>%
  select(id, z0)

z0_19 <- data_19 %>% 
  group_by(id) %>% 
  mutate(trips_sum=sum(trips*price)) %>%
  distinct(id, trips_sum, income) %>%
  mutate(z0=income-trips_sum) %>%
  ungroup() %>%
  mutate(id = row_number()) %>%
  select(id, z0)

#-----------------------------------------------------------------#
#Summarize z1 by draw
#-----------------------------------------------------------------#
z1_17 <- as.data.frame(do.call(rbind, summary_draw_17)) 
z1_17 <- sapply(z1_17, function(x) as.numeric(as.character(x)))

z1_18 <- as.data.frame(do.call(rbind, summary_draw_18)) 
z1_18 <- sapply(z1_18, function(x) as.numeric(as.character(x)))

z1_19 <- as.data.frame(do.call(rbind, summary_draw_19)) 
z1_19 <- sapply(z1_19, function(x) as.numeric(as.character(x)))

#-----------------------------------------------------------------#
#Percent changes of numeraire demand
#-----------------------------------------------------------------#
d_z_17 <- cbind(z0_17, z1_17) %>%
  mutate(across(V1:V30, ~ .x-z0)) %>%
  mutate(across(V1:V30, ~ .x/z0))

d_z_18 <- cbind(z0_18, z1_18) %>%
  mutate(across(V1:V30, ~ .x-z0)) %>%
  mutate(across(V1:V30, ~ .x/z0))

d_z_19 <- cbind(z0_19, z1_19) %>%
  mutate(across(V1:V30, ~ .x-z0)) %>%
  mutate(across(V1:V30, ~ .x/z0))

d_z <- as.data.frame(cbind(colMeans(d_z_17)[3:32], colMeans(d_z_18)[3:32], colMeans(d_z_19)[3:32])) %>%
  rename(d_z_17=V1, d_z_18=V2, d_z_19=V3) %>%
  mutate(sim_id=c(1:30)) %>%
  mutate(d1=d_z_18-d_z_17, d2=d_z_19-d_z_17, d3=d_z_19-d_z_18)

#-----------------------------------------------------------------#
#Elasticity by draw
#-----------------------------------------------------------------#
e_17 <- cbind(z0_17, z1_17) %>%
  mutate(across(V1:V30, ~ .x-z0)) %>%
  mutate(across(V1:V30, ~ .x/z0)) %>%
  mutate(across(V1:V30, ~ .x/0.1))


e_18 <- cbind(z0_18, z1_18) %>%
  mutate(across(V1:V30, ~ .x-z0)) %>%
  mutate(across(V1:V30, ~ .x/z0)) %>%
  mutate(across(V1:V30, ~ .x/0.1))

e_19 <- cbind(z0_19, z1_19) %>%
  mutate(across(V1:V30, ~ .x-z0)) %>%
  mutate(across(V1:V30, ~ .x/z0)) %>%
  mutate(across(V1:V30, ~ .x/0.1))

# Medians
e <- as.data.frame(cbind(colMedians(e_17)[3:32], colMedians(e_18)[3:32], colMedians(e_19)[3:32])) %>%
  rename(e_17=V1, e_18=V2, e_19=V3) %>%
  mutate(sim_id=c(1:30)) %>%
  mutate(d1=e_18-e_17, d2=e_19-e_17, d3=e_19-e_18)


#-----------------------------------------------------------------#
#Confidence intervals for reliability test
#-----------------------------------------------------------------#
#Elasticity
ci_e <- e %>%
  mutate(d1_mean=mean(d1), d1_ci_low=quantile(d1, (1-0.90)/2, na.rm=TRUE), d1_ci_high=quantile(d1, 0.90+(1-0.90)/2, na.rm=TRUE),
         d2_mean=mean(d2), d2_ci_low=quantile(d2, (1-0.90)/2, na.rm=TRUE), d2_ci_high=quantile(d2, 0.90+(1-0.90)/2, na.rm=TRUE),
         d3_mean=mean(d3), d3_ci_low=quantile(d3, (1-0.90)/2, na.rm=TRUE), d3_ci_high=quantile(d3, 0.90+(1-0.90)/2, na.rm=TRUE)) %>%
  distinct(d1_mean, d1_ci_low, d1_ci_high, d2_mean, d2_ci_low, d2_ci_high, d3_mean, d3_ci_low, d3_ci_high) %>%
  mutate(d1insign=ifelse(d1_ci_low<0 & d1_ci_high>0, 1, 0),
         d2insign=ifelse(d2_ci_low<0 & d2_ci_high>0, 1, 0),
         d3insign=ifelse(d3_ci_low<0 & d3_ci_high>0, 1, 0))

