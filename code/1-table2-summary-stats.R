###################################################################
# Author: Lusi Xie, University of Delaware
# Created: 11/03/2020
# Updated: 09/20/2021
#
# R script to create summary stats tables of socio demographics (CB data only)
# 
# Purpose: Replicate Table 2 in the manuscript
# 
# Input: 
#       1. data/2017-data-08-28.csv: 2017 CB data, used in estimation
#       2. data/2018-data-08-28.csv: 2018 CB data, used in estimation
#       3. data/2019-data-08-28.csv: 2019 CB data, used in estimation
#
# Output:
#       1. Table 2 in manuscript
###################################################################

rm(list=ls(all=TRUE))

#-----------------------------------------------------------------#
#Load Packages 
#-----------------------------------------------------------------#
library(tidyverse)
library(reshape2)
library(stargazer)
library(aod)

##########################################################################################################################
# Table 2
##########################################################################################################################
#-----------------------------------------------------------------#
#Import data: not uploaded  
#-----------------------------------------------------------------#
#data17 <- read_csv("data/2017-data-08-28.csv")
#data18 <- read_csv("data/2018-data-08-28.csv")
#data19 <- read_csv("data/2019-data-08-28.csv")

#-----------------------------------------------------------------#
#Summary stats
#-----------------------------------------------------------------#
#Adjust the scale differences:
#cwdprevalence is presented in percentage in the table, but the absolute value is used in estimation;
#yrshunt is divided by 10 in the estimation

data17 <- data17 %>%
  mutate(cwdprevalence=cwdprevalence*100, yrshunt=yrshunt*10)

data18 <- data18 %>%
  mutate(cwdprevalence=cwdprevalence*100, yrshunt=yrshunt*10)

data19 <- data19 %>%
  mutate(cwdprevalence=cwdprevalence*100, yrshunt=yrshunt*10)

#Column 1: 2018
stargazer(as.data.frame(data17[,c(5:14, 94)]), type="text")
#Column 2: 2019
stargazer(as.data.frame(data18[,c(5:14, 87)]), type="text")
#Column 3: 2020
stargazer(as.data.frame(data19[,c(5:14, 94)]), type="text")


#Travel costs of positive trips
price17 <- data17 %>%
  filter(quant!=0) %>%
  select(price)

price18 <- data18 %>%
  filter(quant!=0) %>%
  select(price)

price19 <- data19 %>%
  filter(quant!=0) %>%
  select(price)

stargazer(as.data.frame(price17), as.data.frame(price18), as.data.frame(price19), type="text")