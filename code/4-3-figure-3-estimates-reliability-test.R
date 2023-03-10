###################################################################
# Author: Lusi Xie, University of Alberta, 07/13/2020
# R script to test mean difference of estimates for 2017, 2018, 2019
# 
# Purpose: Figure 3 in the manuscript
# 
# Input: 
#       1. results/original/sp/2017-estimates-mvndraw-09-08.csv: 2018 estimates with mvn draws
#       2. results/original/sp/2018-estimates-mvndraw-09-08.csv: 2019 estimates with mvn draws
#       3. results/original/sp/2019-estimates-mvndraw-09-08.csv: 2020 estimates with mvn draws
#       4. results/original/sp/parameter-names-17-19.csv: parameter names for 2018 and 2020 estimates
#       5. results/original/sp/parameter-names-18.csv: parameter names for 2019 estimates
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

#-----------------------------------------------------------------#
#Import data
#-----------------------------------------------------------------#

#estimates17 <- read_csv("results/original/sp/2017-estimates-mvndraw-09-08.csv")
#estimates18 <- read_csv("results/original/sp/2018-estimates-mvndraw-09-08.csv")
#estimates19 <- read_csv("results/original/sp/2019-estimates-mvndraw-09-08.csv")
#names1719 <- read_csv("results/original/sp/parameter-names17-19.csv")
#names18 <- read_csv("results/original/sp/parameter-names-18.csv")

#-----------------------------------------------------------------#
#Rename columns 
#-----------------------------------------------------------------#
colnames(estimates17) <- names1719$parms
colnames(estimates19) <- names1719$parms
colnames(estimates18) <- names18$parms

#-----------------------------------------------------------------#
#Rearrange data
#-----------------------------------------------------------------#
#Add a "draw id"
estimates17 <- estimates17 %>%
  rownames_to_column() %>%
  rename(id=rowname)

estimates18 <- estimates18 %>%
  rownames_to_column() %>%
  rename(id=rowname)

estimates19 <- estimates19 %>%
  rownames_to_column() %>%
  rename(id=rowname)

#Wide to long
estimateslong17 <- reshape2::melt(estimates17, id.vars = c("id"), measure.vars = c(2:171), variable.name="parm", value.name="estimates17") 
estimateslong18 <- reshape2::melt(estimates18, id.vars = c("id"), measure.vars = c(2:157), variable.name="parm", value.name="estimates18") 
estimateslong19 <- reshape2::melt(estimates19, id.vars = c("id"), measure.vars = c(2:171), variable.name="parm", value.name="estimates19") 

#Merge together
estimates <- left_join(estimateslong17, estimateslong18, by=c("id", "parm")) %>%
  left_join(., estimateslong19, by=c("id", "parm")) %>%
  mutate(d1=estimates18-estimates17, d2=estimates19-estimates17, d3=estimates19-estimates18) 

#-----------------------------------------------------------------#
#90% CI
#-----------------------------------------------------------------#

ci <- estimates %>%
  group_by(parm) %>%
  mutate(d1_mean=mean(d1), d1_ci_low=quantile(d1, (1-0.90)/2, na.rm=TRUE), d1_ci_high=quantile(d1, 0.90+(1-0.90)/2, na.rm=TRUE),
         d2_mean=mean(d2), d2_ci_low=quantile(d2, (1-0.90)/2, na.rm=TRUE), d2_ci_high=quantile(d2, 0.90+(1-0.90)/2, na.rm=TRUE),
         d3_mean=mean(d3), d3_ci_low=quantile(d3, (1-0.90)/2, na.rm=TRUE), d3_ci_high=quantile(d3, 0.90+(1-0.90)/2, na.rm=TRUE)) %>%
  distinct(parm, d1_ci_low, d1_ci_high, d2_ci_low, d2_ci_high, d3_ci_low, d3_ci_high) %>%
  mutate(d1insign=ifelse(d1_ci_low<0 & d1_ci_high>0, 1, 0),
         d2insign=ifelse(d2_ci_low<0 & d2_ci_high>0, 1, 0),
         d3insign=ifelse(d3_ci_low<0 & d3_ci_high>0, 1, 0))

#Reliability across years: prepared for Figure 3
ci <- ci %>% mutate(sum=(d1insign+d2insign+d3insign))
as.data.frame(table(ci$sum)) %>%
  mutate(percent=Freq/156) %>%
  rename(Reliablepairs=Var1) #Number of reliable pairs (e.g. =3, are reliable across three pairs/years)
