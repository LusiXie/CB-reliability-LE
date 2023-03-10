###################################################################
# Author: Lusi Xie, University of Delaware, 01/24/2022
# 
# Purpose: Table 4 (column 2) in the manuscript
# Note, Table D2 (column 2) in Appendix D uses almost identical code except with RP-CB data
# 
# Input: 
#       1. r-output/2018-sp-estimates: estimates with 2019 survey CB data
# Output:
#       1. results/original/sp/2018-welfare-all.csv: full simulation results
###################################################################

rm(list=ls(all=TRUE))

#-----------------------------------------------------------------#
#Load Packages 
#-----------------------------------------------------------------#
library(tidyverse)
library(rmdcev)

#-----------------------------------------------------------------#
#Import data
#-----------------------------------------------------------------#
load("r-output/2018-sp-estimates")


#############################################################################################
#Welfare
#############################################################################################

#-----------------------------------------------------------------#
#WTP for closing one site one at a time
#-----------------------------------------------------------------#


## ---- Prepare scenarios------------------------------------------##
nalts <- mdcev_est[["stan_data"]][["J"]]

npols <- nalts
policies<-  CreateBlankPolicies(npols = npols, 
                                mdcev_est, 
                                price_change_only = TRUE)


policy.fn <- function(i, nalts){
  policies$price_p[[i]] <- c(rep(0, i), 100000000, rep(0, nalts-i))
  return(policies$price_p[[i]])
}


number <- c(1:nalts)
price_list <- lapply(number, policy.fn, nalts)
policies$price_p <- price_list

## ---- Prepare simulation data------------------------------------##

df_sim <- PrepareSimulationData(mdcev_est, policies)


## ---- Welfare simulation-----------------------------------------##

welfare <- mdcev.sim(df_sim$df_indiv,
                     df_common = df_sim$df_common,
                     sim_options = df_sim$sim_options,
                     cond_err = 1, 
                     nerrs = 50, 
                     sim_type = "welfare")


## ---- Save welfare simulation full results------------------------##

options(tibble.print_max = Inf)
summary(welfare)

#WTP by person by draw
wtpall <- as.data.frame(do.call(rbind, welfare)) %>%
  mutate(sim_id=rep(1:30, 883)) %>%
  mutate(id=rep(1:883, each=30))

#############################################################################################
#Export
#############################################################################################
write.csv(wtpall, "results/original/sp/2018-welfare-all-09-08.csv", row.names=FALSE)
