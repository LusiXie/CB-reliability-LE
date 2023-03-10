###################################################################
# Author: Lusi Xie, University of Delaware, 01/25/2022
# R script to simulate demand for price increase by 10% to all sites (2019 survey data) 
# 
# Purpose: Simulate demand for testing reliability of elasticities in the manuscript
# 
# Input: 
#       1. r-output/2018-sp-estimates: estimates with 2019 survey CB data
# Output:
#       1. r-output/2018-z1-demand: full simulation results
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
#Demand
#############################################################################################
nalts <- mdcev_est[["stan_data"]][["J"]]

n <- mdcev_est[["n_individuals"]]

#-----------------------------------------------------------------#
#Demand changes for increase prices by 10% for all sites
#-----------------------------------------------------------------#


## ---- Prepare scenarios------------------------------------------##
npols <- 1 # Choose number of policies

policies <- CreateBlankPolicies(npols, mdcev_est)


## ---- Prepare simulation data------------------------------------##

df_sim<- PrepareSimulationData(mdcev_est, policies)

## ---- Simulation ------------------------------------##

# For loop
#Initial value
df_sim_split <- list(df_sim[["df_indiv"]][c(1:7)], df_sim[["df_common"]][c(1:4)],  df_sim[["sim_options"]][c(1:4)])

for (i in 1:n) {
  df_sim_split[[i]] <- list(lapply(df_sim[["df_indiv"]], `[`, i), df_sim[["df_common"]], df_sim[["sim_options"]])
  df_sim_split[[i]][[2]][["price_p_list"]][[1]][2:73] <- df_sim_split[[i]][[1]][["price"]][[1]][2:73]*0.1
}

demand<- mdcev.sim(df_sim_split[[1]][[1]],
                   df_common = df_sim_split[[1]][[2]],
                   sim_options = df_sim_split[[1]][[3]],
                   cond_err = 1, 
                   nerrs = 50, 
                   sim_type = "demand")

for (i in 1:n){
  demand[i] <- mdcev.sim(df_sim_split[[i]][[1]],
                         df_common = df_sim_split[[i]][[2]],
                         sim_options = df_sim_split[[i]][[3]],
                         cond_err = 1, 
                         nerrs = 50, 
                         sim_type = "demand")
}


summary_draw <- lapply(demand[[1]][c(1:30)], `[`, 1)


for (i in 1:n){
  summary_draw[[i]] <- lapply(demand[[i]][c(1:30)], `[`, 1)
}


#############################################################################################
#Export
#############################################################################################
save(summary_draw, file = "r-output/2018-sp-z1-demand") #SP