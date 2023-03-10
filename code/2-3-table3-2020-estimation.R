###################################################################
# Author: Lusi Xie, University of Alberta, 09/08/2020

# Purpose: Table 3 (column 3) in the manuscript
# Note, Table D1 (column 3) in Appendix D uses almost identical code except with RP-CB data; Appendix E uses the same code to obtain log-likelihood for pooled model
#
# Input: 
#       1. data/separate/2019-data-sp.csv: 2020 survey CB data
# Output:
#       1. r-output/2019-sp-estimates: estimates with 2020 survey CB data saved for simulation
#       2. results/original/sp/2019-estimates.csv: estimation results
#       3. results/original/sp/2019-estimates-mvndraw.csv: estimation results with draws
###################################################################


rm(list=ls(all=TRUE))

#-----------------------------------------------------------------#
#Load Packages 
#-----------------------------------------------------------------#
library(tidyverse)
library(rmdcev)

#-----------------------------------------------------------------#
#Import data: not uploaded
#-----------------------------------------------------------------#
data <- read_csv("data/separate/2019-data-sp.csv")

#############################################################################################
#Estimation
#############################################################################################

#-----------------------------------------------------------------#
#Transform data
#-----------------------------------------------------------------#

data <- mdcev.data(data, id.var="id", alt.var="choice", choice = "trips", price="price", income="income")

#-----------------------------------------------------------------#
#psi.formula
#-----------------------------------------------------------------#
psi.names<- colnames(data[,c(6, 8:95)]) 
psi_formula <- paste(psi.names, collapse = " + ")
psi_formula <- as.formula(paste(paste(" ",psi_formula, sep=" ~ "), 1, sep=" - "))
psi_formula


#-----------------------------------------------------------------#
#Estimation: MDCEV
#-----------------------------------------------------------------#
#Hybrid
mdcev_est <- mdcev(psi_formula,
                   data = data,
                   model = "hybrid",
                   algorithm = "MLE",
                   std_errors = "mvn",
                   psi_ascs = 0,
                   seed = 123,
                   print_iterations = FALSE,
                   flat_priors = 0)
summary(mdcev_est)


estimates <- summary(mdcev_est)[["CoefTable"]] %>%
  rownames_to_column() %>%
  rename(parameter=rowname)

estimatesdraw <- mdcev_est[["stan_fit"]][["theta_tilde"]]


#############################################################################################
#Export
#############################################################################################
#R
save(mdcev_est, file = "r-output/2019-sp-estimates") 

#CSV
write.csv(estimates, "results/original/sp/2019-estimates-09-08.csv", row.names=FALSE)
write.csv(estimatesdraw, "results/original/sp/2019-estimates-mvndraw-09-08.csv", row.names=FALSE)
