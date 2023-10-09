############################################
#
# Availability and update of injection BUP
# Author: Rachael Ross
# Script: Figures
#
############################################

path <- "C:/Users/Rachael Ross/Local/Git/Inj-BUP-Availability-Prescribing/"

library(tidyverse)
library(data.table)
#library(haven)

############################################
# Load data
############################################

dat <- readRDS(paste0(path,"data/clean/merged.rds"))

############################################
# National time trends
############################################

############################################
# 2021 by state
############################################

