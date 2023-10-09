############################################
#
# Availability and update of injection BUP
# Author: Rachael Ross
# Script: Analyze data
#
############################################

path <- "C:/Users/Rachael Ross/Local/Git/Inj-BUP-Availability-Prescribing/"

library(tidyverse)
library(data.table)
#library(haven)

############################################
# Load data
############################################

nssats <- readRDS(paste0(path,"data/clean/nssatsclean.rds"))
sdud <- readRDS(paste0(path,"data/clean/sdudclean.rds"))
denom <- readRDS(paste0(path,"data/clean/denom.rds"))

############################################
# Transform and merge
############################################

### SDUD
sdud_ <- sdud |>
  rename(fills=count) |>
  filter(moud=="bup") |>
  select(year,state,route,fills) |>
  filter(route %in% c("Oral","Injection")) |>
  mutate(year = as.factor(year)) |>
  filter(!(state %in% c("ZZ","PR","DC")))

### NSSATS
setDT(nssats)

# by state
bystate <- nssats[!(STATE %in% c("ZZ","PR","DC")),.(facilityn = .N,
                     cnt_Oral = sum(m_buporal, na.rm = TRUE),
                     cnt_Injection = sum(m_bupxr, na.rm = TRUE),
                     prop_Oral = mean(m_buporal, na.rm = TRUE),
                     prop_Injection = mean(m_bupxr, na.rm = TRUE)), by=.(year,STATE)]


cnt <- c("cnt_Oral", "cnt_Injection")
prop <- c("prop_Oral", "prop_Injection")
bystatelong <- melt(bystate, id.vars = c("year", "STATE", "facilityn"),
             measure.vars = list(cnt,prop),
             value.name = c("facilitycnt","facilityprop")) |> 
  mutate(route = case_when(variable==1 ~ "Oral",
                           variable==2 ~ "Injection",
                           .default = NA)) |>
  select(-variable) |>
  rename(state=STATE) 

# national
natnl <- nssats[!(STATE %in% c("ZZ","PR","DC")),.(facilityn = .N,
                     cnt_Oral = sum(m_buporal, na.rm = TRUE),
                     cnt_Injection = sum(m_bupxr, na.rm = TRUE),
                     prop_Oral = mean(m_buporal, na.rm = TRUE),
                     prop_Injection = mean(m_bupxr, na.rm = TRUE)), by=.(year)]

natnllong <- melt(natnl, id.vars = c("year", "facilityn"),
                    measure.vars = list(cnt,prop),
                    value.name = c("facilitycnt","facilityprop")) |> 
  mutate(route = case_when(variable==1 ~ "Oral",
                           variable==2 ~ "Injection",
                           .default = NA)) |>
  select(-variable) |>
  mutate(state="US")

# Combine
nssats_ <- rbind(bystatelong,natnllong) 

### Merge three data sources
merged <- sdud_ |>
  full_join(nssats_, by=c("year","state","route")) |>
  mutate(fills = case_when(is.na(fills) ~ 0,
                           .default = fills)) |>
  full_join(denom, by=c("state")) |>
  mutate(fills_per = fills/denom,
         facilities_per = facilitycnt/denom * 1000) |>
  arrange(state,year)

write.csv(merged,paste0(path,"data/clean/merged.csv"), row.names=FALSE)
saveRDS(merged,paste0(path,"data/clean/merged.rds"))

min(merged$fills_per,na.rm=TRUE)
max(merged$fills_per,na.rm=TRUE) 
min(merged$facilities_per,na.rm=TRUE)
max(merged$facilities_per,na.rm=TRUE) 
