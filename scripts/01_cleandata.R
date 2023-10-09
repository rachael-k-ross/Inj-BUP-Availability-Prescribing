############################################
#
# Availability and update of injection BUP
# Author: Rachael Ross
# Script: Cleaning the raw data
#
############################################

path <- "C:/Users/Rachael Ross/Local/Git/Inj-BUP-Availability-Prescribing/"

library(tidyverse)
library(data.table)
library(readxl)
#library(haven)

############################################
# NSSATS and NSUMHSS data
############################################

### Path
nssatspath <- paste0(path,"data/nssats/")

inname <- "NSSATS_PUF_2017_R"

### Load data files
toload <- function(inname){
  load(paste0(nssatspath,inname,".RData"))
  if (exists("PUF")) {PUF
  }else{df}
}

dat17 <- toload("NSSATS_PUF_2017_R")
dat18 <- toload("NSSATS_PUF_2018_R")
dat19 <- toload("NSSATS_PUF_2019_R")
dat20 <- toload("NSSATS_PUF_2020_r")
dat21 <- toload("NSUMHSS_2021_PUF_R")

### 2017 data
dat17_clean <- dat17 %>%
  rowwise() %>% 
  mutate(anymoud = ifelse(sum(SRVC108,SRVC85,SRVC87,SRVC86,SRVC129,na.rm=TRUE)>0,1,
                          ifelse(is.na(sum(SRVC108,SRVC85,SRVC87,SRVC86,SRVC129)),NA,1)),
         anyagonist = ifelse(sum(SRVC85,SRVC87,SRVC86,SRVC129,na.rm=TRUE)>0,1,
                             ifelse(is.na(sum(SRVC85,SRVC87,SRVC86,SRVC129)),NA,0)),
         buporal = ifelse(SRVC87==1|SRVC86==1,1,ifelse(SRVC87==0&SRVC86==0,0,NA)),
         bupxr = 0,
         bupimplant = ifelse(SRVC129==1,1,ifelse(SRVC129==0,0,NA)),
         medicaid = ifelse(REVCHK5==1,1,ifelse(REVCHK5==0,0,NA)),
         year=2017) |>
  filter(anymoud==1) |>
  select(anyagonist,buporal,bupxr,bupimplant,medicaid,year,STATE)

### 2018-2020 data (have the same structure)
toclean <- function(dat,year){
  dat %>%
    rowwise() %>% 
    mutate(anymoud = ifelse(sum(SRVC108,SRVC85,SRVC87,SRVC86,SRVC129,SRVC130,na.rm=TRUE)>0,1,
                            ifelse(is.na(sum(SRVC108,SRVC85,SRVC87,SRVC86,SRVC129,SRVC130)),NA,1)),
           anyagonist = ifelse(sum(SRVC85,SRVC87,SRVC86,SRVC129,SRVC130,na.rm=TRUE)>0,1,
                               ifelse(is.na(sum(SRVC85,SRVC87,SRVC86,SRVC129,SRVC130)),NA,0)),
           buporal = ifelse(SRVC87==1|SRVC86==1,1,ifelse(SRVC87==0&SRVC86==0,0,NA)),
           bupxr = ifelse(SRVC130==1,1,0),
           bupimplant = ifelse(SRVC129==1,1,ifelse(SRVC129==0,0,NA)),
           medicaid = ifelse(REVCHK5==1,1,ifelse(REVCHK5==0,0,NA)),
           year=year) |>
    filter(anymoud==1) |>
    select(anyagonist,buporal,bupxr,bupimplant,medicaid,year,STATE)
}

dat18_clean <- toclean(dat18,2018)
dat19_clean <- toclean(dat19,2019)
dat20_clean <- toclean(dat20,2020)

### 2021 data 
dat21_clean <- dat21 %>%
  rowwise() %>% 
  mutate(moud_sum=sum(SRVC108,SRVC85,SRVC87,SRVC86,SRVC129,SRVC130,na.rm=TRUE),
         agon_sum=sum(SRVC85,SRVC87,SRVC86,SRVC129,SRVC130,na.rm=TRUE),
         moud_sumna=sum(SRVC108,SRVC85,SRVC87,SRVC86,SRVC129,SRVC130),
         agon_sumna=sum(SRVC85,SRVC87,SRVC86,SRVC129,SRVC130),) %>%
  ungroup() %>%
  mutate(anymoud = ifelse(moud_sum>0,1,
                          ifelse(is.na(moud_sumna),NA,1)),
         anyagonist = ifelse(agon_sum>0,1,
                             ifelse(is.na(agon_sumna),NA,0)),
         buporal = ifelse(SRVC87==1|SRVC86==1,1,ifelse(SRVC87==0&SRVC86==0,0,NA)),
         bupxr = ifelse(SRVC130==1,1,0),
         bupimplant = ifelse(SRVC129==1,1,0),
         medicaid = ifelse(REVCHK5_SU==1,1,ifelse(REVCHK5_SU==0,0,NA)),
         year=2021) |>
  filter(anymoud==1) |>
  select(anyagonist,buporal,bupxr,bupimplant,medicaid,year,LOCATIONSTATE) |>
  rename(STATE=LOCATIONSTATE)

### Combine data
dat <- rbind(dat17_clean,dat18_clean,dat19_clean,dat20_clean,dat21_clean) %>%
  ungroup() %>%
  mutate(year=factor(year),
         anybup = case_when(buporal==1 ~ 1,
                            bupxr==1 ~ 1,
                            bupimplant==1 ~ 1,
                            buporal+bupxr+bupimplant==0 ~ 0,
                            .default = NA),
         m_anybup = case_when(anybup==1 & medicaid == 1 ~ 1,
                              anybup %in% c(0,1) & medicaid %in% c(0,1) ~ 0,
                              .default = NA),
         m_buporal = case_when(buporal==1 & medicaid == 1 ~ 1,
                               buporal %in% c(0,1) & medicaid %in% c(0,1) ~ 0,
                               .default = NA),
         m_bupxr = case_when(bupxr==1 & medicaid == 1 ~ 1,
                             bupxr %in% c(0,1) & medicaid %in% c(0,1) ~ 0,
                             .default = NA),
         m_bupimplant = case_when(bupimplant==1 & medicaid == 1 ~ 1,
                                  bupimplant %in% c(0,1) & medicaid %in% c(0,1) ~ 0,
                                  .default = NA)) 
setDT(dat)
dat[,.N,by=STATE]

### Save
saveRDS(dat,paste0(path,"data/clean/nssatsclean.rds"))

############################################
# SDUD data
############################################

### Path
sdudpath <- paste0(path,"data/sdud/")

### NDC lists
bup_ndc <- read_csv(paste0(path,"codes/bup_ndc.csv")) |> select(code,moud,route)
ntx_ndc <- read_csv(paste0(path,"codes/ntx_ndc.csv")) |> select(code,moud,route)
ndcs <- rbind(bup_ndc,ntx_ndc)

### Load data
toload <- function(file){
  dat <- read_csv(paste0(sdudpath,file)) |> 
    filter(ndc %in% ndcs$code) |>
    select(year,quarter,state,number_of_prescriptions,ndc,suppression_used) |>
    mutate(number=case_when(suppression_used==TRUE ~ 10, # impute suppressed with 10
                            is.na(number_of_prescriptions) ~ 0,
                            .default=as.numeric(number_of_prescriptions))) |>
    left_join(ndcs, by=c("ndc"="code")) |>
    as.data.table()
  
  summed <- dat[,.(count=sum(number)),by=.(state,year,quarter,moud,route)]
  return(summed)
}

filelist <- list.files(sdudpath, pattern = "*\\.csv$") 
datlist <- map(filelist,toload)
datall <- rbind(datlist[[1]],datlist[[2]],datlist[[3]],datlist[[4]],datlist[[5]],datlist[[6]]) |>
  mutate(route=str_to_title(route))

datbyyr <- datall[,.(count=sum(count)),by=.(year,state,moud,route)
                  ][,state := case_when(state=="XX"~"US",.default = state)]

### Save
saveRDS(datbyyr,paste0(path,"data/clean/sdudclean.rds"))

############################################
# Denominator
############################################

### Path
denompath <- paste0(path,"data/denom/")

### Load data
denom <- read_excel(paste0(denompath,"SUD Dashboard (2020).xlsx"))[,c(1,5,6)] |>
  rename(num19 = "Number of Medicaid beneficiaries treated for an OUD in 2019",
         num20 = "Number of Medicaid beneficiaries treated for an OUD in 2020") |>
  full_join(as_tibble(cbind(state.abb,state.name)), by=c("State"="state.name")) |>
  mutate(state = case_when(State == "United States" ~ "US",
                           .default = state.abb)) |>
  filter(!is.na(state) & state!="TN") |>
  mutate(num19 = as.numeric(num19),
         num20 = as.numeric(num20),
         denom = (num19 + num20)/2) |>
  select(state,denom)

### Save
saveRDS(denom,paste0(path,"data/clean/denom.rds"))

