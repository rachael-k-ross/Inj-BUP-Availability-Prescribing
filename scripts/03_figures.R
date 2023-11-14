############################################
#
# Availability and uptake of injection BUP
# Author: Rachael Ross
# Script: Figures
#
############################################

path <- "C:/Users/Rachael Ross/Local/Git/Inj-BUP-Availability-Prescribing/"

library(tidyverse)
library(data.table)
library(ggplot2)
library(scales)
library(ggpubr)
library(ggridges)
library(gt)
library(flextable)
#library(haven)

############################################
# Load data
############################################

dat <- readRDS(paste0(path,"data/clean/merged.rds")) |>
  mutate(state_star = ifelse(state %in% c("AK","AZ","CT","FL","GA","HI","IA","KS","LA",
                                          "MT","NE","NV","OK","OR","SD","TN","TX","WY"), paste0(state,"*"),state))

############################################
# National time trends
############################################

usdat <- dat |>
  filter(state=="US") |>
  filter(route=="Injection")

rxs <- ggplot(usdat,aes(x = year, y=fills, group = 1)) +
  #geom_point(size=2, col="steelblue") +
  geom_line(col="#2e4057"  , linewidth=1.5) +
  theme_classic() +
  theme(panel.grid.major = element_line(colour="lightgray", size=0.5)) + 
  #labs(x="Year",
  #     y="Prescriptions") + 
  xlab("Year") +
  ylab("Prescriptions") + 
  scale_y_continuous(breaks=seq(0, 200000, 50000), 
                     labels = comma,
                     expand = c(0, 5000),
                     limits = c(0,200000)) +
  scale_x_discrete(expand = c(.03,0))

facs <- ggplot(usdat,aes(x = year, y=facilitycnt, group = 1)) +
  #geom_point(size=2, col="steelblue") +
  geom_line(col="#2e4057"  , linewidth=1.5) +
  theme_classic() +
  theme(panel.grid.major = element_line(colour="lightgray", size=0.5)) + 
  #labs(x="Year",
  #     y="Prescriptions") + 
  xlab("Year") +
  ylab("Facilities") + 
  scale_y_continuous(breaks=seq(0, 2500, 500), 
                     labels = comma,
                     expand = c(0, 50),
                     limits = c(0,2500)) +
  scale_x_discrete(expand = c(.03,0))

ggarrange(facs,rxs, labels=c("A","B"), nrow=1,
          font.label = list(size = 12))

# mult = 100
# ggplot(usdat,aes(x = year, group = 1)) +
#   geom_line(aes(y=fills), col="steelblue", linewidth=1.5) +
#   geom_line(aes(y=facilitycnt*mult), col="green", linewidth=1.5) +
#   theme_classic() +
#   theme(panel.grid.major = element_line(colour="lightgray", size=0.5)) + 
#   #labs(x="Year",
#   #     y="Prescriptions") + 
#   xlab("Year") +
#   #ylab("Prescriptions") + 
#   scale_y_continuous(name = "Prescriptions",
#                      breaks=seq(0, 150000, 50000), 
#                      labels = comma,
#                      #expand = c(0, 5000),
#                      sec.axis = sec_axis(~./mult,name = "Facilities",
#                                          breaks=seq(0,2500,500)))

############################################
# 2021 by state
############################################

statesdat <- dat |>
  filter(state!="US") |>
  filter(route=="Injection") |>
  filter(year==2021) |>
  arrange(fills)

stateorder = statesdat[[2]]


mult = 100
ggplot(statesdat,aes(y=state, group = 1)) +
  geom_point(aes(x=fills), size=2, col="#2e4057") +
  geom_point(aes(x=facilitycnt*mult), size=2, col="#edae49") +
  scale_y_discrete(limits=stateorder) +
    scale_x_continuous(name = "Prescriptions (Navy)",
                       #breaks=seq(0, 300, 50),
                       labels = comma,
                       #expand = c(0, 200),
                       sec.axis = sec_axis(~./mult,name = "Facilities (Gold)")) +
  theme_classic() +
  theme(panel.grid.major = element_line(colour="lightgray", size=0.5)) +
  ylab("State")
 
"#2e4057"  
"#66a182"

############################################
# Distributions by state
############################################

statesdat <- dat |>
  filter(state!="US") |>
  filter(route=="Injection") |>
  arrange(fills)

names(statesdat)

facdist <- ggplot(statesdat |> filter(year!=2022), aes(x = facilitycnt, y = year, group = year)) + 
  geom_density_ridges(fill = "#2e4057", 
                      #rel_min_height = 0.02,
                      scale = 3, 
                      alpha = 0.7,
                      ) +
  ylab("Year") + 
  xlab("Facilities") +
  theme_classic() +
  scale_y_discrete(expand = c(0, .2)) +
  scale_x_continuous(expand = c(.01,0),
                     limits = c(0,200)) + 
  theme(panel.grid.major = element_line(colour="lightgray", size=0.5))

rxdist <- ggplot(statesdat, aes(x = fills, y = year, group = year)) + 
  geom_density_ridges(fill = "#2e4057", 
                      #rel_min_height = 0.02,
                      scale = 3, 
                      labels = comma,
                      alpha = 0.7,
  ) +
  ylab("Year") + 
  xlab("Prescriptions") +
  theme_classic() +
  scale_y_discrete(expand = c(0, .2)) +
  scale_x_continuous(expand = c(.01,0),
                     limits = c(0,10000),labels = comma) +
  theme(panel.grid.major = element_line(colour="lightgray", size=0.5)) 
  
ggarrange(facdist,rxdist, align="h")

############################################
# Annual changes by state
############################################

fillsmax <- 5000
facilitycntmax <- 200 
facilitypropmax <- 1
fillspropmax <- .08

statesdat <- dat |>
  filter(state!="US") |>
  filter(route=="Injection") |>
  dplyr::select(year,state_star,fills,facilitycnt,facilityn) |>
  complete(year,state_star) |>
  left_join(dat |>
              filter(state!="US") |>
              filter(route=="Oral") |>
              dplyr::select(year,state_star,fills,facilitycnt) |>
              rename(fillsoral = fills,
                     faccntoral = facilitycnt),
            by=c("year","state_star")) |>
  mutate(arrowstart = 0,
         fills = ifelse(is.na(fills),0,fills),
         fills_max = min(max(fills),fillsmax),
         fills_arrowend = ifelse(fills>fillsmax,fills_max,NA),
         fillsprop = fills/(fills + fillsoral),
         fillsprop_max = min(max(fillsprop),fillspropmax),
         fillsprop_arrowend = ifelse(fillsprop>fillspropmax,fillsprop_max,NA),
         facilitycnt_max = min(max(facilitycnt, na.rm = TRUE),facilitycntmax),
         facilitycnt_arrowend = ifelse(facilitycnt>facilitycntmax,facilitycnt_max,NA),
         facilityprop = facilitycnt/facilityn,
         facilityprop_max = min(max(facilityprop, na.rm = TRUE),facilitypropmax),
         facilityprop_arrowend = ifelse(facilityprop>facilitypropmax,facilityprop_max,NA),
         facproporal = ifelse(year==2021,faccntoral/facilityn,NA),
         .by=state_star) 

#mean(statesdat$facproporal, na.rm=TRUE)

#stateorder <- statesdat |> filter(year==2022) |> arrange(fills) |> pull(state_star)


# mycolors <- c("#eaebee",
#               "#c0c5cc",
#               "#969fab",
#               "#6c7989",
#               "#425367",
#               "#2e4057")

mycolors <- c("#2e5745",
              "#6c897c",
              "#abbbb4",
              "#b4abbb",
              "#7c6c89",
              "#452e57")


lollipop <- function(variable,max,axistitle){
  fillsflag <- grepl("fills",deparse(substitute(variable)))
  
  var <- ensym(variable)
  varmax <- paste0(deparse(substitute(variable)),"_max")
  varend <- paste0(deparse(substitute(variable)),"_arrowend")
  stateorder <- statesdat |>
    filter(year==if(fillsflag==TRUE){2022}else{2021}) |> arrange(!!var) |> pull(state_star)
  # stateorder <- statesdat |> 
  #   filter(year==2021) |> arrange(facilitycnt) |> pull(state)
  
  statesdat |>
  ggplot() +
  geom_segment( aes(x=state_star, xend=state_star, 
                    y=!!var, yend=!!sym(varmax)), color="darkgrey") +
  geom_segment( aes(x=state_star, xend=state_star,
                    y=arrowstart, yend=!!sym(varend), group=year), lineend = "round", linejoin = "round",
                color="darkgrey", arrow = arrow(length = unit(0.07, "inches"))) +
  geom_point( aes(x=state_star, y=!!var, group=year, color=year),  size=2) + 
  coord_flip(clip = "off")+
  scale_x_discrete(limits=stateorder) + 
  xlab("") +
  ylab(axistitle) +
  theme_classic() + 
  theme(
    legend.position = "top",
    #legend.position = "none",
    legend.title = element_blank(),
    panel.grid.major.x = element_line(color = "lightgray",
                                        linewidth = 0.5,
                                        linetype = 2)
  ) +
  scale_y_continuous(expand = c(.01,0),
                     limits = c(0,max),labels = comma)+
   scale_color_manual(values = mycolors) +
    scale_fill_manual(values = mycolors) +
  guides(color=guide_legend(nrow=1)) +
    theme(plot.margin = margin(5.5, 9, 5.5, 5.5, "pt")) 
  # +
  #   annotate(geom = "table", x = 4500, y = "DE", label = list(numtab), 
  #            vjust = 1, hjust = 0)
}

statesdat |>
  filter(year==2022) |>
  filter(fills >fillsmax) |>
  dplyr::select(state_star,fills) |>
  arrange(-fills) |>
  flextable() |>
    set_header_labels(state_star = "State",
             fills = "Number") |>
  colformat_double(digits = 0
  )

statesdat |>
  filter(year==2021) |>
  filter(facilitycnt >facilitycntmax) |>
  dplyr::select(state_star,facilitycnt) |>
  arrange(-facilitycnt) |>
  flextable() |>
  set_header_labels(state_star = "State",
                    facilitycnt = "Number") |>
  colformat_double(digits = 0
  )

numrx <- lollipop(fills,fillsmax,"Number of prescriptions") 
lollipop(fillsprop,fillspropmax,"Proportion of buprenorphine prescriptions that were injectable")
numfac <- lollipop(facilitycnt,facilitycntmax,"Number of facilities")
lollipop(facilityprop,facilitypropmax,"Proportion of facilities offering injectable buprenorphine")
  
ggarrange(numfac,numrx, labels=c("A","B"), nrow=1,
          font.label = list(size = 12))

# stateorder <- statesdat |> 
#   filter(year==2021) |> arrange(facilityprop) |> pull(state)
#   
# statesdat |>
#   ggplot() +
#   geom_segment( aes(x=state, xend=state, 
#                     y=facilityprop, yend=facilityprop_max), color="darkgrey") +
#   geom_point( aes(x=state, y=facilityprop, group=year, color=year),  size=2) + 
#   geom_point( aes(x=state, y=facproporal),  color= "#66a182", size=2) +
#   coord_flip()+
#   scale_x_discrete(limits=stateorder) + 
#   xlab("") +
#   ylab("Fac proportion") +
#   theme_classic() + 
#   theme(
#     legend.position = "top",
#     legend.title = element_blank(),
#     panel.grid.major.x = element_line(color = "lightgray",
#                                       size = 0.5,
#                                       linetype = 2)
#   ) +
#   scale_y_continuous(expand = c(.01,0),
#                      limits = c(0,.75),labels = comma)+
#   scale_color_manual(values = mycolors) +
#   scale_fill_manual(values = mycolors) +
#   guides(color=guide_legend(nrow=1))

