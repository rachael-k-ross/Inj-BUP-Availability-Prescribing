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
library(maps)
library(mapproj)
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

# Counts
oral <- dat |>
  filter(state=="US") |>
  filter(route=="Oral") %>%
  ggplot(.,aes(x = year, y=fills, group = 1)) +
  geom_line(col="#2e4057"  , linewidth=1) +
  geom_point(size=2) +
  theme_classic() +
  theme(panel.grid.major = element_line(colour="lightgray", linewidth=0.5)) + 
  xlab("Year") +
  ylab("Prescription fills") + 
  scale_y_continuous(labels = comma) +
  scale_x_discrete(expand = c(.03,0)) 

inj <- dat |>
  filter(state=="US") |>
  filter(route=="Injection") %>%
  ggplot(.,aes(x = year, y=fills, group = 1)) +
  geom_line(col="#2e4057"  , linewidth=1) +
  geom_point(size=2) +
  theme_classic() +
  theme(panel.grid.major = element_line(colour="lightgray", size=0.5)) + 
  xlab("Year") +
  ylab("Prescription fills") + 
  scale_y_continuous(breaks=seq(0, 200000, 50000),
                     labels = comma,
                     expand = c(0, 5000),
                     limits = c(0,200000)) +
  scale_x_discrete(expand = c(.03,0)) 

ggarrange(inj,oral, labels=c("A","B"), nrow=1,
          font.label = list(size = 12))

# Rates
oral <- dat |>
  filter(state=="US") |>
  filter(route=="Oral") %>%
  ggplot(.,aes(x = year, y=fills_per, group = 1)) +
  geom_line(col="#2e4057"  , linewidth=1) +
  geom_point(size=2) +
  theme_classic() +
  theme(panel.grid.major = element_line(colour="lightgray", linewidth=0.5)) + 
  xlab("Year") +
  ylab("Fills per 100 beneficiaries") + 
  scale_y_continuous(labels = comma) +
  scale_x_discrete(expand = c(.03,0)) 

inj <- dat |>
  filter(state=="US") |>
  filter(route=="Injection") %>%
  ggplot(.,aes(x = year, y=fills_per, group = 1)) +
  geom_line(col="#2e4057"  , linewidth=1) +
  geom_point(size=2) +
  theme_classic() +
  theme(panel.grid.major = element_line(colour="lightgray", size=0.5)) + 
  xlab("Year") +
  ylab("Fills per 100 beneficiaries") + 
  #scale_y_continuous(breaks=seq(0, 200000, 50000),
                     #labels = comma,
                     #expand = c(0, 5000),
                     #limits = c(0,200000)) +
  scale_x_discrete(expand = c(.03,0)) 

ggarrange(inj,oral, labels=c("A","B"), nrow=1,
          font.label = list(size = 12))


############################################
# Annual changes in # per bene by state
############################################

fills_permax <- 20
fillsmax <- 5000

statesdat <- dat |>
  filter(state!="US") |>
  filter(state!="TN") |>
  filter(route=="Injection") |>
  dplyr::select(year,state_star,fills_per,fills) |>
  complete(year,state_star) |>
  mutate(arrowstart = 0,
         
         fills_per = ifelse(is.na(fills_per),0,fills_per)*100,
         fills_per_max = min(max(fills_per),fills_permax),
         fills_per_arrowend = ifelse(fills_per>fills_permax,fills_permax,NA),
         
         fills = ifelse(is.na(fills),0,fills),
         fills_max = min(max(fills),fillsmax),
         fills_arrowend = ifelse(fills>fillsmax,fillsmax,NA),
         .by=state_star) 

mycolors <- c("#2e5745",
              "#6c897c",
              "#abbbb4",
              "#b4abbb",
              "#7c6c89",
              "#452e57")


lollipop <- function(variable,max,axistitle){
  perflag <- grepl("per",deparse(substitute(variable)))
  
  var <- ensym(variable)
  varmax <- paste0(deparse(substitute(variable)),"_max")
  varend <- paste0(deparse(substitute(variable)),"_arrowend")
  stateorder <- statesdat |>
    filter(year==2022) |> 
    arrange(!!var) |> 
    #arrange(fills) |>
    pull(state_star)
  
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
      #legend.position = "top",
      legend.position = "none",
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
}



#rx <- lollipop(fills,fillsmax,"Number of prescription fills") 
lollipop(fills_per,fills_permax,"Fills per 100 beneficiaries")


# For tables on figures
# statesdat |>
#   filter(year==2022) |>
#   filter(fills >fillsmax) |>
#   dplyr::select(state_star,fills) |>
#   arrange(-fills) |>
#   flextable() |>
#   set_header_labels(state_star = "State",
#                     fills = "Number") |>
#   colformat_double(digits = 0
#   )

statesdat |>
  filter(year==2022) |>
  filter(fills_per >fills_permax) |>
  dplyr::select(state_star,fills_per) |>
  arrange(-fills_per) |>
  flextable() |>
  set_header_labels(state_star = "State",
                    fills_per = "Number") |>
  colformat_double(digits = 1
  )

############################################
# Maps
############################################

statesdat <- dat |>
  filter(state!="US") |>
  #filter(state!="TN") |>
  filter(route=="Injection") |>
  dplyr::select(year,state,fills_per,fills) |>
  complete(year,state) |>
  mutate(fills_per = ifelse(is.na(fills_per),0,fills_per)*100,
         fills = ifelse(is.na(fills),0,fills))
         

states <- map_data("state") |>
  mutate(Region = str_to_title(region)) |>
  full_join(as_tibble(cbind(state.abb,state.name)), by=c("Region"="state.name")) |>
  left_join(statesdat, by=c("state.abb"="state")) |>
  arrange(order) |>
  filter(year %in% c(2018,2019,2020,2021,2022)) |>
  mutate(fills_per = ifelse(fills_per >20,20,fills_per))

ak <- states |> filter(state.abb=="AK")
test <- map_data("state")
table(test$region)

mycolors <- c("white",#"#b4abbb",
              "#452e57")

ggplot(data=states, aes(long, lat)) +
  geom_polygon(aes(group=group, fill=fills_per)) +
  geom_polygon(aes(group=group), color="lightgray", 
               linewidth=.2,fill=NA) +
  coord_map() +
  facet_wrap(vars(year), nrow = 3) + 
  theme(
    strip.text.x = element_text(
      size = 12, face = "bold"
    )
  ) + 
  scale_fill_gradientn(colors=mycolors,
                       breaks=c(0,5,10,15,20),
                       labels=c("0","5","10","15",">20"),
                       na.value = "darkgray",
                       guide = guide_colorbar(
                         frame.colour = "black", 
                         ticks.colour = "black")) +
  theme(rect = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.70,.15),
        legend.direction="horizontal")




