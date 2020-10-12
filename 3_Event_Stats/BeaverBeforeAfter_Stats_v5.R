###Title: BeaverBeforeAfter_Stats_v5
#### Script for analysis and summarizing results of Event Extraction from monitoring of beaver sites using method from ##Project eventEx by J.Ashe
#### As used in  Beaver dams attenuate floods: a multi-site, multi-scale study (in prep) Alan Puttock*, Hugh A. Graham, Josie Ashe, David J. Luscombe and Richard E. Brazier)
#lead and corresponding author. Email: a.k.puttock@exeter.ac.uk
#### V5 12/10/2020

##setup for working offline. If working from Github Combined_Beaver_Hydro:
# full Q and rainfall record for each site in 1_Clean_Flow_Rainfall_Data
# event extraction metrics for each site are in 2_Event_Extraction > OutputData

#Packages required problems with r reverting to a library on U drive so need to set lib path using .libPaths

.libPaths("C:/Program Files/R/R-3.6.3/library")
library(tidyverse)
library(mgcv)
library(fANCOVA)
library(sm)
library(ISLR)
library(voxel)
library(gridExtra)
library(lubridate)
library(lme4)
library(boot)
library(broom)
library(broom.mixed)
library(tcltk)
library(ggfortify)
library(GGally)
library(patchwork)
library(dplyr)
library(tcltk)
library(quantreg)
library(ggpmisc)
library(gridExtra)
library(grid)
library(performance)
library(ggpubr)
library(glm2)
library(cowplot)
library(emmeans)
library(gt)
library(webshot)
library(processx)
library(png)

setwd("C:/Users/akp211/Google Drive/BeaverProject/BeaverCombined/CombinedHydrologyPaper/DataAnalysis/event_Stats_Current")

# -------------- Functions ------------------------------

# glm plotting function

glm.plot <- function(.data, model.data, title) {
  ggplot(.data, aes(x=rain.tot.mm, y=Q.peak.m3.s, colour=Beaver, fill=Beaver))+
    geom_point(alpha = 0.5, size=0.8)+
    geom_line(data=model.data, aes(x=rain.tot.mm, y = .fitted)) +
    geom_ribbon(data=model.data,aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)),
                alpha=0.2, linetype=2, lwd=0.2) +
    scale_color_manual(values = c('#A6190D', '#244ED3')) +
    scale_fill_manual(values = c('#A6190D', '#244ED3')) +
    coord_cartesian(ylim=c(0,1.5))+
    labs(x= ("Total Event Rainfall (mm) "),
         y= (expression("Peak Q   " (m^{3}~s^{-1}))),
         colour = "Beaver Present", 
         fill = "Beaver Present") +
    ggtitle(title) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
}


#glm plotting function 2
# plotting...
glm.plot2 <- function(.data, model.data, title) {
  ggplot(.data, aes(x=rain.tot.mm, y=Q.peak.m3.s, colour=Beaver, fill=Beaver))+
    geom_point(alpha = 0.5, size=0.8)+
    geom_line(data=model.data, aes(x=rain.tot.mm, y = .fitted)) +
    geom_ribbon(data=model.data,aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)), 
                alpha=0.2, linetype=2, lwd=0.2) +
    scale_color_manual(values = c('#A6190D', '#244ED3')) +
    scale_fill_manual(values = c('#A6190D', '#244ED3')) +
    coord_cartesian(ylim=c(0,1.5))+
    labs(x= ("Total Event Rainfall (mm) "),
         y= (expression("Peak Q   " (m^{3}~s^{-1}))),
         colour = "Beaver Present", 
         fill = "Beaver Present") +
    ggtitle(title) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 11),
          strip.text.x = element_text(size = 12, color = "black", face = "italic"),
          strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3),
          legend.position = 'top')
}

# plotting... with increased Y limit for EBud

glm.plot3 <- function(.data, model.data, title) {
  ggplot(.data, aes(x=rain.tot.mm, y=Q.peak.m3.s, colour=Beaver, fill=Beaver))+
    geom_point(alpha = 0.5, size=0.8)+
    geom_line(data=model.data, aes(x=rain.tot.mm, y = .fitted)) +
    geom_ribbon(data=model.data,aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)), 
                alpha=0.2, linetype=2, lwd=0.2) +
    scale_color_manual(values = c('#A6190D', '#244ED3')) +
    scale_fill_manual(values = c('#A6190D', '#244ED3')) +
    coord_cartesian(ylim=c(0,6))+
    labs(x= ("Total Event Rainfall (mm) "),
         y= (expression("Peak Q   " (m^{3}~s^{-1}))),
         colour = "Beaver Present", 
         fill = "Beaver Present") +
    ggtitle(title) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 11),
          strip.text.x = element_text(size = 12, color = "black", face = "italic"),
          strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3),
          legend.position = 'top')
}



add.stat.tab <- function(.model){ # function to create a table for additive model
  tidy(.model) %>%
    mutate_at(vars(estimate, std.error, statistic), round,3) %>%
    mutate(p.value = ifelse(p.value < 0.001, '< 0.001 **', 
                            ifelse(p.value < 0.05, paste(formatC(p.value,format = "f", 3), '*', sep = " "),
                                   ifelse(p.value < 0.1, paste(formatC(p.value,format = "f", 3), '.', sep = " "),
                                          formatC(p.value,format = "f", 3))))) %>%
    rename(T.statistic = statistic) %>%
    mutate(term = c('Intercept', 'Beaver', 'Total Rainfall'))
}

glm1.tab <- function(model.tab){
  tableGrob(model.tab, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.55)),
                                                         colhead = list(fg_params=list(cex = 0.55)),
                                                         rowhead = list(fg_params=list(cex = 0.55)),
                                                         padding = unit(c(2,2),"mm")))
}



inter.stat.tab <- function(.model){
  tidy(.model) %>%
    mutate_at(vars(estimate, std.error, statistic), round,3) %>%
    mutate(p.value = ifelse(p.value < 0.001, '< 0.001 **', 
                            ifelse(p.value < 0.05, paste(formatC(p.value,format = "f", 3), '*', sep = " "),
                                   ifelse(p.value < 0.1, paste(formatC(p.value,format = "f", 3), '.', sep = " "),
                                          formatC(p.value,format = "f", 3))))) %>%
    rename(T.statistic = statistic) %>%
    mutate(term = c('Intercept', 'Total Rainfall', 'Wet Season', 'Beaver', 'Beaver:Wet Season'))
}

add.stat.tab <- function(.model){ # function to create a table for additive model
  tidy(.model) %>%
    mutate_at(vars(estimate, std.error, statistic), round,3) %>%
    mutate(p.value = ifelse(p.value < 0.001, '< 0.001 **', 
                            ifelse(p.value < 0.05, paste(formatC(p.value,format = "f", 3), '*', sep = " "),
                                   ifelse(p.value < 0.1, paste(formatC(p.value,format = "f", 3), '.', sep = " "),
                                          formatC(p.value,format = "f", 3))))) %>%
    rename(T.statistic = statistic) %>%
    mutate(term = c('Intercept', 'Total Rainfall', 'Beaver'))
}


#marginal means function

emmeans.tab <- function(.emmeans.obj){
  tidy(.emmeans.obj) %>%
    select(-df) %>%
    select(-asymp.LCL) %>%
    select(-asymp.UCL) %>%
   
    mutate_at(vars(estimate, std.error), round,3)
  
}

#presentation tables for paper
pretty.tab <- function(model.tab, title, size.cm){
  model.tab %>%
    gt() %>%
    tab_header(
      title = md(sprintf('**<div style="text-align: left"> %s </div>**', title))) %>% 
    gtsave(.,tempfile('tab', fileext = '.png')) %>%
    readPNG(.) %>%
    rasterGrob( interpolate=TRUE, width = unit(size.cm,"cm"))
}



##########Section 1 data input and formatting 

###now split up per site for four sites.

###Site 1 Cornwall Beaver Project at Woodland Valley Farm- abbreviation 'WV'

# event data read_csv("./data/eventEx_EVENTS_WVall.csv")
Hyd_dat_WV <- read_rds("./data/eventEx_EVENTS_metrics_WV4.rds")

#all flow data for peak excedence values
all_flow_WV <- read_csv("./data/FullQdata/event-dat_WV_Qall5_NR.csv") %>% # reload flow inout for calculating correct Excedence...
  select(q) %>%
  drop_na()

#To split up all flow data into periods when beaver were present make sure to check dates!!
# head(Hyd_dat_WV)

all_QR_WV <- read_csv("./data/FullQdata/event-dat_WV_Qall5_NR.csv") %>%
  drop_na()


## *date time to POSIXct (chose correct format based on structure)
all_QR_WV$datetime  <- as.POSIXct(strptime(as.character(all_QR_WV$datetime),                # N.B. strptime() converts from character to POSIXct time
                                         #"%Y-%m-%d %H:%M:%S",                          # Format of origional text date time
                                         "%d/%m/%Y %H:%M",                            # (alternative) Format of origional text date time
                                         tz = "UTC"))                                 # (the UTC specification needed to avoid GMT to BST bug!)


all_QR_WV$Beaver <- "No"
all_QR_WV$Beaver[all_QR_WV$datetime > as.POSIXct("2017-06-17 00:00", "%Y-%m-%d %H:%M", tz = "UTC")] <- "Yes"
all_QR_WV$Beaver[all_QR_WV$datetime > as.POSIXct("2017-03-14 00:00", "%Y-%m-%d %H:%M", tz = "UTC") & 
                     all_QR_WV$datetime < as.POSIXct("2017-06-17 00:00", "%Y-%m-%d %H:%M", tz = "UTC")] <- "Unsure"
all_QR_WV$Beaver <- as.factor(all_QR_WV$Beaver)

summary(all_QR_WV)

#To split up event data into periods when beaver were present make sure to check dates!!
# head(Hyd_dat_WV)
Hyd_dat_WV$Beaver <- "No"
Hyd_dat_WV$Beaver[Hyd_dat_WV$event.start.ts > as.POSIXct("2017-06-17 00:00", "%Y-%m-%d %H:%M", tz = "UTC")] <- "Yes"
Hyd_dat_WV$Beaver[Hyd_dat_WV$event.start.ts > as.POSIXct("2017-03-14 00:00", "%Y-%m-%d %H:%M", tz = "UTC") & 
                 Hyd_dat_WV$event.start.ts < as.POSIXct("2017-06-17 00:00", "%Y-%m-%d %H:%M", tz = "UTC")] <- "Unsure"
Hyd_dat_WV$Beaver <- as.factor(Hyd_dat_WV$Beaver)

head(Hyd_dat_WV)
tail(Hyd_dat_WV)
summary(Hyd_dat_WV)


# To include seasons, hydrological year and site name
Hyd_dat_WV4 <- Hyd_dat_WV %>%
  filter(Beaver != "Unsure" & flimb.dur@.Data > 0 & rlimb.dur@.Data > 0 & Q.peak.m3.s >0) %>%
  # filter(Q.peak.m3.s > quantile(Q.peak.m3.s, 0.7))%>%
  mutate(rain.rate = rain.tot.mm/rain.dur@.Data) %>%
  mutate(flow.rate = Q.response.tot.m3/quickflow.dur@.Data) %>%
  mutate(init.rain.rate = (init.rain.tot.mm/init.rain.dur@.Data)*60*60) %>%
  mutate(Month = (month(event.start.ts))) %>%
  mutate(Year = as.factor(year(event.start.ts))) %>%
  mutate(rlimb.dur.hrs = (rlimb.dur@.Data/60)/60)%>%
  mutate(flimb.dur.hrs = (flimb.dur@.Data/60)/60)%>%
  mutate(Season = as.factor((ifelse(Month >= 3 & Month < 6, "Spring",
                                    (ifelse(Month >=6 & Month <9, "Summer",
                                            (ifelse(Month >=9 & Month <12, "Autumn",
                                                    (ifelse(Month >=12 | Month <3, "Winter", ""))))))))))%>%
  mutate(HydYear = as.factor((ifelse(Month >= 4 & Month < 10, "Dry","Wet"))))%>%
  mutate(Site = "WV")


##for peak excedance
perc_flow_WV<- ecdf(all_flow_WV$q) # calculate Empirical Cumulative Distribution for original Q data

Hyd_dat_WV4 <- Hyd_dat_WV4 %>%
  mutate(per_q = (1-perc_flow_WV(Q.peak.m3.s)) * 100)

head(Hyd_dat_WV4)
tail(Hyd_dat_WV4)
summary(Hyd_dat_WV4)



###Site 2 East Budleigh gauge downstream of Yettington beavers on R.Otter - abbreviation 'EBud'

# read_csv("./data/eventEx_EVENTS_WVall.csv")
Hyd_dat_EBud <- read_rds("./data/EBud_V4_metrics.rds")

all_flow_EBud <- read_rds("./data/FullQdata/EastBud_Q_R_S_ts.rds") %>% # reload flow inout for calculating correct Excedence...
  select(q) %>%
  drop_na()

all_QR_EBud <- read_rds("./data/FullQdata/EastBud_Q_R_S_ts.rds") %>% # reload flow inout for calculating correct Excedence...
  drop_na()

all_QR_EBud$Beaver <- "No"
all_QR_EBud$Beaver[all_QR_EBud$datetime  > as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC")] <- "Yes"
all_QR_EBud$Beaver[all_QR_EBud$datetime  > as.POSIXct("2016-08-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC") & 
                      all_QR_EBud$datetime  < as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC")] <- "Unsure"
all_QR_EBud$Beaver <- as.factor(all_QR_EBud$Beaver)

summary(all_QR_EBud)
  
#To split up data into periods when beaver were present make sure to check dates!!
# head(Hyd_dat_EBud)
Hyd_dat_EBud$Beaver <- "No"
Hyd_dat_EBud$Beaver[Hyd_dat_EBud$event.start.ts > as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC")] <- "Yes"
Hyd_dat_EBud$Beaver[Hyd_dat_EBud$event.start.ts > as.POSIXct("2016-08-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC") & 
                      Hyd_dat_EBud$event.start.ts < as.POSIXct("2017-01-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC")] <- "Unsure"
Hyd_dat_EBud$Beaver <- as.factor(Hyd_dat_EBud$Beaver)
  
head(Hyd_dat_EBud)
tail(Hyd_dat_EBud)
summary(Hyd_dat_EBud)
  
  
  # To include seasons and hydrological year
Hyd_dat_EBud4 <- Hyd_dat_EBud %>%
    filter(Beaver != "Unsure" & flimb.dur@.Data > 0 & rlimb.dur@.Data > 0 & Q.peak.m3.s >0) %>%
    # filter(Q.peak.m3.s > quantile(Q.peak.m3.s, 0.7))%>%
    mutate(rain.rate = rain.tot.mm/rain.dur@.Data) %>%
    mutate(flow.rate = Q.response.tot.m3/quickflow.dur@.Data) %>%
    mutate(init.rain.rate = (init.rain.tot.mm/init.rain.dur@.Data)*60*60) %>%
    mutate(Month = (month(event.start.ts))) %>%
    mutate(Year = as.factor(year(event.start.ts))) %>%
    mutate(rlimb.dur.hrs = (rlimb.dur@.Data/60)/60)%>%
    mutate(flimb.dur.hrs = (flimb.dur@.Data/60)/60)%>%
    mutate(Season = as.factor((ifelse(Month >= 3 & Month < 6, "Spring",
                                      (ifelse(Month >=6 & Month <9, "Summer",
                                              (ifelse(Month >=9 & Month <12, "Autumn",
                                                      (ifelse(Month >=12 | Month <3, "Winter", ""))))))))))%>%
    mutate(HydYear = as.factor((ifelse(Month >= 4 & Month < 10, "Dry","Wet"))))%>%
    mutate(Site = "EBUD")
  
 
##for peak excedance

perc_flow_EBud<- ecdf(all_flow_EBud$q) # calculate Empirical Cumulative Distribution for original Q data

Hyd_dat_EBud4 <- Hyd_dat_EBud4 %>%
  mutate(per_q = (1 - perc_flow_EBud(Q.peak.m3.s)) * 100)

head(Hyd_dat_EBud4)
tail(Hyd_dat_EBud4)
summary(Hyd_dat_EBud4)


###Site 3 Forest of Dean Beaver Project - abbreviation 'FoD'

# read_csv("./data/eventEx_EVENTS_WVall.csv")
Hyd_dat_FoD <- read_rds("./data/eventEx_FOD5_metrics.rds")

#all flow data for peak excedence values
all_flow_FoD<- read_csv("./data/FullQdata/event-dat_FoD_Qall_NR.csv") %>% # reload flow inout for calculating correct Excedence...
  select(q) %>%
  drop_na()

#To split up all flow data into periods when beaver were present make sure to check dates!!
# head(Hyd_dat_WV)

all_QR_FoD <- read_csv("./data/FullQdata/event-dat_FoD_Qall_NR.csv") %>%
  drop_na()


## *date time to POSIXct (chose correct format based on structure)
all_QR_FoD$datetime  <- as.POSIXct(strptime(as.character(all_QR_FoD$datetime),                # N.B. strptime() converts from character to POSIXct time
                                           #"%Y-%m-%d %H:%M:%S",                          # Format of origional text date time
                                           "%d/%m/%Y %H:%M",                            # (alternative) Format of origional text date time
                                           tz = "UTC"))                                 # (the UTC specification needed to avoid GMT to BST bug!)


all_QR_FoD$Beaver <- "No"
all_QR_FoD$Beaver[all_QR_FoD$datetime > as.POSIXct("2018-07-24 00:00", "%Y-%m-%d %H:%M", tz = "UTC")] <- "Yes"
all_QR_FoD$Beaver[all_QR_FoD$datetime > as.POSIXct("2019-05-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC") & 
                   all_QR_FoD$datetime < as.POSIXct("2019-09-19 00:00", "%Y-%m-%d %H:%M", tz = "UTC")] <- "Unsure"
all_QR_FoD$Beaver <- as.factor(all_QR_FoD$Beaver)

summary(all_QR_FoD)

#To split up data into periods when beaver were present make sure to check dates!!
# head(Hyd_dat_FoD)
Hyd_dat_FoD$Beaver <- "No"
Hyd_dat_FoD$Beaver[Hyd_dat_FoD$event.start.ts > as.POSIXct("2018-07-24 00:00", "%Y-%m-%d %H:%M", tz = "UTC")] <- "Yes"
Hyd_dat_FoD$Beaver[Hyd_dat_FoD$event.start.ts > as.POSIXct("2019-05-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC") & 
                      Hyd_dat_FoD$event.start.ts < as.POSIXct("2019-09-19 00:00", "%Y-%m-%d %H:%M", tz = "UTC")] <- "Unsure"
Hyd_dat_FoD$Beaver <- as.factor(Hyd_dat_FoD$Beaver)

head(Hyd_dat_FoD)
tail(Hyd_dat_FoD)
summary(Hyd_dat_FoD)


# To include seasons and hydrological year
Hyd_dat_FoD4 <- Hyd_dat_FoD %>%
  filter(Beaver != "Unsure" & flimb.dur@.Data > 0 & rlimb.dur@.Data > 0 & Q.peak.m3.s >0) %>%
  # filter(Q.peak.m3.s > quantile(Q.peak.m3.s, 0.7))%>%
  mutate(rain.rate = rain.tot.mm/rain.dur@.Data) %>%
  mutate(flow.rate = Q.response.tot.m3/quickflow.dur@.Data) %>%
  mutate(init.rain.rate = (init.rain.tot.mm/init.rain.dur@.Data)*60*60) %>%
  mutate(Month = (month(event.start.ts))) %>%
  mutate(Year = as.factor(year(event.start.ts))) %>%
  mutate(rlimb.dur.hrs = (rlimb.dur@.Data/60)/60)%>%
  mutate(flimb.dur.hrs = (flimb.dur@.Data/60)/60)%>%
  mutate(Season = as.factor((ifelse(Month >= 3 & Month < 6, "Spring",
                                    (ifelse(Month >=6 & Month <9, "Summer",
                                            (ifelse(Month >=9 & Month <12, "Autumn",
                                                    (ifelse(Month >=12 | Month <3, "Winter", ""))))))))))%>%
  mutate(HydYear = as.factor((ifelse(Month >= 4 & Month < 10, "Dry","Wet"))))%>%
  mutate(Site = "FoD")

#filter to remove 0 rainfall values

Hyd_dat_FoD4 <- Hyd_dat_FoD4 %>%
  filter(rain.tot.mm > 0.1) #%>%   # max recorded flow removed as zeros not allowed 
# filter(row_number() != 119L & row_number() != 499L) # removing two outliers with high Cook's Distance values.


#further manual removal of events following checks only use when going back to check hydrographs

Hyd_dat_FoD4 <- Hyd_dat_FoD4[-c(36),]

Hyd_dat_FoD4 <- Hyd_dat_FoD4[-c(38,27,26),]

##for peak excedance--

perc_flow_FoD<- ecdf(all_flow_FoD$q) # calculate Empirical Cumulative Distribution for original Q data

Hyd_dat_FoD4 <- Hyd_dat_FoD4 %>%
  mutate(per_q = (1 - perc_flow_FoD(Q.peak.m3.s)) * 100)

head(Hyd_dat_FoD4)
tail(Hyd_dat_FoD4)
summary(Hyd_dat_FoD4)

###Site 4 Yorkshire Beaver Project - abbreviation 'York'

# read_csv("./data/eventEx_EVENTS_WVall.csv")
Hyd_dat_York <- read_rds("./data/eventEx_York_metrics3.rds")

all_flow_York <- read_csv("./data/FullQdata/event-dat_York_QV2.csv") %>% # reload flow inout for calculating correct Excedence...
  select(q) %>%
  drop_na()


#To split up all flow data into periods when beaver were present make sure to check dates!!
# head(Hyd_dat_WV)

all_QR_York <- read_csv("./data/FullQdata/event-dat_York_QV2.csv") %>%
  drop_na()


## *date time to POSIXct (chose correct format based on structure)
all_QR_York$datetime  <- as.POSIXct(strptime(as.character(all_QR_York$datetime),                # N.B. strptime() converts from character to POSIXct time
                                           #"%Y-%m-%d %H:%M:%S",                          # Format of origional text date time
                                           "%d/%m/%Y %H:%M",                            # (alternative) Format of origional text date time
                                           tz = "UTC"))                                 # (the UTC specification needed to avoid GMT to BST bug!)


all_QR_York$Beaver <- "No"
all_QR_York$Beaver[all_QR_York$datetime > as.POSIXct("2019-04-24 00:00", "%Y-%m-%d %H:%M", tz = "UTC")] <- "Yes"
all_QR_York$Beaver[all_QR_York$datetime > as.POSIXct("2019-04-24 00:00", "%Y-%m-%d %H:%M", tz = "UTC") & 
                     all_QR_York$datetime < as.POSIXct("2019-04-23 00:00", "%Y-%m-%d %H:%M", tz = "UTC")] <- "Unsure"
all_QR_York$Beaver <- as.factor(all_QR_York$Beaver)

summary(all_QR_York)



#To split up data into periods when beaver were present make sure to check dates!!
# head(Hyd_dat_York)
Hyd_dat_York$Beaver <- "No"
Hyd_dat_York$Beaver[Hyd_dat_York$event.start.ts > as.POSIXct("2019-04-24 00:00", "%Y-%m-%d %H:%M", tz = "UTC")] <- "Yes"
Hyd_dat_York$Beaver[Hyd_dat_York$event.start.ts > as.POSIXct("2019-04-24 00:00", "%Y-%m-%d %H:%M", tz = "UTC") & 
                     Hyd_dat_York$event.start.ts < as.POSIXct("2019-04-23 00:00", "%Y-%m-%d %H:%M", tz = "UTC")] <- "Unsure"
Hyd_dat_York$Beaver <- as.factor(Hyd_dat_York$Beaver)

head(Hyd_dat_York)
tail(Hyd_dat_York)
summary(Hyd_dat_York)


# To include seasons and hydrological year
Hyd_dat_York4 <- Hyd_dat_York %>%
  filter(Beaver != "Unsure" & flimb.dur@.Data > 0 & rlimb.dur@.Data > 0 & Q.peak.m3.s >0) %>%
  # filter(Q.peak.m3.s > quantile(Q.peak.m3.s, 0.7))%>%
  mutate(rain.rate = rain.tot.mm/rain.dur@.Data) %>%
  mutate(flow.rate = Q.response.tot.m3/quickflow.dur@.Data) %>%
  mutate(init.rain.rate = (init.rain.tot.mm/init.rain.dur@.Data)*60*60) %>%
  mutate(Month = (month(event.start.ts))) %>%
  mutate(Year = as.factor(year(event.start.ts))) %>%
  mutate(rlimb.dur.hrs = (rlimb.dur@.Data/60)/60)%>%
  mutate(flimb.dur.hrs = (flimb.dur@.Data/60)/60)%>%
  mutate(Season = as.factor((ifelse(Month >= 3 & Month < 6, "Spring",
                                    (ifelse(Month >=6 & Month <9, "Summer",
                                            (ifelse(Month >=9 & Month <12, "Autumn",
                                                    (ifelse(Month >=12 | Month <3, "Winter", ""))))))))))%>%
  mutate(HydYear = as.factor((ifelse(Month >= 4 & Month < 10, "Dry","Wet"))))%>%
  mutate(Site = "Yorkshire")


##for peak excedance
perc_flow_York<- ecdf(all_flow_York$q) # calculate Empirical Cumulative Distribution for original Q data

Hyd_dat_York4 <- Hyd_dat_York4 %>%
  mutate(per_q = (1-perc_flow_WV(Q.peak.m3.s)) * 100)

head(Hyd_dat_York4)
tail(Hyd_dat_York4)
summary(Hyd_dat_York4)

####need to filter values as zeros not allowed not allowed for certain analysis i.e. with percentage exceedance
Hyd_dat_WV3 <- Hyd_dat_WV4 %>% filter(per_q > 0)
Hyd_dat_EBud3 <- Hyd_dat_EBud4 %>% filter(per_q > 0)
Hyd_dat_FoD3 <- Hyd_dat_FoD4 %>% filter(per_q > 0)
Hyd_dat_York3 <- Hyd_dat_York4 %>% filter(per_q > 0)
Hyd_Event_dat_Combined3 <- Hyd_Event_dat_Combined %>% filter(per_q > 0)

################# Section 2 Summary Statistics #####################


##event stats

#all event stats  - using dplyr could also use tapply #add in number of events
CombinedStatsV1 <- Hyd_Event_dat_Combined %>%
  group_by(Site, Beaver) %>% 
  summarize(Qpeak_mean =mean(Q.peak.m3.s), Qpeak_med = median(Q.peak.m3.s), Qpeak_sd = sd(Q.peak.m3.s), Qpeak_max = max(Q.peak.m3.s), Qpeak_min = min(Q.peak.m3.s), 
            Q_mean =mean(Q.response.tot.m3), Q_median = median(Q.response.tot.m3), Q_sd = sd(Q.response.tot.m3),Q_max = max(Q.response.tot.m3), Q_min = min(Q.response.tot.m3),
            ER_mean =mean(rain.tot.mm), ER_median = median(rain.tot.mm), ER_sd = sd(rain.tot.mm),ER_max = max(rain.tot.mm), ER_min = min(rain.tot.mm),
            RL_mean =mean(rlimb.dur.hrs), RL_median = median(rlimb.dur.hrs), RL_sd = sd(rlimb.dur.hrs),RL_max = max(rlimb.dur.hrs), RL_min = min(rlimb.dur.hrs),
            FL_mean =mean(flimb.dur.hrs), FL_median = median(flimb.dur.hrs), FL_sd = sd(flimb.dur.hrs),FL_max = max(flimb.dur.hrs), FL_min = min(flimb.dur.hrs),
            LagStart_mean =mean(lag.start.dur), LagStart_median = median(lag.start.dur), LagStart_sd = sd(lag.start.dur),LagStart_max = max(lag.start.dur), LagStart_min = min(lag.start.dur),
            LagPeak_mean =mean(lag.peak.dur), LagPeak_median = median(lag.peak.dur), LagPeak_sd = sd(lag.peak.dur),LagPeak_max = max(lag.peak.dur), LagPeak_min = min(lag.peak.dur)
  )
CombinedStatsV1  
write.csv(CombinedStatsV1, paste0("./allSite_Eventstats_SiteBeaverAll.csv"))



#originally just site and beaver, Hydrological year added now too as a grouping variable
CombinedStatsV1 <- Hyd_Event_dat_Combined %>%
  group_by(Site, Beaver,HydYear) %>% 
  summarize(Qpeak_mean =mean(Q.peak.m3.s), Qpeak_med = median(Q.peak.m3.s), Qpeak_sd = sd(Q.peak.m3.s), Qpeak_max = max(Q.peak.m3.s), Qpeak_min = min(Q.peak.m3.s), 
            Q_mean =mean(Q.response.tot.m3), Q_median = median(Q.response.tot.m3), Q_sd = sd(Q.response.tot.m3),Q_max = max(Q.response.tot.m3), Q_min = min(Q.response.tot.m3),
            ER_mean =mean(rain.tot.mm), ER_median = median(rain.tot.mm), ER_sd = sd(rain.tot.mm),ER_max = max(rain.tot.mm), ER_min = min(rain.tot.mm),
            RL_mean =mean(rlimb.dur.hrs), RL_median = median(rlimb.dur.hrs), RL_sd = sd(rlimb.dur.hrs),RL_max = max(rlimb.dur.hrs), RL_min = min(rlimb.dur.hrs),
            FL_mean =mean(flimb.dur.hrs), FL_median = median(flimb.dur.hrs), FL_sd = sd(flimb.dur.hrs),FL_max = max(flimb.dur.hrs), FL_min = min(flimb.dur.hrs),
            LagStart_mean =mean(lag.start.dur), LagStart_median = median(lag.start.dur), LagStart_sd = sd(lag.start.dur),LagStart_max = max(lag.start.dur), LagStart_min = min(lag.start.dur),
            LagPeak_mean =mean(lag.peak.dur), LagPeak_median = median(lag.peak.dur), LagPeak_sd = sd(lag.peak.dur),LagPeak_max = max(lag.peak.dur), LagPeak_min = min(lag.peak.dur)
            )
CombinedStatsV1  
write.csv(CombinedStatsV1, paste0("./allSite_Eventstats_SiteBeaverHydyear.csv"))



#Q5 event stats
CombinedStats_Q5 <- Hyd_Event_dat_CombinedQ5 %>%
  group_by(Site, Beaver) %>% 
  summarize(Qpeak_mean =mean(Q.peak.m3.s), Qpeak_med = median(Q.peak.m3.s), Qpeak_sd = sd(Q.peak.m3.s), Qpeak_max = max(Q.peak.m3.s), Qpeak_min = min(Q.peak.m3.s), 
            Q_mean =mean(Q.response.tot.m3), Q_median = median(Q.response.tot.m3), Q_sd = sd(Q.response.tot.m3),Q_max = max(Q.response.tot.m3), Q_min = min(Q.response.tot.m3),
            ER_mean =mean(rain.tot.mm), ER_median = median(rain.tot.mm), ER_sd = sd(rain.tot.mm),ER_max = max(rain.tot.mm), ER_min = min(rain.tot.mm),
            RL_mean =mean(rlimb.dur.hrs), RL_median = median(rlimb.dur.hrs), RL_sd = sd(rlimb.dur.hrs),RL_max = max(rlimb.dur.hrs), RL_min = min(rlimb.dur.hrs),
            FL_mean =mean(flimb.dur.hrs), FL_median = median(flimb.dur.hrs), FL_sd = sd(flimb.dur.hrs),FL_max = max(flimb.dur.hrs), FL_min = min(flimb.dur.hrs),
            LagStart_mean =mean(lag.start.dur), LagStart_median = median(lag.start.dur), LagStart_sd = sd(lag.start.dur),LagStart_max = max(lag.start.dur), LagStart_min = min(lag.start.dur),
            LagPeak_mean =mean(lag.peak.dur), LagPeak_median = median(lag.peak.dur), LagPeak_sd = sd(lag.peak.dur),LagPeak_max = max(lag.peak.dur), LagPeak_min = min(lag.peak.dur)
  )
CombinedStats_Q5 
write.csv(CombinedStats_Q5, paste0("./allSite_Eventstats_Q5.csv"))

# create Summary tibbles - needs editing to fit my  data remove beaver unsure category
Flow.Sum.Tab <- function(.data){
  .data %>%
    group_by(Beaver) %>% 
    summarize(Mean = mean(q), Median = median(q), R2FDC = (log10(quantile(q, 0.66)) - log10(quantile(q, 0.33)))/(0.66-0.33),
              Q5 = quantile(q, 0.95), Q95 = quantile(q, 0.05)) %>%
    mutate(Beaver = c('No','yes'), `Q5:Q95 ratio` = Q5/Q95) %>%
    rename(".Col" = Beaver) %>%
    bind_rows(summarise(.,".Col" = '% Change',
                        Mean = (Mean[2]-Mean[1])/Mean[1]*100,
                        Median = (Median[2]-Median[1])/Median[1]*100,
                        R2FDC= (R2FDC[2]-R2FDC[1])/R2FDC[1]*100,
                        Q5 = (Q5[2]-Q5[1])/Q5[1]*100,
                        Q95 = (Q95[2]-Q95[1])/Q95[1]*100,
                        `Q5:Q95 ratio` = (`Q5:Q95 ratio`[2]-`Q5:Q95 ratio`[1])/`Q5:Q95 ratio`[1]*100,))%>%
    mutate_at(vars(Mean, Median, R2FDC, Q5, Q95, `Q5:Q95 ratio`), round,3) 
}

WV_FlowSumTab <- Flow.Sum.Tab(all_QR_WV)


####stats test for significant differences pre/post####

#data non-parametric test for difference between two samples pre/post - Wilcoxon/Mann-Whitney U test

#peak q currently for Q5 events
wilcox.test(Hyd_dat_EBud_Q5$Q.peak.m3.s ~ Hyd_dat_EBud_Q5$Beaver)
wilcox.test(Hyd_dat_FoD_Q5$Q.peak.m3.s ~ Hyd_dat_FoD_Q5$Beaver)
wilcox.test(Hyd_dat_York_Q5$Q.peak.m3.s ~ Hyd_dat_York_Q5$Beaver)
wilcox.test(Hyd_dat_WV_Q5$Q.peak.m3.s ~ Hyd_dat_WV_Q5$Beaver)

#total q
wilcox.test(Hyd_dat_EBud_Q5$Q.response.tot.m3 ~ Hyd_dat_EBud_Q5$Beaver)
wilcox.test(Hyd_dat_FoD_Q5$Q.response.tot.m3 ~ Hyd_dat_FoD_Q5$Beaver)
wilcox.test(Hyd_dat_York_Q5$Q.response.tot.m3 ~ Hyd_dat_York_Q5$Beaver)
wilcox.test(Hyd_dat_WV_Q5$Q.response.tot.m3 ~ Hyd_dat_WV_Q5$Beaver)


#total ER
wilcox.test(Hyd_dat_EBud_Q5$rain.tot.mm ~ Hyd_dat_EBud_Q5$Beaver)
wilcox.test(Hyd_dat_FoD_Q5$rain.tot.mm ~ Hyd_dat_FoD_Q5$Beaver)
wilcox.test(Hyd_dat_York_Q5$rain.tot.mm ~ Hyd_dat_York_Q5$Beaver)
wilcox.test(Hyd_dat_WV_Q5$rain.tot.mm ~ Hyd_dat_WV_Q5$Beaver)

# peak lag time
wilcox.test(Hyd_dat_EBud_Q5$lag.peak.dur ~ Hyd_dat_EBud_Q5$Beaver)
wilcox.test(Hyd_dat_FoD_Q5$lag.peak.dur ~ Hyd_dat_FoD_Q5$Beaver)
wilcox.test(Hyd_dat_York_Q5$lag.peak.dur ~ Hyd_dat_York_Q5$Beaver)
wilcox.test(Hyd_dat_WV_Q5$lag.peak.dur ~ Hyd_dat_WV_Q5$Beaver)

#rising limb time
wilcox.test(Hyd_dat_EBud_Q5$rlimb.dur.hrs ~ Hyd_dat_EBud_Q5$Beaver) 
wilcox.test(Hyd_dat_FoD_Q5$rlimb.dur.hrs ~ Hyd_dat_FoD_Q5$Beaver)
wilcox.test(Hyd_dat_York_Q5$rlimb.dur.hrs ~ Hyd_dat_York_Q5$Beaver)
wilcox.test(Hyd_dat_WV_Q5$rlimb.dur.hrs ~ Hyd_dat_WV_Q5$Beaver)

####--correlations and relationships between variables--####

###test for correlations
# across all 4 sites rain.tot.mm has best correlation, sig across all sites for Q.peak.m3.s and Q.response.tot.m3


corr_df <- Hyd_dat_WV4 %>%
  select(Q.peak.m3.s, rain.tot.mm, rain.rate, rain.mean,rain.peak.mm.h, Beaver)

ggpairs(corr_df, mapping = aes(colour=Beaver))

################-----relationships----------------------#############

###Final additive model for each site###
#does beaver +rainfall influence peak Q?

#WV
WV_m <- glm2(Q.peak.m3.s ~ Beaver + rain.tot.mm, data= Hyd_dat_WV4, family = Gamma(link='identity'))

summary(WV_m)

autoplot(WV_m, which = 1:6, ncol = 3, label.size = 3)
check_model(WV_m)

WV_m.ND <- Create_Data(.data=Hyd_dat_WV4, var='rain.tot.mm') %>%
  broom::augment(WV_m, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

WV_m.tidy <- add.stat.tab(WV_m)

WV_m.tab <- grid.arrange(tableGrob(WV_m.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                       colhead = list(fg_params=list(cex = 0.75)),
                                                       rowhead = list(fg_params=list(cex = 0.75)),
                                                      )))

#table of marginal means

WV.emm.1 <- emmeans(WV_m, ~ Beaver)
WV.emm.1

WV.mm1.tidy <- emmeans.tab(WV.emm.1)
WV.mm1.tidy

WV_emmm.tab <- grid.arrange(tableGrob(WV.mm1.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                                       colhead = list(fg_params=list(cex = 0.75)),
                                                                                                       rowhead = list(fg_params=list(cex = 0.75)),
)))

WV_m_sum <- grid.arrange(top="Woodland Valley",WV_m.tab, WV_emmm.tab,  ncol = 2)

#'pretty' summary stats table and graph
WV_pretty <- pretty.tab(WV_m.tidy, "Woodland Valley Regression Summary", 8)
WV_pretty_marginalmeans <- pretty.tab(WV.mm1.tidy, "Marginal Means", 4)
WV_m_sum_pretty <- grid.arrange(WV_pretty, WV_pretty_marginalmeans,  ncol = 2)

WV_m_glm2 <- glm.plot2(Hyd_dat_WV4, WV_m.ND, "Woodland Valley")
WV_m_glm2


#if want ot plot without using function
WV_m_glm1 <-  {
  ggplot(Hyd_dat_WV4, aes(x=rain.tot.mm, y=Q.peak.m3.s, colour=Beaver, fill=Beaver))+
    geom_point(alpha = 0.5, size=0.8)+
    geom_line(data=WV_m.ND, aes(x=rain.tot.mm, y = .fitted)) +
    geom_ribbon(data=WV_m.ND,aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)),
                alpha=0.2, linetype=2, lwd=0.2) +
    scale_color_manual(values = c('#A6190D', '#244ED3')) +
    scale_fill_manual(values = c('#A6190D', '#244ED3')) +
    coord_cartesian(ylim=c(0,1))+
    labs(x= ("Total Event Rainfall (mm) "),
         y= (expression("Peak Q " (m^{3}~s^{-1}))),
         colour = "Beaver Present", 
         fill = "Beaver Present") +
    ggtitle("Woodland Valley") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 14))}



#EBud
EBud_m <- glm2(Q.peak.m3.s ~ Beaver + rain.tot.mm, data= Hyd_dat_EBud4, family = Gamma(link='identity'))
EBud_mb  <- glm2(Q.peak.m3.s ~ Beaver + rain.tot.mm, data= Hyd_dat_EBud4, family = Gamma(link='identity'),start = coef(GLM_m2) )
summary(EBud_mb )

autoplot(EBud_mb, which = 1:6, ncol = 3, label.size = 3)
check_model(EBud_mb)

EBud_m.ND <- Create_Data(.data=Hyd_dat_EBud4, var='rain.tot.mm') %>%
  broom::augment(EBud_mb, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

EBud_m.tidy <- add.stat.tab(EBud_mb)
EBud_m.tab <- grid.arrange(tableGrob(EBud_m.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                   colhead = list(fg_params=list(cex = 0.75)),
                                                                   rowhead = list(fg_params=list(cex = 0.75)),   )))
 
                                                                  
                                                                   
# EBuD marginal means
EBuD.emm.1 <- emmeans(EBud_m, ~ Beaver)
EBuD.emm.1

EBud.mm1.tidy <- emmeans.tab(EBuD.emm.1)
EBud.mm1.tidy

EBud_emmm.tab <- grid.arrange(tableGrob(EBud.mm1.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                     colhead = list(fg_params=list(cex = 0.75)),
                                                                                     rowhead = list(fg_params=list(cex = 0.75)),
)))

EBud_m_sum <- grid.arrange(top="Budleigh Brook",EBud_m.tab, EBud_emmm.tab,  ncol = 2)

#if want to plot without using function
EBud_m_glm1 <-  {
  ggplot(Hyd_dat_EBud4, aes(x=rain.tot.mm, y=Q.peak.m3.s, colour=Beaver, fill=Beaver))+
    geom_point(alpha = 0.5, size=0.8)+
    geom_line(data=EBud_m.ND, aes(x=rain.tot.mm, y = .fitted)) +
    geom_ribbon(data=EBud_m.ND,aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)),
                alpha=0.2, linetype=2, lwd=0.2) +
    scale_color_manual(values = c('#A6190D', '#244ED3')) +
    scale_fill_manual(values = c('#A6190D', '#244ED3')) +
    coord_cartesian(ylim=c(0.0,3.5))+
    labs(x= ("Total Event Rainfall (mm) "),
         y= (expression("Peak Q " (m^{3}~s^{-1}))),
         colour = "Beaver Present", 
         fill = "Beaver Present") +
    ggtitle("Budleigh Brook") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 14))}

#new formatting using plotting 2 function
EBud_m_glm2 <- glm.plot3(Hyd_dat_EBud4, EBud_m.ND, "Budleigh Brook")
EBud_m_glm2



#'pretty' summary stats table
EBud_pretty <- pretty.tab(EBud_m.tidy, "Budleigh Brook Regression Summary", 8)
EBud_pretty_marginalmeans <- pretty.tab(EBud.mm1.tidy, "Marginal Means", 4)
EBud_m_sum_pretty <- grid.arrange(EBud_pretty, EBud_pretty_marginalmeans,  ncol = 2)

#York
York_m <- glm2(Q.peak.m3.s ~ Beaver + rain.tot.mm, data= Hyd_dat_York4, family = Gamma(link='identity'))

summary(York_m)

autoplot(York_m, which = 1:6, ncol = 3, label.size = 3)
check_model(York_m)

York_m.ND <- Create_Data(.data=Hyd_dat_York4, var='rain.tot.mm') %>%
  broom::augment(York_m, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

York_m.tidy <- add.stat.tab(York_m)
York_m.tab <- grid.arrange(tableGrob(York_m.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                       colhead = list(fg_params=list(cex = 0.75)),
                                                                       rowhead = list(fg_params=list(cex = 0.75)),
                                                                       )))

#'pretty' summary stats table
York_pretty <- pretty.tab(York_m.tidy, " Yorkshire Regression Summary", 8)
York_pretty_marginalmeans <- pretty.tab(York.mm1.tidy, "Marginal Means", 4)
York_m_sum_pretty <- grid.arrange(York_pretty, York_pretty_marginalmeans,  ncol = 2)


York_m_glm1 <-  {ggplot(Hyd_dat_York4, aes(x=rain.tot.mm, y=Q.peak.m3.s, colour=Beaver, fill=Beaver))+
  geom_point(alpha = 0.5, size=0.8)+
  geom_line(data=York_m.ND, aes(x=rain.tot.mm, y = .fitted)) +
  geom_ribbon(data=York_m.ND,aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)),
              alpha=0.2, linetype=2, lwd=0.2) +
  scale_color_manual(values = c('#A6190D', '#244ED3')) +
  scale_fill_manual(values = c('#A6190D', '#244ED3')) +
  coord_cartesian(ylim=c(0,2))+
  labs(x= ("Total Event Rainfall (mm) "),
       y= (expression("Peak Q " (m^{3}~s^{-1}))),
       colour = "Beaver Present", 
       fill = "Beaver Present") +
  ggtitle("Yorkshire") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14))}

#York_m_glm1 <- glm.plot(Hyd_dat_York4, York_m.ND, "Yorkshire") 
York_m_glm1

#new formatting using plotting 2 function
York_m_glm2 <- glm.plot2(Hyd_dat_York4, York_m.ND, "Yorkshire")
York_m_glm2


# York marginal means
York.emm.1 <- emmeans(York_m, ~ Beaver)
York.emm.1

York.mm1.tidy <- emmeans.tab(York.emm.1)
York.mm1.tidy

York_emmm.tab <- grid.arrange(tableGrob(York.mm1.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                         colhead = list(fg_params=list(cex = 0.75)),
                                                                                         rowhead = list(fg_params=list(cex = 0.75)),
)))

York_pretty_marginalmeans <- pretty.tab(York.mm1.tidy, "Marginal Means", 5)


York_m_sum <- grid.arrange(top="Yorkshire",York_m.tab, York_emmm.tab,  ncol = 2)

York_m_sum_pretty <- grid.arrange(York_pretty, York_pretty_marginalmeans,  ncol = 2)

#FoD
FoD_m <- glm2(Q.peak.m3.s ~ Beaver + rain.tot.mm, data= Hyd_dat_FoD4, family = Gamma(link='identity'))

summary(FoD_m)

autoplot(FoD_m, which = 1:6, ncol = 3, label.size = 3)
check_model(FoD_m)

FoD_m.ND <- Create_Data(.data=Hyd_dat_FoD4, var='rain.tot.mm') %>%
  broom::augment(FoD_m, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

FoD_m.tidy <- add.stat.tab(FoD_m)
FoD_m.tab <- grid.arrange(tableGrob(FoD_m.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                       colhead = list(fg_params=list(cex = 0.75)),
                                                                       rowhead = list(fg_params=list(cex = 0.75)),
       
                                                                                                                                   )))


FoD_m_glm1 <-  {ggplot(Hyd_dat_FoD4, aes(x=rain.tot.mm, y=Q.peak.m3.s, colour=Beaver, fill=Beaver))+
    geom_point(alpha = 0.5, size=0.8)+
    geom_line(data=FoD_m.ND, aes(x=rain.tot.mm, y = .fitted)) +
    geom_ribbon(data=FoD_m.ND,aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)),
                alpha=0.2, linetype=2, lwd=0.2) +
    scale_color_manual(values = c('#A6190D', '#244ED3')) +
    scale_fill_manual(values = c('#A6190D', '#244ED3')) +
    coord_cartesian(ylim=c(0,2))+
    labs(x= ("Total Event Rainfall (mm) "),
         y= (expression("Peak Q " (m^{3}~s^{-1}))),
         colour = "Beaver Present", 
         fill = "Beaver Present") +
    ggtitle("Forest of Dean") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 14))}

#new formatting using plotting 2 function
FoD_m_glm2 <- glm.plot2(Hyd_dat_FoD4, FoD_m.ND, "Forest of Dean")
FoD_m_glm2


# FoD marginal means
FoD.emm.1 <- emmeans(FoD_m, ~ Beaver)
FoD.emm.1

FoD.mm1.tidy <- emmeans.tab(FoD.emm.1)
FoD.mm1.tidy

FoD_emmm.tab <- grid.arrange(tableGrob(FoD.mm1.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                         colhead = list(fg_params=list(cex = 0.75)),
                                                                                         rowhead = list(fg_params=list(cex = 0.75)),
)))

FoD_m_sum <- grid.arrange(top="Forest of Dean",FoD_m.tab, FoD_emmm.tab,  ncol = 2)

#'pretty' summary stats table
FoD_pretty <- pretty.tab(FoD_m.tidy, " Forest of Dean Regression Summary", 8)
FoD_pretty_marginalmeans <- pretty.tab(FoD.mm1.tidy, "Marginal Means", 4)
FoD_m_sum_pretty <- grid.arrange(FoD_pretty, FoD_pretty_marginalmeans,  ncol = 2)

###merging glm plots
join.glm2.plot <- ggarrange(WV_m_glm2, EBud_m_glm2, York_m_glm2, FoD_m_glm2, ncol=2,nrow =2, common.legend = TRUE, legend="bottom", widths = c(3, 2.8))
join.glm2.plot
#Export Plot - change name to site/parameter as applicable
ggsave("Plots/GLM2_combined.jpg", width = 15, height = 15, units = 'cm', dpi = 600)

#merging tables

#join.glm1.tab <- ggarrange(WV_m.tab, EBud_m.tab, York_m.tab, FoD_m.tab, ncol=2,nrow =2)
#join.glm1.tab 

#merging tables incl marginal means
join.glm1_all.tab <- ggarrange(WV_m_sum, EBud_m_sum, York_m_sum, FoD_m_sum, ncol=1,nrow =4)
join.glm1_all.tab 

#merging pretty tables incl marginal means
join.glm1_all_pretty.tab <- ggarrange(WV_m_sum_pretty, EBud_m_sum_pretty, York_m_sum_pretty, FoD_m_sum_pretty, ncol=1,nrow =4)
join.glm1_all_pretty.tab 

#Export Plot - change name to site/parameter as applicable
ggsave("Plots/GLM1_combined_table.jpg", width = 15, height = 15, units = 'cm', dpi = 600)


#simple merge graph and table

cow_glm2_all <- plot_grid(join.glm2.plot, join.glm1_all_pretty.tab, nrow = 2, align = "v", rel_heights = c(12, 8))
cow_glm2_all
ggsave("Plots/GLM_combined_tableMM_2.jpg", width = 25, height = 40, units = 'cm', dpi = 600)


### -Q5- additive model###
#does beaver +rainfall influence peak Q for Q5 subset of events?

#WV_Q5
WV_m_Q5 <- glm2(Q.peak.m3.s ~ Beaver + rain.tot.mm, data= Hyd_dat_WV_Q5, family = Gamma(link='identity'))

summary(WV_m_Q5)

autoplot(WV_m_Q5, which = 1:6, ncol = 3, label.size = 3)
check_model(WV_m_Q5)

WV_m.ND_Q5 <- Create_Data(.data=Hyd_dat_WV_Q5, var='rain.tot.mm') %>%
  broom::augment(WV_m_Q5, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

WV_m_Q5.tidy <- add.stat.tab(WV_m_Q5)
WV_m_Q5.tab <- grid.arrange(tableGrob(WV_m_Q5.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                                        colhead = list(fg_params=list(cex = 0.75)),
                                                                                                        rowhead = list(fg_params=list(cex = 0.75)),
)))

WV_m_glm_Q5 <-  {
  ggplot(Hyd_dat_WV_Q5, aes(x=rain.tot.mm, y=Q.peak.m3.s, colour=Beaver, fill=Beaver))+
    geom_point(alpha = 0.5, size=0.8)+
    geom_line(data=WV_m.ND_Q5, aes(x=rain.tot.mm, y = .fitted)) +
    geom_ribbon(data=WV_m.ND_Q5,aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)),
                alpha=0.2, linetype=2, lwd=0.2) +
    scale_color_manual(values = c('#A6190D', '#244ED3')) +
    scale_fill_manual(values = c('#A6190D', '#244ED3')) +
    coord_cartesian(ylim=c(0,1.5))+
    labs(x= ("Total Event Rainfall (mm) "),
         y= (expression("Peak Q " (m^{3}~s^{-1}))),
         colour = "Beaver Present", 
         fill = "Beaver Present") +
    ggtitle("Woodland Valley") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 14))}
#WV_m_glm_Q5 <- glm.plot(Hyd_dat_WV_Q5, WV_m.ND_Q5, "Woodland Valley")
WV_m_glm_Q5

# WV_Q5 marginal means
WV_m_Q5.emm.1 <- emmeans(WV_m_Q5, ~ Beaver)
WV_m_Q5.emm.1

WV_m_Q5.mm1.tidy <- emmeans.tab(WV_m_Q5.emm.1)
WV_m_Q5.mm1.tidy

WV_Q5_emmm.tab <- grid.arrange(tableGrob(WV_m_Q5.mm1.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                       colhead = list(fg_params=list(cex = 0.75)),
                                                                                       rowhead = list(fg_params=list(cex = 0.75)),
)))

WV_Q5_sum <- grid.arrange(top="Woodland Valley Q5",WV_m_Q5.tab, WV_Q5_emmm.tab,  ncol = 2)

#'pretty' summary stats table and graph
WV_Q5_pretty <- pretty.tab(WV_m_Q5.tidy, "Woodland Valley Q5 Regression Summary", 8)
WV_Q5_pretty_marginalmeans <- pretty.tab(WV_m_Q5.mm1.tidy, "Marginal Means", 4)
WV_Q5m_sum_pretty <- grid.arrange(WV_Q5_pretty, WV_Q5_pretty_marginalmeans,  ncol = 2)

WV_Q5_glm2 <- glm.plot2(Hyd_dat_WV_Q5, WV_m.ND_Q5, "Woodland Valley")
WV_Q5_glm2

#EBud_Q5
EBud_m_Q5 <- glm2(Q.peak.m3.s ~ Beaver + rain.tot.mm, data= Hyd_dat_EBud_Q5, family = Gamma(link='identity'))
EBud_mb_Q5  <- glm2(Q.peak.m3.s ~ Beaver + rain.tot.mm, data= Hyd_dat_EBud_Q5, family = Gamma(link='identity'),start = coef(GLM_m2) )
summary(EBud_mb_Q5 )

autoplot(EBud_mb_Q5, which = 1:6, ncol = 3, label.size = 3)
check_model(EBud_mb_Q5)

EBud_m.ND_Q5 <- Create_Data(.data=Hyd_dat_EBud_Q5, var='rain.tot.mm') %>%
  broom::augment(EBud_mb_Q5, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

EBud_m_Q5.tidy <- add.stat.tab(EBud_mb_Q5)
EBud_m_Q5.tab <- grid.arrange(tableGrob(EBud_m_Q5.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                                                colhead = list(fg_params=list(cex = 0.75)),
                                                                                                                rowhead = list(fg_params=list(cex = 0.75)),
)))

EBud_m_glm_Q5 <-  {
  ggplot(Hyd_dat_EBud_Q5, aes(x=rain.tot.mm, y=Q.peak.m3.s, colour=Beaver, fill=Beaver))+
    geom_point(alpha = 0.5, size=0.8)+
    geom_line(data=EBud_m.ND_Q5, aes(x=rain.tot.mm, y = .fitted)) +
    geom_ribbon(data=EBud_m.ND_Q5,aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)),
                alpha=0.2, linetype=2, lwd=0.2) +
    scale_color_manual(values = c('#A6190D', '#244ED3')) +
    scale_fill_manual(values = c('#A6190D', '#244ED3')) +
    coord_cartesian(ylim=c(0,6))+
    labs(x= ("Total Event Rainfall (mm) "),
         y= (expression("Peak Q " (m^{3}~s^{-1}))),
         colour = "Beaver Present", 
         fill = "Beaver Present") +
    ggtitle("Budleigh Brook") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 14))}

#EBud_m_glm_Q5 <- glm.plot(Hyd_dat_EBud_Q5, EBud_m.ND_Q5, "Budleigh Brook")
EBud_m_glm_Q5

# EBud_Q5 marginal means
EBud_m_Q5.emm.1 <- emmeans(EBud_m_Q5, ~ Beaver)
EBud_m_Q5.emm.1

EBud_m_Q5.mm1.tidy <- emmeans.tab(EBud_m_Q5.emm.1)
EBud_m_Q5.mm1.tidy

EBud_Q5_emmm.tab <- grid.arrange(tableGrob(EBud_m_Q5.mm1.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                             colhead = list(fg_params=list(cex = 0.75)),
                                                                                             rowhead = list(fg_params=list(cex = 0.75)),
)))

EBud_Q5_sum <- grid.arrange(top="Budleigh Brook Q5",EBud_m_Q5.tab, EBud_Q5_emmm.tab,  ncol = 2)


#'pretty' summary stats table and graph - for EBud need to increase plotting function Y limit as great peak Q
EBud_Q5_pretty <- pretty.tab(EBud_m_Q5.tidy, "Budleigh Brook Q5 Regression Summary", 8)
EBud_Q5_pretty_marginalmeans <- pretty.tab(EBud_m_Q5.mm1.tidy, "Marginal Means", 4)
EBud_Q5m_sum_pretty <- grid.arrange(EBud_Q5_pretty, EBud_Q5_pretty_marginalmeans,  ncol = 2)

EBud_Q5_glm2 <- glm.plot2(Hyd_dat_EBud_Q5, EBud_m.ND_Q5, "Budleigh Brook")
EBud_Q5_glm2


#York_Q5
York_m_Q5 <- glm2(Q.peak.m3.s ~ Beaver + rain.tot.mm, data= Hyd_dat_York_Q5, family = Gamma(link='identity'))

summary(York_m_Q5)

autoplot(York_m_Q5, which = 1:6, ncol = 3, label.size = 3)
check_model(York_m_Q5)

York_m.ND_Q5 <- Create_Data(.data=Hyd_dat_York_Q5, var='rain.tot.mm') %>%
  broom::augment(York_m_Q5, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

York_m_Q5.tidy <- add.stat.tab(York_m_Q5)
York_m_Q5.tab <- grid.arrange( tableGrob(York_m_Q5.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                                                colhead = list(fg_params=list(cex = 0.75)),
                                                                                                                rowhead = list(fg_params=list(cex = 0.75)),
)))

York_m_glm_Q5 <-  {
  ggplot(Hyd_dat_York_Q5, aes(x=rain.tot.mm, y=Q.peak.m3.s, colour=Beaver, fill=Beaver))+
    geom_point(alpha = 0.5, size=0.8)+
    geom_line(data=York_m.ND_Q5, aes(x=rain.tot.mm, y = .fitted)) +
    geom_ribbon(data=York_m.ND_Q5,aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)),
                alpha=0.2, linetype=2, lwd=0.2) +
    scale_color_manual(values = c('#A6190D', '#244ED3')) +
    scale_fill_manual(values = c('#A6190D', '#244ED3')) +
    coord_cartesian(ylim=c(0,2.5))+
    labs(x= ("Total Event Rainfall (mm) "),
         y= (expression("Peak Q " (m^{3}~s^{-1}))),
         colour = "Beaver Present", 
         fill = "Beaver Present") +
    ggtitle("Yorkshire") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 14))}
#York_m_glm_Q5 <- glm.plot(Hyd_dat_York_Q5, York_m.ND_Q5, "Yorkshire")
York_m_glm_Q5

# York_Q5 marginal means
York_m_Q5.emm.1 <- emmeans(York_m_Q5, ~ Beaver)
York_m_Q5.emm.1

York_m_Q5.mm1.tidy <- emmeans.tab(York_m_Q5.emm.1)
York_m_Q5.mm1.tidy

York_Q5_emmm.tab <- grid.arrange(tableGrob(York_m_Q5.mm1.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                                 colhead = list(fg_params=list(cex = 0.75)),
                                                                                                 rowhead = list(fg_params=list(cex = 0.75)),
)))

York_Q5_sum <- grid.arrange(top="Yorkshire Q5",York_m_Q5.tab, York_Q5_emmm.tab,  ncol = 2)

#'pretty' summary stats table and graph - for EBud need to increase plotting function Y limit as great peak Q
York_Q5_pretty <- pretty.tab(York_m_Q5.tidy, "Yorkshire Q5 Regression Summary", 8)
York_Q5_pretty_marginalmeans <- pretty.tab(York_m_Q5.mm1.tidy, "Marginal Means", 4)
York_Q5m_sum_pretty <- grid.arrange(York_Q5_pretty, York_Q5_pretty_marginalmeans,  ncol = 2)

York_Q5_glm2 <- glm.plot2(Hyd_dat_York_Q5, York_m.ND_Q5, "Yorkshire ")
York_Q5_glm2

#FoD_Q5
FoD_m_Q5 <- glm2(Q.peak.m3.s ~ Beaver + rain.tot.mm, data= Hyd_dat_FoD_Q5, family = Gamma(link='identity'))

summary(FoD_m_Q5)

autoplot(FoD_m_Q5, which = 1:6, ncol = 3, label.size = 3)
check_model(FoD_m_Q5)

FoD_m.ND_Q5 <- Create_Data(.data=Hyd_dat_FoD_Q5, var='rain.tot.mm') %>%
  broom::augment(FoD_m_Q5, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

FoD_m_Q5.tidy <- add.stat.tab(FoD_m_Q5)
FoD_m_Q5.tab <- grid.arrange(tableGrob(FoD_m_Q5.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                                              colhead = list(fg_params=list(cex = 0.75)),
                                                                                                              rowhead = list(fg_params=list(cex = 0.75)),
)))

FoD_m_glm_Q5 <-  {
  ggplot(Hyd_dat_FoD_Q5, aes(x=rain.tot.mm, y=Q.peak.m3.s, colour=Beaver, fill=Beaver))+
    geom_point(alpha = 0.5, size=0.8)+
    geom_line(data=FoD_m.ND_Q5, aes(x=rain.tot.mm, y = .fitted)) +
    geom_ribbon(data=FoD_m.ND_Q5,aes(y=.fitted, ymin = .fitted - (1.96 *.se.fit), ymax = .fitted + (1.96 *.se.fit)),
                alpha=0.2, linetype=2, lwd=0.2) +
    scale_color_manual(values = c('#A6190D', '#244ED3')) +
    scale_fill_manual(values = c('#A6190D', '#244ED3')) +
    coord_cartesian(ylim=c(0.5,2))+
    labs(x= ("Total Event Rainfall (mm) "),
         y= (expression("Peak Q " (m^{3}~s^{-1}))),
         colour = "Beaver Present", 
         fill = "Beaver Present") +
    ggtitle("Forest of Dean") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 14))}
#FoD_m_glm_Q5 <- glm.plot(Hyd_dat_FoD_Q5, FoD_m.ND_Q5, "Forest of Dean")
FoD_m_glm_Q5

# FoD_Q5 marginal means
FoD_m_Q5.emm.1 <- emmeans(FoD_m_Q5, ~ Beaver)
FoD_m_Q5.emm.1

FoD_m_Q5.mm1.tidy <- emmeans.tab(FoD_m_Q5.emm.1)
FoD_m_Q5.mm1.tidy

FoD_Q5_emmm.tab <- grid.arrange(tableGrob(FoD_m_Q5.mm1.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                                 colhead = list(fg_params=list(cex = 0.75)),
                                                                                                 rowhead = list(fg_params=list(cex = 0.75)),
)))

FoD_Q5_sum <- grid.arrange(top="Forest of Dean Q5",FoD_m_Q5.tab, FoD_Q5_emmm.tab,  ncol = 2)


#'pretty' summary stats table and graph - for EBud need to increase plotting function Y limit as great peak Q
FoD_Q5_pretty <- pretty.tab(FoD_m_Q5.tidy, "Forest of Dean Q5 Regression Summary", 8)
FoD_Q5_pretty_marginalmeans <- pretty.tab(FoD_m_Q5.mm1.tidy, "Marginal Means", 4)
FoD_Q5m_sum_pretty <- grid.arrange(FoD_Q5_pretty, FoD_Q5_pretty_marginalmeans,  ncol = 2)

FoD_Q5_glm2 <- glm.plot2(Hyd_dat_FoD_Q5, FoD_m.ND_Q5, "Forest of Dean")
FoD_Q5_glm2


###merging glm  Q5 plots
join.glm_Q5.plot <- ggarrange(WV_Q5_glm2, EBud_Q5_glm2, York_Q5_glm2, FoD_Q5_glm2, ncol=2,nrow =2, common.legend = TRUE, legend="bottom", widths = c(3, 3))
join.glm_Q5.plot
#Export Plot - change name to site/parameter as applicable
ggsave("Plots/GLM_Q5_combined.jpg", width = 15, height = 15, units = 'cm', dpi = 600)


#merging Q5 tablesincl marginal means
join.glm_q5_all.tab <- ggarrange(WV_Q5m_sum_pretty, EBud_Q5m_sum_pretty, York_Q5m_sum_pretty, FoD_Q5m_sum_pretty, ncol=1,nrow =4)
join.glm_q5_all.tab 

#simple merge graph and table

cow_glm1_Q5 <- plot_grid(join.glm_Q5.plot, join.glm_q5_all.tab, nrow = 2, align = "v", rel_heights = c(12, 8))
cow_glm1_Q5
ggsave("Plots/GLM_combined_tableMM_Q5.jpg", width = 25, height = 40, units = 'cm', dpi = 600)


###-does hydrological season have an interactive effect?-###
#for looking at seasonality... 
#WV
WV_Season <- glm2(Q.peak.m3.s ~ rain.tot.mm + HydYear * Beaver  ,data= Hyd_dat_WV4, family = Gamma(link='identity')) #  prelim model run for start values

summary(WV_Season)
check_model(WV_Season)

WV_m_season.ND <- Create_Data(.data=Hyd_dat_WV4, var='rain.tot.mm') %>%
  broom::augment(WV_Season, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

WV_m_season.tidy <- inter.stat.tab(WV_Season)
WV_m_season.tab <- grid.arrange( tableGrob(WV_m_season.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                                                 colhead = list(fg_params=list(cex = 0.75)),
                                                                                                                 rowhead = list(fg_params=list(cex = 0.75)),
)))

WV.emm.Season <- emmeans(WV_Season, ~ Beaver*HydYear)
WV.emm.Season


WV.emm.Season.tidy <- emmeans.tab(WV.emm.Season)
WV.emm.Season.tidy

WV_m_season_MM.tab <- grid.arrange( tableGrob(WV.emm.Season.tidy,rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                                       colhead = list(fg_params=list(cex = 0.75)),
                                                                                                       rowhead = list(fg_params=list(cex = 0.75)),
)))


WV_m_season_Sum <- grid.arrange(top="Woodland Valley",WV_m_season.tab, WV_m_season_MM.tab,  ncol = 2)


WV.glm_season <- glm.plot(Hyd_dat_WV4, WV_m_season.ND, "Woodland Valley") + 
  facet_wrap(~HydYear, ncol = 1) + 
  theme(strip.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))
WV.glm_season

#'pretty' summary stats table and graph - for EBud need to increase plotting function Y limit as great peak Q
WV_season_pretty <- pretty.tab(WV_m_season.tidy, "Woodland Valley Regression Summary", 9)
WV_season_pretty_marginalmeans <- pretty.tab(WV.emm.Season.tidy, "Marginal Means", 6)
WV_season_sum_pretty <- grid.arrange(WV_season_pretty, WV_season_pretty_marginalmeans,  ncol = 2)

WV_season_glm2 <- glm.plot2(Hyd_dat_WV4, WV_m_season.ND, "Woodland Valley")+ 
  facet_wrap(~HydYear, ncol = 1) + 
  theme(strip.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))
WV_season_glm2

#EBud
EBud_Season <- glm2(Q.peak.m3.s ~ rain.tot.mm + HydYear * Beaver  ,data= Hyd_dat_EBud4, family = Gamma(link='identity')) #  prelim model run for start values
EBud_Seasonb <- glm2(Q.peak.m3.s ~ rain.tot.mm + HydYear * Beaver ,data= Hyd_dat_EBud4, family = Gamma(link='identity'), # final model
               start = coef(EBud_Season))
summary(EBud_Seasonb)
check_model(EBud_Seasonb)

EBud_m_season.ND <- Create_Data(.data=Hyd_dat_EBud4, var='rain.tot.mm') %>%
  broom::augment(EBud_Seasonb, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

EBud_m_season.tidy <- inter.stat.tab(EBud_Seasonb)
EBud_m_season.tab <- grid.arrange( tableGrob(EBud_m_season.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                                                     colhead = list(fg_params=list(cex = 0.75)),
                                                                                                                     rowhead = list(fg_params=list(cex = 0.75)),
)))

EBud.glm_season <- glm.plot(Hyd_dat_EBud4, EBud_m_season.ND, "Budleigh Brook") + 
  facet_wrap(~HydYear, ncol = 1) + 
  theme(strip.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))
EBud.glm_season

#Ebud season marginal means
EBud.emm.Season <- emmeans(EBud_Season, ~ Beaver*HydYear)
EBud.emm.Season

EBud.emm.Season.tidy <- emmeans.tab(EBud.emm.Season)
EBud.emm.Season.tidy

EBud_m_season_MM.tab <- grid.arrange( tableGrob(EBud.emm.Season.tidy,rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                                       colhead = list(fg_params=list(cex = 0.75)),
                                                                                                       rowhead = list(fg_params=list(cex = 0.75)),
)))


EBud_m_season_Sum <- grid.arrange(top="Budleigh Brook",EBud_m_season.tab, EBud_m_season_MM.tab,  ncol = 2)

#'pretty' summary stats table and graph - for EBud need to increase plotting function Y limit as great peak Q
EBud_season_pretty <- pretty.tab(EBud_m_season.tidy, "Budleigh Brook Regression Summary", 9)
EBud_season_pretty_marginalmeans <- pretty.tab(EBud.emm.Season.tidy, "Marginal Means", 6)
EBud_season_sum_pretty <- grid.arrange(EBud_season_pretty, EBud_season_pretty_marginalmeans,  ncol = 2)

EBud_season_glm2 <- glm.plot2(Hyd_dat_EBud4, EBud_m_season.ND, "Budleigh Brook")+ 
  facet_wrap(~HydYear, ncol = 1) + 
  theme(strip.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))
EBud_season_glm2

#York
York_Season <- glm2(Q.peak.m3.s ~ rain.tot.mm + HydYear * Beaver  ,data= Hyd_dat_York4, family = Gamma(link='identity')) #  prelim model run for start values

summary(York_Season)
check_model(York_Season)

York_m_season.ND <- Create_Data(.data=Hyd_dat_York4, var='rain.tot.mm') %>%
  broom::augment(York_Season, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

York_m_season.tidy <- inter.stat.tab(York_Season)
York_m_season.tab <- grid.arrange( tableGrob(York_m_season.tidy, rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                                                     colhead = list(fg_params=list(cex = 0.75)),
                                                                                                                     rowhead = list(fg_params=list(cex = 0.75)),
)))

York.emm.Season <- emmeans(York_Season, ~ Beaver*HydYear)
York.emm.Season

York.emm.Season.tidy <- emmeans.tab(York.emm.Season)
York.emm.Season.tidy

York_m_season_MM.tab <- grid.arrange( tableGrob(York.emm.Season.tidy,rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                                     colhead = list(fg_params=list(cex = 0.75)),
                                                                                                     rowhead = list(fg_params=list(cex = 0.75)),
)))


York_m_season_Sum <- grid.arrange(top="Yorkshire",York_m_season.tab, York_m_season_MM.tab,  ncol = 2)



York.glm_season <- glm.plot(Hyd_dat_York4, York_m_season.ND, "Yorkshire") + 
  facet_wrap(~HydYear, ncol = 1) + 
  theme(strip.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))
York.glm_season

#'pretty' summary stats table and graph - for EBud need to increase plotting function Y limit as great peak Q
York_season_pretty <- pretty.tab(York_m_season.tidy, "Yorkshire Regression Summary", 9)
York_season_pretty_marginalmeans <- pretty.tab(York.emm.Season.tidy, "Marginal Means", 6)
York_season_sum_pretty <- grid.arrange(York_season_pretty, York_season_pretty_marginalmeans,  ncol = 2)

York_season_glm2 <- glm.plot2(Hyd_dat_York4, York_m_season.ND, "Yorkshire")+ 
  facet_wrap(~HydYear, ncol = 1) + 
  theme(strip.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))
York_season_glm2

#FoD
FoD_Season <- glm2(Q.peak.m3.s ~ rain.tot.mm + HydYear * Beaver  ,data= Hyd_dat_FoD4, family = Gamma(link='identity')) #  prelim model run for start values

summary(FoD_Season)
check_model(FoD_Season)

FoD_m_season.ND <- Create_Data(.data=Hyd_dat_FoD4, var='rain.tot.mm') %>%
  broom::augment(FoD_Season, type.predict = "response",
                 type.residuals = "deviance",
                 se_fit = T, newdata=.)

FoD_m_season.tidy <- inter.stat.tab(FoD_Season)
FoD_m_season.tab <- grid.arrange( tableGrob(FoD_m_season.tidy,rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                                                   colhead = list(fg_params=list(cex = 0.75)),
                                                                                                                   rowhead = list(fg_params=list(cex = 0.75)),
)))

FoD.emm.Season <- emmeans(FoD_Season, ~ Beaver*HydYear)
FoD.emm.Season

FoD.emm.Season.tidy <- emmeans.tab(FoD.emm.Season)
FoD.emm.Season.tidy
FoD_m_season_MM.tab <- grid.arrange( tableGrob(FoD.emm.Season.tidy,rows=NULL, theme = ttheme_minimal(core = list(fg_params=list(cex = 0.75)),
                                                                                                                     colhead = list(fg_params=list(cex = 0.75)),
                                                                                                                     rowhead = list(fg_params=list(cex = 0.75)),
)))


FoD_m_season_Sum <- grid.arrange(top="Forest of Dean",FoD_m_season.tab, FoD_m_seasonM.tab,  ncol = 2)


FoD.glm_season <- glm.plot(Hyd_dat_FoD4, FoD_m_season.ND, "Forest of Dean") + 
  facet_wrap(~HydYear, ncol = 1) + 
  theme(strip.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))
FoD.glm_season

#'pretty' summary stats table and graph - for EBud need to increase plotting function Y limit as great peak Q
FoD_season_pretty <- pretty.tab(FoD_m_season.tidy, "Forest of Dean Regression Summary", 9)
FoD_season_pretty_marginalmeans <- pretty.tab(FoD.emm.Season.tidy, "Marginal Means", 6)
FoD_season_sum_pretty <- grid.arrange(FoD_season_pretty, FoD_season_pretty_marginalmeans,  ncol = 2)

FoD_season_glm2 <- glm.plot2(Hyd_dat_FoD4, FoD_m_season.ND, "Forest of Dean")+ 
  facet_wrap(~HydYear, ncol = 1) + 
  theme(strip.text.x = element_text(size = 12, color = "black", face = "italic"),
        strip.background = element_rect(color="black", fill="#F6F6F8", linetype=3))
FoD_season_glm2

## seasonality merge plots

join.glm_Season.plot <- ggarrange(WV_season_glm2, EBud_season_glm2, York_season_glm2, FoD_season_glm2, ncol=2,nrow =2, common.legend = TRUE, legend="bottom", widths = c(3, 3))
join.glm_Season.plot
#Export Plot - change name to site/parameter as applicable
ggsave("Plots/GLM_season_combined.jpg", width = 15, height = 15, units = 'cm', dpi = 600)


## seasonality merge tables

join.glm_season.tab <- ggarrange(WV_season_sum_pretty, EBud_season_sum_pretty, York_season_sum_pretty, FoD_season_sum_pretty, ncol=1,nrow =4)
join.glm_season.tab 
ggsave("Plots/GLM_season_combined_table.jpg", width = 15, height = 15, units = 'cm', dpi = 600)

#seasonality merge graph and table

cow_all_season <- plot_grid(join.glm_Season.plot, join.glm_season.tab, nrow = 2, align = "v", rel_heights = c(12, 12))
cow_all_season
ggsave("Plots/GLM_season_combined_tableMM_2.jpg", width = 20, height = 40, units = 'cm', dpi = 600)



