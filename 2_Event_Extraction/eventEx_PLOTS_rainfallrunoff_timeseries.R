##### TITLE: PLOTS rainfall runoff timerseries #################################

# PLOTS using final outputs

# Project: eventEx

##### NOTES: ###################################################################


#---- PLOT: an specified time period --------------------------------

legnd <- c("Event","Rainfall", "I. Slow flow","III. Flow", 
           "Event (rain)", "Event (response)", "II. Intermediate flow")

update_geom_defaults("line", list(size = 0.2))

## For a specific time period:
xlim_min <- as.POSIXct(strptime("2017-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))     # set the start date for the plot
xlim_max <- as.POSIXct(strptime("2018-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))     # set the end date for the plot

p1 <-  ggplot(outputs) + 
  geom_col(aes(datetime, event*ylimit1, fill=legnd[1]), width=1*60*15, position=position_dodge(1*60*15)) +    
  geom_col(aes(datetime, response.event*ylimit1, fill=legnd[6]), width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), width = 0.5) + 
  #ylab("Rainfall (mm/h)")+
  ylab (expression(Rainfall~(mm~h^{-1}))) +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                        # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_reverse(limits = c()) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "none",
                                  axis.title.x=element_blank()) +
  scale_colour_manual(values= c("grey50")) +
  scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.2), "grey50"))

p2 <-   ggplot(outputs) +
  geom_col(aes(datetime, event*ylimit2, fill=legnd[5]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, response.event*ylimit2, fill=legnd[6]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_line(aes(datetime, slowflow, colour = legnd[3], linetype =legnd[3])) +
  geom_line(aes(datetime, slowflow+interflow, colour = legnd[7], linetype =legnd[7])) +
  geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
  xlab ("Date") +
  ylab (expression(Flow~(m^{3}~s^{-1}))) +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_continuous(limits = c(0,ylimit2)) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(),
                                  legend.position = "bottom",
                                  legend.justification="left") +                           # blank element removes background to legend
  
  scale_colour_manual(values= c("grey40", "grey40",  "black")) +
  scale_linetype_manual(values= c("longdash", "dotted","solid")) +
  scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.2))) +
  guides(colour = guide_legend(ncol = 1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.2),
                             ncol = 1))


p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
print(p)

tiff(paste0("plots/ts_07_eventEx_rainrunoff_events_OUTPUT_",           
            sitename, "_",
            format(head(dat$datetime, 1),"%Y%m%d"), "_",
            format(tail(dat$datetime, 1),"%Y%m%d"),".tiff"),
     width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()


#---- PLOT: an indervidual event -----------------------------------------------

legnd <- c("Event","Rainfall", "I. Slow flow","III. Flow", 
           "Event (rain)", "Event (response)", "II. Intermediate flow")

update_geom_defaults("line", list(size = 0.2))

## For a specific time event:
# call specific event ID plot
i=4
# event window only (variable)
xlim_min <- as.POSIXct(EVENTS$event.start.ts[i]-(86400*0.5), tz = "UTC")         # set the start date for the plot
xlim_max <- as.POSIXct(EVENTS$event.start.ts[i]+(86400*3), tz = "UTC")           # set the end date for the plot

p1 <-  ggplot(outputs) + 
  geom_col(aes(datetime, event*ylimit1, fill=legnd[1]), width=1*60*15, position=position_dodge(1*60*15)) +    
  geom_col(aes(datetime, response.event*ylimit1, fill=legnd[6]), width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), width = 0.5) + 
  #ylab("Rainfall (mm/h)")+
  ylab (expression(Rainfall~(mm~h^{-1}))) +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                        # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_reverse(limits = c()) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "none",
                                  axis.title.x=element_blank()) +
  scale_colour_manual(values= c("grey50")) +
  scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.2), "grey50"))

p2 <-   ggplot(outputs) +
  geom_col(aes(datetime, event*ylimit2, fill=legnd[5]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, response.event*ylimit2, fill=legnd[6]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_line(aes(datetime, slowflow, colour = legnd[3], linetype =legnd[3])) +
  geom_line(aes(datetime, slowflow+interflow, colour = legnd[7], linetype =legnd[7])) +
  geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
  xlab ("Date") +
  ylab (expression(Flow~(m^{3}~s^{-1}))) +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_continuous(limits = c(0,ylimit2)) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(),
                                  legend.position = "bottom",
                                  legend.justification="left") +                           # blank element removes background to legend
  
  scale_colour_manual(values= c("grey40", "grey40",  "black")) +
  scale_linetype_manual(values= c("longdash", "dotted","solid")) +
  scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.2))) +
  guides(colour = guide_legend(ncol = 1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.2),
                             ncol = 1))


p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
print(p)

tiff(paste0("plots/ts_07_eventEx_rainrunoff_events_OUTPUT_",           
            sitename, "_",
            format(head(dat$datetime, 1),"%Y%m%d"), "_",
            format(tail(dat$datetime, 1),"%Y%m%d"),".tiff"),
     width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()


#---- PLOT: calander year plots ------------------------------------------------

## VALUES plot (by year) .......................................................

yr = c('2012', '2013', '2014', '2015', '2016', '2017', '2018')

legnd <- c("Event","Rainfall", "I. Slow flow","III. Flow", 
           "Event (rain)", "Event (response)", "II. Intermediate flow")


update_geom_defaults("line", list(size = 0.2))

counter = 1
    
for(n in yr){
    
  sub <- subset(outputs, format(datetime,'%Y') == n)

  p1 <-  ggplot(sub) + 
    geom_col(aes(datetime, event*ylimit1, fill=legnd[1]), width=1*60*15, position=position_dodge(1*60*15)) +    
    geom_col(aes(datetime, response.event*ylimit1, fill=legnd[6]), width=1*60*15, position=position_dodge(1*60*15)) +
    geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), width = 0.5) + 
    ylab (expression(Rainfall~(mm~h^{-1}))) +
    scale_x_datetime(date_labels = ("%Y-%m-%d\n%H:%M")) +
    scale_y_reverse(limits = c()) +
    theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                    legend.title=element_blank(), 
                                    legend.position = "none",
                                    axis.title.x=element_blank()) +
    scale_colour_manual(values= c("grey50")) +
    scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.2), "grey50"))
  
  p2 <-   ggplot(sub) +
    geom_col(aes(datetime, event*ylimit2, fill=legnd[5]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
    geom_col(aes(datetime, response.event*ylimit2, fill=legnd[6]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
    geom_line(aes(datetime, slowflow, colour = legnd[3], linetype =legnd[3])) +
    geom_line(aes(datetime, slowflow+interflow, colour = legnd[7], linetype =legnd[7])) +
    geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
    xlab ("Date") +
    ylab (expression(Flow~(m^{3}~s^{-1}))) +
    scale_x_datetime(date_labels = ("%Y-%m-%d\n%H:%M")) +
    scale_y_continuous(limits = c(0,ylimit2)) +
    theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                    legend.title=element_blank(),
                                    legend.position = "bottom",
                                    legend.justification="left") +                           # blank element removes background to legend
    
    scale_colour_manual(values= c("grey40", "grey40",  "black")) +
    scale_linetype_manual(values= c("longdash", "dotted","solid")) +
    scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.2))) +
    guides(colour = guide_legend(ncol = 1)) +
    guides(fill = guide_legend(override.aes = list(alpha = 0.2),
                               ncol = 1))
  
    p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
   
    p <- assign(paste0("plot",counter),p)
    counter <- counter + 1

  rm(p)
  
}
  
  ggp <- mget(ls(pattern="^plot.$"))
  ggpls <- (list = ls(pattern = "^plot.$"))
  length(ggpls)
  
  p <- grid.arrange(grobs = ggp, nrow = length(ggpls), align = "v") ## display plot
  p <- cowplot::plot_grid(plotlist = ggp, nrow = length(ggpls), align = "v")
  
  tiff(paste0("plots/ts_eventEx_rainrunoff_events_OUTPUT_calanderyear_",           
              sitename, "_",
              format(head(outputs$datetime, 1),"%Y%m%d"), "_",
              format(tail(outputs$datetime, 1),"%Y%m%d"),".tiff"),
       width = 210, height = 297, units = 'mm', res = 600, compression = "zip") # to make squares
  print(p)
  dev.off()
 
  rm(list = ls(pattern="^p.$"))
