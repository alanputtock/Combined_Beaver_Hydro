#### PLOTS: annual event plots #################################################


#---- 2012-2013 ----------------------------------------------------------------

## or specify the start and end times for the plot .............................

xlim_min <- as.POSIXct(strptime("2012-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))
xlim_max <- as.POSIXct(strptime("2013-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))

legnd <- c("Event","Rainfall", "Baseflow","Q",  #1:4
           "Event (rain)", "Event (response)", "Event (flow sep.)", "Value", #5:8
           "Intermediate flow", "Water quality sample window") #10
update_geom_defaults("line", list(size = 0.2))

p1 <-   ggplot(eventEx) + 
  geom_col(aes(datetime, event.temp*ylimit1, fill=legnd[1]), width=1*60*15, position=position_dodge(1*60*15)) +    
  geom_col(aes(datetime, response.event*ylimit1, fill=legnd[6]), width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), width = 0.5) + 
  ylab("Rainfall intensity\n(mm h\u207B\u00B9)") +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                        # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_reverse(limits = c()) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "none",
                                  axis.title.x=element_blank()) +
  scale_colour_manual(values= c("grey50")) +
  scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.2), "grey50"))

p2 <-  ggplot(eventEx) +
  geom_col(aes(datetime, event.temp*ylimit2, fill=legnd[5]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, response.event*ylimit2, fill=legnd[6]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_line(aes(datetime, baseflow, colour = legnd[3], linetype =legnd[3])) +
  geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
  #geom_line(data = datwq, aes(datetime, Event_Flag*ylimit2, colour = legnd[10], linetype =legnd[10])) +
  xlab ("Date") +
  ylab (expression(Flow~(m^{3}~s^{-1}))) +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_continuous(limits = c(0,ylimit2)) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(),
                                  legend.position = "bottom",
                                  legend.justification="left") +                           # blank element removes background to legend
  scale_colour_manual(values= c("grey40", "black")) +
  scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.3))) +
  scale_linetype_manual(values= c("longdash", "solid")) +
  guides(colour = guide_legend(ncol = 1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.2),
                             ncol = 1)) +
  geom_line(aes(datetime, interflow, colour = legnd[9], linetype =legnd[9])) +
  scale_size_manual(values = c(.2, .2, .2, 2)) +
  scale_colour_manual(values= c("grey40", "grey40", "black", "deeppink")) +
  scale_linetype_manual(values= c("longdash", "dotted", "solid", "solid")) 

p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
print(p)

tiff(paste0("plots/ts_07_eventEx_comb_events_check_20122013",           
            sitename, "_",
            format(Sys.time(),"%Y%m%d_%H%M"),".tiff"),
     width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()

#---- 2013-2014 ----------------------------------------------------------------

## or specify the start and end times for the plot .............................

xlim_min <- as.POSIXct(strptime("2013-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))
xlim_max <- as.POSIXct(strptime("2014-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))

legnd <- c("Event","Rainfall", "Baseflow","Q",  #1:4
           "Event (rain)", "Event (response)", "Event (flow sep.)", "Value", #5:8
           "Intermediate flow", "Water quality sample window") #10
update_geom_defaults("line", list(size = 0.2))

p1 <-   ggplot(eventEx) + 
  geom_col(aes(datetime, event.temp*ylimit1, fill=legnd[1]), width=1*60*15, position=position_dodge(1*60*15)) +    
  geom_col(aes(datetime, response.event*ylimit1, fill=legnd[6]), width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), width = 0.5) + 
  ylab("Rainfall intensity\n(mm h\u207B\u00B9)") +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                        # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_reverse(limits = c()) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "none",
                                  axis.title.x=element_blank()) +
  scale_colour_manual(values= c("grey50")) +
  scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.2), "grey50"))

p2 <-  ggplot(eventEx) +
  geom_col(aes(datetime, event.temp*ylimit2, fill=legnd[5]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, response.event*ylimit2, fill=legnd[6]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_line(aes(datetime, baseflow, colour = legnd[3], linetype =legnd[3])) +
  geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
  #geom_line(data = datwq, aes(datetime, Event_Flag*ylimit2, colour = legnd[10], linetype =legnd[10])) +
  xlab ("Date") +
  ylab (expression(Flow~(m^{3}~s^{-1}))) +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_continuous(limits = c(0,ylimit2)) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(),
                                  legend.position = "bottom",
                                  legend.justification="left") +                           # blank element removes background to legend
  scale_colour_manual(values= c("grey40", "black")) +
  scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.3))) +
  scale_linetype_manual(values= c("longdash", "solid")) +
  guides(colour = guide_legend(ncol = 1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.2),
                             ncol = 1)) +
  geom_line(aes(datetime, interflow, colour = legnd[9], linetype =legnd[9])) +
  scale_size_manual(values = c(.2, .2, .2, 2)) +
  scale_colour_manual(values= c("grey40", "grey40", "black", "deeppink")) +
  scale_linetype_manual(values= c("longdash", "dotted", "solid", "solid")) 

p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
#print(p)

tiff(paste0("plots/ts_07_eventEx_comb_events_check_20132014",           
            sitename, "_",
            format(Sys.time(),"%Y%m%d_%H%M"),".tiff"),
     width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()

#---- 2014-2015 ----------------------------------------------------------------

## or specify the start and end times for the plot .............................

xlim_min <- as.POSIXct(strptime("2014-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))
xlim_max <- as.POSIXct(strptime("2015-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))

legnd <- c("Event","Rainfall", "Baseflow","Q",  #1:4
           "Event (rain)", "Event (response)", "Event (flow sep.)", "Value", #5:8
           "Intermediate flow", "Water quality sample window") #10
update_geom_defaults("line", list(size = 0.2))

p1 <-   ggplot(eventEx) + 
  geom_col(aes(datetime, event.temp*ylimit1, fill=legnd[1]), width=1*60*15, position=position_dodge(1*60*15)) +    
  geom_col(aes(datetime, response.event*ylimit1, fill=legnd[6]), width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), width = 0.5) + 
  ylab("Rainfall intensity\n(mm h\u207B\u00B9)") +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                        # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_reverse(limits = c()) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "none",
                                  axis.title.x=element_blank()) +
  scale_colour_manual(values= c("grey50")) +
  scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.2), "grey50"))

p2 <-  ggplot(eventEx) +
  geom_col(aes(datetime, event.temp*ylimit2, fill=legnd[5]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, response.event*ylimit2, fill=legnd[6]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_line(aes(datetime, baseflow, colour = legnd[3], linetype =legnd[3])) +
  geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
  #geom_line(data = datwq, aes(datetime, Event_Flag*ylimit2, colour = legnd[10], linetype =legnd[10])) +
  xlab ("Date") +
  ylab (expression(Flow~(m^{3}~s^{-1}))) +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_continuous(limits = c(0,ylimit2)) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(),
                                  legend.position = "bottom",
                                  legend.justification="left") +                           # blank element removes background to legend
  scale_colour_manual(values= c("grey40", "black")) +
  scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.3))) +
  scale_linetype_manual(values= c("longdash", "solid")) +
  guides(colour = guide_legend(ncol = 1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.2),
                             ncol = 1)) +
  geom_line(aes(datetime, interflow, colour = legnd[9], linetype =legnd[9])) +
  scale_size_manual(values = c(.2, .2, .2, 2)) +
  scale_colour_manual(values= c("grey40", "grey40", "black", "deeppink")) +
  scale_linetype_manual(values= c("longdash", "dotted", "solid", "solid")) 

p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
#print(p)

tiff(paste0("plots/ts_07_eventEx_comb_events_check_20142015",           
            sitename, "_",
            format(Sys.time(),"%Y%m%d_%H%M"),".tiff"),
     width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()

#---- 2015-2016 ----------------------------------------------------------------

## or specify the start and end times for the plot .............................

xlim_min <- as.POSIXct(strptime("2015-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))
xlim_max <- as.POSIXct(strptime("2016-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))

legnd <- c("Event","Rainfall", "Baseflow","Q",  #1:4
           "Event (rain)", "Event (response)", "Event (flow sep.)", "Value", #5:8
           "Intermediate flow", "Water quality sample window") #10
update_geom_defaults("line", list(size = 0.2))

p1 <-   ggplot(eventEx) + 
  geom_col(aes(datetime, event.temp*ylimit1, fill=legnd[1]), width=1*60*15, position=position_dodge(1*60*15)) +    
  geom_col(aes(datetime, response.event*ylimit1, fill=legnd[6]), width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), width = 0.5) + 
  ylab("Rainfall intensity\n(mm h\u207B\u00B9)") +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                        # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_reverse(limits = c()) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "none",
                                  axis.title.x=element_blank()) +
  scale_colour_manual(values= c("grey50")) +
  scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.2), "grey50"))

p2 <-  ggplot(eventEx) +
  geom_col(aes(datetime, event.temp*ylimit2, fill=legnd[5]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, response.event*ylimit2, fill=legnd[6]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_line(aes(datetime, baseflow, colour = legnd[3], linetype =legnd[3])) +
  geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
  #geom_line(data = datwq, aes(datetime, Event_Flag*ylimit2, colour = legnd[10], linetype =legnd[10])) +
  xlab ("Date") +
  ylab (expression(Flow~(m^{3}~s^{-1}))) +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_continuous(limits = c(0,ylimit2)) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(),
                                  legend.position = "bottom",
                                  legend.justification="left") +                           # blank element removes background to legend
  scale_colour_manual(values= c("grey40", "black")) +
  scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.3))) +
  scale_linetype_manual(values= c("longdash", "solid")) +
  guides(colour = guide_legend(ncol = 1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.2),
                             ncol = 1)) +
  geom_line(aes(datetime, interflow, colour = legnd[9], linetype =legnd[9])) +
  scale_size_manual(values = c(.2, .2, .2, 2)) +
  scale_colour_manual(values= c("grey40", "grey40", "black", "deeppink")) +
  scale_linetype_manual(values= c("longdash", "dotted", "solid", "solid")) 

p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
#print(p)

tiff(paste0("plots/ts_07_eventEx_comb_events_check_20152016",           
            sitename, "_",
            format(Sys.time(),"%Y%m%d_%H%M"),".tiff"),
     width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()

#---- 2016-2017 ----------------------------------------------------------------

## or specify the start and end times for the plot .............................

xlim_min <- as.POSIXct(strptime("2016-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))
xlim_max <- as.POSIXct(strptime("2017-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))

legnd <- c("Event","Rainfall", "Baseflow","Q",  #1:4
           "Event (rain)", "Event (response)", "Event (flow sep.)", "Value", #5:8
           "Intermediate flow", "Water quality sample window") #10
update_geom_defaults("line", list(size = 0.2))

p1 <-   ggplot(eventEx) + 
  geom_col(aes(datetime, event.temp*ylimit1, fill=legnd[1]), width=1*60*15, position=position_dodge(1*60*15)) +    
  geom_col(aes(datetime, response.event*ylimit1, fill=legnd[6]), width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), width = 0.5) + 
  ylab("Rainfall intensity\n(mm h\u207B\u00B9)") +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                        # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_reverse(limits = c()) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "none",
                                  axis.title.x=element_blank()) +
  scale_colour_manual(values= c("grey50")) +
  scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.2), "grey50"))

p2 <-  ggplot(eventEx) +
  geom_col(aes(datetime, event.temp*ylimit2, fill=legnd[5]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, response.event*ylimit2, fill=legnd[6]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_line(aes(datetime, baseflow, colour = legnd[3], linetype =legnd[3])) +
  geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
  #geom_line(data = datwq, aes(datetime, Event_Flag*ylimit2, colour = legnd[10], linetype =legnd[10])) +
  xlab ("Date") +
  ylab (expression(Flow~(m^{3}~s^{-1}))) +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_continuous(limits = c(0,ylimit2)) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(),
                                  legend.position = "bottom",
                                  legend.justification="left") +                           # blank element removes background to legend
  scale_colour_manual(values= c("grey40", "black")) +
  scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.3))) +
  scale_linetype_manual(values= c("longdash", "solid")) +
  guides(colour = guide_legend(ncol = 1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.2),
                             ncol = 1)) +
  geom_line(aes(datetime, interflow, colour = legnd[9], linetype =legnd[9])) +
  scale_size_manual(values = c(.2, .2, .2, 2)) +
  scale_colour_manual(values= c("grey40", "grey40", "black", "deeppink")) +
  scale_linetype_manual(values= c("longdash", "dotted", "solid", "solid")) 

p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
#print(p)

tiff(paste0("plots/ts_07_eventEx_comb_events_check_20162017",           
            sitename, "_",
            format(Sys.time(),"%Y%m%d_%H%M"),".tiff"),
     width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()

#---- 2017-2018 ----------------------------------------------------------------

## or specify the start and end times for the plot .............................

xlim_min <- as.POSIXct(strptime("2017-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))
xlim_max <- as.POSIXct(strptime("2018-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))

legnd <- c("Event","Rainfall", "Baseflow","Q",  #1:4
           "Event (rain)", "Event (response)", "Event (flow sep.)", "Value", #5:8
           "Intermediate flow", "Water quality sample window") #10
update_geom_defaults("line", list(size = 0.2))

p1 <-   ggplot(eventEx) + 
  geom_col(aes(datetime, event.temp*ylimit1, fill=legnd[1]), width=1*60*15, position=position_dodge(1*60*15)) +    
  geom_col(aes(datetime, response.event*ylimit1, fill=legnd[6]), width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), width = 0.5) + 
  ylab("Rainfall intensity\n(mm h\u207B\u00B9)") +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                        # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_reverse(limits = c()) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "none",
                                  axis.title.x=element_blank()) +
  scale_colour_manual(values= c("grey50")) +
  scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.2), "grey50"))

p2 <-  ggplot(eventEx) +
  geom_col(aes(datetime, event.temp*ylimit2, fill=legnd[5]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, response.event*ylimit2, fill=legnd[6]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_line(aes(datetime, baseflow, colour = legnd[3], linetype =legnd[3])) +
  geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
  #geom_line(data = datwq, aes(datetime, Event_Flag*ylimit2, colour = legnd[10], linetype =legnd[10])) +
  xlab ("Date") +
  ylab (expression(Flow~(m^{3}~s^{-1}))) +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_continuous(limits = c(0,ylimit2)) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(),
                                  legend.position = "bottom",
                                  legend.justification="left") +                           # blank element removes background to legend
  scale_colour_manual(values= c("grey40", "black")) +
  scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.3))) +
  scale_linetype_manual(values= c("longdash", "solid")) +
  guides(colour = guide_legend(ncol = 1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.2),
                             ncol = 1)) +
  geom_line(aes(datetime, interflow, colour = legnd[9], linetype =legnd[9])) +
  scale_size_manual(values = c(.2, .2, .2, 2)) +
  scale_colour_manual(values= c("grey40", "grey40", "black", "deeppink")) +
  scale_linetype_manual(values= c("longdash", "dotted", "solid", "solid")) 

p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
#print(p)

tiff(paste0("plots/ts_07_eventEx_comb_events_check_20172018",           
            sitename, "_",
            format(Sys.time(),"%Y%m%d_%H%M"),".tiff"),
     width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()

#---- 2018-2019 ----------------------------------------------------------------

## or specify the start and end times for the plot .............................

xlim_min <- as.POSIXct(strptime("2018-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))
xlim_max <- as.POSIXct(strptime("2019-10-01 00:00", "%Y-%m-%d %H:%M", tz = "UTC"))

legnd <- c("Event","Rainfall", "Baseflow","Q",  #1:4
           "Event (rain)", "Event (response)", "Event (flow sep.)", "Value", #5:8
           "Intermediate flow", "Water quality sample window") #10
update_geom_defaults("line", list(size = 0.2))

p1 <-   ggplot(eventEx) + 
  geom_col(aes(datetime, event.temp*ylimit1, fill=legnd[1]), width=1*60*15, position=position_dodge(1*60*15)) +    
  geom_col(aes(datetime, response.event*ylimit1, fill=legnd[6]), width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, rainfall, colour = legnd[2], fill=legnd[2]), width = 0.5) + 
  ylab("Rainfall intensity\n(mm h\u207B\u00B9)") +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                        # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_reverse(limits = c()) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "none",
                                  axis.title.x=element_blank()) +
  scale_colour_manual(values= c("grey50")) +
  scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.2), "grey50"))

p2 <-  ggplot(eventEx) +
  geom_col(aes(datetime, event.temp*ylimit2, fill=legnd[5]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_col(aes(datetime, response.event*ylimit2, fill=legnd[6]), alpha = 0.2, width=1*60*15, position=position_dodge(1*60*15)) +
  geom_line(aes(datetime, baseflow, colour = legnd[3], linetype =legnd[3])) +
  geom_line(aes(datetime, q, colour = legnd[4], linetype =legnd[4])) +
  #geom_line(data = datwq, aes(datetime, Event_Flag*ylimit2, colour = legnd[10], linetype =legnd[10])) +
  xlab ("Date") +
  ylab (expression(Flow~(m^{3}~s^{-1}))) +
  scale_x_datetime(limits = c(xlim_min,xlim_max),                             # get the x axis limits previously set
                   date_labels = ("%Y-%m-%d\n%H:%M")) +
  scale_y_continuous(limits = c(0,ylimit2)) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(),
                                  legend.position = "bottom",
                                  legend.justification="left") +                           # blank element removes background to legend
  scale_colour_manual(values= c("grey40", "black")) +
  scale_fill_manual(values = c(alpha (c("cyan"), 0.3), alpha (c("dodgerblue3"), 0.3))) +
  scale_linetype_manual(values= c("longdash", "solid")) +
  guides(colour = guide_legend(ncol = 1)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.2),
                             ncol = 1)) +
  geom_line(aes(datetime, interflow, colour = legnd[9], linetype =legnd[9])) +
  scale_size_manual(values = c(.2, .2, .2, 2)) +
  scale_colour_manual(values= c("grey40", "grey40", "black", "deeppink")) +
  scale_linetype_manual(values= c("longdash", "dotted", "solid", "solid")) 

p <- plot_grid(p1, p2, nrow = 2, align = "v", rel_heights = c(0.5, 1.2))
#print(p)

tiff(paste0("plots/ts_07_eventEx_comb_events_check_20182019",           
            sitename, "_",
            format(Sys.time(),"%Y%m%d_%H%M"),".tiff"),
     width = 210, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()
