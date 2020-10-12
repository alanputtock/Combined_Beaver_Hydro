
#### Flow duration curves ######################################################

dat <- eventEx

## Hydrological year
dat$wateryear <- NA

## start of the water year
wyear.STR <- as.character("2010-10-01 00:00:00")

## number of years needed
wyear.n <- 10
wateryearSTR <- seq(as.POSIXct(wyear.STR,tz="UTC"),by="1 year",length.out=wyear.n)

## mutate the column wateryear for each year of the sequence defined above
for (i in 1:wyear.n){ # start loop
  dat <- dat %>%                           
    
    mutate(wateryear =                                 
             ifelse(datetime >= wateryearSTR[i] & datetime < wateryearSTR[i+1], 
                    paste0(format(wateryearSTR[i], format="%Y"),"-", format(wateryearSTR[i+1], format="%Y")),
                    wateryear)) 
} # end loop

dat$wateryear_f <- factor(dat$wateryear)

#---- > 4.1 Flow Duration Curve ------------------------------------------------

#.... calculations .............................................................

## rank the flow
flow <-  dat$q
flow <-  sort(flow, decreasing = TRUE)
rankedflow <- data.frame(flow)
rankedflow$pcntexceedance <- seq (0, 1, by = 1/(nrow(rankedflow)-1))

rm(flow)

#.... PLOT: Flow Duration Curve ................................................

legnd <- c("flow", "flow (smoothed)")

p.log <- ggplot() + 
  geom_line(data = rankedflow, 
            aes(x = pcntexceedance, y = flow, colour = legnd[1], linetype =legnd[1])) +
  xlab ("% time flow equalled or exceeded") + 
  ylab(expression(Flow~(m^{3}~s^{-1})))+
  scale_x_continuous(labels = scales::percent) +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "none",
                                  panel.grid.minor = element_blank()) +
  scale_colour_manual(values= c("black"))+
  scale_linetype_manual(values= c("solid"))

print(p.log)

p<- ggplot() + 
  geom_line(data = rankedflow, 
            aes(x = pcntexceedance, y = flow, colour = legnd[1], linetype =legnd[1])) +
  xlab ("% time flow equalled or exceeded") + 
  ylab(expression(Flow~(m^{3}~s^{-1})))+
  scale_x_continuous(labels = scales::percent) +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "none",
                                  panel.grid.minor = element_blank()) +
  scale_colour_manual(values= c("black"))+
  scale_linetype_manual(values= c("solid"))

print(p)

## save plot to file
tiff(paste0("plots/","FDClog_",
            userinput$sitename, "_100x100_",
            format(head(dat$datetime, 1),"%Y%m%d"), "_",
            format(tail(dat$datetime, 1),"%Y%m%d"),".tiff"),
     width = 100, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p.log)
dev.off()

tiff(paste0("plots/","FDClog_",
            userinput$sitename, "_60x60_",
            format(head(dat$datetime, 1),"%Y%m%d"), "_",
            format(tail(dat$datetime, 1),"%Y%m%d"),".tiff"),
     width = 60, height = 60, units = 'mm', res = 600, compression = "zip") # to make squares
print(p.log)
dev.off()

tiff(paste0("plots/","FDC_",
            userinput$sitename, "_100x100_",
            format(head(dat$datetime, 1),"%Y%m%d"), "_",
            format(tail(dat$datetime, 1),"%Y%m%d"),".tiff"),
     width = 100, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()

tiff(paste0("plots/","FDC_",
            userinput$sitename, "_60x60_",
            format(head(dat$datetime, 1),"%Y%m%d"), "_",
            format(tail(dat$datetime, 1),"%Y%m%d"),".tiff"),
     width = 60, height = 60, units = 'mm', res = 600, compression = "zip") # to make squares
print(p)
dev.off()

rm(p, p.log)

#---- annually -----------------------------------------------------------------

## rank the flow
flow <-  as.data.frame(dat[,c("datetime","q", "wateryear")])
head(flow, 10)

flow2013 <- subset(flow, wateryear == "2012-2013")
flow2014 <- subset(flow, wateryear == "2013-2014")
flow2015 <- subset(flow, wateryear == "2014-2015")
flow2016 <- subset(flow, wateryear == "2015-2016")
flow2017 <- subset(flow, wateryear == "2016-2017")
flow2018 <- subset(flow, wateryear == "2017-2018")
flow2019 <- subset(flow, wateryear == "2018-2019")

flow.list <- list_in <- mget(ls(pattern="^flow20"))
rm(list = ls(pattern="^flow20")) 

for (i in 1:length(flow.list)){

  flow.list[[i]] <- flow.list[[i]][order(flow.list[[i]]$q, decreasing = TRUE),]  
  head(flow.list[[i]], 10)
  flow.list[[i]]$pcntexceedance <- seq (0, 1, by = 1/(nrow(flow.list[[i]])-1))
  }

head(flow.list[[1]])

unique(dat$wateryear)

legnd <- c(unique(dat$wateryear))

p.log <- ggplot() + 
  geom_line(data = flow.list[[1]], 
            aes(x = pcntexceedance, y = q, colour = legnd[2], linetype =legnd[2])) +
  geom_line(data = flow.list[[2]], 
            aes(x = pcntexceedance, y = q, colour = legnd[3], linetype =legnd[3])) +
  geom_line(data = flow.list[[3]], 
            aes(x = pcntexceedance, y = q, colour = legnd[4], linetype =legnd[4])) +
  geom_line(data = flow.list[[4]], 
            aes(x = pcntexceedance, y = q, colour = legnd[5], linetype =legnd[5])) +
  geom_line(data = flow.list[[5]], 
            aes(x = pcntexceedance, y = q, colour = legnd[6], linetype =legnd[6])) +
  geom_line(data = flow.list[[6]], 
            aes(x = pcntexceedance, y = q, colour = legnd[7], linetype =legnd[7])) +
  geom_line(data = flow.list[[7]], 
            aes(x = pcntexceedance, y = q, colour = legnd[8], linetype =legnd[8])) +
  xlab ("% time flow equalled or exceeded") + 
  xlab ("% time flow equalled or exceeded") + 
  ylab(expression(Flow~(m^{3}~s^{-1})))+
  scale_x_continuous(labels = scales::percent) +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  theme_bw(base_size = 8) + theme(legend.key = element_blank(),
                                  legend.title=element_blank(), 
                                  legend.position = "right",
                                  panel.grid.minor = element_blank()) +
  scale_colour_viridis_d() +
  scale_linetype_manual(values= c(rep("solid", 7)))

p.log

## save plot to file
tiff(paste0("plots/","FDClog_hydrologicalyear",
            userinput$sitename, "_100x100_",
            format(head(dat$datetime, 1),"%Y%m%d"), "_",
            format(tail(dat$datetime, 1),"%Y%m%d"),".tiff"),
     width = 125, height = 100, units = 'mm', res = 600, compression = "zip") # to make squares
print(p.log)
dev.off()

tiff(paste0("plots/","FDClog_hydrologicalyear",
            userinput$sitename, "_60x60_",
            format(head(dat$datetime, 1),"%Y%m%d"), "_",
            format(tail(dat$datetime, 1),"%Y%m%d"),".tiff"),
     width = 85, height = 60, units = 'mm', res = 600, compression = "zip") # to make squares
print(p.log)
dev.off()
