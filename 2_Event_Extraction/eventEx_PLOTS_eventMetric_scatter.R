
install.packages("viridis")  # Install
library("viridis")           # Load

dat <- EVENTS

names(EVENTS)

#----  Plot: outliers with total Q for each event -------------------------------

ggplot (EVENTS) +
  geom_point(data = subset(check2, MADoutlier == F), 
             aes(x = Q.response.tot.m3, y = lag.start.dur), shape = 1) +
  geom_point(data = subset(check2, MADoutlier == T), 
             aes(x = Q.response.tot.m3, y = lag.start.dur), shape = 16, colour = "red") +
  geom_text(data = subset(check2, MADoutlier == T), 
            aes(x = Q.response.tot.m3, y = lag.start.dur, label = eventID),
            hjust = -1, vjust = 1, colour =  "red") +
  geom_ribbon(data = check2,
              aes(x = Q.response.tot.m3,
                  ymin = head(check2$MADoutlier.thrs.low, 1), 
                  ymax = head(check2$MADoutlier.thrs.high, 1)),
              fill = "red",
              alpha = 0.1,
              linetype = 0)

#----  Plot:  ----

ggplot (EVENTS) +
  
  geom_point(aes(x = rlimb.rain,
                 y = (Q.response.tot.m3-Q.response.base.m3), 
                 colour = interevent.Q.median)) +
  
  #xlab (expression(Rainfall~on~rising~limb~(mm))) +
  #ylab (expression(Quick~flow~(m^{3}))) + 
  
  #scale_x_continuous(limits = c(0,200)) +                               # get the x axis limits previously set
  #scale_y_continuous(limits = c(0,10)) +                                # for colour the sensor is limited to 200 Hz
  
  theme_bw(base_size = 8) + theme(legend.key = element_blank()) +
  #legend.title=element_blank()) +
  
  scale_colour_viridis_c(option = "viridis", direction = -1)


#----  Plot: Scatter with simple liniar regression model  -----------------------

## plot: scatter for two parameters
ggplot (EVENTS, aes(x = rlimb.rain, y = Q.response.quick.m3)) +
  geom_point(colour = "black", 
             shape = 1) +
  geom_smooth(method = lm, 
              size = 0.3, 
              colour = "black", fill = "black", alpha = 0.2) +
  # geom_text(aes(label = eventIDnew),
  #           hjust = -1, vjust = 1, 
  #           colour =  "red") +
  scale_x_continuous() +
  scale_y_continuous() +                              
  theme_bw(base_size = 8) + theme(legend.key = element_blank()) 





## simple liniar model
mod1 <- lm(formula = Q.response.quick.m3 ~ rlimb.rain,
           data = EVENTS,
           na.action = na.omit)

mod1

## Model coefficient of determination (r squared)
summary(mod1)$r.squared 

## Pearson's correlation coefficiaent R squared using cor() 
## (NOTE: the ^2 in the code must be inlcuded)
r_squared <- (cor(EVENTS[,c("Q.response.quick.m3", "rlimb.rain")] , 
                  method = "pearson",
                  use = "pairwise.complete.obs"))^2 

## Calculate
r_squared <- 

plot(mod1)






#----  Plot:  ----

ggplot (EVENTS) +
  geom_point(aes(x = rlimb.rain,
                 y = (Q.tot.m3-baseflow.tot.m3), 
                 colour = interevent.Q.median)) +
  ylab (expression(Quickflow~(m^{3}))) + 
  xlab ("Rainfall total initialising event (mm)") +
  #scale_x_continuous(limits = c(0,200)) +                               # get the x axis limits previously set
  #scale_y_continuous(limits = c(0,10)) +                                # for colour the sensor is limited to 200 Hz
  theme_bw(base_size = 8) + theme(legend.key = element_blank()
  ) +
  # scale_fill_gradientn(colours = viridis(256, option = "D")) 
  scale_colour_viridis_c(option = "viridis", direction = -1)
