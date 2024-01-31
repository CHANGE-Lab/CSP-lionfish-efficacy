#create new dataframe
library (plotrix)
library(ggplot2)
library(tidyverse)


##Bring in the data set your created earlier (see load and tidy data)
lionfish_removals = read_csv(here::here('./data/clean/lionfish_removals.csv'))

# Create a colorblind friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#rearrange the subregions to the order desired
lionfish_removals$Sub_region <- factor(lionfish_removals$Sub_region, 
                                       levels = c("FKNMS", "BNP",
                                                  "FGBNMS", "BIRNM") )
#arrange data by the season 
lionfish_removals$Date2 <- factor(lionfish_removals$Date2, 
                                  levels = c("Winter 2013", "Spring 2013", "Summer 2013", "Fall 2013",
                                             "Winter 2014", "Spring 2014", "Summer 2014", "Fall 2014",
                                             "Winter 2015", "Spring 2015", "Summer 2015", "Fall 2015",
                                             "Summer 2016", "Summer 2018", "Summer 2019") )

###summary statistics for the plot
fullLFremovals= lionfish_removals %>% 
  group_by(Sub_region, Date2) %>% 
  dplyr::summarize(Avg_prop = mean(Proportion_caught),
                   Avg_size = mean(Lionfish_size_TL),
                   Avg_density = mean(SiteDens),
                   Avg_time = mean(Adj_min),
                   std_prop = plotrix::std.error(Proportion_caught),
                   std_size = plotrix::std.error(Lionfish_size_TL),
                   std_density = plotrix::std.error(SiteDens),
                   std_time = plotrix::std.error(Adj_min))

summary(fullLFremovals)
unique(lionfish_removals$Date2)


### panel c mean time
timetime <- ggplot(fullLFremovals,aes(x= Date2, y=Avg_time, 
                                      group = `Sub_region`, 
                                      colour = `Sub_region`, 
                                      linetype = `Sub_region`,
                                      shape = `Sub_region`))+
  theme_classic()+
  geom_point(aes(shape=Sub_region, size=2),show.legend = FALSE)+
  geom_line(size = 1.2) +
  geom_point(size = 3)+
  scale_color_manual(values = cbPalette )+
  #manual('Sub_region', values = colllll) +
  scale_linetype_manual('Sub_region', values=c(1,1,1,1)) +
  scale_shape_manual('Sub_region', values = c(19,15,17,18)) +
  geom_errorbar(aes(ymin=Avg_time-std_time, ymax=Avg_time+std_time), width=1)+
  xlab("") + 
  ylab("Mean time (min)") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.title= element_blank())+
  theme(legend.justification=c(0.025,1),legend.position=c(0.025,1))+
  annotate('text', x = 15, y = 8, label = '(c)')

  
timetime


###plot a mean density
timeden <- ggplot(fullLFremovals,aes(x= Date2, y=Avg_density*10000,  
                                     group = `Sub_region`, 
                                     colour = `Sub_region`, 
                                     linetype = `Sub_region`,
                                     shape = `Sub_region`))+
  theme_classic()+
  geom_point(aes(shape=Sub_region, size=2),show.legend = FALSE)+
  geom_line(size = 1.2) +
  geom_point(size = 3)+
  scale_color_manual(values = cbPalette )+
  #manual('Sub_region', values = colllll) +
  scale_linetype_manual('Sub_region', values=c(1,1,1,1)) +
  scale_shape_manual('Sub_region', values = c(19,15,17,18)) +
  geom_errorbar(aes(ymin=(Avg_density-std_density)*10000, ymax=(Avg_density+std_density)*10000), width=.5)+
  xlab("") + 
  ylab("Mean density/ Hectare") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.position="none")+
  annotate('text', x = 15, y = 65, label = '(a)')
timeden
timetime+timeden

plot.new()
plot(timeden)
axis.break(axis=1,breakpos=2.5,pos=1, breakcol="black", style="slash", bgcol="white")

#3Panel b mean size
timesize <- ggplot(fullLFremovals,aes(x= Date2, y=Avg_size, 
                                      group = `Sub_region`, 
                                      colour = `Sub_region`, 
                                      linetype = `Sub_region`,
                                      shape = `Sub_region`))+
  theme_classic()+
  geom_point(aes(shape=Sub_region, size=2),show.legend = FALSE)+
  geom_line(size = 1.2) +
  geom_point(size = 3)+
  scale_color_manual(values = cbPalette )+
  #manual('Sub_region', values = colllll) +
  scale_linetype_manual('Sub_region', values=c(1,1,1,1)) +
  scale_shape_manual('Sub_region', values = c(19,15,17,18)) +
  geom_errorbar(aes(ymin=Avg_size-std_size, ymax=Avg_size+std_size), width=.5)+
  xlab("") + 
  ylab("Mean size (cm)") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.title= element_blank()) +
  theme(legend.position="none")+
  annotate('text', x = 15, y = 40, label = '(b)')
  
timesize

##3Panel d proportion removed
timeprop <- ggplot(fullLFremovals,aes(x= Date2, y=Avg_prop, 
                                      group = `Sub_region`, 
                                      colour = `Sub_region`, 
                                      linetype = `Sub_region`,
                                      shape = `Sub_region`))+
  theme_classic()+
  geom_point(aes(shape=Sub_region, size=2),show.legend = FALSE)+
  geom_line(size = 1.2) +
  geom_point(size = 3)+
  scale_color_manual(values = cbPalette )+
  #manual('Sub_region', values = colllll) +
  scale_linetype_manual('Sub_region', values=c(1,1,1,1)) +
  scale_shape_manual('Sub_region', values = c(19,15,17,18)) +
  #scale_x_discrete(breaks= ("Summer 2016"))+
  geom_errorbar(aes(ymin=Avg_prop-std_prop, ymax=Avg_prop+std_prop), width=1)+
  xlab("") + 
  ylab("Proportion Removed") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none")+
  annotate('text', x = 15, y = 1, label = '(d)')

timeprop

###Final plot together
library(cowplot)
finaltime = plot_grid(timeden,timesize,timetime,timeprop, 
                      align = "v",rel_heights = c(0.75,1),
                      nrow = 2, ncol = 2)
finaltime


ggplot2::ggsave(here('./figures/Fig3_TimeSeries.png'), plot = finaltime,
                width = 12, height = 10,
                dpi = 300)



###################Anovas###############################

attempts<-subset(lionfish_removals, lionfish_removals$Num_attempts !="NA")
unique(attempts$Num_attempts)
mean(attempts$Num_attempts)

Summary 
numatemp_test = attempts%>%
  group_by(Sub_region)%>%
  dplyr::summarize(avg_num = mean (Num_attempts),
                   std_num = plotrix::std.error(Num_attempts),
                   var_num = var(Num_attempts))
summary(numatemp_test)
att_anova<-aov(Num_attempts~Sub_region, data= attempts)
tukey_hsd(att_anova)

anova(lionfish_removals, Num_attempts)

timesum<-lionfish_removals%>%
  group_by(Cap_exp_new)%>%
  mean(Adj_min)

density_anova<-aov( Avg_density~Sub_region, data=fullLFremovals )
tukey_hsd(density_anova)

size_anova<-aov( Avg_size~Sub_region, data=fullLFremovals )
tukey_hsd(size_anova)

summary(fullLFremovals$Avg_time)

######################################################
###spplimentary plot code 
# global_plot = ggplot(data = global_driver, aes(x = Year, y = no_obs, 
#                                                group = `Global Change Driver`, 
#                                                colour = `Global Change Driver`, 
#                                                linetype = `Global Change Driver`,
#                                                shape = `Global Change Driver`)) +
#   geom_line(size = 1.2) +
#   geom_point(size = 3)+
#   theme6()+
#   scale_color_manual('Global Change Driver', values = pal) +
#   scale_linetype_manual('Global Change Driver', values=c(1,5,6,4,2,3)) +
#   scale_shape_manual('Global Change Driver', values = c(19,19,19,19,19,19)) +
#   labs(x = 'Year', y = 'Number of Studies') +
#   scale_y_continuous(limits = c(0,15)) +
#   scale_x_discrete(breaks = c(1992,2002,2004,2006,2008,2010,2012,2014,2016,2018))
