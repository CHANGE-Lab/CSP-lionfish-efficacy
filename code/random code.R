##########
# This script creates ????????
# in Davis et al. (2021) 
# An investigation into whats effects lionfish removal efficiency and efficacy
##########
##########
# AUTHOR: Alexandra CD Davis
# DATE OF CREATION: 2020-11-10
##########
##########
# Note to future readers. This code is kind of a mess. I acknowledge this. May 
# the coding gods just be happy it runs and is quasi-reproducible

# set-up =======================================================================

library(reshape2)
library(plyr)
library(ggplot2)
library(here)
library(readr)

lionfish_removals = read_csv(here::here('./data/clean/lionfish_removals.csv'))
time_per_fish = read_csv(here::here('./data/clean/time_per_fish.csv'))


TOD  <- ggplot(time_per_fish, aes(TOD,Adj_min, fill = TOD))+
  geom_boxplot(outlier.shape = 21,  outlier.size = 2, show.legend = FALSE)+
  theme_classic()+
  #scale_fill_grey(start = 0.8, end = 0.3,name= "Jurisdiction")+  
  xlab("") + 
  ylab(expression('Removal time/ fish (min)'))+
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))
TOD

time.aov <-aov(Adj_min~TOD, data=time_per_fish)
summary(time.aov)
TukeyHSD((time.aov))
#plot(time.aov)

exp.aov<- aov(Adj_min ~Cap_exp_new , data= time_per_fish)
summary(exp.aov)
TukeyHSD(exp.aov)

summary(time_per_fish$Adj_min)

plot(AvgCoral~Average, data = lionfish_removals)
plot(abline(glm(AvgCoral~Average, data = lionfish_removals)))

vrsubset= lionfish_removals %>%
  select(id, AvgCoral, Average, Site_area_m2, AvgGorg)
corrplot(vrsubset, method = "circle")
cor(vrsubset$Average, vrsubset$AvgGorg,
    method = "spearman")

summary(crepuscular_filter$Adj_Prop)
summary(midday_filter$Adj_Prop)

attempts%>%
  drop_na()
unique(attempts$Num_attempts)
count(attempts$Num_attempts)
mean(attempts$Num_attempts)

mean(FGBNMS$Depth_ft)

behaviour_anova<-aov(data=LFBENA, Adj_behaviour~Sub_region)
tukey_hsd(behaviour_anova)

high<-subset(lionfish_removals, Cap_exp_new =="High")
summary(high$Adj_min)

low<- subset(lionfish_removals, Cap_exp_new !="High")
summary(low$Adj_min)
std.error(low$Adj_min)
