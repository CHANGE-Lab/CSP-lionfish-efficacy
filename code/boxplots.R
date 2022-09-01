########## 
##########
# This script creates box and whisker plots for figure 2  
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
#####Location Comparison boxplot.stats
library(ggplot2)
library(PNWColors)
library(cowplot)
library(RColorBrewer)
library(here)

lionfish_removals = read_csv(here::here('./data/clean/lionfish_removals.csv'))

# Create a colorblind friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

###### order variables how you ant them to display in plots
lionfish_removals$Sub_region <- factor(lionfish_removals$Sub_region, 
                                       levels = c("FKNMS", "BNP",
                                                  "FGBNMS", "BIRNM") )
lionfish_removals$Cap_exp_new <- factor(lionfish_removals$Cap_exp_new, 
                                       levels = c("High", "Medium",
                                                  "Low", "None") )
lionfish_removals$TOD <- factor(lionfish_removals$TOD, 
                                        levels = c("Crepuscular", "midday") )
##### model outcomes
Prop_loc <- ggplot (lionfish_removals) +
  geom_boxplot(aes(y= Adj_Prop*100, x = Sub_region, fill = Sub_region),show.legend = FALSE)+
  theme_classic()+
  #scale_fill_grey(start = 0.8, end = 0.3,name= "Jurisdiction")+ 
  xlab("") + 
  ylab("Percentage removed") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(legend.title= element_text(size= 16))
Prop_loc 

plot_time <-subset(lionfish_removals, Adj_min <"60")
TIme_loc <- ggplot (plot_time) +
  geom_boxplot(aes(y= Adj_min, x = Sub_region, fill = Sub_region),show.legend = FALSE)+
  theme_classic()+
  #scale_fill_grey(start = 0.8, end = 0.3,name= "Jurisdiction")+ 
  xlab("") + 
  ylab("Minutes for removal") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(legend.title= element_text(size= 16))

TIme_loc

like_loc <- ggplot(lionfish_removals, aes(Sub_region, fill = Captured ))+
  geom_bar(position = "dodge")+
  theme_classic()+
  scale_fill_grey(start = 0.8, end = 0.3,name= "Captured")+  
  xlab("") + 
  ylab("Number of Occurrences") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(legend.title= element_text(size= 16))+ 
  theme(legend.position = c(0.88, 0.88))
like_loc

modelplot = plot_grid(Prop_loc,TIme_loc,like_loc,
                      nrow = 3, ncol = 1)
modelplot

##lionfish attributes compared over the 4 locations
Dens_loc <- ggplot (lionfish_removals) +
  geom_boxplot(aes(y= SiteDens*10000, x = Sub_region, fill = Sub_region),
               outlier.shape = 21,  outlier.size = 2, show.legend = FALSE)+
  theme_classic()+
  scale_fill_manual(values = cbPalette )+
  #scale_fill_grey(start = 0.8, end = 0.3,name= "Jurisdiction")+  
  xlab("") + 
  ylab("Density/ Hectare") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(legend.title= element_text(size= 16))+
  annotate('text', x = 0.75, y = 100, label = '(a)')
Dens_loc 


Size_loc <- ggplot (lionfish_removals) +
  geom_boxplot(aes(y= Lionfish_size_TL, x = Sub_region, fill = Sub_region),
               outlier.shape = 21,  outlier.size = 2, show.legend = FALSE)+
  theme_classic()+
  scale_fill_manual(values = cbPalette )+
  #scale_fill_grey(start = 0.8, end = 0.3,name= "Jurisdiction")+  
  xlab("") + 
  ylab("Size TL (cm)") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(legend.title= element_text(size= 16))+
  annotate('text', x = 0.75, y = 45, label = '(b)')
Size_loc 


gorg_loc<- ggplot (lionfish_removals) +
  geom_boxplot(aes(y= AvgGorg, x = Sub_region, fill = Sub_region),
               outlier.shape = 21,  outlier.size = 2, show.legend = FALSE)+
  theme_classic()+
  scale_fill_manual(values = cbPalette )+
  #scale_fill_grey(start = 0.8, end = 0.3,name= "Jurisdiction")+  
  xlab("") + 
  ylab("% Gorgonian Cover") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(legend.title= element_text(size= 16))+
  annotate('text', x = 0.75, y = 50, label = '(c)')
gorg_loc 

coral_loc<- ggplot (lionfish_removals) +
  geom_boxplot(aes(y= AvgCoral, x = Sub_region, fill = Sub_region),
               outlier.shape = 21,  outlier.size = 2,show.legend = FALSE)+
  theme_classic()+
  scale_fill_manual(values = cbPalette )+
  xlab("") + 
  ylab("% Coral Cover") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(legend.title= element_text(size= 16))+
  annotate('text', x = 0.75, y = 100, label = '(g)')
coral_loc 

VR_loc<- ggplot (lionfish_removals) +
  geom_boxplot(aes(y= Average, x = Sub_region, fill = Sub_region),
               outlier.shape = 21,  outlier.size = 2, show.legend = FALSE)+
  theme_classic()+
  scale_fill_manual(values = cbPalette )+
  #scale_fill_grey(start = 0.8, end = 0.3,name= "Jurisdiction")+  
  xlab("") + 
  ylab("Avg Vertical Releif (cm)") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(legend.title= element_text(size= 16))+
  annotate('text', x = 0.75, y = 400, label = '(g)')
VR_loc 

depth_loc<- ggplot (lionfish_removals) +
  geom_boxplot(mapping = aes(y= Depth_ft*.3048, x = Sub_region, fill = Sub_region),
               outlier.shape = 21,  outlier.size = 2, show.legend = FALSE)+
  scale_fill_manual(values = cbPalette )+
  #scale_fill_grey(start = 0.8, end = 0.3,name= "Jurisdiction")+ 
  xlab("") + 
  ylab("Depth (m)") +
  theme_classic()+
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(legend.title= element_text(size= 16))+
  annotate('text', x = 0.75, y = 35, label = '(e)')
depth_loc

# depth_loc1<- ggplot (lionfish_removals) +
#   geom_jitter(mapping = aes(y= Depth_ft, x = Sub_region, color = Sub_region),
#               outlier.fill = NULL, show.legend = FALSE)+ 
#   scale_color_grey(start = 0.8, end = 0.3,name= "Jurisdiction")+ 
#   xlab("") + 
#   ylab("Depth (ft)") +
#   theme_classic()+
#   theme(axis.title = element_text(size = 16)) +
#   theme(axis.text= element_text(size=12))+
#   theme(legend.title= element_text(size= 16))
# 
# depth_loc2<- ggplot (lionfish_removals) +
#   geom_boxplot(mapping = aes(y= Depth_ft, x = Sub_region, fill = Sub_region),
#               outlier.fill = NULL, show.legend = FALSE)+ 
#   scale_fill_grey(start = 0.8, end = 0.3,name= "Jurisdiction")+ 
#   xlab("") + 
#   ylab("Depth (ft)") +
#   theme_classic()+
#   theme(axis.title = element_text(size = 16)) +
#   theme(axis.text= element_text(size=12))+
#   theme(legend.title= element_text(size= 16))
# 
# variety = plot_grid(depth_loc,depth_loc1, depth_loc2,
#                      nrow = 3, ncol = 1)
# variety

numatmp_loc  <- ggplot(lionfish_removals, aes( Sub_region,Num_attempts, fill = Sub_region))+
  geom_boxplot(outlier.shape = 21,  outlier.size = 2, show.legend = FALSE)+
  theme_classic()+
  scale_fill_manual(values = cbPalette )+
  #scale_fill_grey(start = 0.8, end = 0.3,name= "Jurisdiction")+  
  xlab("") + 
  ylab("Number of attempts") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  annotate('text', x = 0.75, y = 10, label = '(f)')
numatmp_loc

reefsize_loc  <- ggplot(lionfish_removals, aes( Sub_region,Site_area_m2*.0001, fill = Sub_region))+
  geom_boxplot(outlier.shape = 21,  outlier.size = 2, show.legend = FALSE)+
  theme_classic()+
  scale_fill_manual(values = cbPalette )+
  #scale_fill_grey(start = 0.8, end = 0.3,name= "Jurisdiction")+  
  xlab("") + 
  ylab('Reef Size (Hectares)')+ 
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  annotate('text', x = 0.75, y = 1, label = '(d)')
reefsize_loc

#expression(~m^{"2"}))
library(cowplot)
finalbox = plot_grid(Dens_loc,Size_loc,gorg_loc, 
                     #coral_loc,
                     #VR_loc,
                     reefsize_loc,depth_loc,numatmp_loc,
                  nrow = 3, ncol = 2)
finalbox

#############
levels(lionfish_removals$Sub_region) <- gsub(" ", "\n", levels(lionfish_removals$Sub_region))

library(viridisLite)
LFVISNA <- subset(lionfish_removals, Lionfish_visibility !="NA")
lfvis_loc <- ggplot(LFVISNA, aes(Sub_region, fill = Lionfish_visibility))+
  geom_bar(position = "stack")+
  theme_classic()+
  scale_fill_grey(name= "Postition\non Reef", start = 0.8, end = 0.3,
                  labels=c("Exposed", "Sheltered"))+  
  xlab("") + 
  ylab("Number of Occurrences") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y = element_text(size = 12)) +
  theme(axis.text.y= element_text(size=12))+
  theme(legend.title= element_text(size= 16))+ 
  theme(legend.position = c(0.88, 0.84))+
  annotate('text', x = 0.75, y = 300, label = '(a)')
lfvis_loc

LFBENA <- subset(lionfish_removals, Lionfish_behaviour !="NA")
lfbehave_loc <- ggplot(LFBENA, aes(Sub_region, fill = Adj_behaviour))+
  geom_bar(position = "stack")+
  theme_classic()+
  scale_fill_grey(name= "Behaviour", start = 0.8, end = 0.3,
                  labels=c("Active", "Resting"))+  
  xlab("") + 
  ylab("Number of Occurrences") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title = element_text(size = 12)) +
  theme(axis.text= element_text(size=12))+
  theme(legend.title= element_text(size= 16))+ 
  theme(legend.position = c(0.88, 0.88))+
  annotate('text', x = 0.75, y = 310, label = '(b)')
lfbehave_loc

LFTODNA <- subset(lionfish_removals, TOD !="NA")
LFTODNA$TOD <- factor(LFTODNA$TOD, levels = c( "midday","Crepuscular") )
lfTOD_loc <- ggplot(LFTODNA, aes(Sub_region, fill = TOD))+
  geom_bar(position = "stack")+
  theme_classic()+
  scale_fill_grey(name= "Time of Day", start = 0.8, end = 0.3,
                  labels=c("Midday", "Crepuscular"))+    
  xlab("") + 
  ylab("Number of Occurrences") +
  #theme(axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank())+
  theme(axis.title = element_text(size = 12)) +
  theme(axis.text= element_text(size=12))+
  theme(legend.title= element_text(size= 16))+ 
  theme(legend.position = c(0.88, 0.88))+
  annotate('text', x = 0.75, y = 340, label = '(d)')
lfTOD_loc

LFexpNA <- subset(lionfish_removals, Cap_exp_new !="NA")

lfexp_loc <- ggplot(LFexpNA, aes(Sub_region, fill = Cap_exp_new))+
  geom_bar(position = "stack")+
  theme_classic()+
  scale_fill_grey(name= "Experience \nLevel", start = 0.8, end = 0.3,
                  labels=c("High", "Medium", "Low", "None"))+     
  xlab("") + 
  ylab("Number of Occurrences") +
  theme(axis.title = element_text(size = 12)) +
  theme(axis.text= element_text(size=12))+
  theme(legend.title= element_text(size= 16))+ 
  theme(legend.position = c(0.88, 0.8))+
  annotate('text', x = 0.75, y = 340, label = '(c)')
lfexp_loc

finalbar = plot_grid(lfvis_loc, lfbehave_loc,lfexp_loc,lfTOD_loc, 
                     nrow = 2, ncol = 2)
finalbar

#############

library(reshape2)
library(plyr)
library(ggplot2)
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

experience  <- ggplot(lionfish_removals, aes(Cap_exp_new,Adj_min, fill = TOD))+
  geom_boxplot(outlier.shape = 21,  outlier.size = 2, show.legend = TRUE)+
  theme_classic()+
  scale_fill_manual(name= "Time of Day", values=cbPalette,
                  labels=c("Crepuscular","Midday" )) +
  xlab("Remover Experience") + 
  ylab(expression('Removal time/ fish (min)'))+
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+ 
  theme(legend.position = c(0.1, 0.85))
experience

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
