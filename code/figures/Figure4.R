##########
# This script creates column plots for figure 4  
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
library(ggplot2)
library(cowplot)
library(here)
library(readr)

lionfish_removals = read_csv(here::here('./data/clean/lionfish_removals.csv'))

# Create a colorblind friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", 
               "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

###### order variables how you ant them to display in plots
lionfish_removals$Sub_region <- factor(lionfish_removals$Sub_region, 
                           levels = c("FKNMS", "BNP","FGBNMS", "BIRNM")) 

levels(lionfish_removals$Sub_region) <- gsub(" ", "\n", 
                                    levels(lionfish_removals$Sub_region))

lionfish_removals$Cap_exp_new <- factor(lionfish_removals$Cap_exp_new, 
                                        levels = c("High", "Medium",
                                                   "Low", "None") )
lionfish_removals$TOD <- factor(lionfish_removals$TOD, 
                                levels = c("Crepuscular", "midday") )

####Subset data set for individual plots
LFVISNA <- subset(lionfish_removals, Lionfish_visibility !="NA")
LFBENA <- subset(lionfish_removals, Lionfish_behaviour !="NA")
LFTODNA <- subset(lionfish_removals, TOD !="NA")
LFTODNA$TOD <- factor(LFTODNA$TOD, levels = c( "midday","Crepuscular"))
LFexpNA <- subset(lionfish_removals, Cap_exp_new !="NA")

## panel a lionfish position on reef (visibility)
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

#Panel b lionfish behavior
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

#panel c remover experience level
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

#Panel d time of day
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


finalbar = plot_grid(lfvis_loc, lfbehave_loc,lfexp_loc,lfTOD_loc, 
                     nrow = 2, ncol = 2)
finalbar


ggplot2::ggsave(here('./figures/figure_4.png'), plot = finalbar,
                width = 12, height = 10,
                dpi = 300)
