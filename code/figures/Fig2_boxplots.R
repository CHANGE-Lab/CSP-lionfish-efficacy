########## 
##########
# This script creates box and whisker plots for figure 2 adn supplimental fig 1 
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
#####Location Comparison boxplot stats
library(ggplot2)
library(cowplot)
library(here)
library(tidyverse)

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

## attributes compared over the 4 locations
#panel a, lionfish density
Dens_loc <- ggplot (lionfish_removals) +
  geom_boxplot(aes(y= SiteDens*10000, x = Sub_region, fill = Sub_region),
               outlier.shape = 21,  outlier.size = 2, show.legend = FALSE)+
  theme_classic()+
  scale_fill_manual(values = cbPalette )+
  xlab("") + 
  ylab("Density/ ha") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(legend.title= element_text(size= 16))+
  annotate('text', x = 0.75, y = 100, label = '(a)')
Dens_loc 

#panel b, lionfish size
Size_loc <- ggplot (lionfish_removals) +
  geom_boxplot(aes(y= Lionfish_size_TL, x = Sub_region, fill = Sub_region),
               outlier.shape = 21,  outlier.size = 2, show.legend = FALSE)+
  theme_classic()+
  scale_fill_manual(values = cbPalette )+
  xlab("") + 
  ylab("Size TL (cm)") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(legend.title= element_text(size= 16))+
  annotate('text', x = 0.75, y = 45, label = '(b)')
Size_loc 

#panel c, gorgonian cover
gorg_loc<- ggplot (lionfish_removals) +
  geom_boxplot(aes(y= AvgGorg, x = Sub_region, fill = Sub_region),
               outlier.shape = 21,  outlier.size = 2, show.legend = FALSE)+
  theme_classic()+
  scale_fill_manual(values = cbPalette )+
  xlab("") + 
  ylab("% Gorgonian Cover") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(legend.title= element_text(size= 16))+
  annotate('text', x = 0.75, y = 50, label = '(c)')
gorg_loc 

#panel d, reef size
reefsize_loc  <- ggplot(lionfish_removals, aes( Sub_region,Site_area_m2*.0001, fill = Sub_region))+
  geom_boxplot(outlier.shape = 21,  outlier.size = 2, show.legend = FALSE)+
  theme_classic()+
  scale_fill_manual(values = cbPalette )+
  xlab("") + 
  ylab('Reef Size (Hectares)')+ 
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  annotate('text', x = 0.75, y = 1, label = '(d)')
reefsize_loc

#panel d, depth
depth_loc<- ggplot (lionfish_removals) +
  geom_boxplot(mapping = aes(y= Depth_ft*.3048, x = Sub_region, fill = Sub_region),
               outlier.shape = 21,  outlier.size = 2, show.legend = FALSE)+
  scale_fill_manual(values = cbPalette )+
  xlab("") + 
  ylab("Depth (m)") +
  theme_classic()+
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(legend.title= element_text(size= 16))+
  annotate('text', x = 0.75, y = 35, label = '(e)')
depth_loc

#panel f, number of attempts
numatmp_loc  <- ggplot(lionfish_removals, aes( Sub_region,Num_attempts, fill = Sub_region))+
  geom_boxplot(outlier.shape = 21,  outlier.size = 2, show.legend = FALSE)+
  theme_classic()+
  scale_fill_manual(values = cbPalette )+
  xlab("") + 
  ylab("Number of attempts") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  annotate('text', x = 0.75, y = 10, label = '(f)')
numatmp_loc


finalbox = plot_grid(Dens_loc,Size_loc,gorg_loc, 
                     reefsize_loc,depth_loc,numatmp_loc,
                  nrow = 3, ncol = 2)
finalbox


ggplot2::ggsave(here('./figures/figure_2.png'), plot = finalbox,
                width = 12, height = 10,
                dpi = 300)

#########
##supplemental plot
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