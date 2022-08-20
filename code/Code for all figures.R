########packages
##################
library(ggplot2)
library(ggcorrplot)
library(devtools)
devtools::install_github("nschiett/fishualize", force = TRUE)
install.packages("fishualize")
library(fishualize)
library(PNWColors)
library(cowplot)
library (plotrix)
install.packages(c('maps', 'maptools','mapdata', 'ggmap', 'ggrepel',
                   'raster', 'ggthemes','ggsn','rgeos','rgdal'))
#load libraries we may need (we won't use all of them)
library(mapdata)
library(maps)
library(maptools)
library(ggmap)
library(ggrepel)
library(raster)
library(ggthemes)
library(ggsn)
library(rgeos)
library(rgdal)
library(tidyverse)
library(cowplot)



#############
###########Figure 1: Site Map
######################
getData('ISO3') 
ccodes()
#UMI is the minor outlying islands
#BHS for bahamas
#VIR for us virgin islands

#pull data from the internet
us <- getData('GADM', country="USA", level= 1)
mx <- getData('GADM', country="MEX", level= 1)
#ui <- getData('GADM', country="UMI", level= 1)
bz <- getData('GADM', country="BLZ", level= 1)
gt <- getData('GADM', country="GTM", level= 1)
bs <- getData('GADM', country="BHS", level= 1)
cb <- getData('GADM', country="CUB", level= 1)
cy <- getData('GADM', country="CYM", level= 1)
jm <- getData('GADM', country="JAM", level= 1)
tc <- getData('GADM', country="TCA", level= 1)
hi <- getData('GADM', country="HTI", level= 1)
dr <- getData('GADM', country="DOM", level= 1)
pr <- getData('GADM', country="PRI", level= 1)
vb <- getData('GADM', country="VGB", level= 1)
vs <- getData('GADM', country="VIR", level= 1)
#ag <- getData('GADM', country="AIA", level= 1)

carib <- bind (us, mx, bz, gt, bs, cb, cy, jm, tc, hi, dr, pr, vb, vs)
plot(carib)
#keep only Florida
florida <- c('Florida')
gulf <- c('Texas', "Louisiana")
us.fl <- us[us$NAME_1 %in% florida,]
us.tx <- us[us$NAME_1 %in% gulf,]

#read in the data and keep only what we need
site_data = read_csv('site_coordinates.csv')

site_data$Region <- factor(site_data$Region, 
                           levels = c("FGBNMS","BNP","FKNMS", 
                                      "BIRNM") )
sites = keys_fish_data %>% 
  filter(Type == 'Site') 

#make the themes
fte_theme_map_small <- function(){
  color.background = 'blue'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill= 'white',color = 'white')) +
    theme(plot.background = element_rect(fill = color.background, color = color.background)) +
    theme(panel.border = element_rect(colour = 'white', fill=NA, size=.15)) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 15, vjust = 1.25)) +
    theme(axis.text.x = element_blank()) + 
    theme(axis.text.y = element_blank()) + 
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.line.x = element_line(color="white", size = 0.15),
          axis.line.y = element_line(color="white", size = 0.15)) +
    theme(legend.position = "none")
}
fte_theme_map_sites <- function(){
  color.background = 'black'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill = 'white', color = 'white')) +
    theme(plot.background = element_rect(fill=color.background,color = color.background)) +
    theme(panel.border = element_rect(colour = 'black')) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_line(color="black", size = 0.15)) +
    theme(plot.title = element_text(color = color.title, size = 15, vjust = 1.25)) +
    theme(axis.text.x = element_text(size = 12, color = color.axis.text, angle = 90)) + 
    theme(axis.text.y = element_text(size = 12, color = color.axis.text)) + 
    theme(axis.title.x = element_text(size = 14, color = color.axis.title, vjust = 0)) +
    theme(axis.title.y = element_text(size = 14, color = color.axis.title, vjust = 1.25)) +
    theme(plot.title = element_blank()) +
    theme(axis.line.x = element_line(color="black", size = 0.15),
          axis.line.y = element_line(color="black", size = 0.15)) +
    theme(legend.position = "none",
          legend.background = element_rect(colour = 'black'),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 12))
}

#make larger map to put study area in context
continent = ggplot()+
  geom_polygon(data = carib,aes(x=long,y=lat,group=group), colour = 'grey70', #put in the acutal shape file
               size = 0.01, fill = 'grey70')+
  # geom_polygon(data = us.states,aes(x=long,y=lat,group=group), colour = 'midnightblue', size = 0.01, #put in FL shapefile
  #              fill = 'grey70')+
  coord_cartesian(xlim = c(-96,-63), ylim = c(17,36)) + #delimit where we are
  fte_theme_map_small() + #bring in the map
  annotate("rect", xmin = -79.75, xmax = -81.25, ymin = 24.6, ymax = 25.6, alpha = .2,fill= "blue", size =0.6, colour = "blue")+ #florida
  annotate("rect", xmin = -93.1, xmax = -94.6, ymin = 27.5, ymax = 28.5, alpha = .2, fill = "black",size =0.6, colour= "black")+ #fgbnms
  annotate("rect", xmin = -65.25, xmax = -64.25, ymin = 17.5, ymax = 18, alpha = .2,fill = "coral",size =0.6,colour= "coral")+ #usvi
  annotate('text', x = -91, y = 25.8, label = 'Gulf of Mexico', size = 7, fontface = 'italic')+
  annotate('text', x = -72, y = 25.8, label = 'Atlantic Ocean', size = 7, fontface = 'italic')+
  # annotate('text', x = -64.25, y = 31.2, label = 'Study Areas', size = 4)+
  # annotate("rect", xmin = -66.75, xmax = -65.75, ymin = 30.9, ymax = 31.4, alpha = .7,colour= "black")+
  # annotate('segment',x=-87, y=27.28, xend=-82.2, yend=25.1, arrow=arrow(length = unit(0.04, "npc")), #arrow
  #          alpha = 0.8, size=1.1, color="black")+
  scalebar(x.min = -96, x.max = -88, y.min = 17.5, y.max = 18.5, dist = 250, dist_unit = 'km', st.size = 5, #add scalebar
           transform = TRUE, model = 'WGS84', location = 'bottomleft', st.dist = 0.5, height = 0.18)+ #transform = TRUE assumes coordinates are in decimal degrees
  north(location = 'topright', scale = 0.9, symbol = 12, #add north arrow
        x.min = -92.75, x.max = -94.75, y.min = 17.5, y.max = 19.5)
continent

florida_map = ggplot()+
  geom_polygon(data = us.fl,aes(x=long,y=lat,group=group), colour = 'grey40', size = 0.01, 
               fill = 'grey90',show.legend = FALSE)+
  coord_cartesian(xlim = c(-81.25, -79.75), ylim = c(24.6,25.6)) +
  geom_point(data = site_data, aes(x=Long,y=Lat, fill= Region, 
                                   shape = Region), size = 4)+ #acutally plot the points of our sites
  scale_shape_manual(values = c(20,25,22,23)) + #use the shapes we want (need these ones cuz they have black borders)
  scale_fill_manual(values = c("black", "blue","cornflowerblue","coral"))+ #make the points fill properly
  fte_theme_map_small() +
  labs(x = 'Longitude', y = 'Latitude')+
  annotate('text', x = -80.65, y = 25.55, label = 'Florida', size = 6, fontface = 'italic')+
  scalebar(x.min = -80.25, x.max = -80.35, y.min = 24.65, y.max = 24.78, dist = 25, dist_unit = 'km', #add scalebar
           transform = TRUE, model = 'WGS84', location = 'bottomleft', st.dist = 0.4, height = 0.18) #transform = TRUE assumes coordinates are in decimal degrees
florida_map

USVI_map = ggplot()+
  geom_polygon(data = vs,aes(x=long,y=lat,group=group), colour = 'grey40', size = 0.01, 
               fill = 'grey90')+
  coord_cartesian(xlim = c(-64.7,-64.57), ylim = c(17.7,17.82)) +
  geom_point(data = site_data, aes(x=Long,y=Lat, fill= Region, shape = Region), size = 4)+ #acutally plot the points of our sites
  scale_shape_manual(values = c(20,25,22,23)) + #use the shapes we want (need these ones cuz they have black borders)
  scale_fill_manual(values = c("black", "blue","cornflowerblue","coral"))+ 
  fte_theme_map_small() +
  labs(x = 'Longitude', y = 'Latitude')+  
  annotate('text', x = -64.635, y = 17.815, label = 'St.Croix', size = 6, fontface = 'italic')+
  # annotate('text', x = -64.67, y = 17.738, label = 'St Croix', size = 6)+
  scalebar(x.min = -64.57, x.max = -64.6, y.min = 17.708, y.max = 17.725, dist = 1.5, dist_unit = 'km', #add scalebar
           transform = TRUE, model = 'WGS84', location = 'bottomleft', st.dist = 0.35, height = 0.18) #transform = TRUE assumes coordinates are in decimal degrees
USVI_map
# buck_map = ggplot()+
#   geom_polygon(data = vs,aes(x=long,y=lat,group=group), colour = 'grey40', size = 0.01, 
#                fill = 'grey90')+
#   coord_cartesian(xlim = c(-64.661,-64.567), ylim = c(17.74,17.81)) +
#   geom_point(data = site_data, aes(x=Long,y=Lat, fill= Region, shape = Region), size = 3)+ #acutally plot the points of our sites
#   scale_shape_manual(values = c(16,16,16,20)) + #use the shapes we want (need these ones cuz they have black borders)
#   scale_fill_grey()+
#   fte_theme_map_small() +
#   labs(x = 'Longitude', y = 'Latitude')+
#   annotate('text', x = -64.61, y = 17.8, label = 'Buck Isand', size = 5, fontface = 'italic')+
#   #annotate('text', x = -64.66, y = 17.745, label = 'St Croix', size = 5, fontface = 'bold')+
#   scalebar(x.min = -64.661, x.max = -64.545, y.min = 17.744, y.max = 17.753, dist = 2.5, dist_unit = 'km', #add scalebar
#            transform = TRUE, model = 'WGS84', location = 'bottomleft', st.dist = 0.49, height = 0.18) #transform = TRUE assumes coordinates are in decimal degrees
# buck_map
usviall = ggdraw()+
  draw_plot(USVI_map) + 
  draw_plot(buck_map, x=0.015, y=0.667, width=0.35, height=0.32)
usviall

FGB_map = ggplot()+
  geom_polygon(data = us.tx,aes(x=long,y=lat,group=group), colour = 'grey40', size = 0.01, 
               fill = 'grey90')+
  coord_cartesian(xlim = c(-93.84,-93.56), ylim = c(27.87,27.9125)) +
  geom_point(data = site_data, aes(x=Long,y=Lat, fill= Region, 
                                   shape = Region), size = 4)+ #acutally plot the points of our sites
  scale_shape_manual(values = c(20,25,22,18)) + #use the shapes we want (need these ones cuz they have black borders)
  scale_fill_manual(values = c("black", "blue","cornflowerblue","coral"))+ 
  fte_theme_map_small() +
  labs(x = 'Longitude', y = 'Latitude')+
  annotate('text', x = -93.7, y = 27.91, label = 'Gulf of Mexico', size = 6, fontface = 'italic')+
  scalebar(x.min = -93.57, x.max = -93.65, y.min = 27.8717, y.max = 27.8767, dist = 5, dist_unit = 'km', #add scalebar
           transform = TRUE, model = 'WGS84', location = 'bottomleft', st.dist = 0.4, height = 0.18) #transform = TRUE assumes coordinates are in decimal degrees
FGB_map


flowers = ggdraw()+
  draw_plot(FGB_map) + 
  draw_plot(banks_map, x=0.0075, y=0.693, width=0.33, height=0.30)
flowers
#make the one plot inset with the other
insetmap = ggdraw()+
  draw_plot(continent) + 
  draw_plot(FGB_map, x=0.0075, y=0.663, width=0.328, height=0.33)+
  draw_plot(florida_map, x=0.337, y=0.663, width=0.328, height=0.33)+ 
  draw_plot(USVI_map, x=0.66644, y=0.663, width=0.328, height=0.33) 
insetmap

ggsave('study_map.png', plot = insetmap,
       width = 8, height = 7.5,
       dpi = 300)








#####




#############
###########Figure 2:Time series line graph
###############
#create new dataframe
library (plotrix)

lionfish_removals$Sub_region <- factor(lionfish_removals$Sub_region, 
                                       levels = c("FKNMS", "BNP",
                                                  "FGBNMS", "BIRNM") )
lionfish_removals$Date2 <- factor(lionfish_removals$Date2, 
                                  levels = c("Winter 2013", "Spring 2013", "Summer 2013", "Fall 2013",
                                             "Winter 2014", "Spring 2014", "Summer 2014", "Fall 2014",
                                             "Winter 2015", "Spring 2015", "Summer 2015", "Fall 2015",
                                             "Summer 2016", "Summer 2018", "Summer 2019") )

colllll= pnw_palette('Bay',4)

fullLFremovals= lionfish_removals %>% 
  group_by(Sub_region, Date2) %>% 
  dplyr::summarize(Avg_prop = mean(Proportion_caught),
                   Avg_size = mean(Lionfish_size_TL),
                   Avg_density = mean(SiteDens),
                   Avg_time = mean(Adj_min),
                   std_prop = std.error(Proportion_caught),
                   std_size = std.error(Lionfish_size_TL),
                   std_density = std.error(SiteDens),
                   std_time = std.error(Adj_min))

summary(fullLFremovals)
unique(lionfish_removals$Date2)

timetime <- ggplot(fullLFremovals,aes(x= Date2, y=Avg_time, 
                                      group = `Sub_region`, 
                                      colour = `Sub_region`, 
                                      linetype = `Sub_region`,
                                      shape = `Sub_region`))+
  theme_classic()+
  geom_point(aes(shape=Sub_region, size=2),show.legend = FALSE)+
  geom_line(size = 1.2) +
  geom_point(size = 3)+
  scale_color_discrete()+
  #manual('Sub_region', values = colllll) +
  scale_linetype_manual('Sub_region', values=c(1,1,1,1)) +
  scale_shape_manual('Sub_region', values = c(19,15,17,18)) +
  geom_errorbar(aes(ymin=Avg_time-std_time, ymax=Avg_time+std_time), width=1)+
  xlab("") + 
  ylab("Average time (min)") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.title= element_blank())+
  theme(legend.justification=c(0.025,1),legend.position=c(0.025,1))
timetime


timeden <- ggplot(fullLFremovals,aes(x= Date2, y=Avg_density*10000,  
                                     group = `Sub_region`, 
                                     colour = `Sub_region`, 
                                     linetype = `Sub_region`,
                                     shape = `Sub_region`))+
  theme_classic()+
  geom_point(aes(shape=Sub_region, size=2),show.legend = FALSE)+
  geom_line(size = 1.2) +
  geom_point(size = 3)+
  scale_color_discrete()+
  #manual('Sub_region', values = colllll) +
  scale_linetype_manual('Sub_region', values=c(1,1,1,1)) +
  scale_shape_manual('Sub_region', values = c(19,15,17,18)) +
  geom_errorbar(aes(ymin=(Avg_density-std_density)*10000, ymax=(Avg_density+std_density)*10000), width=.5)+
  xlab("") + 
  ylab("Average density/ hectare") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none")
timeden


timesize <- ggplot(fullLFremovals,aes(x= Date2, y=Avg_size, group = `Sub_region`, 
                                      colour = `Sub_region`, 
                                      linetype = `Sub_region`,
                                      shape = `Sub_region`))+
  theme_classic()+
  geom_point(aes(shape=Sub_region, size=2),show.legend = FALSE)+
  geom_line(size = 1.2) +
  geom_point(size = 3)+
  scale_color_discrete()+
  #manual('Sub_region', values = colllll) +
  scale_linetype_manual('Sub_region', values=c(1,1,1,1)) +
  scale_shape_manual('Sub_region', values = c(19,15,17,18)) +
  geom_errorbar(aes(ymin=Avg_size-std_size, ymax=Avg_size+std_size), width=.5)+
  xlab("") + 
  ylab("Average  size (cm)") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none")
timesize

timeprop <- ggplot(fullLFremovals,aes(x= Date2, y=Avg_prop, group = `Sub_region`, 
                                      colour = `Sub_region`, 
                                      linetype = `Sub_region`,
                                      shape = `Sub_region`))+
  theme_classic()+
  geom_point(aes(shape=Sub_region, size=2),show.legend = FALSE)+
  geom_line(size = 1.2) +
  geom_point(size = 3)+
  scale_color_discrete()+
  #manual('Sub_region', values = colllll) +
  scale_linetype_manual('Sub_region', values=c(1,1,1,1)) +
  scale_shape_manual('Sub_region', values = c(19,15,17,18)) +
  scale_x_discrete(breaks= ("Summer 2016"))+
  geom_errorbar(aes(ymin=Avg_prop-std_prop, ymax=Avg_prop+std_prop), width=1)+
  xlab("") + 
  ylab("Proportion Removed") +
  theme(axis.title = element_text(size = 16)) +
  theme(axis.text= element_text(size=12))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.title= element_blank()) +
  theme(legend.position="none")
timeprop

library(cowplot)
finaltime = plot_grid(timetime,timeprop,timesize,timeden, 
                      align = "v",
                      nrow = 2, ncol = 2)
finaltime





#####




#############
###########Figure 3:Parameter estimates
#############
#### creating a estimates plot
library(ggplot2)

est_capALL = avgmod.95pCMALL$coefficients
est_capALL = est_capALL[2,]
est_capALL = as.numeric(est_capALL)

confCMALL = confint(avgmod.95pCMALL)
var_namesCMALL = colnames(data.frame(avgmod.95pCMALL$coefficients))

plot_CMALL = data.frame(cbind(est_capALL, confCMALL, var_namesCMALL)) 
plot_CMALL = plot_CMALL %>% 
  dplyr::rename(est_capALL = est_capALL, low = `X2.5..`, high = `X97.5..`)

plot_CMALL$low = as.numeric(as.character(plot_CMALL$low))
plot_CMALL$est_capALL = as.numeric(as.character(plot_CMALL$est_capALL))
plot_CMALL$high = as.numeric(as.character(plot_CMALL$high))

plot_CMALL = plot_CMALL[2:9,]
test_plot_cmall = plot_CMALL
test_plot_cmall$var_namesCMALL= as.character(test_plot_cmall$var_namesCMALL)

test_plot_cmall[1,5]= ("sig")
test_plot_cmall[2,5]= ("sig")
test_plot_cmall[3,5]= ("sig")
test_plot_cmall[4,5]= ("sig")
test_plot_cmall[5,5]= ("not")
test_plot_cmall[6,5]= ("not")
test_plot_cmall[7,5]= ("sig")
test_plot_cmall[8,5]= ("not")

test_plot_cmall[1,6]= ("neg")
test_plot_cmall[2,6]= ("pos")
test_plot_cmall[3,6]= ("pos")
test_plot_cmall[4,6]= ("pos")
test_plot_cmall[5,6]= ("pos")
test_plot_cmall[6,6]= ("neg")
test_plot_cmall[7,6]= ("pos")
test_plot_cmall[8,6]= ("neg")

# test_plot_cmall[8,1]= 2
# test_plot_cmall[8,2]= 0
# test_plot_cmall[8,3]= 0
# test_plot_cmall[8,4]= ("Lionfish_size_TL.Lionfish_visibilitySH")
# test_plot_cmall[8,5]= ("not")

test_plot_cmall[9,1]= 2
test_plot_cmall[9,2]= 0
test_plot_cmall[9,3]= 0
test_plot_cmall[9,4]= ("Lionfish_visibilitySH")
test_plot_cmall[9,5]= ("not")

test_plot_cmall[10,1]= 2
test_plot_cmall[10,2]= 0
test_plot_cmall[10,3]= 0
test_plot_cmall[10,4]= ("Remover Experience")
test_plot_cmall[10,5]= ("not")

test_plot_cmall[11,1]= 2
test_plot_cmall[11,2]= 0
test_plot_cmall[11,3]= 0
test_plot_cmall[11,4]= ("Number of Attempts")
test_plot_cmall[11,5]= ("not")

test_plot_cmall[12,1]= 2
test_plot_cmall[12,2]= 0
test_plot_cmall[12,3]= 0
test_plot_cmall[12,4]= ("Lionfish_behaviourrest")
test_plot_cmall[12,5]= ("not")

test_plot_cmall[13,1]= 2
test_plot_cmall[13,2]= 0
test_plot_cmall[13,3]= 0
test_plot_cmall[13,4]= ("Lionfish_size_TL.Lionfish_visibilitySH")
test_plot_cmall[13,5]= ("not")


test_plot_cmall =test_plot_cmall %>% 
  mutate(real= ifelse(est_capALL==2, "dummy", "real"))
test_plot_cmall$var_namesCMALL <- factor(test_plot_cmall$var_namesCMALL, 
                                         levels = c("TODmidday",
                                                    "Remover Experience",
                                                    "Number of Attempts",
                                                    "Average.AvgGorg",
                                                    "Depth_ft",
                                                    "AvgGorg",
                                                    "AvgCoral",
                                                    "Average",
                                                    "Lionfish_behaviourrest",
                                                    "Lionfish_size_TL.Lionfish_visibilitySH",
                                                    "Lionfish_visibilitySH",
                                                    "Lionfish_size_TL",
                                                    "I.SiteDens...1000."
                                         ))

likelihoodALL<- ggplot(test_plot_cmall, fill = "white") +
  geom_vline(xintercept = 0, color = "black", size=0.5)+
  geom_point(aes(x = est_capALL, y = var_namesCMALL, color= real,
                 shape = V6), size =3, show.legend = FALSE)+
  geom_errorbarh(aes(xmin=low, xmax = high, y = var_namesCMALL), 
                 height = 0 , colour = 'Black', size = 1)+
  scale_x_continuous(limits = c(-.78, .78)) +
  scale_color_manual(values= c( "white", "black"))+
  scale_shape_manual(values=c(19, 19)) +
  theme_classic()+ 
  scale_y_discrete(labels=c("",
                            "","",
                            "","",""," ",
                            "",
                            "", "",
                            "","", ""
  ))+
  xlab("Parameter Estimates") + 
  ylab("") +
  labs(title = "b. Likelihood of Removal")+
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(axis.text= element_text(size=14))+
  theme(axis.title= element_text(size= 16))

likelihoodALL

#### creating a estimates plot
library(ggplot2)

est_propALL = avgmod.95pWPMALL$coefficients
est_propALL = est_propALL[2,]
est_propALL = as.numeric(est_propALL)

confWPMALL = confint(avgmod.95pWPMALL)
var_namesWPMALL = colnames(data.frame(avgmod.95pWPMALL$coefficients))

plot_WPMALL = data.frame(cbind(est_propALL, confWPMALL, var_namesWPMALL)) 
plot_WPMALL = plot_WPMALL %>% 
  dplyr::rename(est_propALL = est_propALL, low = `X2.5..`, high = `X97.5..`)

plot_WPMALL$low = as.numeric(as.character(plot_WPMALL$low))
plot_WPMALL$est_propALL = as.numeric(as.character(plot_WPMALL$est_propALL))
plot_WPMALL$high = as.numeric(as.character(plot_WPMALL$high))

plot_WPMALL = plot_WPMALL[2:9,]
test_plot_wpmall = plot_WPMALL
test_plot_wpmall$var_namesWPMALL= as.character(test_plot_wpmall$var_namesWPMALL)




test_plot_wpmall[9,1]= 2
test_plot_wpmall[9,2]= 0
test_plot_wpmall[9,3]= 0
test_plot_wpmall[9,4]= ("Lionfish_visibilitySH")

test_plot_wpmall[10,1]= 2
test_plot_wpmall[10,2]= 0
test_plot_wpmall[10,3]= 0
test_plot_wpmall[10,4]= ("Remover Experience")

test_plot_wpmall[11,1]= 2
test_plot_wpmall[11,2]= 0
test_plot_wpmall[11,3]= 0
test_plot_wpmall[11,4]= ("Number of Attempts")

test_plot_wpmall[12,1]= 2
test_plot_wpmall[12,2]= 0
test_plot_wpmall[12,3]= 0
test_plot_wpmall[12,4]= ("Adj_behaviourrest")

test_plot_wpmall[13,1]= 2
test_plot_wpmall[13,2]= 0
test_plot_wpmall[13,3]= 0
test_plot_wpmall[13,4]= ("Lionfish_size_TL.Lionfish_visibilitySH")

test_plot_wpmall[1,5]= ("sig")
test_plot_wpmall[2,5]= ("not")
test_plot_wpmall[3,5]= ("sig")
test_plot_wpmall[4,5]= ("sig")
test_plot_wpmall[5,5]= ("sig")
test_plot_wpmall[6,5]= ("not")
test_plot_wpmall[7,5]= ("not")

test_plot_wpmall[1,6]= ("neg")
test_plot_wpmall[2,6]= ("pos")
test_plot_wpmall[3,6]= ("pos")
test_plot_wpmall[4,6]= ("pos")
test_plot_wpmall[5,6]= ("pos")
test_plot_wpmall[6,6]= ("neg")
test_plot_wpmall[7,6]= ("neg")
test_plot_wpmall[8,6]= ("neg")

test_plot_wpmall =test_plot_wpmall %>% 
  mutate(real= ifelse(est_propALL==2, "dummy", "real"))
test_plot_wpmall$var_namesWPMALL <- factor(test_plot_wpmall$var_namesWPMALL, 
                                           levels = c("TODmidday",
                                                      "Remover Experience",
                                                      "Number of Attempts",
                                                      "Average.AvgGorg",
                                                      "Depth_ft",
                                                      "AvgGorg",
                                                      "AvgCoral",
                                                      "Average",
                                                      "Adj_behaviourrest",
                                                      "Lionfish_size_TL.Lionfish_visibilitySH",
                                                      "Lionfish_visibilitySH",
                                                      "Lionfish_size_TL",
                                                      "I.SiteDens...1000."
                                           ))

WeightpropALL<- ggplot(test_plot_wpmall, fill = "white") +
  geom_vline(xintercept = 0, color = "black", size=0.5)+
  geom_point(aes(x = est_propALL, y = var_namesWPMALL, color= real,
                 shape = V6), size =3, show.legend = FALSE)+
  geom_errorbarh(aes(xmin=low, xmax = high, y = var_namesWPMALL), 
                 height = 0 , colour = 'Black', size = 1)+
  scale_x_continuous(limits = c(-.2, .2)) +
  scale_color_manual(values= c( "white", "black"))+
  scale_shape_manual(values=c(19, 19)) +
  theme_classic()+ 
  scale_y_discrete(labels=c("",
                            "","",
                            "","",""," ",
                            "",
                            "", "",
                            "","", ""
  ))+
  xlab("") + 
  ylab("") +
  labs(title = "c. Proportion Removed")+
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(axis.text= element_text(size=14))+
  theme(axis.title= element_text(size= 16))

WeightpropALL

#### creating a estimates plot
library(ggplot2)

est_timeALL = avgmod.95pTMALL$coefficients
est_timeALL = est_timeALL[2,]
est_timeALL = as.numeric(est_timeALL)

confTMALL = confint(avgmod.95pTMALL)
var_namesTMALL = colnames(data.frame(avgmod.95pTMALL$coefficients))

plot_TMALL = data.frame(cbind(est_timeALL, confTMALL, var_namesTMALL)) 
plot_TMALL = plot_TMALL %>% 
  dplyr::rename(est_timeALL = est_timeALL, low = `X2.5..`, high = `X97.5..`)

plot_TMALL$low = as.numeric(as.character(plot_TMALL$low))
plot_TMALL$est_timeALL = as.numeric(as.character(plot_TMALL$est_timeALL))
plot_TMALL$high = as.numeric(as.character(plot_TMALL$high))

plot_TMALL = plot_TMALL[2:8,]
test_plot_tmall = plot_TMALL
test_plot_tmall$var_namesTMALL= as.character(test_plot_tmall$var_namesTMALL)


test_plot_tmall[1,5]= ("not")
test_plot_tmall[2,5]= ("sig")
test_plot_tmall[3,5]= ("not")
test_plot_tmall[4,5]= ("not")
test_plot_tmall[5,5]= ("not")
test_plot_tmall[6,5]= ("not")
test_plot_tmall[7,5]= ("not")

test_plot_tmall[1,6]= ("neg")
test_plot_tmall[2,6]= ("pos")
test_plot_tmall[3,6]= ("neg")
test_plot_tmall[4,6]= ("pos")
test_plot_tmall[5,6]= ("pos")
test_plot_tmall[6,6]= ("pos")
test_plot_tmall[7,6]= ("neg")

test_plot_tmall[8,1]= 2
test_plot_tmall[8,2]= 0
test_plot_tmall[8,3]= 0
test_plot_tmall[8,4]= ("Lionfish_size_TL.Lionfish_visibilitySH")
test_plot_tmall[8,5]= ("not")

test_plot_tmall[9,1]= 2
test_plot_tmall[9,2]= 0
test_plot_tmall[9,3]= 0
test_plot_tmall[9,4]= ("Lionfish_visibilitySH")
test_plot_tmall[9,5]= ("not")

test_plot_tmall[10,1]= 2
test_plot_tmall[10,2]= 0
test_plot_tmall[10,3]= 0
test_plot_tmall[10,4]= ("Remover Experience")
test_plot_tmall[10,5]= ("not")

test_plot_tmall[11,1]= 2
test_plot_tmall[11,2]= 0
test_plot_tmall[11,3]= 0
test_plot_tmall[11,4]= ("Number of Attempts")
test_plot_tmall[11,5]= ("not")

test_plot_tmall[12,1]= 2
test_plot_tmall[12,2]= 0
test_plot_tmall[12,3]= 0
test_plot_tmall[12,4]= ("Lionfish_behaviourrest")
test_plot_tmall[12,5]= ("not")

test_plot_tmall[13,1]= 2
test_plot_tmall[13,2]= 0
test_plot_tmall[13,3]= 0
test_plot_tmall[13,4]= ("Average.AvgGorg")
test_plot_tmall[13,5]= ("not")

test_plot_tmall =test_plot_tmall %>% 
  mutate(real= ifelse(est_timeALL==2, "dummy", "real"))
test_plot_tmall$var_namesTMALL <- factor(test_plot_tmall$var_namesTMALL, 
                                         levels = c("TODmidday",
                                                    "Remover Experience",
                                                    "Number of Attempts",
                                                    "Average.AvgGorg",
                                                    "Depth_ft",
                                                    "AvgGorg",
                                                    "AvgCoral",
                                                    "Average",
                                                    "Lionfish_behaviourrest",
                                                    "Lionfish_size_TL.Lionfish_visibilitySH",
                                                    "Lionfish_visibilitySH",
                                                    "Lionfish_size_TL",
                                                    "I.SiteDens...1000."
                                         ))

TimeALL<- ggplot(test_plot_tmall, fill = "white") +
  geom_vline(xintercept = 0, color = "black", size=0.5)+
  geom_point(aes(x = est_timeALL, y = var_namesTMALL, color = real, 
                 shape = V6), size =3, show.legend = FALSE)+
  geom_errorbarh(aes(xmin=low, xmax = high, y = var_namesTMALL), 
                 height = 0 , colour = 'Black', size = 1)+
  scale_x_continuous(limits = c(-.78, .78)) +
  scale_color_manual(values= c( "white", "black"))+
  scale_shape_manual(values=c(19, 19)) +
  theme_classic()+ 
  scale_y_discrete(labels=c("Mid-day",
                            "Low Experience","Number of Attempts", "VR and Gorgonian",
                            "Depth (ft)","% Gorgonian Cover","% Coral Cover","Vertical Relief (cm)",
                            "Resting",
                            "Small and Sheltered", "Sheltered",
                            "Lionfish Size","Lionfish Density"
  ))+
  xlab("") + 
  ylab("") +
  labs(title = "a. Time for Removal")+
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(axis.text= element_text(size=14))+
  theme(axis.title= element_text(size= 16))

TimeALL

library(cowplot)

finalglobals = plot_grid(TimeALL,likelihoodALL,WeightpropALL,
                         nrow = 1, ncol = 3)
finalglobals


#####




#############
###########Figure 4:Heat plots
#############

pal= pnw_palette('Bay',100)

###likelihood of capture
image.plot(InterplikeavgmostALL, col= (pal),
           main="Likelihood of Capture",
           cex.main=2,
           xlab = "Lionfish size", xlim =c(4,46), 
           ylab = "Lionfish density/ hectare", ylim = c(0,180),
           cex.lab = 2.5, cex.axis = 2,
           axis.args = list(cex.axis = 1.4))

contour(InterplikeavgmostALL, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, labcex = 2.4, labtype = "bold")

### proportion removed

image.plot(Interppropall, col= (pal),
           main="Proportion Removed",
           cex.main=2,
           xlab = "% Coral Cover", xlim =c(0,97), 
           ylab = "Lionfish density/ hectare", ylim = c(0,180),
           cex.lab = 2.5, cex.axis = 2,
           axis.args = list(cex.axis = 1.4))


contour(Interppropall, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, labcex = 2.4, labtype = "bold")

