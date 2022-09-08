

install.packages(c('maps', 'maptools','mapdata', 'ggmap', 'ggrepel',
                   'raster', 'ggthemes','ggsn','rgeos','rgdal'))

#load libraries we may need (we won't use all of them)
library(mapdata)
library(maps)
library(maptools)
library(ggmap)
library(raster)
library(ggsn)
library(tidyverse)
library(cowplot)
library(here)

#ccodes() #country codes so you know what to pull
#USA is United States
#MX is Mexico
#VIR for us virgin islands

#pull data from the internet
us <- getData('GADM', country="USA", level= 1)
mx <- getData('GADM', country="MEX", level= 1)

##pull individual countries to "create" the Caribbean
bz <- getData('GADM', country="BLZ", level= 1)
gt <- getData('GADM', country="GTM", level= 1)


bs <- getData('GADM', country="BHS", level= 1)
bs <- readRDS(here::here(
  gadm36_BHS_1_sp.rds
))


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

#bind the countries together
carib <- raster::bind (us, mx, bz, gt, bs, cb, cy, jm, tc, hi, dr, pr, vb, vs)
plot(carib)
#Subset areas of the US for regional maps
florida <- c('Florida')
gulf <- c('Texas', "Louisiana")
us.fl <- us[us$NAME_1 %in% florida,]
us.tx <- us[us$NAME_1 %in% gulf,]

#read in the data and subset to keep only what we need
site_data = readr::read_csv(here::here('./data/raw/site_coordinates.csv'))

site_data$Region <- factor(site_data$Region, 
                                       levels = c("FGBNMS","BNP","FKNMS", 
                                                   "BIRNM") )
sites = keys_fish_data %>% 
  dplyr::filter(Type == 'Site') 

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
           transform = TRUE, model = 'WGS84', location = 'bottomleft', st.dist = 0.5, height = 0.18)
  # + #transform = TRUE assumes coordinates are in decimal degrees
  # ggmap::north(location = 'topright', scale = 0.9, symbol = 12, #add north arrow
  #       x.min = -92.75, x.max = -94.75, y.min = 17.5, y.max = 19.5)
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
buck_map = ggplot()+
  geom_polygon(data = vs,aes(x=long,y=lat,group=group), colour = 'grey40', size = 0.01,
               fill = 'grey90')+
  coord_cartesian(xlim = c(-64.661,-64.567), ylim = c(17.74,17.81)) +
  geom_point(data = site_data, aes(x=Long,y=Lat, fill= Region, shape = Region), size = 3)+ #acutally plot the points of our sites
  scale_shape_manual(values = c(16,16,16,20)) + #use the shapes we want (need these ones cuz they have black borders)
  scale_fill_grey()+
  fte_theme_map_small() +
  labs(x = 'Longitude', y = 'Latitude')+
  annotate('text', x = -64.61, y = 17.8, label = 'Buck Isand', size = 5, fontface = 'italic')+
  #annotate('text', x = -64.66, y = 17.745, label = 'St Croix', size = 5, fontface = 'bold')+
  scalebar(x.min = -64.661, x.max = -64.545, y.min = 17.744, y.max = 17.753, dist = 2.5, dist_unit = 'km', #add scalebar
           transform = TRUE, model = 'WGS84', location = 'bottomleft', st.dist = 0.49, height = 0.18) #transform = TRUE assumes coordinates are in decimal degrees
buck_map
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


