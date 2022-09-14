

#install.packages(c('maps', 'maptools','mapdata', 'ggmap', 'ggrepel',
                  # 'raster', 'ggthemes','ggsn','rgeos','rgdal'))

#load libraries we may need (we won't use all of them)
#library(mapdata)
library(maps)
library(maptools)
library(ggmap)
library(raster)
library(ggsn)
library(tidyverse)
library(cowplot)
library(here)


#pull data geo-data folder, has alredy been downloaded to the repo to save time
usa <- readRDS(here::here('./data/geo-data/gadm36_USA_1_sp.rds'))
mex <- readRDS(here::here('./data/geo-data/gadm36_MEX_1_sp.rds'))

##pull individual countries to "create" the Caribbean
blz <- readRDS(here::here('./data/geo-data/gadm36_BLZ_1_sp.rds'))
gtm <- readRDS(here::here('./data/geo-data/gadm36_GTM_1_sp.rds'))
bhs <- readRDS(here::here('./data/geo-data/gadm36_BHS_1_sp.rds'))
cub <- readRDS(here::here('./data/geo-data/gadm36_CUB_1_sp.rds'))
cym <- readRDS(here::here('./data/geo-data/gadm36_CYM_1_sp.rds'))
jam <- readRDS(here::here('./data/geo-data/gadm36_JAM_1_sp.rds'))
tca <- readRDS(here::here('./data/geo-data/gadm36_TCA_1_sp.rds'))
hti <- readRDS(here::here('./data/geo-data/gadm36_HTI_1_sp.rds'))
dom <- readRDS(here::here('./data/geo-data/gadm36_DOM_1_sp.rds'))
pri <- readRDS(here::here('./data/geo-data/gadm36_PRI_1_sp.rds'))
vgb <- readRDS(here::here('./data/geo-data/gadm36_VGB_1_sp.rds'))
vir <- readRDS(here::here('./data/geo-data/gadm36_VIR_1_sp.rds'))

#bind the countries together
carib <- raster::bind (usa, mex, blz, gtm, bhs, cub, cym, jam, tca, hti, 
                       dom, pri, vgb, vir)

#Subset areas of the US for regional maps
florida <- c('Florida')
gulf <- c('Texas', "Louisiana")
us.fl <- usa[usa$NAME_1 %in% florida,]
us.tx <- usa[usa$NAME_1 %in% gulf,]

#read in the data and subset to keep only what we need
site_data = readr::read_csv(here::here('./data/raw/site_coordinates.csv'))

site_data$Region <- factor(site_data$Region, 
                                       levels = c("FGBNMS","BNP","FKNMS", 
                                                   "BIRNM") )

#make the themes
fte_theme_map_continent <- function(){
  color.background = 'grey'
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

fte_theme_map_florida <- function(){
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

fte_theme_map_USVI <- function(){
  color.background = 'coral'
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

fte_theme_map_FGBNMS <- function(){
  color.background = 'black'
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


#make larger map to put study area in context
continent = ggplot()+
  geom_polygon(data = carib,aes(x=long,y=lat,group=group), colour = 'grey70', #put in the actual shape file
               size = 0.01, fill = 'grey70')+
  coord_cartesian(xlim = c(-96,-63), ylim = c(17,36)) + #delimit where we are
  fte_theme_map_continent() + #bring in the map theme
  annotate("rect", xmin = -79.75, xmax = -81.25, ymin = 24.6, ymax = 25.6, 
           alpha = .2,fill= "blue", size =0.6, colour = "blue")+ #florida
  annotate("rect", xmin = -93.1, xmax = -94.6, ymin = 27.5, ymax = 28.5, 
           alpha = .2, fill = "black",size =0.6, colour= "black")+ #fgbnms
  annotate("rect", xmin = -65.25, xmax = -64.25, ymin = 17.5, ymax = 18, 
           alpha = .2,fill = "coral",size =0.6,colour= "coral")+ #usvi
  annotate('text', x = -91, y = 25.8, label = 'Gulf of Mexico', size = 7, 
           fontface = 'italic')+
  annotate('text', x = -72, y = 25.8, label = 'Atlantic Ocean', size = 7, 
           fontface = 'italic')+
  scalebar(x.min = -96, x.max = -88, y.min = 17.5, y.max = 18.5, dist = 250, 
           dist_unit = 'km', st.size = 5, #add scalebar
           transform = TRUE, model = 'WGS84', location = 'bottomleft',
           st.dist = 0.5, height = 0.18)+
   ggsn::north(location = 'topright', scale = 0.9, symbol = 12, #add north arrow
        x.min = -92.75, x.max = -94.75, y.min = 17.5, y.max = 19.5)

continent

florida_map = ggplot()+
  geom_polygon(data = us.fl,aes(x=long,y=lat,group=group), colour = 'grey40', 
               size = 0.01, fill = 'grey90',show.legend = FALSE)+
  coord_cartesian(xlim = c(-81.25, -79.75), ylim = c(24.6,25.6)) +
  geom_point(data = site_data, aes(x=Long,y=Lat, fill= Region, 
                                   shape = Region), size = 4)+ #acutally plot the points of our sites
  scale_shape_manual(values = c(20,25,22,23)) + #use the shapes we want (need these ones cuz they have black borders)
  scale_fill_manual(values = c("black", "blue","cornflowerblue","coral"))+ #make the points fill properly
  fte_theme_map_florida() +
  labs(x = 'Longitude', y = 'Latitude')+
  annotate('text', x = -80.65, y = 25.55, label = 'Florida', size = 6, 
           fontface = 'italic')+
  scalebar(x.min = -80.25, x.max = -80.35, y.min = 24.65, y.max = 24.78, 
           dist = 25, dist_unit = 'km', #add scalebar
           transform = TRUE, model = 'WGS84', location = 'bottomleft', 
           st.dist = 0.4, height = 0.18) #transform = TRUE assumes coordinates are in decimal degrees
florida_map

USVI_map = ggplot()+
  geom_polygon(data = vir,aes(x=long,y=lat,group=group), colour = 'grey40', 
               size = 0.01, fill = 'grey90')+
  coord_cartesian(xlim = c(-64.7,-64.57), ylim = c(17.7,17.82)) +
  geom_point(data = site_data, aes(x=Long,y=Lat, fill= Region, shape = Region), 
             size = 4)+ #acutally plot the points of our sites
  scale_shape_manual(values = c(20,25,22,23)) + #use the shapes we want (need these ones cuz they have black borders)
  scale_fill_manual(values = c("black", "blue","cornflowerblue","coral"))+ 
  fte_theme_map_USVI() +
  labs(x = 'Longitude', y = 'Latitude')+  
  annotate('text', x = -64.635, y = 17.815, label = 'St.Croix', size = 6, 
           fontface = 'italic')+
  scalebar(x.min = -64.57, x.max = -64.6, y.min = 17.708, y.max = 17.725, 
           dist = 1.5, dist_unit = 'km', #add scalebar
           transform = TRUE, model = 'WGS84', location = 'bottomleft', 
           st.dist = 0.35, height = 0.18) #transform = TRUE assumes coordinates are in decimal degrees
USVI_map


FGB_map = ggplot()+
  geom_polygon(data = us.tx,aes(x=long,y=lat,group=group), colour = 'grey40',
               size = 0.01,  fill = 'grey90')+
  coord_cartesian(xlim = c(-93.84,-93.56), ylim = c(27.87,27.9125)) +
  geom_point(data = site_data, aes(x=Long,y=Lat, fill= Region, 
                                   shape = Region), size = 4)+ #actually plot the points of our sites
  scale_shape_manual(values = c(20,25,22,18)) + #use the shapes we want (need these ones cuz they have black borders)
  scale_fill_manual(values = c("black", "blue","cornflowerblue","coral"))+ 
  fte_theme_map_FGBNMS() +
  labs(x = 'Longitude', y = 'Latitude')+
  annotate('text', x = -93.7, y = 27.91, label = 'Gulf of Mexico', size = 6, 
           fontface = 'italic')+
  scalebar(x.min = -93.57, x.max = -93.65, y.min = 27.8717, y.max = 27.8767, 
           dist = 5, dist_unit = 'km', #add scalebar
           transform = TRUE, model = 'WGS84', location = 'bottomleft',
           st.dist = 0.4, height = 0.18) #transform = TRUE assumes coordinates are in decimal degrees
FGB_map


#make the one plot inset with the other
insetmap = ggdraw()+
  draw_plot(continent) + 
  draw_plot(FGB_map, x=0.0075, y=0.663, width=0.328, height=0.33)+
  draw_plot(florida_map, x=0.337, y=0.663, width=0.328, height=0.33)+ 
  draw_plot(USVI_map, x=0.66644, y=0.663, width=0.328, height=0.33) 
insetmap

ggplot2::ggsave(here('./figures/study_map.png'), plot = insetmap,
       width = 13, height = 9,
       dpi = 300)


