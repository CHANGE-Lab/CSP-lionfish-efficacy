## Code for Site map/Figure 1 is Davis et al 2021 CSP Paper
##Updated 8 September 2023

#load libraries we may need 
library(ggmap)
library(Rcpp)
library(raster)
library(ggsn)
library(tidyverse)
library(cowplot)
library(here)
here()

#read in the data and subset to keep only what we need
site_data = readr::read_csv(here::here
      ('./data/raw/site_coordinates.csv'))

site_data$Region <- factor(site_data$Region, 
                           levels = c("FGBNMS","BNP","FKNMS", 
                                      "BIRNM") )

###pull in map objects to create maps
bs <- readRDS(here::here('./data/geo-data/gadm36_BHS_1_sp.rds')) #Bahamas
bz <- readRDS(here::here('./data/geo-data/gadm36_BLZ_1_sp.rds')) #Belize
cb <- readRDS(here::here('./data/geo-data/gadm36_CUB_1_sp.rds')) #Cuba
cy <- readRDS(here::here('./data/geo-data/gadm36_CYM_1_sp.rds')) #Cayman Islands
dr <- readRDS(here::here('./data/geo-data/gadm36_DOM_1_sp.rds')) #Dominican Rep
gt <- readRDS(here::here('./data/geo-data/gadm36_GTM_1_sp.rds')) #Guatemala
hi <- readRDS(here::here('./data/geo-data/gadm36_HTI_1_sp.rds')) #Haiti
jm <- readRDS(here::here('./data/geo-data/gadm36_JAM_1_sp.rds')) #Jamaica
mx <- readRDS(here::here('./data/geo-data/gadm36_MEX_1_sp.rds')) #Mexico
pr <- readRDS(here::here('./data/geo-data/gadm36_PRI_1_sp.rds')) #Puerto Rico
tc <- readRDS(here::here('./data/geo-data/gadm36_TCA_1_sp.rds')) #Turks&Caicos
vb <- readRDS(here::here('./data/geo-data/gadm36_VGB_1_sp.rds')) #British VI
vs <- readRDS(here::here('./data/geo-data/gadm36_VIR_1_sp.rds')) #US VI
us <- readRDS(here::here('./data/geo-data/gadm36_USA_1_sp.rds'))#United States

#bind the countries together
fullmap <- raster::bind (us, mx, bz, gt, bs, cb, cy, 
                       jm, tc, hi, dr, pr, vb, vs)
#plot(carib)
write_rds(fullmap, here::here('./data/geo-data/Caribbean.rds'))

#Subset areas of the US for regional maps
florida <- c('Florida')
gulf <- c('Texas', "Louisiana")
us.fl <- us[us$NAME_1 %in% florida,]
us.tx <- us[us$NAME_1 %in% gulf,]


#make the themes
fte_theme_florida <- function(){
  color.background = 'blue'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill= 'white',color = 'white')) +
    theme(plot.background = element_rect(fill = color.background, 
                                         color = color.background)) +
    theme(panel.border = element_rect(colour = 'white', fill=NA, size=.15)) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 15, 
                                    vjust = 1.25)) +
    theme(axis.text.x = element_blank()) + 
    theme(axis.text.y = element_blank()) + 
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.line.x = element_line(color="white", size = 0.15),
          axis.line.y = element_line(color="white", size = 0.15)) +
    theme(legend.position = "none")
}

fte_theme_Gulf <- function(){
  color.background = 'black'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill= 'white',color = 'white')) +
    theme(plot.background = element_rect(fill = color.background, 
                                         color = color.background)) +
    theme(panel.border = element_rect(colour = 'white', fill=NA, size=.15)) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 15, 
                                    vjust = 1.25)) +
    theme(axis.text.x = element_blank()) + 
    theme(axis.text.y = element_blank()) + 
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.line.x = element_line(color="white", size = 0.15),
          axis.line.y = element_line(color="white", size = 0.15)) +
    theme(legend.position = "none")
}

fte_theme_USVI <- function(){
  color.background = 'coral'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill= 'white',color = 'white')) +
    theme(plot.background = element_rect(fill = color.background, 
                                         color = color.background)) +
    theme(panel.border = element_rect(colour = 'white', fill=NA, size=.15)) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 15, 
                                    vjust = 1.25)) +
    theme(axis.text.x = element_blank()) + 
    theme(axis.text.y = element_blank()) + 
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.line.x = element_line(color="white", size = 0.15),
          axis.line.y = element_line(color="white", size = 0.15)) +
    theme(legend.position = "none")
}

fte_theme_large<- function(){
  color.background = 'grey'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill= 'white',color = 'white')) +
    theme(plot.background = element_rect(fill = color.background, 
                                         color = color.background)) +
    theme(panel.border = element_rect(colour = 'white', fill=NA, size=.15)) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 15, 
                                    vjust = 1.25)) +
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
  geom_polygon(data = fullmap,aes(x=long,y=lat,group=group), colour = 'grey70', #put in the acutal shape file
               size = 0.01, fill = 'grey70')+
  coord_cartesian(xlim = c(-96,-63), ylim = c(17,36)) + #delimit where we are
  fte_theme_large() + #bring in the map
  annotate("rect", xmin = -79.75, xmax = -81.25, ymin = 24.6, ymax = 25.6, alpha = .2,fill= "blue", size =0.6, colour = "blue")+ #florida
  annotate("rect", xmin = -93.1, xmax = -94.6, ymin = 27.5, ymax = 28.5, alpha = .2, fill = "black",size =0.6, colour= "black")+ #fgbnms
  annotate("rect", xmin = -65.25, xmax = -64.25, ymin = 17.5, ymax = 18, alpha = .2,fill = "coral",size =0.6,colour= "coral")+ #usvi
  annotate('text', x = -91, y = 25.8, label = 'Gulf of Mexico', size = 7, fontface = 'italic')+
  annotate('text', x = -72, y = 25.8, label = 'Atlantic Ocean', size = 7, fontface = 'italic')+
  scalebar(x.min = -96, x.max = -88, y.min = 17.5, y.max = 18.5, dist = 250, dist_unit = 'km', st.size = 5, #add scalebar
           transform = TRUE, model = 'WGS84', location = 'bottomleft', st.dist = 0.5, height = 0.18)
continent

florida_map = ggplot()+
  geom_polygon(data = us.fl,aes(x=long,y=lat,group=group), colour = 'grey40', size = 0.01, 
               fill = 'grey90',show.legend = FALSE)+
  coord_cartesian(xlim = c(-81.25, -79.75), ylim = c(24.6,25.6)) +
  geom_point(data = site_data, aes(x=Long,y=Lat, fill= Region, 
                                   shape = Region), size = 4)+ #acutally plot the points of our sites
  scale_shape_manual(values = c(20,25,22,23)) + #use the shapes we want (need these ones cuz they have black borders)
  scale_fill_manual(values = c("black", "blue","cornflowerblue","coral"))+ #make the points fill properly
  fte_theme_florida() +
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
  fte_theme_USVI() +
  labs(x = 'Longitude', y = 'Latitude')+  
  annotate('text', x = -64.635, y = 17.815, label = 'St.Croix', size = 6, fontface = 'italic')+
  # annotate('text', x = -64.67, y = 17.738, label = 'St Croix', size = 6)+
  scalebar(x.min = -64.57, x.max = -64.6, y.min = 17.708, y.max = 17.725, dist = 1.5, dist_unit = 'km', #add scalebar
           transform = TRUE, model = 'WGS84', location = 'bottomleft', st.dist = 0.35, height = 0.18) #transform = TRUE assumes coordinates are in decimal degrees
USVI_map


FGB_map = ggplot()+
  geom_polygon(data = us.tx,aes(x=long,y=lat,group=group), colour = 'grey40', size = 0.01, 
               fill = 'grey90')+
  coord_cartesian(xlim = c(-93.84,-93.56), ylim = c(27.87,27.9125)) +
  geom_point(data = site_data, aes(x=Long,y=Lat, fill= Region, 
                                   shape = Region), size = 4)+ #acutally plot the points of our sites
  scale_shape_manual(values = c(20,25,22,18)) + #use the shapes we want (need these ones cuz they have black borders)
  scale_fill_manual(values = c("black", "blue","cornflowerblue","coral"))+ 
  fte_theme_Gulf() +
  labs(x = 'Longitude', y = 'Latitude')+
  annotate('text', x = -93.7, y = 27.91, label = 'Gulf of Mexico', size = 6, fontface = 'italic')+
  scalebar(x.min = -93.57, x.max = -93.65, y.min = 27.8717, y.max = 27.8767, dist = 5, dist_unit = 'km', #add scalebar
           transform = TRUE, model = 'WGS84', location = 'bottomleft', st.dist = 0.4, height = 0.18) #transform = TRUE assumes coordinates are in decimal degrees
FGB_map

#make the one plot inset with the other
insetmap = ggdraw()+
  draw_plot(continent) + 
  draw_plot(FGB_map, x=0.0075, y=0.663, width=0.328, height=0.33)+
  draw_plot(florida_map, x=0.337, y=0.663, width=0.328, height=0.33)+ 
  draw_plot(USVI_map, x=0.66644, y=0.663, width=0.328, height=0.33) 
insetmap

#save map
ggsave(here::here('./figures/Fig1_SiteMap.png'), plot = insetmap,
       width = 12, height = 11.25,
       dpi = 300)
