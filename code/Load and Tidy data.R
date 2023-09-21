########## 
##########
# This script inputs raw csv files and cleans data for analyses used  
# in Davis et al. (2021) Conservation Science and Practice manuscript
# An investigation into what effects lionfish removal efficiency and efficacy
##########
##########
# AUTHOR: Alexandra CD Davis
# DATE OF CREATION: 2020-11-10
##########
##########
# Note to future readers. This code is kind of a mess. I acknowledge this. May 
# the coding gods just be happy it runs and is quasi-reproducible

# set-up ======================================================================= 
   library(data.table)
   library(here)
   library(tidyverse)
   library(matrixStats)
   library(triangle)
   library(plyr)
   library(nlme)
   library(perturbR)
   library(ggplot2)
   library(lme4)
   library(MuMIn)
   library(clusterGeneration)
   #require(MASS)
   library(mgcv)
   library(rstatix)
   library(dplyr)
   library(rlang)
   library(car)
   library(readr)
rm(list=ls(all=TRUE))

###Capture effort for lionfish, identifiers are lionfish id and survey number
CaptureEffort = read_csv(here::here('./data/raw/Capture_Effort_Raw.csv'))
str (CaptureEffort)
head(CaptureEffort)
names(CaptureEffort)


###Survey metadata, has the site information associated with each survey number
LionfishMeta = read_csv(here::here('./data/raw/All_Regions_Metadata.csv'))
str(LionfishMeta)
names(LionfishMeta)

###Vertical Relief data set
VerticalR = read_csv(here::here('./data/raw/VRelief_All_Regions.csv'))

unique(VerticalR$Site_ID)

# make new column so that I can sum the number of LF together
AverageVR <- plyr::ddply(VerticalR, c("Site_ID"),
                   summarize, Average = mean(Relief_cm))

data.table::setDT(VerticalR) #makes into data.table
data.table::setDT(LionfishMeta) #makes into data.table
data.table::setkey(LionfishMeta, Site_ID) #setting a join key
data.table::setkey(VerticalR, Site_ID) #setting a join key
LionfishMeta = LionfishMeta[AverageVR] #left-joining based on key
  

##subset the meta data to only have the removal survey #s
temp =  LionfishMeta %>% 
  filter(SurveyNum %in% CaptureEffort$SurveyNum)


###Combine the two dataframes
data.table::setDT(CaptureEffort) #makes into data.table
data.table::setDT(temp) #makes into data.table
data.table::setkey(CaptureEffort, SurveyNum) #setting a join key
data.table::setkey(temp, SurveyNum) #setting a join key
attemp1 = CaptureEffort[temp] #left-joining based on key

CaptureLion =  attemp1
names(CaptureLion)


#######################################################
####### calculate average time to collect lionfish per site:

# make new column so that I can sum the number of LF together
CaptureLion$LF<- rep(1,nrow(CaptureLion)) 

#calculate number of lionfish and density for each Survey:
NumLFSurv <- ddply(CaptureLion, c("SurveyNum", "Site_ID", "trans_area"),
                   summarize, NumPerSurv = sum(LF), AvgTimeSurv =mean(Adj_min)) 
                  
data.table::setDT(CaptureLion) #makes into data.table
data.table::setDT(NumLFSurv) #makes into data.table
data.table::setkey(CaptureLion, SurveyNum) #setting a join key
data.table::setkey(NumLFSurv, SurveyNum) #setting a join key
CaptureLion = CaptureLion[NumLFSurv] #left-joining based on key


#calculate density of lionfish per Survey:
density_trans <- ddply(CaptureLion, c("SurveyNum", "NumPerSurv", "trans_area"),
                   summarize, TransDens = mean(NumPerSurv/trans_area))


data.table::setDT(density_trans) #makes into data.table
data.table::setkey(CaptureLion, SurveyNum) #setting a join key
data.table::setkey(density_trans, SurveyNum) #setting a join key
CaptureLion = CaptureLion[density_trans] #left-joining based on key


#average the number of lionfish per site and the average time to get each one
AvgSiteLF<- ddply(CaptureLion, c("Site_ID", "Site_area_m2"),
                  summarize, AvgLFSite = mean(NumPerSurv), AvgTimeSite = mean(AvgTimeSurv))

data.table::setDT(AvgSiteLF) #makes into data.table
data.table::setkey(CaptureLion, Site_ID) #setting a join key
data.table::setkey(AvgSiteLF, Site_ID) #setting a join key
CaptureLion = CaptureLion[AvgSiteLF] #left-joining based on key

density_site <- ddply(AvgSiteLF, c("Site_ID", "Site_area_m2"),
                        summarize, SiteDens1000 = I(1000*(AvgLFSite/Site_area_m2)),
                      SiteDens = (AvgLFSite/Site_area_m2))

data.table::setDT(density_site) #makes into data.table
data.table::setkey(CaptureLion, Site_ID) #setting a join key
data.table::setkey(density_site, Site_ID) #setting a join key
CaptureLion = CaptureLion[density_site] #left-joinging based on key

#Bring in the habitat composition data frame
HabComp = read_csv(here::here('./data/raw/HabComp_All_Regions.csv'))
names(HabComp) = gsub(" ", "_", names(HabComp))
names(HabComp)

######################################################
####### calculate average % cover by site:

#replace NAs for zero :
HabComp[is.na(HabComp)] <- 0

#calculate the mean % cover per site for variables of interest
CompAvg <- ddply(HabComp, c("Site_ID"),
                 summarize, AvgCoral = mean(C)+ mean(DCA) + mean(DC),
                 AvgGorg = mean(G))


### add to the capture lion data frame so that all your variables are in one spot
CompAvg = CompAvg %>% 
  arrange(Site_ID)
CaptureLion = CaptureLion %>% 
  arrange(Site_ID)

unique(CaptureLion$Site_ID)
unique(CompAvg$Site_ID)
# unique(CaptureLion$Site_ID)==unique(CompAvg$Site_ID)


##subset the meta data to only have the matching sites
CaptureLion = CaptureLion %>% 
  filter (Site_ID %in% CompAvg$Site_ID)
unique(CaptureLion$Site_ID) #should be like 34 reefs

temp1 =  CompAvg %>% 
  filter(Site_ID %in% CaptureLion$Site_ID)
CompAvg = temp1

data.table::setDT(CaptureLion) #makes into data.table
data.table::setDT(CompAvg) #makes into data.table
data.table::setkey(CaptureLion, Site_ID) #setting a join key
data.table::setkey(CompAvg, Site_ID) #setting a join key
CaptureLion = CaptureLion[CompAvg] #left-joining based on key

new2<- subset(CaptureLion, Lionfish_size_TL >0)

lionfish_removals = new2
write_csv(lionfish_removals, here::here('./data/clean/lionfish_removals.csv'))



########################
###Create data subsets that will be used later for plotting


#remove instances where they did not try and catch the fish, 
#recorded as '0' for time
### will be used in time series analysis
time_per_fish = subset(lionfish_removals, Adj_min !="0")
write_csv(time_per_fish, here::here('./data/clean/time_per_fish.csv'))

###subset data by region
NOFGBNMS= subset(lionfish_removals, Region != "FGBNMS")
FloridaOnly = subset(lionfish_removals, Region == "Florida") #428

### subset data by sub-region
FGBNMS= subset(lionfish_removals, Region == "FGBNMS") #354
write_csv(FGBNMS, here::here('./data/clean/FGBNMS.csv'))
          
USVIONly = subset(lionfish_removals, Region == "USVI") #85
write_csv(USVIONly, here::here('./data/clean/USVIONly.csv'))
          
Biscayne = subset(lionfish_removals, Location == "Biscayne") #181
write_csv(Biscayne, here::here('./data/clean/Biscayne.csv'))
          
FKNMS = subset(FloridaOnly, Location != "Biscayne") #247
write_csv(FKNMS, here::here('./data/clean/FKNMS.csv'))
          
###subset by time of day
midday_filter <- subset(lionfish_removals, TOD !="Crepuscular")
write_csv(midday_filter, here::here('./data/clean/midday_filter.csv'))
          
crepuscular_filter <- subset(lionfish_removals, TOD !="midday")
write_csv(crepuscular_filter, here::here('./data/clean/crepuscular_filter.csv'))

