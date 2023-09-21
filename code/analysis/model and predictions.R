########## 
##########
# This script runs the analyses used  
# in Davis et al. (2021) 
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
library(mgcv)
library(rstatix)
library(dplyr)
library(rlang)
library(car)
library(here)
library(readr)
library(devtools)

##Bring in the data set your created earlier (see load and tidy data)
lionfish_removals = read_csv(here::here('./data/clean/lionfish_removals.csv'))

################################
####### Model 1 lionfish likelihood of capture - binary response

options(na.action = "na.omit") ##required to work with glmm
CaptureModelall<-glmer (Captured_bin ~  Depth_ft  + AvgGorg
                         + I(Site_area_m2/100) 
                         + Lionfish_visibility*Lionfish_size_TL 
                         + Adj_behaviour + SiteDens1000
                         + Cap_exp_new + TOD + Num_attempts
                         + (1|Site_ID) +(1|Sub_region),
                         family=binomial(link="logit"),
                         data = lionfish_removals)
summary(CaptureModelall)
vif(CaptureModelall)
write_rds(CaptureModelall, here::here('./output/CaptureModel.rds'))

###Create a global model for binary capture model (Likelihood of removal)
###  CaptureModellall  had comparable AIC, lowest BIC
require(MASS)
options(na.action = "na.fail") ###required to work with the dredge function
dredge.CapModALL<-dredge(CaptureModelall)
summary(dredge.CapModALL)

#dredge.CCMALL = subset(dredge.CapModALL, 95% confidence set)
dredge.CCMALL = subset(dredge.CapModALL,cumsum(weight) <= 0.95)

#visualize the model selection table
if (require(graphics))
  plot(dredge.CCMALL)

# 'Best' model
summary(get.models(dredge.CCMALL, 1))[[1]]

#get modal averages
modall.like<-model.avg(dredge.CCMALL, cumsum(weight) <= 0.95, fit = TRUE)
summary(modall.like)
confint(modall.like)

write_rds(modall.like, here::here('./output/modall.like.rds'))



################################
###### Model 2 lionfish proportion removed - uses glmmTMB
#devtools::install_github("glmmTMB/glmmTMB/glmmTMB")
library(glmmTMB)
options(na.action = "na.omit")
PropRemovedModel<- glmmTMB (Adj_Prop ~  Depth_ft +AvgGorg
                        + I(Site_area_m2/100)  + Adj_behaviour
                        + Lionfish_visibility*Lionfish_size_TL + I(TransDens*1000)
                        + Cap_exp + TOD + Num_attempts
                        + (1|Site_ID) +(1|Sub_region),
                        family=beta_family(link = "logit"),
                        data = lionfish_removals)
summary(PropRemovedModel)
write_rds(PropRemovedModel, here::here('./output/PropRemovedModel.rds'))

###Create a global model for proportion removed model 
require(MASS)
options(na.action = "na.fail") ###required to work with the dredge function

###Global model for proportions (from gam.test script)
dredge.WPModALL<-dredge(PropRemovedModel)
summary(dredge.WPModALL)
#dredge.WPMALL = subset(dredge.WPModALL, delta <4)
dredge.WPMALL = subset(dredge.WPModALL,cumsum(weight) <= 0.95)

#visualize the model selection table
if (require(graphics))
  plot(dredge.WPMALL)

# 'Best' model
summary(get.models(dredge.WPMALL, 1))[[1]]

#get modal averages
modall.prop<-model.avg(dredge.WPMALL, cumsum(weight) <= 0.95, fit = TRUE)
summary(modall.prop)
confint(modall.prop)

write_rds(modall.prop, here::here('./output/modall.prop.rds'))


################################
#######Model 3 time spent in capturing lionfish
#devtools::install_github("glmmTMB/glmmTMB/glmmTMB")

options(na.action = "na.omit")
time_per_fish = subset(lionfish_removals, Adj_min !="0")

#use this one
TimeModall<-glmer(Adj_min ~ Depth_ft + AvgGorg
                  + I(Site_area_m2/100) 
                  + Num_attempts + Cap_exp_new
                  + Lionfish_size_TL*Lionfish_visibility + SiteDens1000 
                  + Adj_behaviour +TOD
                  + (1|Site_ID) +(1|Sub_region), 
                  family = Gamma(link = "log"),
                  data = time_per_fish)
summary(TimeModall, dispersion=1)
vif(TimeModall)

write_rds(TimeModall, here::here('./output/TimeModall.rds'))

###### create a global model for removal time
options(na.action = "na.fail") ## need for the dredge function
dredge.TModALL<-dredge(TimeModall)
summary(dredge.TModALL)

dredge.TMALL = subset(dredge.TModALL,cumsum(weight) <= 0.95)

#visualize the model selection table
if (require(graphics))
  plot(dredge.TMALL)

# 'Best' model
summary(get.models(dredge.TMALL, 1))[[1]]

### get model averages
newtime.true<-model.avg(dredge.TMALL, cumsum(weight) <= 0.95,fit=TRUE)
summary(newtime.true)
confint(newtime.true)

write_rds(newtime.true, here::here('./output/newtime.true.rds'))

### outputs modall.like, modall.prop, and newtime.true will be used to create 
#the parameter estimates in a separate script


