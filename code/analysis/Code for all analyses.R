########## 
##########
# This script runs the analyses used  
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

devtools::install_github("glmmTMB/glmmTMB/glmmTMB")
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

lionfish_removals = read_csv(here::here('./data/clean/lionfish_removals.csv'))

################################
#######Calculations for Model 1 lionfish capture binary response

names(lionfish_removals)
options(na.action = "na.omit") ##required to work with glmm

CaptureModelall<-glmer (Captured_bin ~  Depth_ft + Average + AvgGorg
                        + I(Site_area_m2/100) +AvgCoral
                        + Lionfish_visibility*Lionfish_size_TL + Adj_behaviour 
                        + SiteDens1000 + Cap_exp_new + TOD + Num_attempts 
                        + (1|Site_ID) +(1|Sub_region),
                        family=binomial(link="logit"),
                        data = lionfish_removals)

summary(CaptureModelall)
vif(CaptureModelall)
range(lionfish_removals$SiteDens1000)

CaptureModelall2<-glmer (Captured_bin ~  Depth_ft  + AvgGorg
                        + I(Site_area_m2/100) 
                        + Lionfish_visibility*Lionfish_size_TL 
                        + Adj_behaviour + SiteDens1000
                        + Cap_exp_new + TOD + Num_attempts
                        + (1|Site_ID) +(1|Sub_region),
                        family=binomial(link="logit"),
                        data = lionfish_removals)
summary(CaptureModelall2)
vif(CaptureModelall2)

CaptureModelall3<-glmer (Captured_bin ~  Depth_ft  + AvgCoral + AvgGorg
                         + Lionfish_visibility*Lionfish_size_TL 
                         + Adj_behaviour + SiteDens1000
                         + Cap_exp_new + TOD + Num_attempts
                         + (1|Site_ID) +(1|Sub_region),
                         family=binomial(link="logit"),
                         data = lionfish_removals)
summary(CaptureModelall3)
vif(CaptureModelall3)

AIC(CaptureModelall, CaptureModelall2, CaptureModelall3 )
BIC(CaptureModelall, CaptureModelall2, CaptureModelall3 )

###Global model for binary capture model
### using CaptureModellall2 bc it had comparble AIC, lowest BIC
require(MASS)
options(na.action = "na.fail") ###required to work with the dredge function
dredge.CapModALL<-dredge(CaptureModelall2)
summary(dredge.CapModALL)
#dredge.CCMALL = subset(dredge.CapModALL, delta <4)
dredge.CCMALL = subset(dredge.CapModALL,cumsum(weight) <= 0.95)

#visualize the model selection table
if (require(graphics))
  plot(dredge.CCMALL)

# 'Best' model
summary(get.models(dredge.CCMALL, 1))[[1]]

avgmod.deltaCMALL <- model.avg(dredge.CCMALL)
summary(avgmod.deltaCMALL)
confint(avgmod.deltaCMALL)

# or as a 95% confidence set:
avgmod.95pCMALL <- model.avg(dredge.CCMALL, cumsum(weight) <= 0.95)
summary(avgmod.95pCMALL)
confint(avgmod.95pCMALL)

modall.like<-model.avg(dredge.CCMALL, cumsum(weight) <= 0.95, fit = TRUE)
summary(modall.like)
confint(modall.like)

##################peters fooling around like a fool#####################
####ref size and lionnfish density as axis and set lionfish sizes s.m.l 
### model includes density, size, gorg, depth, reef, experience, tod
range(lionfish_removals$Lionfish_size_TL)
summary(lionfish_removals$Lionfish_size_TL)
count(lionfish_removals$Lionfish_size_TL >= "30")

n=100 #number of points in matrix row
mindensity=min(lionfish_removals$SiteDens1000)
maxdensity=max(lionfish_removals$SiteDens1000)
density=seq(mindensity,maxdensity,length.out=n)

minarea=min(lionfish_removals$Site_area_m2)
maxarea=max(lionfish_removals$Site_area_m2)
area=seq(minarea,maxarea,length.out=n)

lfsizettot= rep(mean(lionfish_removals$Lionfish_size_TL),n^2)
lfsizesmall= rep(9.5,n^2)
lfsizemed= rep(22.5,n^2)
lfsizelarge= rep(38,n^2)

depth=rep(mean(lionfish_removals$Depth_ft),n^2)
gorg=rep(mean(lionfish_removals$AvgGorg),n^2)
timeofday=rep("Crepuscular",n^2)
#timeofdayTM=rep("midday",n^2)
experience=rep("High",n^2)


mean(time_per_site[time_per_site$site==site1, ]$depth)
mean(lionfish_removals$Lionfish_size_TL<=15)
mean(c(30,46))


new.data=data.frame(Site_area_m2=rep(area,each=n),  SiteDens1000=rep(density, n),
                   Depth_ft=depth,  AvgGorg=gorg, Lionfish_size_TL=lfsizemed, 
                    Cap_exp_new=experience, TOD=timeofday)
names(lionfish_removals)


# Captured_bin ~  Depth_ft + Average + AvgGorg
# + AvgCoral 
# + Lionfish_visibility*Lionfish_size_TL + Adj_behaviour +SiteDens1000
# + Cap_exp_new + TOD + Num_attempts


newpredict=predict(modall.like, newdata = new.data, type='response', 
                   backtransform =FALSE, re.form=NA) #check this 

#newpredict2=predict(modall.like, newdata = new.data, re.form=NULL)

library(lattice)
library(fields)
library(akima)
library(grDevices)
library(MASS)


image.plot(interp(new.data$Site_area_m2,new.data$SiteDens1000,newpredict))
points(lionfish_removals$Lionfish_size_TL,lionfish_removals$SiteDens1000 )

#### creating a estimates plot
library(ggplot2)

est_capALL = modall.like$coefficients
est_capALL = est_capALL[2,]
est_capALL = as.numeric(est_capALL)

confCMALL = confint(modall.like)
var_namesCMALL = colnames(data.frame(modall.like$coefficients))

plot_CMALL = data.frame(cbind(est_capALL, confCMALL, var_namesCMALL)) 
plot_CMALL = plot_CMALL %>% 
  dplyr::rename(est_capALL = est_capALL, low = `X2.5..`, high = `X97.5..`)

plot_CMALL$low = as.numeric(as.character(plot_CMALL$low))
plot_CMALL$est_capALL = as.numeric(as.character(plot_CMALL$est_capALL))
plot_CMALL$high = as.numeric(as.character(plot_CMALL$high))

plot_CMALL = plot_CMALL[2:10,]
test_plot_cmall = plot_CMALL
test_plot_cmall$var_namesCMALL= as.character(test_plot_cmall$var_namesCMALL)

test_plot_cmall[1,5]= ("sig")
test_plot_cmall[2,5]= ("sig")
test_plot_cmall[3,5]= ("not")
test_plot_cmall[4,5]= ("not")
test_plot_cmall[5,5]= ("sig")
test_plot_cmall[6,5]= ("sig")
test_plot_cmall[7,5]= ("sig")
test_plot_cmall[8,5]= ("not")
test_plot_cmall[9,5]= ("not")

test_plot_cmall[1,6]= ("neg")
test_plot_cmall[2,6]= ("neg")
test_plot_cmall[3,6]= ("neg")
test_plot_cmall[4,6]= ("pos")
test_plot_cmall[5,6]= ("pos")
test_plot_cmall[6,6]= ("neg")
test_plot_cmall[7,6]= ("pos")
test_plot_cmall[8,6]= ("neg")
test_plot_cmall[9,6]= ("neg")


test_plot_cmall$var_namesCMALL <- factor(test_plot_cmall$var_namesCMALL, 
                                         levels = c("TODmidday",
                                                    "Cap_exp_newLow",
                                                    "Cap_exp_newMedium",
                                                    "Cap_exp_newNone",
                                                    "I.Site_area_m2.100.",
                                                    "Depth_ft",
                                                    "AvgGorg",
                                                    "Average",
                                                    "Lionfish_size_TL",
                                                    "SiteDens1000"
                                         ))

likelihoodALL<- ggplot(test_plot_cmall, fill = "white") +
  geom_vline(xintercept = 0, color = "black", size=0.5)+
  geom_point(aes(x = est_capALL, y = var_namesCMALL, color= V5,
                 shape = V5), size =3, show.legend = FALSE)+
  geom_errorbarh(aes(xmin=low, xmax = high, y = var_namesCMALL), 
                 height = 0 , colour = 'Black', size = 1)+
  scale_x_continuous(limits = c(-1.8, 1.8)) +
  scale_color_manual(values= c( "black", "black"))+
  scale_shape_manual(values=c(19, 8)) +
  theme_classic()+ 
  scale_y_discrete(labels=c("Midday",
                            "Low Exp", "Medium Exp", "No Exp", "Reef Size",
                            "Depth (m)","% Gorgonian Cover",
                            "Lionfish Size","Lionfish Density"
  ))+
  xlab("") + 
  ylab("") +
  labs(title = "Likelihood of Removal")+
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(axis.text= element_text(size=14))+
  theme(axis.title= element_text(size= 16))

likelihoodALL



######### 7) prediction plots

##### a) first get negative log likelihood values
#Most likely to be eaten: nocturnal, shoaling/solitay (no coeff added)
#MOD <- avgmod.delta4NP
MODLIKE <- summary(avgmod.95pCMALL)
MODLIKE
# NO SCALE plot fishes:
Intercept <- coef(summary(MODLIKE))[1]
CoralL <- coef(summary(MODLIKE))[2]
DepthL <- coef(summary(MODLIKE))[3]
LFSizeL <- coef(summary(MODLIKE))[4]
SIteDensL <- coef(summary(MODLIKE))[5]
GorgoniansL <- coef(summary(MODLIKE))[6]
VerticalRL<- coef(summary(MODLIKE))[7]
habcomboL <- coef(summary(MODLIKE))[8]
TODL <- coef(summary(MODLIKE))[9]




MODLIKE


### b) next get probabilty of being captured
#significant variables included lionfish density, lionfish size, depth,  and coral
most.like <- Intercept + LFSizeL*lionfish_removals$Lionfish_size_TL+
  SIteDensL*lionfish_removals$SiteDens *10000 +
  CoralL*lionfish_removals$AvgCoral+
  DepthL * lionfish_removals$Depth_ft
#   GorgoniansL * lionfish_removals$AvgGorg
# ##

### least likely to be caught
# #(pelagic, schooling, cleaning)
# #
# least.like <- Intercept +  
#   LFSizeL*lionfish_removals$Lionfish_size_TL+
#   CoralL*lionfish_removals$AvgCoral+
#   SIteDensL*lionfish_removals$SiteDens100

### c) next covert to probabilities for:
###most likely to be captured
#average value for fixed effects
most.avg.mod <- (exp(most.like)/ (1 + exp(most.like)))
most.up.mod<- (exp(most.like + 1.96)/(1 + exp(most.like + 1.96)))
most.low.mod <- (exp(most.like - 1.96)/(1 + exp(most.like - 1.96)))

#### least likely to be captured
#average value for fixed effects
least.avg.mod <- (exp(least.like)/ (1 + exp(least.like)))
## CIs including random effects; 95% values are between -1.96*stdev random effect and 1.96*stdev random effect (here = 2.01)
least.up.mod<- (exp(least.like + 1.96)/(1 + exp(least.like + 1.96)))
least.low.mod <- (exp(least.like - 1.96)/(1 + exp(least.like - 1.96)))


### d) get data for variables to plot
LionfishSizeL <- lionfish_removals$Lionfish_size_TL
LionfishDensityL <- lionfish_removals$SiteDens*10000
GorgonianDL <- lionfish_removals$AvgGorg
CoralDL <- lionfish_removals$AvgCoral
DepthL <- lionfish_removals$Depth_ft
VertReliefAvgL  <-lionfish_removals$Average
TODLL <-lionfish_removals$TOD

PredictData.like <- cbind(LionfishSizeL, LionfishDensityL, GorgonianDL, CoralDL, DepthL, 
                          VertReliefAvgL,
                          most.avg.mod, most.up.mod, most.low.mod
)
PredictData.like1 <-as.data.frame(PredictData.like)

PredictData.like1

str(PredictData.like1)


#write.table(PredictData1,file="C:\\Users\\Steph\\Desktop\\PredictData1.csv",sep=",",row.names=F)






########## 8) plots of predicted model

### a)  Load libraries
library(lattice)
library(fields)
library(akima)
library(grDevices)
library(MASS)

## subset the data into the habitat characteristics that lead to a ikely capture... low coral deep depth...

lowcoral = subset(PredictData.like1, CoralDL <15)
depthdeep = subset(PredictData.like1, DepthL >29 )
highcoral = subset(PredictData.like1, CoralDL> 14)
depthshallow = subset(PredictData.like1, DepthL <30)

### b)  interpolate data frame to matrix for plots
 
InterplikeavgmostALL <- interp(PredictData.like1$LionfishSizeL,PredictData.like1$LionfishDensityL,
                               PredictData.like1$most.avg.mod, duplicate = "strip")

Interplikeavgdeep <- interp(depthdeep$LionfishSizeL,depthdeep$LionfishDensityL,
                            depthdeep$most.avg.mod, duplicate = "strip")
Interplikeavgshallow <- interp(depthshallow$LionfishSizeL,depthshallow$LionfishDensityL,
                               depthshallow$most.avg.mod, duplicate = "strip")

Interplikeavgcoral <- interp(lowcoral$LionfishSizeL,lowcoral$LionfishDensityL,
                             lowcoral$most.avg.mod, duplicate = "strip")
Interplikeavgcoralhigh <- interp(highcoral$LionfishSizeL,highcoral$LionfishDensityL,
                                 highcoral$most.avg.mod, duplicate = "strip")

### c) contour plots for mean and CIs of detection probability
### mean probabilities
# to change plot for each prey type, go to step 7b) and change variables included in pro statement

#windows()
pal <- wes_palette("GrandBudapest2", 100, type = "continuous")
pal2 <-colorRampPalette(brewer.pal(n = 9, name= "RdYlBu"))(100)
pal3<- colorRampPalette(wes_palette("Royal2", type = "continuous"))(100)
par(mar = c(5,6,2,2), mgp = c(3.5,1,0))

image.plot(InterplikeavgmostALL, col= (pal),
           main="Likelihood of Capture",
           cex.main=2,
           xlab = "Lionfish size", xlim =c(4,46), 
           ylab = "Lionfish density/ hectare", ylim = c(0,180),
           cex.lab = 2.5, cex.axis = 2,
           axis.args = list(cex.axis = 1.4))

contour(InterplikeavgmostALL, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, labcex = 2.4, labtype = "bold")



#########
#Calculations for Model 2 lionfish proportion removed
################################
######Calculations for Model 2 lionfish proportion removed
devtools::install_github("glmmTMB/glmmTMB/glmmTMB")

names(lionfish_removals)
options(na.action = "na.omit")

WeightedPropall<- glmer (Prop_remaining ~  Depth_ft + AvgGorg
                         + I(Site_area_m2/100)  + Adj_behaviour
                         + Lionfish_visibility*Lionfish_size_TL  + SiteDens1000
                         + Cap_exp + TOD + Num_attempts
                         + (1|Site_ID) +(1|Sub_region),
                         family=binomial(link="logit"),
                        weights= NumPerSurv,
                        data = lionfish_removals)

summary(WeightedPropall)
res <-simulateResiduals(propremoved, plot = T)

plot(WeightedPropall)
par(mfrow= c(2,2))
gam.check(WeightedPropall, pch= 19, cex= .3)
par(mfrow= c(1,1))

library(MuMIn)
library(clusterGeneration)
require(MASS)
options(na.action = "na.fail")

###Global model for proportions (from gam.test script)
dredge.WPModALL<-dredge(propremoved2)
summary(dredge.WPModALL)
#dredge.WPMALL = subset(dredge.WPModALL, delta <4)
dredge.WPMALL = subset(dredge.WPModALL,cumsum(weight) <= 0.95)

#visualize the model selection table
if (require(graphics))
  plot(dredge.WPMALL)

# 'Best' model
summary(get.models(dredge.WPMALL, 1))[[1]]

# avgmod.deltaWPMALL <- model.avg(dredge.WPMALL)
# summary(avgmod.deltaWPMALL)
# confint(avgmod.deltaWPMALL)

# or as a 95% confidence set:
avgmod.95pWPMALL <- model.avg(dredge.WPMALL, cumsum(weight) <= 0.95)
summary(avgmod.95pWPMALL)
confint(avgmod.95pWPMALL)

modall.prop<-model.avg(dredge.WPMALL, cumsum(weight) <= 0.95, fit = TRUE)
summary(modall.prop)
confint(modall.prop)

#### creating a estimates plot
library(ggplot2)

est_propALL = modall.prop$coefficients
est_propALL = est_propALL[2,]
est_propALL = as.numeric(est_propALL)

confWPMALL = confint(modall.prop)
var_namesWPMALL = colnames(data.frame(modall.prop$coefficients))

plot_WPMALL = data.frame(cbind(est_propALL, confWPMALL, var_namesWPMALL)) 
plot_WPMALL = plot_WPMALL %>% 
  dplyr::rename(est_propALL = est_propALL, low = `X2.5..`, high = `X97.5..`)

plot_WPMALL$low = as.numeric(as.character(plot_WPMALL$low))
plot_WPMALL$est_propALL = as.numeric(as.character(plot_WPMALL$est_propALL))
plot_WPMALL$high = as.numeric(as.character(plot_WPMALL$high))

plot_WPMALL = plot_WPMALL[2:7,]
test_plot_wpmall = plot_WPMALL
test_plot_wpmall$var_namesWPMALL= as.character(test_plot_wpmall$var_namesWPMALL)




test_plot_wpmall[7,1]= 2
test_plot_wpmall[7,2]= 0
test_plot_wpmall[7,3]= 0
test_plot_wpmall[7,4]= ("Lionfish_visibilitySH")

test_plot_wpmall[8,1]= 2
test_plot_wpmall[8,2]= 0
test_plot_wpmall[8,3]= 0
test_plot_wpmall[8,4]= ("Remover Experience")

test_plot_wpmall[9,1]= 2
test_plot_wpmall[9,2]= 0
test_plot_wpmall[9,3]= 0
test_plot_wpmall[9,4]= ("Number of Attempts")

test_plot_wpmall[10,1]= 2
test_plot_wpmall[10,2]= 0
test_plot_wpmall[10,3]= 0
test_plot_wpmall[10,4]= ("Adj_behaviourrest")

test_plot_wpmall[11,1]= 2
test_plot_wpmall[11,2]= 0
test_plot_wpmall[11,3]= 0
test_plot_wpmall[11,4]= ("Lionfish_size_TL.Lionfish_visibilitySH")

test_plot_wpmall[1,5]= ("sig")
test_plot_wpmall[2,5]= ("sig")
test_plot_wpmall[3,5]= ("sig")
test_plot_wpmall[4,5]= ("not")
test_plot_wpmall[5,5]= ("not")
test_plot_wpmall[6,5]= ("not")
#test_plot_wpmall[7,5]= ("not")

test_plot_wpmall[1,6]= ("pos")
test_plot_wpmall[2,6]= ("neg")
test_plot_wpmall[3,6]= ("pos")
test_plot_wpmall[4,6]= ("pos")
test_plot_wpmall[5,6]= ("pos")
test_plot_wpmall[6,6]= ("pos")
#test_plot_wpmall[7,6]= ("neg")
#test_plot_wpmall[8,6]= ("neg")

test_plot_wpmall =test_plot_wpmall %>% 
  mutate(real= ifelse(est_propALL==2, "dummy", "real"))
test_plot_wpmall$var_namesWPMALL <- factor(test_plot_wpmall$var_namesWPMALL, 
                                           levels = c("cond.TODmidday.",
                                                      #"Remover Experience",
                                                      #"Number of Attempts",
                                                      #"Average.AvgGorg",
                                                      "cond.I.Site_area_m2.100..",
                                                      "cond.Depth_ft.",
                                                      "cond.AvgGorg.",
                                                      #"Average",
                                                      #"Adj_behaviourrest",
                                                      #"Lionfish_size_TL.Lionfish_visibilitySH",
                                                      #"Lionfish_visibilitySH",
                                                      "cond.Lionfish_size_TL.",
                                                      "cond.I.TransDens...1000.."
                                           ))

WeightpropALL<- ggplot(test_plot_wpmall, fill = "white") +
  geom_vline(xintercept = 0, color = "black", size=0.5)+
  geom_point(aes(x = est_propALL, y = var_namesWPMALL, color= V5,
                 shape = V5), size =3, show.legend = FALSE)+
  geom_errorbarh(aes(xmin=low, xmax = high, y = var_namesWPMALL), 
                 height = 0 , colour = 'Black', size = 1)+
  scale_x_continuous(limits = c(-.5, .5)) +
  scale_color_manual(values= c( "black", "black"))+
  scale_shape_manual(values=c(19, 8)) +
  theme_classic()+ 
  scale_y_discrete(labels=c("Midday",
                            "Reef Size",
                            "Depth (m)",
                            "% Gorgonian Cover",
                            "Lionfish Size",
                            "Lionfish Density"
  ))+
  xlab("Parameter Estimates") + 
  ylab("") +
  labs(title = "Proportion Removed")+
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(axis.text= element_text(size=14))+
  theme(axis.title= element_text(size= 16))

WeightpropALL



######### 7) prediction plots

##### a) first get negative log likelihood values
#Most likely to be eaten: nocturnal, shoaling/solitay (no coeff added)
#MOD <- avgmod.delta4NP
MODPROP <- summary(avgmod.95pWPMALL)
MODPROP
# NO SCALE plot fishes:
Intercept <- coef(summary(MODPROP))[1]
VertRelfP <- coef(summary(MODPROP))[2]
CoralP <- coef(summary(MODPROP))[3]
GorgoniansP <- coef(summary(MODPROP))[4]
DepthP <- coef(summary(MODPROP))[5]
LionfishDensityP <- coef(summary(MODPROP))[6]
VRandGOrg <- coef(summary(MODPROP))[7]
LFSizeP<- coef(summary(MODPROP))[8]
TODPP<- coef(summary(MODPROP))[9]



MODPROP


3.### b) next get probabilty of being captured
#positive variables included numperserv, lionfish size, depth, 
high.prop <- Intercept + CoralP*lionfish_removals$AvgCoral+
  LionfishDensityP*lionfish_removals$SiteDens*10000+
  DepthP * lionfish_removals$Depth_ft

##

### least likely to be caught
#(pelagic, schooling, cleaning)
#
low.prop <- Intercept +  
  GorgoniansP*lionfish_removals$AvgGorg+
  CoralP*lionfish_removals$AvgCoral+
  SIteDensP*lionfish_removals$SiteDens100

### c) next covert to probabilities for:
###most likely to be captured
#average value for fixed effects
high.avg.mod <- (exp(high.prop)/ (1 + exp(high.prop)))
high.up.mod<- (exp(high.prop + 1.96)/(1 + exp(high.prop + 1.96)))
high.low.mod <- (exp(high.prop - 1.96)/(1 + exp(high.prop - 1.96)))

#### least likely to be vaptured
#average value for fixed effects
low.avg.mod <- (exp(low.prop)/ (1 + exp(low.prop)))
## CIs including random effects; 95% values are between -1.96*stdev random effect and 1.96*stdev random effect (here = 2.01)
low.up.mod<- (exp(low.prop + 1.96)/(1 + exp(low.prop + 1.96)))
low.low.mod <- (exp(low.prop - 1.96)/(1 + exp(low.prop - 1.96)))


### d) get data for variables to plot
VRandGOrg <- lionfish_removals$Average
LionfishDensityP <- lionfish_removals$SiteDens*10000
GorgonianDP <- lionfish_removals$AvgGorg
CoralDP <- lionfish_removals$AvgCoral
DepthP <- lionfish_removals$Depth_ft
VertReliefAvgP  <-lionfish_removals$Average
TODPP <-lionfish_removals$TOD

PredictData.prop <- cbind( LionfishDensityP, CoralDP, DepthP, 
                           high.avg.mod, high.up.mod, high.low.mod
)
PredictData.prop1 <-as.data.frame(PredictData.prop)

PredictData.prop1

str(PredictData.prop1)

as.numeric(PredictData.prop1$DepthP)

#write.table(PredictData1,file="C:\\Users\\Steph\\Desktop\\PredictData1.csv",sep=",",row.names=F)


highcoralprop = subset(PredictData.prop1, CoralDP> 14)
lowcoralprop = subset(PredictData.prop1, CoralDP <15)

depthdeepprop = subset(PredictData.prop1, DepthP > 29 )
depthshallowprop = subset(PredictData.prop1, DepthP < 30)




########## 8) plots of predicted model

### a)  Load libraries
library(lattice)
library(fields)
library(akima)
library(grDevices)
library(MASS)

### b)  interpolate data frame to matrix for plots
##### most likely (yes)
#mean 
Interppropall <- interp(PredictData.prop1$CoralDP,PredictData.prop1$LionfishDensityP,
                        PredictData.prop1$high.avg.mod, duplicate = "strip")


Interppropavghighc <- interp(highcoralprop$DepthP,highcoralprop$LionfishDensityP,
                             highcoralprop$high.avg.mod, duplicate = "strip")

Interppropavglowc<- interp(lowcoralprop$DepthP,lowcoralprop$LionfishDensityP,
                           lowcoralprop$high.avg.mod, duplicate = "strip")

Interppropavgdeep <- interp(depthdeepprop$CoralDP,depthdeepprop$LionfishDensityP,
                            depthdeepprop$high.avg.mod, duplicate = "strip")

Interppropavgshallow <- interp(depthshallowprop$CoralDP,depthshallowprop$LionfishDensityP,
                               depthshallowprop$high.avg.mod, duplicate = "strip")

### c) contour plots for mean and CIs of detection probability


image.plot(Interppropall, col= (pal),
           main="Proportion Removed",
           cex.main=2,
           xlab = "% Coral Cover", xlim =c(0,97), 
           ylab = "Lionfish density/ hectare", ylim = c(0,180),
           cex.lab = 2.5, cex.axis = 2,
           axis.args = list(cex.axis = 1.4))


contour(Interppropall, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, labcex = 2.4, labtype = "bold")


#Depth vs density by coral
image.plot(Interppropavghighc, col= (pal),
           main="High Coral",
           cex.main=2,
           xlab = "Depth (ft)", xlim =c(30,100), 
           ylab = "Lionfish density/ 1000m", ylim = c(0,8),
           cex.lab = 3, cex.axis = 2.4,
           axis.args = list(cex.axis = 1.4))


contour(Interppropavghighc, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, labcex = 2.4, labtype = "bold")

image.plot(Interppropavglowc, col= (pal),
           main="Low Coral",
           cex.main=2,
           xlab = "Depth (ft)", xlim =c(8,80), 
           ylab = "Lionfish density/ 1000m", ylim = c(0,18),
           cex.lab = 3, cex.axis = 2.4,
           axis.args = list(cex.axis = 1.4))


contour(Interppropavglowc, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, labcex = 2.4, labtype = "bold")


### coral vs density by depth
image.plot(Interppropavgshallow, col= (pal),
           main="Shallow Reefs",
           cex.main=2,
           xlab = "% Coral Cover", xlim =c(0, 13), 
           ylab = "Lionfish Density/1000m", ylim = c(0,17),
           cex.lab = 2.5, cex.axis = 2,
           axis.args = list(cex.axis = 1.4))

contour(Interppropavgshallow, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, labcex = 2.4, labtype = "bold")


image.plot(Interppropavgdeep, col= (pal),
           main="Deep Reefs",
           cex.main=2,
           xlab = "% Coral Cover", xlim =c(0, 100), 
           ylab = "Lionfish Density/1000m", ylim = c(0,17),
           cex.lab = 2.5, cex.axis = 2,
           axis.args = list(cex.axis = 1.4))

contour(Interppropavgdeep, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, labcex = 2.4, labtype = "bold")



#########
#Calculations for Model 3 time spent in capturing lionfish
################################
#######Calculations for Model 3 time spent in capturing lionfish
devtools::install_github("glmmTMB/glmmTMB/glmmTMB")

names(lionfish_removals)
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

plot(TimeModall)

TimeModallcoral<-glmer(Adj_min ~ Depth_ft + AvgGorg
                          + Average 
                          + Num_attempts + Cap_exp_new
                          + Lionfish_size_TL*Lionfish_visibility + SiteDens1000 
                          + Adj_behaviour +TOD
                          + (1|Site_ID) +(1|Sub_region), 
                          family = Gamma(link = "log"),
                          data = time_per_fish)

summary(TimeModallcoral)
plot(TimeModallcoral)
AIC(TimeModall,TimeModallcoral)
library(car)
vif(TimeModall)


library(MuMIn)
library(clusterGeneration)
require(MASS)
options(na.action = "na.fail")

######Global model
dredge.TModALL<-dredge(TimeModall)
summary(dredge.TModALL)
#dredge.TMALL = subset(dredge.TModALL, delta <4)
dredge.TMALL = subset(dredge.TModALL,cumsum(weight) <= 0.95)

#visualize the model selection table
if (require(graphics))
  plot(dredge.TMALL)

# 'Best' model
summary(get.models(dredge.TMALL, 1))[[1]]

avgmod.deltaTMALL <- model.avg(dredge.TMALL)
summary(avgmod.deltaTMALL)
confint(avgmod.deltaTMALL)

# or as a 95% confidence set:
avgmod.95pTMALL <- model.avg(dredge.TMALL, cumsum(weight) <= 0.95)
summary(avgmod.95pTMALL)
confint(avgmod.95pTMALL)

newtime.true<-model.avg(dredge.TMALL, cumsum(weight) <= 0.95,fit=TRUE)
summary(newtime.true)
confint(newtime.true)

library(gap)
str(newtime.true)
testdata<-data.frame(newtime.true)
ESplot(newtime.true, SE=FALSE)

################peter is a fool###################
n=100 #number of points in matrix row
mindensityT=min(time_per_fish$SiteDens1000)
maxdensityT=max(time_per_fish$SiteDens1000)
densityT=seq(mindensityT,maxdensityT,length.out=n)

minareaT=min(time_per_fish$Site_area_m2)
maxareaT=max(time_per_fish$Site_area_m2)
areaT=seq(minareaT,maxareaT,length.out=n)

lfsizeT= rep(mean(time_per_fish$Lionfish_size_TL),n^2)
depthT=rep(mean(time_per_fish$Depth_ft),n^2)
gorgT=rep(mean(time_per_fish$AvgGorg),n^2)

timeofdayTC=rep("Crepuscular",n^2)
timeofdayTM=rep("midday",n^2)

experienceTH=rep("High",n^2)
experienceTM=rep("Medium",n^2)
experienceTL=rep("Low",n^2)
experienceTN=rep("None",n^2)

#visibility=rep("SH",n^2)

#vrT=rep(mean(time_per_fish$Average),n^2)



#sitearea=rep(mean(time_per_fish$Site_area_m2),n^2)
#expereince/high
new.dataTCexp=data.frame(Site_area_m2=rep(areaT,each=n),  SiteDens1000=rep(densityT, n),
                    Depth_ft=depthT,  AvgGorg=gorgT, Lionfish_size_TL = lfsizeT,  
                    Cap_exp_new=experienceTH, TOD=timeofdayTC)

new.dataTMexp=data.frame(Site_area_m2=rep(areaT,each=n),  SiteDens1000=rep(densityT, n),
                      Depth_ft=depthT,  AvgGorg=gorgT, Lionfish_size_TL = lfsizeT,  
                      Cap_exp_new=experienceTH, TOD=timeofdayTM)


#Medium
new.dataTCmed=data.frame(Site_area_m2=rep(areaT,each=n),  SiteDens1000=rep(densityT, n),
                         Depth_ft=depthT,  AvgGorg=gorgT, Lionfish_size_TL = lfsizeT,  
                         Cap_exp_new=experienceTM, TOD=timeofdayTC)

new.dataTMemed=data.frame(Site_area_m2=rep(areaT,each=n),  SiteDens1000=rep(densityT, n),
                         Depth_ft=depthT,  AvgGorg=gorgT, Lionfish_size_TL = lfsizeT,  
                         Cap_exp_new=experienceTM, TOD=timeofdayTM)


#Low
new.dataTClow=data.frame(Site_area_m2=rep(areaT,each=n),  SiteDens1000=rep(densityT, n),
                         Depth_ft=depthT,  AvgGorg=gorgT, Lionfish_size_TL = lfsizeT,  
                         Cap_exp_new=experienceTL, TOD=timeofdayTC)

new.dataTMlow=data.frame(Site_area_m2=rep(areaT,each=n),  SiteDens1000=rep(densityT, n),
                         Depth_ft=depthT,  AvgGorg=gorgT, Lionfish_size_TL = lfsizeT,  
                         Cap_exp_new=experienceTL, TOD=timeofdayTM)

#Novice
new.dataTCnov=data.frame(Site_area_m2=rep(areaT,each=n),  SiteDens1000=rep(densityT, n),
                         Depth_ft=depthT,  AvgGorg=gorgT, Lionfish_size_TL = lfsizeT,  
                         Cap_exp_new=experienceTN, TOD=timeofdayTC)

new.dataTMnov=data.frame(Site_area_m2=rep(areaT,each=n),  SiteDens1000=rep(densityT, n),
                     Depth_ft=depthT,  AvgGorg=gorgT, Lionfish_size_TL = lfsizeT, 
                     Cap_exp_new=experienceTN, TOD=timeofdayTM)

# Captured_bin ~  Depth_ft + Average + AvgGorg
# + AvgCoral 
# + Lionfish_visibility*Lionfish_size_TL + Adj_behaviour +SiteDens1000
# + Cap_exp_new + TOD + Num_attempts

##cexperience high, this is where you bring in your model 
newpredictTIMEcH=predict(newtime.true, newdata = new.dataTCexp, type='response', 
                       backtransform =FALSE, re.form=NA) #check this 

TimeInterpCH <-interp(new.dataTCexp$Site_area_m2,new.dataTCexp$SiteDens1000,newpredictTIMEcH)


##midday
newpredictTIMEm=predict(newtime.true, newdata = new.dataTMexp, type='response', 
                        backtransform =FALSE, re.form=NA) #check this 

TimeInterpM <-interp(new.dataTMexp$Site_area_m2,new.dataTMexp$SiteDens1000,newpredictTIMEm)


image.plot(TimeInterpC)
#newpredict2=predict(modall.like, newdata = new.data, re.form=NULL)

library(lattice)
library(fields)
library(akima)
library(grDevices)
library(MASS)
library(viridis)
library(PNWColors)
names(pnw_palettes)
pal=pnw_palette("Sunset2",100)

###Crepuscular###
set.panel() # reset plotting device
image.plot(TimeInterpCH, col= pal,
           main = "Crepuscular Hours",
           cex.main =2,
           xlab = "Site Area (m2)", xlim =c(0,8000), 
           ylab = "Density (#/1000)", ylim = c(0,11.1),
           cex.lab = 2, cex.axis = 1.4,
           axis.args = list(cex.axis = 1.4),
           legend.width = 1.5,
           legend.mar = 3,
           legend.args = list( text = "Time (min)",
                               cex = 1.4,
                               side = 2,
                               line = .3),
           zlim=c(0.4,3.3))

contour(TimeInterpCH, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, labcex = 2.4, labtype = "bold", col= "white")
points(2080,1.7,pch="*", cex=3)
points(4250,2.3,pch="+", cex=3)
       

###midday###
set.panel() # reset plotting device
image.plot(TimeInterpM, col= pal,
          main = "Midday Hours",
          cex.main =2,
          xlab = "Site Area", xlim =c(0,8000), 
          ylab = "Density (#/1000)", ylim = c(0,11.1),
          cex.lab = 2, cex.axis = 1.4,
          axis.args = list(cex.axis = 1.4),
          legend.width = 1.5,
          legend.mar = 3,
          legend.args = list( text = "Time (min)",
                              cex = 1.4,
                              side = 2,
                              line = .3),
          zlim=c(0.4,3.3)) ##make sure you set this upper limit to the highest of time

contour(TimeInterpM, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, labcex = 2.4, labtype = "bold", col= "white")
points(2080,1.7,pch="*", cex=3)
points(4250,2.3,pch="+", cex=3)
#points(time_per_fish$Site_area_m2,time_per_fish$SiteDens1000 )
summary(time_per_fish$SiteDens1000)

range(time_per_fish$SiteDens1000)
range(time_per_fish$Site_area_m2)
#### creating a estimates plot
library(ggplot2)

est_timeALL = newtime.true$coefficients
est_timeALL = est_timeALL[2,]
est_timeALL = as.numeric(est_timeALL)

confTMALL = confint(newtime.true)
var_namesTMALL = colnames(data.frame(newtime.true$coefficients))

plot_TMALL = data.frame(cbind(est_timeALL, confTMALL, var_namesTMALL)) 
plot_TMALL = plot_TMALL %>% 
  dplyr::rename(est_timeALL = est_timeALL, low = `X2.5..`, high = `X97.5..`)

plot_TMALL$low = as.numeric(as.character(plot_TMALL$low))
plot_TMALL$est_timeALL = as.numeric(as.character(plot_TMALL$est_timeALL))
plot_TMALL$high = as.numeric(as.character(plot_TMALL$high))

plot_TMALL = plot_TMALL[2:9,]
test_plot_tmall = plot_TMALL
test_plot_tmall$var_namesTMALL= as.character(test_plot_tmall$var_namesTMALL)


test_plot_tmall[1,5]= ("sig")
test_plot_tmall[2,5]= ("sig")
test_plot_tmall[3,5]= ("sig")
test_plot_tmall[4,5]= ("sig")
test_plot_tmall[5,5]= ("sig")
test_plot_tmall[6,5]= ("not")
test_plot_tmall[7,5]= ("not")
test_plot_tmall[8,5]= ("not")
# test_plot_tmall[9,5]= ("not")
# test_plot_tmall[10,5]= ("not")

test_plot_tmall[1,6]= ("pos")
test_plot_tmall[2,6]= ("pos")
test_plot_tmall[3,6]= ("pos")
test_plot_tmall[4,6]= ("neg")
test_plot_tmall[5,6]= ("pos")
test_plot_tmall[6,6]= ("neg")
test_plot_tmall[7,6]= ("pos")
test_plot_tmall[8,6]= ("pos")
# test_plot_tmall[9,6]= ("neg")
# test_plot_tmall[10,6]= ("neg")

test_plot_tmall[11,1]= 2
test_plot_tmall[11,2]= 0
test_plot_tmall[11,3]= 0
test_plot_tmall[11,4]= ("Lionfish_size_TL.Lionfish_visibilitySH")
test_plot_tmall[11,5]= ("not")

test_plot_tmall[12,1]= 2
test_plot_tmall[12,2]= 0
test_plot_tmall[12,3]= 0
test_plot_tmall[12,4]= ("Lionfish_visibilitySH")
test_plot_tmall[12,5]= ("not")

test_plot_tmall[13,1]= 2
test_plot_tmall[13,2]= 0
test_plot_tmall[13,3]= 0
test_plot_tmall[13,4]= ("Number of Attempts")
test_plot_tmall[13,5]= ("not")

test_plot_tmall[9,1]= 2
test_plot_tmall[9,2]= 0
test_plot_tmall[9,3]= 0
test_plot_tmall[9,4]= ("Lionfish_behaviourrest")
test_plot_tmall[9,5]= ("not")

test_plot_tmall[10,1]= 2
test_plot_tmall[10,2]= 0
test_plot_tmall[10,3]= 0
test_plot_tmall[10,4]= ("Lionfish_size_TL")
test_plot_tmall[10,5]= ("not")


test_plot_tmall =test_plot_tmall %>% 
  mutate(real= ifelse(est_timeALL==2, "dummy", "real"))
test_plot_tmall$var_namesTMALL <- factor(test_plot_tmall$var_namesTMALL, 
                                         levels = c("TODmidday",
                                                    "Cap_exp_newLow",
                                                    "Cap_exp_newMedium",
                                                    "Cap_exp_newNone",
                                                    "I.Site_area_m2.100.",
                                                    #"Number of Attempts",
                                                    #"Average.AvgGorg",
                                                    "Depth_ft",
                                                    "AvgGorg",
                                                    #"Average",
                                                    #"Lionfish_behaviourrest",
                                                    #"Lionfish_size_TL.Lionfish_visibilitySH",
                                                    #"Lionfish_visibilitySH",
                                                    #"Lionfish_size_TL",
                                                    "SiteDens1000"
                                         ))

TimeALL<- ggplot(test_plot_tmall, fill = "white") +
  geom_vline(xintercept = 0, color = "black", size=0.5)+
  geom_point(aes(x = est_timeALL, y = var_namesTMALL, color = V5, 
                 shape = V5), size =3, show.legend = FALSE)+
  geom_errorbarh(aes(xmin=low, xmax = high, y = var_namesTMALL), 
                 height = 0 , colour = 'Black', size = 1)+
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  scale_color_manual(values= c( "black", "black"))+
  scale_shape_manual(values=c(19, 8)) +
  theme_classic()+ 
  scale_y_discrete(labels=c("Midday",
                            "Low Exp", "Medium Exp", "No Exp", "Reef Size",
                            #"Number of Attempts", #"VR and Gorgonian",
                            "Depth (m)","% Gorgonian Cover",#"Vertical Relief (cm)",
                            #"Resting",
                            #"Small and Sheltered", "Sheltered",
                            #"Lionfish Size",
                            "Lionfish Density"
  ))+
  xlab("") + 
  ylab("") +
  labs(title = "Time for Removal")+
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(axis.text= element_text(size=14))+
  theme(axis.title= element_text(size= 16))

TimeALL

library(cowplot)

finalglobals = plot_grid(TimeALL,likelihoodALL,WeightpropALL,
                         nrow = 3, ncol = 1)
finalglobals


######### 7) prediction plots

##### a) first get negative log likelihood values
#Most likely to be eaten: nocturnal, shoaling/solitay (no coeff added)
#MOD <- avgmod.delta4NP
MODTIME <- summary(avgmod.95pTMALL)
MODTIME
# NO SCALE plot fishes:
Intercept <- coef(summary(MODTIME))[1]
VertRelf <- coef(summary(MODTIME))[2]
Coral <- coef(summary(MODTIME))[3]
Gorgonians <- coef(summary(MODTIME))[4]
Depth <- coef(summary(MODTIME))[5]
LFSize <- coef(summary(MODTIME))[6]
SIteDens <- coef(summary(MODTIME))[7]
TOD2<- coef(summary(MODTIME))[8]


MODTIME


### b) next get probabilty of being captured
#positive variables included numperserv, lionfish size, depth, 
shortest.time <- Intercept + Coral*lionfish_removals$AvgCoral+
  SIteDens*lionfish_removals$SiteDens +  
  SizeTL*lionfish_removals$Lionfish_size_TL
##

### least likely to be caught
#(pelagic, schooling, cleaning)
#
longest.time <- Intercept + TOD2 + 
  SizeTL*lionfish_removals$Lionfish_size_TL+
  Gorgonian*lionfish_removals$AvgGorg +
  Coral*lionfish_removals$AvgCoral+
  VertRelf*lionfish_removals$Average

### c) next covert to probabilities for:
###most likely to be captured
#average value for fixed effects
shortest.avg.mod <- (exp(shortest.time)/ (1 + exp(shortest.time)))
shortest.up.mod<- (exp(shortest.time + 1.96)/(1 + exp(shortest.time + 1.96)))
shortest.low.mod <- (exp(shortest.time - 1.96)/(1 + exp(shortest.time - 1.96)))

#### least likely to be vaptured
#average value for fixed effects
longest.avg.mod <- (exp(longest.time)/ (1 + exp(longest.time)))
## CIs including random effects; 95% values are between -1.96*stdev random effect and 1.96*stdev random effect (here = 2.01)
longest.up.mod<- (exp(longest.time + 1.96)/(1 + exp(longest.time + 1.96)))
longest.low.mod <- (exp(longest.time - 1.96)/(1 + exp(longest.time - 1.96)))


### d) get data for variables to plot
crepuscular_filter4 = lionfish_removals %>%
  filter(TOD == 'Crepuscular')

summary(crepuscular_filter4$TOD)

LionfishSizeT <- crepuscular_filter$Lionfish_size_TL
LionfishDensityT <- crepuscular_filter$SiteDens*1000
GorgonianDT <- crepuscular_filter$AvgGorg
CoralDT <- crepuscular_filter$AvgCoral
DepthT <- crepuscular_filter$Depth_ft
VertReliefAvgT  <-crepuscular_filter$Average
TODT <-crepuscular_filter$TOD

PredictData.Time <- cbind(LionfishSizeT, LionfishDensityT, GorgonianDT, CoralDT, DepthT, 
                          VertReliefAvgT,
                          shortest.avg.mod, shortest.up.mod, shortest.low.mod
)
PredictData.Crep <-as.data.frame(PredictData.Time)

PredictData.Crep

str(PredictData.Crep)

summary(PredictData.Crep)




midday_filter4 = lionfish_removals %>%
  filter(TOD == 'midday')
summary(midday_filter$Captured_bin)

LionfishSizeM <- midday_filter$Lionfish_size_TL
LionfishDensityM <- midday_filter$SiteDens*1000
GorgonianDM <- midday_filter$AvgGorg
CoralDM <- midday_filter$AvgCoral
DepthM <- midday_filter$Depth_ft
VertReliefAvgM  <-midday_filter$Average
TODM <-midday_filter$TOD

PredictData.Time <- cbind(LionfishSizeM, LionfishDensityM, GorgonianDM, CoralDM, DepthM, 
                          VertReliefAvgM,
                          shortest.avg.mod, shortest.up.mod, shortest.low.mod
)
PredictData.TimeM <-as.data.frame(PredictData.Time)

PredictData.TimeM

str(PredictData.TimeM)

summary(PredictData.TimeM)

#write.table(PredictData1,file="C:\\Users\\Steph\\Desktop\\PredictData1.csv",sep=",",row.names=F)








########## 8) plots of predicted model

### a)  Load libraries
library(lattice)
library(fields)
library(akima)
library(grDevices)
library(MASS)

### b)  interpolate data frame to matrix for plots
##### most likely (yes)
#mean 
InterpTimeavgCrep <- interp(PredictData.Crep$LionfishSizeT,PredictData.Crep$LionfishDensityT,
                        PredictData.Crep$shortest.avg.mod, duplicate = "strip")


### c) contour plots for mean and CIs of detection probability
### mean probabilities
# to change plot for each prey type, go to step 7b) and change variables included in pro statement

#windows()
pal <- wes_palette("GrandBudapest2", 100, type = "continuous")
pal2 <-colorRampPalette(brewer.pal(n = 9, name= "RdYlBu"))(100)
pal3<- colorRampPalette(wes_palette("Royal2", type = "continuous"))(100)
par(mar = c(5,6,2,2), mgp = c(3.5,1,0))

image.plot(InterpTimeavgCrep, col= (pal),
           xlab = "size", xlim =c(0,45), 
           ylab = "Density (#/1000)", ylim = c(0,18),
           cex.lab = 3, cex.axis = 2.4,
           axis.args = list(cex.axis = 1.4))

contour(InterpTimeavgCrep, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, labcex = 2.4, labtype = "bold")



InterplargeavgMid <- interp( PredictData.TimeM$LionfishSizeM, PredictData.TimeM$LionfishDensityM,
                             PredictData.TimeM$shortest.avg.mod, duplicate = "strip") 

image.plot(InterplargeavgMid, col= (pal),
           main="Midday",
           cex.main=2,
           xlab = "Size", xlim =c(4, 45), 
           ylab = "Lionfish density/ 1000m", ylim = c(0,17),
           cex.lab = 2.5, cex.axis = 2,
           axis.args = list(cex.axis = 1.4))


contour(InterplargeavgMid, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, labcex = 2.4, labtype = "bold")


###midday
Interplargeavgmd <- interp( PredictData.TimeM$LionfishSize, PredictData.TimeM$LionfishDensity,
                            PredictData.TimeM$shortest.avg.mod, duplicate = "strip")
image.plot(Interplargeavgmd, col= (pal),
           main="Mid-day",
           cex.main=2,
           xlab = "Size", xlim =c(4, 45), 
           ylab = "Lionfish density/ 1000m", ylim = c(0,17),
           cex.lab = 2.5, cex.axis = 2,
           axis.args = list(cex.axis = 1.4))


contour(Interplargeavgmd, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, labcex = 2.4, labtype = "bold")


###



#least likely to get captured

InterpCapavgNot <- interp(PredictData.Capture1$LionfishSize,PredictData.Capture1$GorgonianD,
                          PredictData.Capture1$p.leastavg.mod, duplicate = "strip")

par(mar = c(5,6,2,2), mgp = c(3.5,1,0))

image.plot(InterpCapavgNot,col= (pal), 
           xlab = "Lionfish size", xlim =c(4,50), 
           ylab = "Lionfish density/ 1000m", ylim = c(0,18),
           cex.lab = 3, cex.axis = 2.4,
           axis.args = list(cex.axis = 1.4))

contour(InterpCapavgNot, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, labcex = 2.4, labtype = "bold")


###
InterpCapavgVR <- interp(PredictData.Capture1$GorgonianD,PredictData.Capture1$VertReliefAvg,
                         PredictData.Capture1$p.leastavg.mod, duplicate = "strip")
image.plot(InterpCapavgVR, xlab = "Average Gorgonian % Cover", 
           ylab = "Average Vertical Relief (cm)",
           cex.lab = 3, cex.axis = 2.4,
           axis.args = list(cex.axis = 1.4))

contour(InterpCapavgVR, vfont = c("sans serif", "bold"), nlevels = 12, add = TRUE, labcex = 2.4, labtype = "bold")

