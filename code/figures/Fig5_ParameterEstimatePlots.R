#### creating a estimates plots for all the model outputs
library(ggplot2)
library(cowplot)

###Fig 5a- Time for removal
newtime.true = read_csv(here::here('./outputs/newtime.true.rds'))

#create estimates
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


test_plot_tmall[1,6]= ("pos")
test_plot_tmall[2,6]= ("pos")
test_plot_tmall[3,6]= ("pos")
test_plot_tmall[4,6]= ("neg")
test_plot_tmall[5,6]= ("pos")
test_plot_tmall[6,6]= ("neg")
test_plot_tmall[7,6]= ("pos")
test_plot_tmall[8,6]= ("pos")


test_plot_tmall$var_namesTMALL <- factor(test_plot_tmall$var_namesTMALL, 
                                         levels = c("TODmidday",
                                                    "Cap_exp_newLow",
                                                    "Cap_exp_newMedium",
                                                    "Cap_exp_newNone",
                                                    "I.Site_area_m2.100.",
                                                    "Depth_ft",
                                                    "AvgGorg",
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
                            "Depth (m)","% Gorgonian Cover",
                            "Lionfish Density"
  ))+
  xlab("") + 
  ylab("") +
  labs(title = "Time for Removal")+
  theme(plot.title = element_text(size = 16, face = "bold")) +
  theme(axis.text= element_text(size=14))+
  theme(axis.title= element_text(size= 16))

TimeALL


###Fig 5b- likelihood of removal
modall.like = read_csv(here::here('./outputs/modall.like.rds'))

#create estimates
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

###Fig 5c- Proportion removed
modal.prop = read_csv(here::here('./outputs/modal.prop.rds'))

#create estimates
est_propALL = modall.prop$coefficients
est_propALL = est_propALL[2,]
est_propALL = as.numeric(est_propALL)

confWPMALL = confint(modall.prop)
var_namesWPMALL = colnames(data.frame(modall.prop$coefficients))

#create new data frame
plot_WPMALL = data.frame(cbind(est_propALL, confWPMALL, var_namesWPMALL)) 
plot_WPMALL = plot_WPMALL %>% 
  dplyr::rename(est_propALL = est_propALL, low = `X2.5..`, high = `X97.5..`)

plot_WPMALL$low = as.numeric(as.character(plot_WPMALL$low))
plot_WPMALL$est_propALL = as.numeric(as.character(plot_WPMALL$est_propALL))
plot_WPMALL$high = as.numeric(as.character(plot_WPMALL$high))

plot_WPMALL = plot_WPMALL[2:7,]
test_plot_wpmall = plot_WPMALL
test_plot_wpmall$var_namesWPMALL= as.character(test_plot_wpmall$var_namesWPMALL)

#add in columns for significance and pos/neg
test_plot_wpmall[1,5]= ("sig")
test_plot_wpmall[2,5]= ("sig")
test_plot_wpmall[3,5]= ("sig")
test_plot_wpmall[4,5]= ("not")
test_plot_wpmall[5,5]= ("not")
test_plot_wpmall[6,5]= ("not")


test_plot_wpmall[1,6]= ("pos")
test_plot_wpmall[2,6]= ("neg")
test_plot_wpmall[3,6]= ("pos")
test_plot_wpmall[4,6]= ("pos")
test_plot_wpmall[5,6]= ("pos")
test_plot_wpmall[6,6]= ("pos")

#reorder variiables
test_plot_wpmall$var_namesWPMALL <- factor(test_plot_wpmall$var_namesWPMALL, 
                                           levels = c("cond.TODmidday.",
                                                      "cond.I.Site_area_m2.100..",
                                                      "cond.Depth_ft.",
                                                      "cond.AvgGorg.",
                                                      "cond.Lionfish_size_TL.",
                                                      "cond.I.TransDens...1000.."
                                           ))


#plot and give variales better names
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



##### Create the final combined plot with all 3 models, uses cowplot to stich
finalglobals = plot_grid(TimeALL,likelihoodALL,WeightpropALL,
                         nrow = 3, ncol = 1)
finalglobals

ggsave(here::here('./figures/Fig5_ParameterEstimates.png'), plot = finalglobals,
       width = 10, height = 15,
       dpi = 300)
