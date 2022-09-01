####Time series analysis
##repeated measure or generalized  least squares regression
###analysis of time series
library(rstatix)
###time in  removal
time.aov.time <- anova_test(data = USVIONly, Adj_min~ Date2 )
TukeyHSD(time.aov.time)
get_anova_table(time.aov.time)
names(lionfish_removals)

tukey_hsd(USVIONly, Adj_min~ Date2)

??tukey_hsd
# pairwise comparisons
time_per_fish = read_csv(here::here('./data/clean/time_per_fish.csv'))
pwc.time <- time_per_fish %>% 
  #group_by(Sub_region) %>%
  pairwise_t_test(time_per_fish, Adj_min~ Date2 + Sub_region, paired = FALSE,
                  p.adjust.method = "bonferroni"
  )
pwc.time

# pairwise comparisons
pwc.time <- Biscayne %>% 
  #group_by(Sub_region) %>%
  pairwise_t_test(Lionfish_size_TL~ Date2, paired = FALSE,
                  p.adjust.method = "bonferroni"
  )
pwc.time

wahterver = subset(small, Cap_exp_new =="High")
length(wahterver$Cap_exp_new)
propremove =subset(lionfish_removals,Proportion_caught<"1")
porpremove2 = subset(propremove, Proportion_caught > 0)

plot(porpremove2$Proportion_caught,porpremove2$SiteDens1000)
abline(lm(porpremove2$Proportion_caught~porpremove2$SiteDens1000))

names(lionfish_removals)

bis.aov<-aov(Lionfish_size_TL~Date2, data= Biscayne)
summary(bis.aov)
TukeyHSD(bis.aov)

###proportion   removed
prop.aov.time <- anova_test(data = lionfish_removals, Lionfish_size_TL~ Date2 + Sub_region)
get_anova_table(prop.aov.time)

# pairwise comparisons
pwc.prop <- FloridaOnly %>% 
  group_by(Sub_region) %>%
  pairwise_t_test(Lionfish_size_TL~ Date2, paired = FALSE,
                  p.adjust.method = "bonferroni"
  )
pwc.prop

summary(FKNMS$Sub_region)

###density   
dens.aov.time <- anova_test(data = lionfish_removals, SiteDens~ Date2 + Sub_region)
get_anova_table(dens.aov.time)

# pairwise comparisons
pwc.dens <- lionfish_removals %>% 
  group_by(Sub_region) %>%
  pairwise_t_test(SiteDens~ Date2, paired = FALSE,
                  p.adjust.method = "bonferroni"
  )
pwc.dens

###size   
size.aov.time <- anova_test(data = lionfish_removals, Lionfish_size_TL~ Date2 + Sub_region)
get_anova_table(size.aov.time)

# pairwise comparisons
pwc.size <- lionfish_removals %>% 
  group_by(Sub_region) %>%
  pairwise_t_test(Lionfish_size_TL~ Date2, paired = FALSE,
                  p.adjust.method = "bonferroni"
  )
pwc.size






fgb.time.time <-gls(Adj_min ~ Date2, correlation=corAR1(form = ~1|Date2), data= Biscayne)
summary(fgb.time.time)
library(multcomp)
multCompTukey <- glht(fgb.time.time, linfct = mcp(Date2 = "Tukey"))

fgb.prop.time <-gls(Adj_Prop ~ Date2, correlation=corAR1(form = ~1|Date), data= FGBNMS)
summary(fgb.prop.time)
anova(fgb.prop.time)

fgb.size.time <-gls(Lionfish_size_TL ~ Date2, correlation=corAR1(form = ~1|Date), data= FGBNMS)
summary(fgb.size.time)

fgb.dens.time <-gls(SiteDens ~ Date2, correlation=corAR1(form = ~1|Date), data= lionfish_removals)
summary(fgb.dens.time)

## Run the following lines. These introduce methods for 'gls' objects.
model.matrix.gls <- function(object, ...) {
  model.matrix(terms(object), data = getData(object), ...)
}
model.frame.gls <- function(object, ...) {
  model.frame(formula(object), data = getData(object), ...)
}
terms.gls <- function(object, ...) {
  terms(model.frame(object), ...)

}
FGBNMS = read_csv(here::here('./data/clean/FGBNMS.csv'))
gamdens <-gam(SiteDens ~ Date2, correlation=corAR1(form = ~1|Date), data= FGBNMS)
summary(gamdens)

lmdateprop <-aov(SiteDens
                 ~ Date2, data= FGBNMS)
TukeyHSD(lmdateprop)
summary(lmdateprop)

names(lionfish_removals)
options(na.action = "na.omit")
