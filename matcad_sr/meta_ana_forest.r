#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
META-ANALYSIS
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
##Load packages
require(metafor)
require(tidyverse)


#check the working directory has been changed 
getwd()

##Set your working directory
setwd()
list.files()

#read in dummy data set
SR <- read.csv(file.choose()) #load csv of SR titles and m&m outcomes
#found: ("/Users/cherylachary/Documents/PhD/chapters/systematic review/systematic review outcomes/workings/excel working/maternal outcomes.csv")


#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
#VIEW, ACCESS AND REFORMAT   ####
#META-ANALYSIS DATA          ####
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

##VIEW
dim(SR)
View(SR)
names(SR)
head(SR)
tail(SR)

table(SR$study_type) #spread of different types of studies
lapply(SR, class)


##SUBSET
SR.cc <- SR %>%
  filter(reporting_adverse == "retro_cc")
View(SR.cc)

#reorder by year of publication
SR.cc <- SR.cc[order(SR.cc$year),]
View(SR.cc)

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Binary outcome meta-analysis: MACE
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

#binary outcome effect estimates#
#MACE###
SR.MACE <- SR.cc %>%
  drop_na(cardiac_mace_event)

##MACE analysis - RR
SR.MACE.RR <- escalc(
  ai=SR.MACE$cardiac_mace_event,
  ci=SR.MACE$non_cardiac_mace_event,
  n1i=SR.MACE$cardiac_all_pop,
  n2i=SR.MACE$non_cardiac_pop,
  measure = "RR"
)
#ai=the number of events in the cardiac
#group.
#ci=the number of events in the control group.
#n1i=sample size in the cardiac group.
#n2i=sample size in the control group.

rma(SR.MACE.RR) #does random effect as standard

#store and review meta-analysis values
MACE.res <- rma(SR.MACE.RR)
str(MACE.res)

MACE.res$b #pooled effect
MACE.res$ci.lb #lower CI
MACE.res$ci.ub #upper CI
MACE.res$I2

#forest
forest(MACE.res, 
       slab=paste(SR.MACE$Author,SR.MACE$year, sep=" "), 
       atransf = exp,
       ilab = cbind(SR.MACE$cardiac_mace_event,
                    SR.MACE$cardiac_all_pop,
                    SR.MACE$non_cardiac_mace_event,
                    SR.MACE$non_cardiac_pop),
       xlim = c(-10, 25),
       refline =0.01,
       ilab.xpos = c(-6, -5, -3.5, -1.5))

text(c(-6, -3.5), 16, 
     c("Events Cardiac", 
       "Events Other Pregnant"),
     cex = 0.8)

text(c(-5, -1.5), 15.5, 
     c("Total Cardiac Population", 
       "Total Other Pregnant Population"),
     cex = 0.8)

dev.copy(device=png,"forestplot_logRR4.png",
         width=1500,height=500)
dev.off()

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~
# Binary outcome meta-analysis: Mortality
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~

#binary outcome effect estimates#
#MACE###
SR.MORT <- SR.cc %>%
  drop_na(cardiac_mort_event)

##MACE analysis - RR
SR.MORT.RR <- escalc(
  ai=SR.MORT$cardiac_mort_event,
  ci=SR.MORT$non_cardiac_mort_event,
  n1i=SR.MORT$cardiac_all_pop.1,
  n2i=SR.MORT$non_cardiac_pop.1,
  measure = "RR"
)
#ai=the number of events in the cardiac
#group.
#ci=the number of events in the control group.
#n1i=sample size in the cardiac group.
#n2i=sample size in the control group.

rma(SR.MORT.RR) #does random effect as standard

#store and review meta-analysis values
MORT.res <- rma(SR.MORT.RR)
str(MORT.res)

MORT.res$b #pooled effect
MORT.res$ci.lb #lower CI
MORT.res$ci.ub #upper CI
MORT.res$I2

#forest
forest(MORT.res, 
       slab=paste(SR.MORT$Author,SR.MORT$year, sep=" "), 
       atransf = exp,
       ilab = cbind(SR.MORT$cardiac_mort_event,
                    SR.MORT$cardiac_all_pop.1,
                    SR.MORT$non_cardiac_mort_event,
                    SR.MORT$non_cardiac_pop.1),
       xlim = c(-10, 25),
       refline =0.01,
       width = 2,
       ilab.xpos = c(-6, -5, -3.5, -2.5))

text(c(-6, -3.5), 16, 
     c("Deaths Cardiac", 
       "Deaths Other Pregnant"),
     cex = 0.8)

text(c(-5, -1.5), 15.5, 
     c("Total Cardiac Population", 
       "Total Other Pregnant Population"),
     cex = 0.8)

dev.copy(device=png,"forestplot_logRR5.png",
         width=1000,height=500)
dev.off()

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
# EXPLORING HETEROGENEITY  ####
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

#dummy exploration of conditions, WHO
MACE_h <- SR.MACE %>% select(
  Author,
  year,
  cardiac_mace_number,
  cardiac_all_count,
  non_cardiac_mace_number,
  non_cardiac_count
)

MACE_h$condition <- c("ppcm", "ppcm", "pah", "ms")
MACE_h$who <- c(3,3,4,3)

SR.MACE.RR <- escalc(
  ai=MACE_h$cardiac_mace_number,
  ci=MACE_h$non_cardiac_mace_number,
  n1i=MACE_h$cardiac_all_count,
  n2i=MACE_h$non_cardiac_count,
  measure = "RR"
)

subgroup_ppcm <-
  rma(MACE_h.RR[MACE_h$condition == "ppcm",])
cond <- c("pah", "ms")
subgroup_not_ppcm <-
  rma(MACE_h.RR[MACE_h$condition %in% cond,])


#Test for a difference between subgroups.
subgroup.yi <- c(subgroup_ppcm$b, subgroup_not_ppcm$b)
subgroup.sei <- 
  c(subgroup_ppcm$se, subgroup_not_ppcm$se)

#run a meta-analysis, treating the pooled 
#effect in each sub-group as a separate 
#'study' in the meta-analysis
testdiff <- rma(yi = subgroup.yi, 
                sei = subgroup.sei)
testdiff$tau2
testdiff$I2
testdiff$QEp
#no significant difference between the two - subgroup p-value p = 0.44689

