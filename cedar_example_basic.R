setwd("~/Dropbox/2018WorkingFiles/Admin/Courses/Courses_short/BEF-Stressors/comp_sel example")

dat<-read.csv("Cedar_e120data2008.csv")

#plot biomass
dat$biomass<-apply(dat[,5:ncol(dat)],1,sum)

#1 is there a relationship between species richness and biomass production?

#2 how many plots show overyielding?

#3 how many show transgressive overyielding?