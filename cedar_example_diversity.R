library(vegan)
library(picante)
library(FD)
library(hillR)

library(car) #use scatterplotmatrix to show correlations


setwd("~/Dropbox/2018WorkingFiles/Admin/Courses/Courses_short/BEF-Stressors/comp_sel example")

dat<-read.csv("Cedar_e120data2008.csv")

dat.m<-dat[,5:ncol(dat)]
rownames(dat)<-dat$Plot

tree<-rcoal(ncol(dat.m),tip.label=colnames(dat.m))
plot(tree)

traits<-as.matrix(data.frame(T1=rnorm(ncol(dat.m),125,35),
                   T2=rnorm(ncol(dat.m),534,122),T3=rnorm(ncol(dat.m),21,4)))

rownames(traits)<-colnames(dat.m)

#calculate 4 different alpha diversity measures and test the correlations among them

#example functions: vegan:diversity; hillR:hill_taxa; FD:dbFD; Picante: ses.mpd

