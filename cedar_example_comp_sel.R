#####Relative yield, complementary and selection from Loreau and Hector, using cedar creek

setwd("~/Dropbox/2018WorkingFiles/Admin/Courses/Courses_short/BEF-Stressors/comp_sel example")

dat<-read.csv("Cedar_e120data2008.csv")

planted<-read.csv("Cedar_e120data2008_planted.csv")

plots<-planted$Plot

##turn into matrix
planted<-as.matrix(planted[,5:ncol(planted)])
rownames(planted)<-plots

#monocultures
monos<-as.character(dat$Plot[dat$NumSp==1])

monos<-dat[match(monos,dat$Plot),]

mono_vals_all<-apply(monos[,5:ncol(monos)],2,function(x){
	mean(x[x>0])})

##for each plot, need to calculate delta_RY,and species monoculture biomass
#use list of polycultures

poly<-as.character(dat$Plot[dat$NumSp>1])

##in list combine monoculture and polyculture biomass for species in poly

poly_list<-list()

for (i in 1:length(poly)){
	
	mon_sp<-colnames(planted)[planted[poly[i],]==1]
	poly_vals<-dat[dat$Plot==poly[i],match(mon_sp,colnames(dat))]
	mon_vals<-mono_vals_all[match(mon_sp,names(mono_vals_all))]
	
	poly_list[[i]]<-rbind(mon_vals,poly_vals)
	rownames(poly_list[[i]])<-c("mono","poly")
	names(poly_list)[[i]]<-poly[i]
	
	}
	
###calculate RY and partitioning equation

deltaY<-data.frame(comp=NULL,sel=NULL,deltaY=NULL,spp=NULL, sum_prod=NULL)

for (i in 1:length(poly_list)){
	
	RYo<-poly_list[[i]][2,]/ poly_list[[i]][1,]
	RYe<-1/ncol(poly_list[[i]])
	
	RYe<-as.vector(as.matrix(RYe))
	RYo<-as.vector(as.matrix(RYo))
	
	dRY<-RYo - RYe
	
	deltaY[i,1]<-ncol(poly_list[[i]])*
		mean(dRY)*
		mean(as.matrix(poly_list[[i]][1,]))
		
	deltaY[i,2]<-ncol(poly_list[[i]])*
		cov(dRY,as.vector(as.matrix(poly_list[[i]][1,])))
		
	deltaY[i,3]<-sum(deltaY[i,1],deltaY[i,2])	
				
	deltaY[i,4]<-length(RYo)	
	
	deltaY[i,5]<-sum(poly_list[[i]][2,])	

		
	}

names(deltaY)<-c("COMP","SELECT","DeltaY","SPP_NUM","Biomass")
rownames(deltaY)<-poly
	
	
##plot selection and comp effects	

comp<-lm(deltaY$COMP~deltaY$SPP_NUM)
summary(comp)

sel<-lm(deltaY$SELECT~deltaY$SPP_NUM)
summary(sel)

Dy<-lm(deltaY$DeltaY~deltaY$SPP_NUM)
summary(Dy)

prod<-lm(deltaY$Biomass~deltaY$SPP_NUM)
summary(prod)

quartz()
par(mfrow=c(2,2),cex.lab=1.2)

plot(deltaY$COMP~deltaY$SPP_NUM,main="Complementarity")
abline(comp)

plot(deltaY$SELECT~deltaY$SPP_NUM,main="Selection")
abline(sel)

plot(deltaY$DeltaY~deltaY$SPP_NUM,main="Delta Y")
abline(Dy)

plot(deltaY$Biomass~deltaY$SPP_NUM,main="Biomass")
abline(prod)




