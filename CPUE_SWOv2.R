
# set working directory
setwd("C:/Assessments/CPUEstandardization/")
# Check if its there
getwd()

# Read-in csv 
dat = read.csv("Data/LL_CPUEkgMay2017.csv")

dat[is.na(dat)]<-0
# check column heading
names(dat)


species = "SWO"  
assessment = paste0("SWO_ICCAT")
dir.create(paste(assessment),showWarnings = FALSE)


# check first few records
head(dat)

# make Lat negative
dat$Lat = ifelse(dat$Lat<1,dat$Lat,-dat$Lat)
#SET species
dat$y = dat$SWFS



#*****************************************************************************
# Map set up
#*****************************************************************************
library(maptools)
library(mapplots)

# read shape file
library(shapefiles)
library(maps)

gis = "c:\\rdata\\GIS"

sa <- read.shapefile(paste0(gis,"/southafrica"))
bathy = read.shapefile(paste0(gis,"/bathysa"))
world = read.shapefile(paste0(gis,"/world/country"))
d = subset(dat, Year>2005 & HOOKnr>100)

d$y = d$y/d$HOOKnr*1000
breaks=seq(0,14,2)
long.lim=c(8,40)
lat.lim=c(-38,-20)


Par = list(mfrow=c(3,3),mai=c(0.2,0.15,.2,0),omi = c(0.3,0.25,0,0) + 0.1,mgp=c(3,0.7,0), tck = -0.02,cex=0.9)
png(file = paste0(assessment,"/Sets_",species,"_ALL.png"), width = 15, height = 12, 
    res = 300, units = "in")
par(Par)
Yr = rev(2007:2015)
for(i in 1:length(Yr)){
  dy = subset(d,Year==Yr[i])
  zy = subset(d,Year==Yr[i] & y==0)
  
  ty = subset(d,Year==Yr[i] & y>0)
  basemap(xlim=long.lim,ylim=lat.lim,main=paste(Yr[i]),xaxt="n",yaxt="n")
  if(i==1|i==4|i==7){axis(2,labels=T)} else {axis(2,labels=F)}
  if(i>6){axis(1,labels=T)} else {axis(1,labels=F)}
  
  #draw.shape(bathy, col="darkgrey",type="l")
  #abline(h=-35,lwd=2)
  
  draw.shape(world, col="cornsilk1")
  draw.shape(sa, col="cornsilk1")
  
  points(zy$Long,zy$Lat,cex=0.7,col=1,bg=grey(0.5,1),pch=21)
  draw.bubble(dy$Long,dy$Lat, dy$y, maxradius=1.5, pch=21, bg=rgb(0.5,0,0.5,0.3))
  #points(ty$Long,ty$Lat,cex=0.8,col=1.2,bg="blue",pch=21)
  legend("bottomleft",c("Sets(y=0)","Sets(y>0)"),pch=21,pt.bg=c(grey(0.5,1),rgb(.5,0,0.5,0.3)),pt.cex=c(1,1.5),bty="n")
  #text(7,-24.6,"15",cex=1.8)
  #text(38,-24.6,"14",cex=1.8)
  #text(20,-40,"9",cex=1.8)
  lines(c(20,20),c(-100,-35),col=1,lwd=2)
  #lines(c(25,25),c(-100,-31.8),col=1,lwd=1,lty=2)
  
}
mtext(paste("Longitude"), side=1, outer=T, at=0.5,line=1,cex=1)
mtext(paste("Latitude"), side=2, outer=T, at=0.5,line=1,cex=1)
dev.off()


# Shark directed
dS = subset(d,TARGET=="PEL SHARK")
Par = list(mfrow=c(3,3),mai=c(0.2,0.15,.2,0),omi = c(0.3,0.25,0,0) + 0.1,mgp=c(3,0.7,0), tck = -0.02,cex=0.9)
png(file = paste0(assessment,"/Sets_",species,"ShrLL.png"), width = 15, height = 12, 
    res = 300, units = "in")
par(Par)
Yr = rev(2007:2015)
for(i in 1:length(Yr)){
  dy = subset(dS,Year==Yr[i])
  zy = subset(dS,Year==Yr[i] & y==0)
  
  ty = subset(dS,Year==Yr[i] & y>0)
  basemap(xlim=long.lim,ylim=lat.lim,main=paste(Yr[i]),xaxt="n",yaxt="n")
  if(i==1|i==4|i==7){axis(2,labels=T)} else {axis(2,labels=F)}
  if(i>6){axis(1,labels=T)} else {axis(1,labels=F)}
  
  
  draw.shape(world, col="cornsilk1")
  draw.shape(sa, col="cornsilk1")
  
  points(zy$Long,zy$Lat,cex=0.7,col=1,bg=grey(0.5,1),pch=21)
  draw.bubble(dy$Long,dy$Lat, dy$y, maxradius=1.5, pch=21, bg=rgb(1,0,0,0.3))
  #points(ty$Long,ty$Lat,cex=0.8,col=1.2,bg="blue",pch=21)
  legend("bottomleft",c("Sets(y=0)","Sets(y>0)"),pch=21,pt.bg=c(grey(0.5,1),rgb(1,0,0,0.3)),pt.cex=c(1,1.5),bty="n")
  #text(7,-24.6,"15",cex=1.8)
  #text(38,-24.6,"14",cex=1.8)
  #text(20,-40,"9",cex=1.8)
  lines(c(20,20),c(-100,-35),col=1,lwd=2)
  lines(c(25,25),c(-100,-31.8),col=1,lwd=1,lty=2)
  
}
mtext(paste("Longitude"), side=1, outer=T, at=0.5,line=1,cex=1)
mtext(paste("Latitude"), side=2, outer=T, at=0.5,line=1,cex=1)
dev.off()


# TUNA directed
dT = subset(d,TARGET=="TUN")
Par = list(mfrow=c(3,3),mai=c(0.2,0.15,.2,0),omi = c(0.3,0.25,0,0) + 0.1,mgp=c(3,0.7,0), tck = -0.02,cex=0.9)
png(file = paste0(assessment,"/Sets_",species,"_TUNA.png"), width = 15, height = 12, 
    res = 300, units = "in")
par(Par)
Yr = rev(2007:2015)
for(i in 1:length(Yr)){
  dy = subset(dT,Year==Yr[i])
  zy = subset(dT,Year==Yr[i] & y==0)
  
  ty = subset(dT,Year==Yr[i] & y>0)
  basemap(xlim=long.lim,ylim=lat.lim,main=paste(Yr[i]),xaxt="n",yaxt="n")
  if(i==1|i==4|i==7){axis(2,labels=T)} else {axis(2,labels=F)}
  if(i>6){axis(1,labels=T)} else {axis(1,labels=F)}
  
  
  draw.shape(world, col="cornsilk1")
  draw.shape(sa, col="cornsilk1")
  
  points(zy$Long,zy$Lat,cex=0.7,col=1,bg=grey(0.5,1),pch=21)
  draw.bubble(dy$Long,dy$Lat, dy$y, maxradius=1.5, pch=21, bg=rgb(0,0,1,0.3))
  #points(ty$Long,ty$Lat,cex=0.8,col=1.2,bg="blue",pch=21)
  legend("bottomleft",c("Sets(y=0)","Sets(y>0)"),pch=21,pt.bg=c(grey(0.5,1),rgb(0,0,1,0.3)),pt.cex=c(1,1.5),bty="n")
  #text(7,-24.6,"15",cex=1.8)
  #text(38,-24.6,"14",cex=1.8)
  #text(20,-40,"9",cex=1.8)
  lines(c(20,20),c(-100,-35),col=1,lwd=2)
  lines(c(25,25),c(-100,-31.8),col=1,lwd=1,lty=2)
  
}
mtext(paste("Longitude"), side=1, outer=T, at=0.5,line=1,cex=1)
mtext(paste("Latitude"), side=2, outer=T, at=0.5,line=1,cex=1)
dev.off()

# SWO directed
dSW = subset(d,TARGET=="SWO")
Par = list(mfrow=c(3,3),mai=c(0.2,0.15,.2,0),omi = c(0.3,0.25,0,0) + 0.1,mgp=c(3,0.7,0), tck = -0.02,cex=0.9)
png(file = paste0(assessment,"/Sets_",species,".png"), width = 15, height = 12, 
    res = 300, units = "in")
par(Par)
Yr = rev(2007:2015)
for(i in 1:length(Yr)){
  dy = subset(dSW,Year==Yr[i])
  zy = subset(dSW,Year==Yr[i] & y==0)
  
  ty = subset(dSW,Year==Yr[i] & y>0)
  basemap(xlim=long.lim,ylim=lat.lim,main=paste(Yr[i]),xaxt="n",yaxt="n")
  if(i==1|i==4|i==7){axis(2,labels=T)} else {axis(2,labels=F)}
  if(i>6){axis(1,labels=T)} else {axis(1,labels=F)}
  
  
  draw.shape(world, col="cornsilk1")
  draw.shape(sa, col="cornsilk1")
  
  points(zy$Long,zy$Lat,cex=0.7,col=1,bg=grey(0.5,1),pch=21)
  draw.bubble(dy$Long,dy$Lat, dy$y, maxradius=1.5, pch=21, bg=rgb(0,1,0,0.3))
  #points(ty$Long,ty$Lat,cex=0.8,col=1.2,bg="blue",pch=21)
  legend("bottomleft",c("Sets(y=0)","Sets(y>0)"),pch=21,pt.bg=c(grey(0.5,1),rgb(0,1,0,0.3)),pt.cex=c(1,1.5),bty="n")
  #text(7,-24.6,"15",cex=1.8)
  #text(38,-24.6,"14",cex=1.8)
  #text(20,-40,"9",cex=1.8)
  lines(c(20,20),c(-100,-35),col=1,lwd=2)
  #lines(c(29,29),c(-100,-31.8),col=1,lwd=1,lty=2)
  
}
mtext(paste("Longitude"), side=1, outer=T, at=0.5,line=1,cex=1)
mtext(paste("Latitude"), side=2, outer=T, at=0.5,line=1,cex=1)
dev.off()

#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
# End of Spatial MAPS
#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
library(MASS)
library("mgcv")
library("vegan")
library("cluster")
require("nFactors")
library("gplots")

#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>
# START CPUE STANDARDIZATION
#><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>

years <- min(dat$Year):max(dat$Year)

names(dat)

# VESSEL TARGETING
Vtarget = unique(dat$TARGET) 

#exclude SHARK for SWO standardization
Vtarget = Vtarget[-2]

#Subsets of Fleet specific data sets
Fleets = c("dSWO","dTUNA")
for(i in 1:length(Vtarget)) assign(Fleets[i],subset(dat,TARGET==paste(Vtarget[i])))

names(dat)

#define species matrix 
sp.st = 15
sp.end = ncol(dat)
nspec = length(sp.st:sp.end)

#############################################################################
#****************************************************************************
# Choose and model type for the analysis
#****************************************************************************
#############################################################################

# run by fleet
#for(k in 1:length(Vtarget){
k=1
d = get(paste(Fleets[k]))
fleet = paste(Fleets[k])

# For ICCAT assessment use only Long < 20 of domestic and long <25 
# for chartered as control
if(fleet=="dSWO"){d=subset(d,Long<20 & Year>2003)}else{subset(d,Long<25 & Year>2003)}

# Species matrix
spd = d[,sp.st:sp.end]
sp.names = names(spd)

## prepare species composition for PCA by root-root transformation
pca_dat <- (sqrt(sqrt(spd/apply(spd, 1,sum))))

pca_dat[is.na(pca_dat)]<-0

#run PCA
pca<-prcomp(pca_dat, scale=T) 

# Predict PCA Loadings
pr_pca <- predict(pca,pca_dat)

pcs <- data.frame(pr_pca[,1:(ncol(spd)-1)])


# get Eigenvalue
eig = summary(pca)$sdev^2

# run Optimal Coordinates test
OCtest = nScree(eig)



Par = list(mfrow=c(1,1),mex=1,mar = c(5, 5, 1, 1), mgp =c(3,1,0), tck = -0.02,cex=1,mai=c(1,1,0.2,0.2))

png(file = paste0(assessment,"/",Fleets,"_Scree.png"), width = 8, height = 6,res = 200, units = "in")
plotnScree(OCtest)
# retain number of PCs in combination with Eigenvalue > 1
dev.off()

nPC = max(min(OCtest$Components$nkaiser,OCtest$Components$noc),1)


col=c("grey",1)

# prepare cluster data
clus_dat <- pca_dat

nclus = (nPC+1)

cl_sel = clara(pcs[,1:nPC], nclus,sample=200,sampsize=250,pamLike = TRUE) 

# FT clusters
FT = as.matrix(cl_sel$cluster)


sp.comp = aggregate(as.matrix(spd)~FT,dat,sum)
sp.prop = round(sp.comp[,2:ncol(sp.comp)]/apply(sp.comp[,2:ncol(sp.comp)],1,sum)*100,1)
# Group composition
sp.prop 


tactics = 1:nrow(sp.prop)

col.sp = NULL
FT.sp = NULL
# Find the dominant species per cluster
for(i in 1:length(tactics)){
  col.sp[i] = ifelse(max(sp.prop[i,])>30,(1:nspec)[sp.prop[i,]==max(sp.prop[i,])],nspec+1) 
  FT.sp[i] = ifelse(max(sp.prop[i,])>30,sp.names[sp.prop[i,]==max(sp.prop[i,])],"Mix")
}



#Set up PCA plot
load = pca$rotation*10

specs = names(pca_dat)


Par = list(mfrow=c(1,1),mex=1,mar = c(5, 5, 1, 1), mgp =c(3,1,0), tck = -0.02,cex=1,mai=c(1,1,0.2,0.2))

png(file = paste0(assessment,"/",Fleets,"_PCA.png"), width = 8, height = 8,res = 200, units = "in")
par(Par)
ylim=c(min(load[,2]*1.4),max(load[,2]*1.2))
xlim=c(min(load[,1]*1.4),max(load[,1]*1.2))

plot(pca$x[,1],pca$x[,2],col=(FT+1),ylim=ylim,xlim=xlim,cex=0.5,ylab="PC2",xlab="PC1",pch=16)
load = pca$rotation*10
for(i in 1:ncol(pcs))
{
  lines(c(0,load[i,1]),c(0,load[i,2]),col=1,lwd=1)
  text(load[i,1]*1.1,load[i,2]*1.1,paste(specs[i]),cex=1.2,font=2)
}

dev.off() # Graphic end


pcsBin = ifelse(pcs>0,1,0)
dataset= data.frame(d,pcs[,1:5],FT)

names(dataset)

#Choose species
  dataset$Pr = ifelse(dataset$y>0,1,0) 
  final_dat <- subset(dataset, y <quantile(dataset$y, probs = c(0.99))& HOOKnr>500) # exclude outliers
  final_dat$y=final_dat$y/final_dat$HOOKnr*1000
  
  check = aggregate(y~Year,final_dat,mean)
  
  nom <- aggregate(y~Year,final_dat,mean)$y
  nom=nom[nom!=0]
  
  
  #choose optimal p for tweedie distribution
  p = 1.3
  kn=5
  nPC
  
  models = c("Nominal","Standardized","Spatial","Cluster")
  #nominal
  f0 = formula(y~as.factor(Year))
  
  # Basic
  f1 = formula(y~as.factor(Year)+s(Month,bs="cc",k=8))
  
  # Spatial
  f2 = formula(y~as.factor(Year)+s(Month,bs="cc",k=8)+s(Lat,Long,k=10))
  
  # Tactic 
  f3 = formula(y~as.factor(Year)+s(Month,bs="cc",k=8)+s(Lat,Long,k=10)+as.factor(FT))
  
  
  m0 <- bam(f0,data = final_dat,family=Tweedie(p=p))
  m1 <- bam(f1,data = final_dat,family=Tweedie(p=p))
  m2 <- bam(f2,data = final_dat,family=Tweedie(p=p))
  m3 <- bam(f3,data = final_dat,family=Tweedie(p=p))

  bic = BIC(m0,m1,m2,m3)
  bic
  
  # choose model with lowest BIC automatically
  best = bic[bic[,2]==min(bic[,2]),]
  model.fixed = get(row.names(best)) 
  names(model.fixed)
  
  # optimise tweedie
  p_i = seq(1.1,1.8,0.05)
  
  LL=NULL
  for(l in 1:length(p_i)){
    if(l==1) cat(paste0("\n","Running Tweediee Profiling","\n"))
    if(l==1) cat(paste("\n","|"))
    cat("*")
    LL[l] = logLik(bam(formula.gam(model.fixed),data = final_dat,family=Tweedie(p=p_i[l])))[1]
    if(l==length(p_i)) cat(paste("|","\n"))
    }
  
  p_opt = p_i[LL==max(LL)]
  
  Par = list(mfrow=c(1,1),mex=1,mar = c(5, 5, 1, 1), mgp =c(3,1,0), tck = -0.02,cex=1,mai=c(1,1,0.2,0.2))
  
  png(file = paste0(assessment,"/",species,"_",Fleets,"_Tweedei.png"), width = 6, height = 6,res = 200, units = "in")
  par(Par)
  plot(p_i,LL,type="l",col=1,lwd=2,xlab="Tweedie p")
  lines(c(p_opt,p_opt),c(-10000,min(LL)),lty=2)
  dev.off()

  
  rv = factor(final_dat$Vessel_No)  
  # check for random vessel effect
  mod.rv = gamm(formula(model.fixed),data=final_dat,family=Tweedie(p=p_opt),random=list(rv=~1))
  mod.r0 = gamm(formula(model.fixed),data=final_dat,family=Tweedie(p=p_opt))
  AIC(mod.r0$lme,mod.rv$lme)

  model = mod.rv
  
  
  years = sort(unique(final_dat$Year))
  
  #select best month
  Mon =  aggregate(y~Month,final_dat,FUN=mean)
  sel.Mon = subset(Mon,y==max(y))[,1]
  
  FTs =  aggregate(y~FT,final_dat,FUN=mean)
  sel.FT = subset(FTs,y==max(y))[,1]
  
  # select 
  Pr = ifelse(final_dat$y>0,1,0)
  Lat.degree = round(final_dat$Lat,0)
  Long.degree = round(final_dat$Long,0)
  
  agg.degrees = aggregate(Pr~Lat.degree+Long.degree, final_dat,sum)
  # select coordinates with at least 10 encounters to avoid inflated SEs
  sel.LatLong = agg.degrees[agg.degrees$Pr>9,1:2]
  
  
  #predict year
  log.y=se2=mat.or.vec(nrow(sel.LatLong),length(years))
  for(i in  1:nrow(sel.LatLong))
  {  
    pdat  <- data.frame(Year=years,Month=sel.Mon,FT=sel.FT,Lat=sel.LatLong[i,1],Long=sel.LatLong[i,2])
    
    p <- predict(model$gam,pdat,type="link",se=T)
    log.y[i,] = p$fit
    se = p$se.fit
    se2[i,] = se^2
   
  }
  logmu.Y = apply(log.y,2,mean)
  mu.Y = exp(logmu.Y)
  se.Y = sqrt(apply(se2,2,mean))
  lcl.Y = exp(logmu.Y-se.Y*1.96)
  ucl.Y = exp(logmu.Y+se.Y*1.96)
  
  
  # Predict Month (for final year)
  log.m=se2=mat.or.vec(nrow(sel.LatLong),12)
  for(i in  1:nrow(sel.LatLong)){  
    pdat  <- data.frame(Year=max(years),Month=1:12,FT=sel.FT,Lat=sel.LatLong[i,1],Long=sel.LatLong[i,2])
    
    p <- predict(model$gam,pdat,type="link",se=T)
    log.m[i,] = p$fit
    se = p$se.fit
    se2[i,] = se^2
  }
  logmu.m = apply(log.m,2,mean)
  mu.m = exp(logmu.m)
  se.m = sqrt(apply(se2,2,mean))
  lcl.m = exp(logmu.m-se.m*1.96)
  ucl.m = exp(logmu.m+se.m*1.96)
  
  # Predict FT (for final year)
  log.ft=se2=mat.or.vec(nrow(sel.LatLong),nrow(FTs))
  for(i in  1:nrow(sel.LatLong)){  
    pdat  <- data.frame(Year=max(years),Month=sel.Mon,FT=1:nrow(FTs),Lat=sel.LatLong[i,1],Long=sel.LatLong[i,2])
    
    p <- predict(model$gam,pdat,type="link",se=T)
    log.ft[i,] = p$fit
    se = p$se.fit
    se2[i,] = se^2
  }
  logmu.ft = apply(log.ft,2,mean)
  mu.ft = exp(logmu.ft)
  se.ft = sqrt(apply(se2,2,mean))
  lcl.ft = exp(logmu.ft-se.ft*1.96)
  ucl.ft = exp(logmu.ft+se.ft*1.96)
  
  
  #create prediction dataset
  target_dat = subset(final_dat,y>0)
  pred_dat <- data.frame (Year=sort(unique(target_dat$Year)),
                         Month = median(target_dat$Month),
                         Lat = median(target_dat$Lat),Long = median(target_dat$Long),FT=1)
  
  
  
  models.all = c(models,"FINAL_GAMM")
  CPUEs = NULL
  normI = NULL
 
  for(m in 1:5){
    
    if(m==5){
    cpue <- mu.Y  
    cpue.se <- se.Y
      
    assign(paste0("Index_",species,"_",models.all[m]),data.frame(Year=pred_dat[,1],nominal=nom,cpue,cpue.se,lci=lcl.Y,uci=ucl.Y))
    write.csv(get(paste0("Index_",species,"_",models.all[m])),paste0(assessment,"/Index_",species,"_",Fleets[k],models.all[m],".csv"))
      
    } else {
    pred <- predict(get(paste0("m",m-1)),pred_dat,type="link",se=T)}  
    cpue <- exp(pred$fit)  
    cpue.se <- (pred$se)
    CPUEs = cbind(CPUEs,cpue)
    normI = cbind(normI,cpue/mean(cpue))
  }  
  
  Par = list(mfrow=c(2,1),mai=c(0.1,0.6,0,.15),omi = c(0.5,0.,0.2,0) + 0.1,mgp=c(2,0.5,0), tck = -0.02,cex=0.8)
  png(file = paste0(assessment,"/Indices_",species,"_",Fleets[k],".png"), width = 7, height = 8,res = 200, units = "in")
  par(Par)
  
  plot(years,years,type="n",ylim=range(c(0.8*lcl.Y),c(1.05*ucl.Y)),xaxt="n", xlab="",ylab="Standardized CPUE (kg/1000 hooks)")
  axis(1,labels = F)
  polygon(c(years,rev(years)),c(lcl.Y,rev(ucl.Y)),col="grey", border=0)
  lines(years,mu.Y)
  #abline(h=mean(cpue),lty=2)
  plot(years,years,type="n",ylim=range(normI),xlab="Year",ylab="normalized CPUE")
  for(m in 1:4) lines(sort(unique(target_dat$Year)),normI[,m],col=rainbow(4)[m],lwd=2,lty=2)
  lines(years,normI[,5],lwd=1)
  abline(h=1,lty=2)
  legend('topright',paste(models.all)[1:5],lwd=2,col=c(rainbow(4)[1:4],1),cex=0.8,bty="n",lty= c(rep(2,4),1))
  mtext("Year",side=1,line=+2,cex=0.9)
  dev.off()
  
  Par = list(mfrow=c(2,1),mai=c(0.7,0.7,0,.1),omi = c(0.,0.,0.,0) + 0.1,mgp=c(2,0.2,0), tck = -0.02,cex=0.9)
  png(file = paste0(assessment,"/Frequency_",species,"_",Fleets[k],".png"), width = 5, height = 8,res = 200, units = "in")
  par(Par)
  hist(final_dat$y,freq=T,breaks=200,xlab="CPUE (kg/1000 hooks)",main="")
  hist(final_dat$y,freq=F,breaks=200,xlab="CPUE (kg/1000 hooks)",main="")
  # predict expected values
  yhat = predict(model$gam,type="response")
  # generate
  tweedie.dist =rTweedie(rep(yhat,100),p=p_opt,phi= summary(model$gam)$dispersion)
  hist(tweedie.dist,breaks=280,freq=F,col=rgb(1,0,0,0.3),add=T)
  dev.off()
  write.csv(final_dat,paste0(assessment,"/Data",species,fleet[k],".csv"))
  
  
  # Plot random effects
  Par = list(mfrow=c(1,1),mai=c(0.5,0.5,0,.1),omi = c(0.,0.,0.,0) + 0.1,mgp=c(2,0.2,0), tck = -0.02,cex=0.9)
  png(file = paste0(assessment,"/RandomEffects_",species,"_",Fleets[k],".png"), width = 8, height = 5,res = 200, units = "in")
  par(Par)
  re = as.numeric(summary(model$lme)$coefficient$random$rv)
  nB =length(re)
  CI = c(-1,1)*(sd(re)*1.96)
  plotCI(re, ylab="Random Effect Coefficient",xlab="Vessel",type="n",xlim=c(0.5,nB+0.5),ylim=range(CI))
  alpha = rep(0,length(re))
  #polygon(c(1:nB,nB:1),c(rep(CI[1],nB),rev(rep(CI[2],nB))),col="grey",border=2)
  abline(h=CI[1],lty=2)
  abline(h=CI[2],lty=2)
  points(re,pch=21,bg="white",cex=1.4)
  abline(0,0,lty=3)
  dev.off()
  
  
  Par = list(mfrow=c(1,2),mai=c(0.5,0.1,0,.15),omi = c(0.1,0.6,0.2,0) + 0.1,mgp=c(2,0.5,0), tck = -0.02,cex=0.8)
  png(file = paste0(assessment,"/FixedEffects_",species,"_",Fleets[k],".png"), width = 8, height = 4,res = 200, units = "in")
  par(Par)
  
  plot(1:12,mu.m,type="n",ylim=range(c(0),c(1.05*ucl.m)),xaxt="n", xlab="Month",ylab="Standardized CPUE (kg/1000 hooks)")
  axis(1,labels = paste0(1:12),at=1:12)
  polygon(c(1:12,rev(1:12)),c(lcl.m,rev(ucl.m)),col="grey", border=0)
  lines(1:12,mu.m)
  plotCI(1:nrow(FTs),mu.ft,type="n",ui = ucl.ft,li =lcl.ft ,ylim=range(c(0.8*lcl.ft),c(1.05*ucl.ft)),xaxt="n",xlim=c(0.7,(nrow(FTs)+.3)), xlab="Fishing Cluster",ylab="Standardized CPUE",gap=0)
  points(1:nrow(FTs),mu.ft,pch=21,bg="white",cex=1.3)
  axis(1,at=1:nrow(FTs))
  mtext("Standardized CPUE (kg/1000 hooks)",2,outer=T,line=1,cex=0.9)
  dev.off()
  
  #} THE END  
  
