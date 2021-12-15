  # CODED BY CAMILO BRANDAO-DE-RESENDE
  
  #--------------------------------------------------------------------------------------
#--------------------------------- DETTACH PACKAGES -----------------------------------
#--------------------------------------------------------------------------------------

detachAllPackages <- function() {
  basic.packages <- c("package:pixmap","package:dotCall64","package:grid","package:spam","package:maps","package:tiff","package:rtiff","package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

library('pixmap',verbose=FALSE)
library('tiff',verbose=FALSE)
library('fields',verbose=FALSE)
library('stats',verbose=FALSE)
library('graphics',verbose=FALSE)
library('grDevices',verbose=FALSE)
library('utils',verbose=FALSE)
library('datasets',verbose=FALSE)
library('methods',verbose=FALSE)
library('base',verbose=FALSE)
library('mrbsizeR',verbose=FALSE)

#--------------------------------------------------------------------------------------
##--------------------------- FUNCTIONS -----------------------------------------------
#--------------------------------------------------------------------------------------

#.........................................................................
#                             IMAGE FUNCTIONS 
#.........................................................................


imagetoarray <- function (img) {
  z=array(rep(1, length(img)*256*256), dim=c(256,256, length(img)))
  for (i in 1:length(img))
  {
    z[,,i]=matrix(unlist(img[i]),nrow=256)
  }
  return(z)
}

avgimage <- function (img) {
  avg=array(rep(0, dim(img)[1]*dim(img)[1]), dim=c(dim(img)[1],dim(img)[1]))
  for (i in 1:dim(img)[3]){
    avg=avg+img[,,i]
  }
  avg=avg/dim(img)[3]
  return(avg)
}

medianimage <- function (img) {
  med=array(rep(0, dim(img)[1]*dim(img)[1]), dim=c(dim(img)[1],dim(img)[1]))
  for (i in 1:dim(img)[1])
    for (j in 1:dim(img)[2]){
      med[i,j]=trunc(median(img[i,j,]))
    }
  return(med)
}


subtractbackground <- function (img,background) {
  if (is.na(dim(img)[3])) return(img-background+128)
  if (!is.na(dim(img)[3])) {
    for (i in 1:dim(img)[3]) img[,,i]=(img[,,i]-background+128)
    return(img)
  }
}

findcenterz2 <- function (img) {
  img=img[-227:-256,-227:-256]
  img=img[-1:-30,-1:-30]
  
  rmean=cbind(31:226,rowMeans(img[,(98-80):(98+80)]))
  rmean[,2]=rmean[,2]-predict(lm(rmean[,2]~rmean[,1]))
  r1=rmean[1:49,]
  peak1=r1[(r1[,2]==max(r1[,2])),]
  r2=rmean[50:98,]
  peak2=r2[r2[,2]==max(r2[,2]),]
  r3=rmean[99:147,]
  peak3=r3[r3[,2]==max(r3[,2]),]
  r4=rmean[148:196,]
  peak4=r4[r4[,2]==max(r4[,2]),]
  
  peaks=rbind(peak1,peak2,peak3,peak4)
  for (j in 2:4) if ((peaks[j,1]-peaks[j-1,1])<20) {
    if (peaks[j,2]>peaks[j-1,2]) peaks[j-1,2]=0
    if (peaks[j,2]<=peaks[j-1,2]) peaks[j,2]=0
  }
  peaks=rbind(peaks[(peaks[,2]==sort(peaks[,2])[3]),],peaks[(peaks[,2]==sort(peaks[,2])[4]),])
  rcenter=trunc(mean(peaks[,1]))
  
  cmean=cbind(31:226,colMeans(img[(98-80):(98+80),]))
  cmean[,2]=cmean[,2]-predict(lm(cmean[,2]~cmean[,1]))
  c1=cmean[1:49,]
  peak1=c1[(c1[,2]==max(c1[,2])),]
  c2=cmean[50:98,]
  peak2=c2[c2[,2]==max(c2[,2]),]
  c3=cmean[99:147,]
  peak3=c3[c3[,2]==max(c3[,2]),]
  c4=cmean[148:196,]
  peak4=c4[c4[,2]==max(c4[,2]),]
  
  peaks=rbind(peak1,peak2,peak3,peak4)
  for (j in 2:4) if ((peaks[j,1]-peaks[j-1,1])<20) {
    if (peaks[j,2]>peaks[j-1,2]) peaks[j-1,2]=0
    if (peaks[j,2]<=peaks[j-1,2]) peaks[j,2]=0
  }
  peaks=rbind(peaks[(peaks[,2]==sort(peaks[,2])[3]),],peaks[(peaks[,2]==sort(peaks[,2])[4]),])
  ccenter=trunc(mean(peaks[,1]))
  
  rmean=cbind(1:41,rowMeans(img[(rcenter-20):(rcenter+20),(ccenter-20):(ccenter+20)]))
  peak=rmean[rmean[,2]==max(rmean[,2]),1]
  rcenter=rcenter+trunc((peak-21)/3)
  
  cmean=cbind(1:41,colMeans(img[(rcenter-20):(rcenter+20),(ccenter-20):(ccenter+20)]))
  peak=cmean[cmean[,2]==max(cmean[,2]),1]
  ccenter=ccenter+trunc((peak-21)/3)
  
  return(c(rcenter,ccenter))
}

findcenterz4 <- function (img,centerz2) {
  img=img[-227:-256,-227:-256]
  img=img[-1:-30,-1:-30]
  
  rmean=cbind(31:226,rowMeans(img[,max(centerz2[2]-30-80,0):min(centerz2[2]-30+80,196)]))
  rmean[,2]=rmean[,2]-predict(lm(rmean[,2]~rmean[,1]))
  r1=rmean[1:49,]
  peak1=r1[(r1[,2]==max(r1[,2])),]
  r2=rmean[50:98,]
  peak2=r2[r2[,2]==max(r2[,2]),]
  r3=rmean[99:147,]
  peak3=r3[r3[,2]==max(r3[,2]),]
  r4=rmean[148:196,]
  peak4=r4[r4[,2]==max(r4[,2]),]
  
  peaks=rbind(peak1,peak2,peak3,peak4)
  for (j in 2:4) if ((peaks[j,1]-peaks[j-1,1])<20) {
    if (peaks[j,2]>peaks[j-1,2]) peaks[j-1,2]=0
    if (peaks[j,2]<=peaks[j-1,2]) peaks[j,2]=0
  }
  peaks=rbind(peaks[(peaks[,2]==sort(peaks[,2])[3]),],peaks[(peaks[,2]==sort(peaks[,2])[4]),])
  rcenter=trunc(mean(peaks[,1]))
  
  cmean=cbind(31:226,colMeans(img[max(centerz2[1]-30-80,0):min(centerz2[1]-30+80,196),]))
  cmean[,2]=cmean[,2]-predict(lm(cmean[,2]~cmean[,1]))
  c1=cmean[1:49,]
  peak1=c1[(c1[,2]==max(c1[,2])),]
  c2=cmean[50:98,]
  peak2=c2[c2[,2]==max(c2[,2]),]
  c3=cmean[99:147,]
  peak3=c3[c3[,2]==max(c3[,2]),]
  c4=cmean[148:196,]
  peak4=c4[c4[,2]==max(c4[,2]),]
  
  peaks=rbind(peak1,peak2,peak3,peak4)
  for (j in 2:4) if ((peaks[j,1]-peaks[j-1,1])<20) {
    if (peaks[j,2]>peaks[j-1,2]) peaks[j-1,2]=0
    if (peaks[j,2]<=peaks[j-1,2]) peaks[j,2]=0
  }
  peaks=rbind(peaks[(peaks[,2]==sort(peaks[,2])[3]),],peaks[(peaks[,2]==sort(peaks[,2])[4]),])
  ccenter=trunc(mean(peaks[,1]))
  
  rmean=cbind(1:41,rowMeans(img[(rcenter-20):(rcenter+20),(ccenter-20):(ccenter+20)]))
  peak=rmean[rmean[,2]==max(rmean[,2]),1]
  rcenter=rcenter+trunc((peak-21)/3)
  
  cmean=cbind(1:41,colMeans(img[(rcenter-20):(rcenter+20),(ccenter-20):(ccenter+20)]))
  peak=cmean[cmean[,2]==max(cmean[,2]),1]
  ccenter=ccenter+trunc((peak-21)/3)
  
  if (abs(rcenter-centerz2[1])>=50) rcenter=centerz2[1]
  if (abs(ccenter-centerz2[2])>=50) ccenter=centerz2[2]
  
  return(c(rcenter,ccenter))
}

findcenterH <- function (img) {
  img[img<0.1] = 0
  matriz128=matrix(1:128,nrow=128,ncol=128)
  rcenter=round(sum(matriz128*img)/sum(img))
  ccenter=round(sum(t(matriz128)*img)/sum(img))
  return(c(rcenter,ccenter))
}

make128x128 <- function (img,center) {
  if (is.na(dim(img)[3])) return(img[(center[1]-63):(center[1]+64),(center[2]-63):(center[2]+64)])
  if (!is.na(dim(img)[3])) {
    imgg=array(rep(1, dim(img)[3]*128*128), dim=c(128,128, dim(img)[3]))
    for (i in 1:dim(img)[3]) imgg[,,i]=(img[(center[1]-63):(center[1]+64),(center[2]-63):(center[2]+64),i])
    return(imgg)
  }
}

thickness <- function (H,centerH,n=56,pixelsize= 0.098) {
  centers=array(rep(0, n*2), dim=c(n,2))
  colnames(centers)=c('D',"MeanThick")
  centers[,1]=(0:(n-1))*pixelsize
  cc1=rowMeans(H[centerH[1]:(centerH[1]+n-1),(centerH[2]-1):(centerH[2]+1)])
  cc2=rowMeans(H[centerH[1]:(centerH[1]-n+1),(centerH[2]-1):(centerH[2]+1)])
  cc3=colMeans(H[(centerH[1]-1):(centerH[1]+1),centerH[2]:(centerH[2]+n-1)])
  cc4=colMeans(H[(centerH[1]-1):(centerH[1]+1),centerH[2]:(centerH[2]-n+1)])
  centers[,2]=(cc1+cc2+cc3+cc4)/4
  
  return(centers)
}

contrastfluctuation <- function (img,n=56,centerH,pixelsize= 0.098,background) {
  if (dim(img)[3]>1000) img=img[,,1:1000]
  centers=array(rep(0, n*2), dim=c(n,2))
  colnames(centers)=c('D',"<(deltaC)2> ")
  interval=3
  centers[,1]=(0:(n-1))
  cc1=cc2=cc3=cc4=centers[,2]
  CC=array(rep(0,128*128),dim=c(128,128))
  
  for (i in 1:128) for (j in (centerH[2]-interval):(centerH[2]+interval)) {
    I=img[i,j,]
    I0=background[i,j]
    CC[i,j]=(mean(I^2)-(mean(I)^2))/(I0^2)
  }
  
  for (i in (centerH[1]-interval):(centerH[1]+interval)) for (j in 1:128) {
    I=img[i,j,]
    I0=background[i,j]
    CC[i,j]=(mean(I^2)-(mean(I)^2))/(I0^2)
  }
  
  CCC=CC
  for (i in (1+interval): (128-interval)) CC[i,centerH[2]]=mean(CCC[(i-interval):(i+interval),(centerH[2]-interval):(centerH[2]+interval)])
  for (j in (1+interval): (128-interval)) CC[centerH[1],j]=mean(CCC[(centerH[1]-interval):(centerH[1]+interval),(j-interval):(j+interval)])
  
  cc1=CC[centerH[1]:(centerH[1]+n-1),centerH[2]]
  cc2=CC[centerH[1]:(centerH[1]-n+1),centerH[2]]
  cc3=CC[centerH[1],centerH[2]:(centerH[2]+n-1)]
  cc4=CC[centerH[1],centerH[2]:(centerH[2]-n+1)]
  
  centers[,1]=pixelsize*centers[,1]
  centers[,2]=(cc1+cc2+cc3+cc4)/4
  return(centers)
}

findpeaks <- function (H,centerH) {
  h=deltah=colMeans(H[(centerH[1]-4):(centerH[1]+4),])
  v=deltav=rowMeans(H[,(centerH[2]-4):(centerH[2]+4)])
  for (i in 2:128){
    deltah[i]=h[i]-h[i-1]
    deltav[i]=v[i]-v[i-1]
  }
  deltav[1]=deltah[1]=0
  for (i in centerH[1]:128) deltav[i]=-deltav[i]
  for (i in centerH[2]:128) deltah[i]=-deltah[i]
  
  hlim2=centerH[2]+which(deltah[(centerH[2]+1):128]==max(deltah[(centerH[2]+1):128]))
  hlim1=which(deltah[1:centerH[2]]==max(deltah[1:centerH[2]]))
  hd1=hlim2-hlim1
  hlim22=centerH[2]+which(deltah[(centerH[2]+1):128]==min(deltah[(centerH[2]+1):128]))
  hlim11=which(deltah[1:centerH[2]]==min(deltah[1:centerH[2]]))
  hd11=hlim22-hlim11
  
  vlim2=centerH[1]+which(deltav[(centerH[1]+1):128]==max(deltav[(centerH[1]+1):128]))
  vlim1=which(deltav[1:centerH[1]]==max(deltav[1:centerH[1]]))
  vd1=vlim2-vlim1
  vlim22=centerH[1]+which(deltav[(centerH[1]+1):128]==min(deltav[(centerH[1]+1):128]))
  vlim11=which(deltav[1:centerH[1]]==min(deltav[1:centerH[1]]))
  vd11=vlim22-vlim11
  
  HMAX2=centerH[2]+which(h[(centerH[2]+1):128]==max(h[(centerH[2]+1):128]))
  HMAX1=which(h[1:centerH[2]]==max(h[1:centerH[2]]))
  dh=HMAX2-HMAX1
  
  vmax2=centerH[1]+which(v[(centerH[1]+1):128]==max(v[(centerH[1]+1):128]))
  vmax1=which(v[1:centerH[1]]==max(v[1:centerH[1]]))
  dv=vmax2-vmax1
  
  rHMAX=round((dh+dv)/4)
  r=round((hd1+vd1)/4)
  p1=round((hd11+vd11)/4)
  return(c(0,p1,rHMAX,r))
}



getimages <- function (k, folder) {
  input_file_background = paste(folder,'background.tif',sep='')
  background=imagetoarray(readTIFF(input_file_background,all = T,as.is=T))
  background=avgimage(background)
  
  input_file_z0=paste(folder,'H',k,' Z0.tif',sep='')
  input_file_z2=paste(folder,'H',k,' Z2.tif',sep='')
  input_file_z4=paste(folder,'H',k,' Z4.tif',sep='')
  
  
  z2=imagetoarray(readTIFF(input_file_z2,all = T,as.is=T))
  avg_z2=subtractbackground(avgimage(z2),background)
  remove(z2)
  centerz2=findcenterz2(avg_z2)
  
  z0=imagetoarray(readTIFF(input_file_z0,all = T,as.is=T))
  avg_z0=subtractbackground(avgimage(z0),background)
  centerz0=centerz2
  remove(z0)
  
  z4=imagetoarray(readTIFF(input_file_z4,all = T,as.is=T))
  z4=subtractbackground(z4,background)
  avg_z4=avgimage(z4)
  centerz4=findcenterz4(avg_z4,centerz2)
  
  return(list(avg_z0=make128x128(avg_z0,centerz0),avg_z2=make128x128(avg_z2,centerz2),avg_z4=make128x128(avg_z4,centerz4),z4=make128x128(z4,centerz4),background=make128x128(background,centerz4)))
}

thickprofile <- function(avg_z0,avg_z2,avg_z4,z4) {
  C0 = (avg_z0-mean(avg_z0))/mean(avg_z0)
  C2 = (avg_z2-mean(avg_z2))/mean(avg_z2)
  
  C = C0 - C2
  x=y=-64:63
  mx=t(array(x,dim=c(128,128)))
  my=array(-y,dim=c(128,128))
  
  FShift=fft(C)
  FF=fftshift(FShift)
  
  qx=qy=2*pi*(-64:63)/128
  mqx=t(array(qx,dim=c(128,128)))
  mqy=array(-qy,dim=c(128,128))
  
  Q2 = mqx*mqx+mqy*mqy
  Q2[65,65]=1
  FqShift = (-FF/Q2)
  Fq = fftshift(FqShift)
  H0 = -Re(fft(Fq,inverse = TRUE)/length(Fq))*(pixelsize ^2*n_ob/(delta_n*2))
  
  R = RR = sqrt(mx*mx + my*my)
  R[(R*pixelsize)<5]=0
  R[(R*pixelsize)>=5]=1
  
  RR[(RR*pixelsize)<5]=1
  RR[(RR*pixelsize)>=5]=0
  
  Z0 = H0*R
  H=H0-mean(Z0[Z0!=0])
  H[H<0]=0
  centerH=findcenterH(H)
  NNN=min(cbind(centerH,128-centerH))-3
  
  return(list(H=H,centerH=centerH,NNN=NNN,thickprofile= thickness(H,centerH,n=NNN)))
}

cellcoordinates <- function(thickprofile,pixelsize,peaks) {
  library('pracma',verbose=FALSE)
  HMAX=max(thickprofile[,2])
  SMAX=max(gradient(thickprofile[,2],pixelsize))
  Concavity=max(gradient(gradient(thickprofile[,2],pixelsize),pixelsize)[1:peaks[3]])
  r=peaks[4]*pixelsize
  detach('package:pracma',character.only=T)
  return(list(r=r,Concavity=Concavity,HMAX=HMAX,SMAX=SMAX))
}


areavolumesphericity <- function(H,r) {
  library('pracma',verbose=FALSE)
  CENTERDISTANCE=array(rep(0,128*128),dim=c(128,128))
  for (i in 1:128) for (j in 1:128) CENTERDISTANCE[i,j]=pixelsize*sqrt((i-64)^2+((j-64)^2))
  meanHoutside=mean(H[CENTERDISTANCE>(r+pixelsize)])
  H=H-meanHoutside
  H[H<0.5]=0
  H1=H/2
  H1[H1<0.5]=0
  
  HH=gradient(H1,pixelsize,pixelsize)
  HHX=HH$X
  HHY=HH$Y
  inte2=sqrt(1+HHX^2+HHY^2)
  inte2[inte2==1]=0
  area=2*sum(inte2)*pixelsize*pixelsize
  volume = sum(H)*pixelsize*pixelsize
  sphericity=(pi^(1/3)) * ((6*volume)^(2/3)) / (area)
  
  detach('package:pracma',character.only=T)
  
  return(list(area=area,volume=volume,sphericity=sphericity))
}



plotcell <- function(folder,k,avg_z0,r,avg_z2,area,avg_z4,volume,H,centerH,sphericity,thickprofile,contrast,peaks,rrr,smax,concavity,hmax){
  library(plotrix)
  png(paste(folder,'results/','cell',k,'.png',sep=''),width = 900, height = 600)
  par(mfrow=c(2,3))
  avg_z0[1,1]=1
  avg_z0[128,128]=256
  image(apply(t(avg_z0),2,rev),col=grey(seq(0, 1, length = 128)), axes = F)
  #plot(avg_z0,rescale=FALSE)
  title(main='A: zf = 0 µm', cex.main = 2)
  segments(38/128, 5/128, 88/128, 5/128, col= 'yellow',lwd = 3)
  text(64/128,10/128,'5 µm', col= 'yellow', cex=1.7)
  
  avg_z2[1,1]=1
  avg_z2[128,128]=256
  image(apply(t(avg_z2),2,rev),col=grey(seq(0, 1, length = 128)), axes = F)
  #plot(avg_z2,rescale=FALSE)
  title(main='B: zf = +2 µm', cex.main = 2)
  segments(38/128, 5/128, 88/128, 5/128, col= 'yellow',lwd = 3)
  text(64/128,10/128,'5 µm', col= 'yellow', cex=1.7)
  
  avg_z4[1,1]=1
  avg_z4[128,128]=256
  image(apply(t(avg_z4),2,rev),col=grey(seq(0, 1, length = 128)), axes = F)
  #plot(avg_z4,rescale=FALSE)
  title(main='C: zf = +4 µm', cex.main = 2)
  segments(38/128, 5/128, 88/128, 5/128, col= 'yellow',lwd = 3)
  text(64/128,10/128,'5 µm', col= 'yellow', cex=1.7)
  
  H[1,1]=1
  H[128,128]=2.5
  H[35:44,114:126]=0
  H[45:54,114:126]=0.5
  H[55:64,114:126]=1
  H[65:74,114:126]=1.5
  H[75:84,114:126]=2
  H[85:94,114:126]=2.5
  
  cellcolors <- unlist(H)
  cellcolors <- ifelse(cellcolors < 0.4, "white", ifelse(cellcolors < 0.5, heat.colors(21,rev = T)[1], 
                                                         ifelse(cellcolors < 0.6, heat.colors(21,rev = T)[2], 
                                                                ifelse(cellcolors < 0.7, heat.colors(21,rev = T)[3], 
                                                                       ifelse(cellcolors < 0.8, heat.colors(21,rev = T)[4], 
                                                                              ifelse(cellcolors < 0.9, heat.colors(21,rev = T)[5], 
                                                                                     ifelse(cellcolors < 1, heat.colors(21,rev = T)[6], 
                                                                                            ifelse(cellcolors < 1.1, heat.colors(21,rev = T)[7], 
                                                                                                   ifelse(cellcolors < 1.2, heat.colors(21,rev = T)[8], 
                                                                                                          ifelse(cellcolors < 1.3, heat.colors(21,rev = T)[9], 
                                                                                                                 ifelse(cellcolors < 1.4, heat.colors(21,rev = T)[10], 
                                                                                                                        ifelse(cellcolors < 1.5, heat.colors(21,rev = T)[11], 
                                                                                                                               ifelse(cellcolors < 1.6, heat.colors(21,rev = T)[12], 
                                                                                                                                      ifelse(cellcolors < 1.7, heat.colors(21,rev = T)[13], 
                                                                                                                                             ifelse(cellcolors < 1.8, heat.colors(21,rev = T)[14], 
                                                                                                                                                    ifelse(cellcolors < 1.9, heat.colors(21,rev = T)[15], 
                                                                                                                                                           ifelse(cellcolors < 2, heat.colors(21,rev = T)[16], 
                                                                                                                                                                  ifelse(cellcolors < 2.1, heat.colors(21,rev = T)[17], 
                                                                                                                                                                         ifelse(cellcolors < 2.2, heat.colors(21,rev = T)[18], 
                                                                                                                                                                                ifelse(cellcolors < 2.3, heat.colors(21,rev = T)[19], 
                                                                                                                                                                                       ifelse(cellcolors < 2.4, heat.colors(21,rev = T)[20],heat.colors(21,rev = T)[21])))))))))))))))))))))
  color2D.matplot(H, ylab="", xlab = "", axes=F, cellcolors = cellcolors,border=NA)
  box(col = 'black')
  #plot(H,rescale=FALSE)
  title(main='D: Cell Thickness Map (µm)', cex.main = 2)
  draw.circle(centerH[1],128-centerH[2],r/pixelsize,nv=100,border = 'black',lwd=1.5)
  segments(38, 5, 88, 5, col= 'black',lwd = 3)
  text(64,10,'5 µm', col= 'black', cex=1.7)
  text(119,40,'2.5', col= 'black', cex=1.7)
  text(119,50,'2.0', col= 'black', cex=1.7)
  text(119,60,'1.5', col= 'black', cex=1.7)
  text(119,70,'1.0', col= 'black', cex=1.7)
  text(119,80,'0.5', col= 'black', cex=1.7)
  text(119,90,'0.0', col= 'black', cex=1.7)
  text(119,100,'µm', col= 'black', cex=1.7)
  
  plot(thickprofile[,1],thickprofile[,2],xlab='Distance from center (µm)' ,ylab='Thickness (µm)',type='b',cex.axis=1.5,cex.lab=1.8)
  abline(v=peaks[4]*pixelsize)
  title(main='E: Angular Average of Cell Thickness', cex.main = 2)
  
  plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '',xlim=c(0,10),ylim=c(0,12))
  title(main='F: Cell Characteristics', cex.main = 2)
  text(c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5), c(11,10,9,8,7,6,5,4,3,2,1),  
       c(paste(c('Radius =',round(r,digits=2),'µm'),collapse=" "),
         paste(c('Area =',round(area,digits=2),'µm2'),collapse=" "),
         paste(c('Volume =',round(volume,digits=2),'µm3'),collapse=" "),
         paste(c('Sphericity',round(sphericity,digits=3)),collapse=" = "),
         paste(c('T0 =',round(thickprofile[1,2],digits=2),'µm'),collapse=" "),
         paste(c('TMAX =',round(hmax,digits=2),'µm'),collapse=" "),
         paste(c('DTMAX =',round(peaks[3]*pixelsize,digits=2),'µm'),collapse=" "),
         paste(c('SMAX =',round(smax,digits=2)),collapse=" "),
         paste(c('DSMAX =',round(peaks[2]*pixelsize,digits=2),'µm'),collapse=" "),
         paste(c('CMAX =',round(concavity,digits=2),'/µm'),collapse=" "),
         paste(c('<u> =',round(rrr[5],digits=2),'nm'),collapse=" ")),
       cex=2, pos=4)
  
  dev.off()
}

processimage <- function(k, folder){
  images = getimages(k = k, folder = folder)
  
  z4=images$z4[,,c(31:(31+26))]
  
  tp = thickprofile(avg_z0=images$avg_z0,avg_z2=images$avg_z2,avg_z4=images$avg_z4,z4=images$z4)
  
  contrast=contrastfluctuation(images$z4,tp$centerH,n=tp$NNN,pixelsize=pixelsize,background=images$background)
  
  peaks=findpeaks(tp$H,tp$centerH)
  
  cellcoord = cellcoordinates(thickprofile=tp$thickprofile,pixelsize=pixelsize,peaks=peaks)
  r = cellcoord$r
  Concavity = cellcoord$Concavity
  HMAX = cellcoord$HMAX
  SMAX = cellcoord$SMAX
  
  AVS = areavolumesphericity(tp$H,r)
  area = AVS$area
  volume = AVS$volume
  sphericity = AVS$sphericity
  
  rrr=c(r,area,volume,sphericity,sqrt(contrast[1,2])/(delta_n*k0),contrast[peaks[2],2],contrast[peaks[3],2],contrast[peaks[4],2])
  names(rrr)=c('radius','area','volume','sphericity','<u>','C2-1','C2-2','C2-R')
  rrr=round(rrr,digits=6)
  
  plotcell(folder,k,images$avg_z0,r,images$avg_z2,area,images$avg_z4,volume,tp$H,tp$centerH,sphericity,tp$thickprofile,contrast,peaks,rrr,SMAX,Concavity,HMAX)
  
  results=cbind(MCHC,k,tp$thickprofile,sqrt(contrast[,2])/(delta_n*k0),r,area,volume,sphericity,sqrt(contrast[1,2])/(delta_n*k0),contrast[peaks[2],2],contrast[peaks[3],2],contrast[peaks[4],2],HMAX,SMAX,Concavity,peaks[2]*pixelsize,peaks[3]*pixelsize,peaks[4]*pixelsize)
  results=round(results,digits=6)
  colnames(results)=c('MCHC','cell #','D','height','<u>','radius','area','volume','sphericity','<u>','C2-1','C2-2','C2-R','HMAX','SMAX','Concavity','D1','D2','DR')
  resultssummary=cbind(results[1,])
  resultssummary=t(round(resultssummary,digits=6))
  
  meanH=array(rep(0,128*128),dim=c(128,128))
  dif=max(abs(64-tp$centerH))
  for (i in (dif+1):(128-dif)) for (j in (dif+1):(128-dif)) meanH[i,j]=tp$H[i+tp$centerH[1]-64,j+tp$centerH[2]-64]
  
  return(list(results=results,resultssummary=resultssummary,meanH = meanH,peaks=peaks, z4 = z4))
}

getaverageresults <- function(cellresults,resultssummary){
  min_NNN=dim(cellresults[cellresults[,"cell #"]==1,])[1]
  n_cells=max(cellresults[,"cell #"])
  for (i in 2:n_cells) min_NNN=min(min_NNN,dim(cellresults[cellresults[,"cell #"]==i,])[1])
  
  avg_results=cellresults[cellresults[,"cell #"]==1,][1:min_NNN,]
  for (i in 2:n_cells) avg_results=avg_results+cellresults[cellresults[,"cell #"]==i,][1:min_NNN,]
  avg_results=avg_results/n_cells
  
  sd_results = (cellresults[cellresults[,"cell #"]==1,][1:min_NNN,]-avg_results)^2
  for (i in 2:n_cells) sd_results=sd_results+(cellresults[cellresults[,"cell #"]==i,][1:min_NNN,]-avg_results)^2
  sd_results=(sd_results/(n_cells-1))^0.5
  ci_results = 1.96*sd_results/(n_cells^0.5)
  
  
  
  
  
  meanH=summeanH/n_cells
  meanHH=array(rep(0,64*64),dim=c(64,64))
  for (i in 1:64) for (j in 1:64) meanHH[i,j]=mean(meanH[(i*2-1):(i*2),(j*2-1):(j*2)])
  meanH=meanHH
  meanH[1:20,][meanH[1:20,]<0.5]=NA
  meanH[,1:20][meanH[,1:20]<0.5]=NA
  meanH[45:64,][meanH[45:64,]<0.5]=NA
  meanH[,45:64][meanH[,45:64]<0.5]=NA
  meanHH=meanH
  for (i in 1:(dim(meanH)[1]-2)) {
    seq=meanHH[i,]
    if (sum(is.na(seq))!=0) if (sum(is.na(seq))<length(seq)) {
      meanH[i,min(which(!is.na(seq)))-1]=0
      meanH[i,max(which(!is.na(seq)))+1]=0
    }
    
    seq=meanHH[,i]
    if (sum(is.na(seq))!=0) if (sum(is.na(seq))<length(seq)) {
      meanH[min(which(!is.na(seq)))-1,i]=0
      meanH[max(which(!is.na(seq)))+1,i]=0
    }
  }
  
  
  meanH=meanH[(min(which(!is.na(meanH[,32])))-2):(max(which(!is.na(meanH[,32])))+2),(min(which(!is.na(meanH[32,])))-2):(max(which(!is.na(meanH[32,])))+2)]
  
  
  return(list(avg_results=avg_results,meanH=meanH,sd_results=sd_results,ci_results=ci_results))
}


plotmeancell <- function(meanH,meanpeaks,avg_results,ci_results,folder) {
  png(paste(folder,'results/','avgresults.png',sep=''),width=15,height=5,units="in",res=1200)
  par(mfrow=c(1,3))
  persp(1:dim(meanH)[1]*2*pixelsize,1:dim(meanH)[2]*2*pixelsize,meanH, theta = 30, phi = 20, scale=F,border="red", col='white',xlab='x',ylab='y',zlab='Height',box=F)
  title(main='3D Thickness', cex.main = 2)
  
  plot(avg_results[,'D'],avg_results[,'height'],xlab='Distance from center (µm)',ylab='Thickness (µm)',type='b',ylim = c(0,3),cex.axis=1.5,cex.lab=1.5)
  arrows(avg_results[,'D'], avg_results[,'height']-ci_results[,'height'], avg_results[,'D'], avg_results[,'height']+ci_results[,'height'], length=0.02, angle=90, code=3)
  abline(v=meanpeaks[4]*pixelsize)
  title(main='Angular Average of Cell Thickness', cex.main = 2)
  
  
  plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '',xlim=c(0,10),ylim=c(0,12))
  title(main='Cell Characteristics', cex.main = 2)
  text(c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5), c(11,10,9,8,7,6,5,4,3,2,1),  
       c(paste(c('Radius =',round(avg_results[1,'radius'],digits=2),
                 '[',round(avg_results[1,'radius']-ci_results[1,'radius'],digits=2),'-',round(avg_results[1,'radius']+ci_results[1,'radius'],digits=2),']',
                 'µm'),collapse=" "),
         paste(c('Area =',round(avg_results[1,'area'],digits=1),
                 '[',round(avg_results[1,'area']-ci_results[1,'area'],digits=2),'-',round(avg_results[1,'area']+ci_results[1,'area'],digits=2),']',
                 'µm2'),collapse=" "),
         paste(c('Volume =',round(avg_results[1,'volume'],digits=1),
                 '[',round(avg_results[1,'volume']-ci_results[1,'volume'],digits=2),'-',round(avg_results[1,'volume']+ci_results[1,'volume'],digits=2),']',
                 'µm3'),collapse=" "),
         paste(c('Sphericity =',round(avg_results[1,'sphericity'],digits=2),
                 '[',round(avg_results[1,'sphericity']-ci_results[1,'sphericity'],digits=2),'-',round(avg_results[1,'sphericity']+ci_results[1,'sphericity'],digits=2),']',
                 ' '),collapse=" "),
         paste(c('T0 =',round(avg_results[1,'height'],digits=2),
                 '[',round(avg_results[1,'height']-ci_results[1,'height'],digits=2),'-',round(avg_results[1,'height']+ci_results[1,'height'],digits=2),']',
                 'µm'),collapse=" "),
         paste(c('TMAX =',round(avg_results[1,'HMAX'],digits=2),
                 '[',round(avg_results[1,'HMAX']-ci_results[1,'HMAX'],digits=2),'-',round(avg_results[1,'HMAX']+ci_results[1,'HMAX'],digits=2),']',
                 'µm'),collapse=" "),
         paste(c('DTMAX =',round(avg_results[1,'D2'],digits=2),
                 '[',round(avg_results[1,'D2']-ci_results[1,'D2'],digits=2),'-',round(avg_results[1,'D2']+ci_results[1,'D2'],digits=2),']',
                 'µm'),collapse=" "),
         paste(c('SMAX =',round(avg_results[1,'SMAX'],digits=2),
                 '[',round(avg_results[1,'SMAX']-ci_results[1,'SMAX'],digits=2),'-',round(avg_results[1,'SMAX']+ci_results[1,'SMAX'],digits=2),']',
                 ' '),collapse=" "),
         paste(c('DSMAX =',round(avg_results[1,'D1'],digits=2),
                 '[',round(avg_results[1,'D1']-ci_results[1,'D1'],digits=2),'-',round(avg_results[1,'D1']+ci_results[1,'D1'],digits=2),']',
                 'µm'),collapse=" "),
         paste(c('CMAX =',round(avg_results[1,'Concavity'],digits=2),
                 '[',round(avg_results[1,'Concavity']-ci_results[1,'Concavity'],digits=2),'-',round(avg_results[1,'Concavity']+ci_results[1,'Concavity'],digits=2),']',
                 '/µm'),collapse=" "),
         paste(c('<u> =',round(avg_results[1,'<u>'],digits=2),
                 '[',round((avg_results[1,'<u>']-ci_results[1,'<u>']),digits=2),'-',round((avg_results[1,'<u>']+ci_results[1,'<u>']),digits=2),']',
                 'nm'),collapse=" ")),
       cex=1.5, pos=4)
  dev.off() 
  
}


