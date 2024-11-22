###################################################################################################
##  All Code was Written by Yan Liu
## 
## Last updated: 2024.11.22
###################################################################################################

rm(list = ls())
library(readr)
library(circlize)

data <- read_csv(".../RT_2014.csv")
T1wk <- data$T1wk
P1wk <- data$P1wk
T <- data$T
R <- data$R
############ T
T1=T1wk;T2=T1wk;T3=T1wk
T1[T1>35|T1<25]=NA;T2[T2>25|T2<15]=NA;T3[T3>15]=NA
ddT=cbind(T1,T2,T3,T)
dT=ddT
#Create data
data = data.frame(
  factor =c( rep("January", 31), rep("February", 28), rep("March", 31),
             rep("April", 30),rep("May", 31),rep("June", 30)
             ,rep("July", 31),rep("August", 31),rep("September", 30),
             rep("October", 31),rep("November", 30),rep("December", 31)),#数据分组命名,相同类的必须放一起，并且不同类的样本量必须一样，否则画图时圆形不能均分
  x = 1:365, 
  y1 = dT[,1],
  y2 = dT[,2],
  y3 = dT[,3],
  z = dT[,4]
)
data$factor <- factor(data$factor, levels = c("January", "February", "March", "April", "May", "June", 
                                              "July", "August", "September", "October", "November", "December"))

circos.clear()
#Initialize the plot.
par(mar =  c(0.5, 2, 2, 0.1)*3 ,family = "serif") 
par(xpd = TRUE) 
circos.par(points.overflow.warning = FALSE,"gap.degree"=5 )
circos.initialize(factors = data$factor, x = data$x )

dT[is.na(dT)]=0

circos.trackPlotRegion(factors = data$factor,x=data$x,ylim = c(0,1),track.height = 0.05,
                       bg.border = NA)
circos.text(15, 4.5, "January", sector.index="January",facing = "bending.inside", cex = 1.3)
circos.text(45, 4.5, "February", sector.index="February",facing = "bending.inside", cex = 1.3)
circos.text(75, 4.5, "March", sector.index="March",facing = "bending.inside", cex = 1.3)
circos.text(105, 4.3, "April", sector.index="April",facing = "bending.inside", cex = 1.3)
circos.text(137, 4.5, "May", sector.index="May",facing = "bending.inside", cex = 1.3)
circos.text(167, 4.5, "June", sector.index="June",facing = "bending.inside", cex = 1.3)
circos.text(199, 4.5, "July", sector.index="July",facing = "bending.inside", cex = 1.3)
circos.text(229, 4.5, "August", sector.index="August",facing = "bending.inside", cex = 1.3)
circos.text(260, 4.5, "September", sector.index="September",facing = "bending.inside", cex = 1.3)
circos.text(290, 4.5, "October", sector.index="October",facing = "bending.inside", cex = 1.3)
circos.text(320, 4.5, "November", sector.index="November",facing = "bending.inside", cex = 1.3)
circos.text(350, 4.5, "December", sector.index="December",facing = "bending.inside", cex = 1.3)
# Build the regions of track #1 

circos.trackPlotRegion(factors = data$factor, y=dT[,1],ylim = c(25,35), panel.fun = function(x, y) {
  circos.axis(labels.cex=0.8, labels.font=2, lwd=0.8)
},track.height = 0.15)
circos.yaxis( at=c(26,34),labels=c('26','34'),labels.cex=0.8,tick.length=0.5,side = "left",sector.index = "October",labels.col=rgb(0.1,0.5,0.8,1),labels.font=2)

# --> Add a scatterplot on it:
circos.trackPoints(data$factor, data$x, data$y1, col = rgb(0.1,0.5,0.8,0.3), pch=20,cex=2)


# Build the regions of track #2:
circlize::circos.trackPlotRegion(factors = data$factor, y=dT[,2],ylim = c(15,25), panel.fun = function(x, y) {
  circos.axis(labels=FALSE, major.tick=FALSE)
},track.height = 0.15)
circos.yaxis( at=c(16,24),labels=c('16','24'),labels.cex=0.8,tick.length=0.5,side = "left",sector.index = "October",labels.col=rgb(0.9,0.5,0.8,1),labels.font=2)
# --> Add a scatterplot on it
circos.trackPoints(data$factor, data$x, data$y2, col = rgb(0.9,0.5,0.8,0.3), pch=20, cex=2)

# Build the regions of track #3:
circlize::circos.trackPlotRegion(factors = data$factor, y=dT[,3],ylim = c(0,15), panel.fun = function(x, y) {
  circos.axis(labels=FALSE, major.tick=FALSE)
},track.height = 0.15)
circos.yaxis( at=c(1,14),labels=c('1','14'),labels.cex=0.8,tick.length=0.5,side = "left",sector.index = "October",labels.col=rgb(0.9,0.5,0.3,1),labels.font=2)
# --> Add a scatterplot on it
circos.trackPoints(data$factor, data$x, data$y3, col = rgb(0.9,0.5,0.3,0.3), pch=20, cex=2)

# Add track #4 --> don't forget you can custom the height of tracks!
circos.par("track.height" = 0.35)
circos.trackPlotRegion(factors = data$factor, y=dT[,4], panel.fun = function(x, y) {
  circos.axis(labels=FALSE, major.tick=FALSE)
})
circos.trackLines(data$factor, data$x, data$z, col = rgb(0.9,0.5,0.1,0.3), pch="-", cex=2, type="h",lwd=2)
circos.yaxis(labels.cex=0.8,tick.length=0.8,side = "left",sector.index = "April",labels.col=rgb(0.1,0.5,0.8,1),labels.font=2)
# and continue as long as needed!

title("(a) Temperature index",mgp=c(1, 1, 0),cex.main=1.5)#

legend("topleft",pch = c(20,20,20,NA),lty=c(0,0,0,1),lwd=c(2,2,2,2),
       pt.cex=c(2,2,2,2),col = c(rgb(0.1,0.5,0.8,0.3),rgb(0.9,0.5,0.8,0.3),
                                 rgb(0.9,0.5,0.3,0.3),rgb(0.9,0.5,0.1,0.3)), 
       legend = c(expression(italic(T[w])~'(25~35)'),expression(italic(T[w])~'(15~25)')
                  ,expression(italic(T[w])<15),expression(italic(T[w])~'/°C')),
       inset = c(0.03, -0.05))
                  
#846, 713     

############ P
P1=P1wk;P2=P1wk;P3=P1wk
P1[P1<212.8]=NA;P2[P2>212.8|P2<131.2]=NA;P3[P3>131.2]=NA
ddP=cbind(P1,P2,P3,R)
dP=ddP
#Create data
data = data.frame(
  factor =c( rep("January", 31), rep("February", 28), rep("March", 31),
             rep("April", 30),rep("May", 31),rep("June", 30)
             ,rep("July", 31),rep("August", 31),rep("September", 30),
             rep("October", 31),rep("November", 30),rep("December", 31)),
  x = 1:365,#c(336:368,1:335), 
  y1 = dP[,1],
  y2 = dP[,2],
  y3 = dP[,3],
  z = dP[,4]
)
data$factor <- factor(data$factor, levels = c("January", "February", "March", "April", "May", "June", 
                                              "July", "August", "September", "October", "November", "December"))

circos.clear()##
#Initialize the plot.
par(mar =  c(0.5, 0.1, 2, 2)*3 ,family = "serif") #
par(xpd = TRUE) 
circos.par(points.overflow.warning = FALSE,"gap.degree"=5 )#
circos.initialize(factors = data$factor, x = data$x )##

dP[is.na(dP)]=0

circos.trackPlotRegion(factors = data$factor,x=data$x,ylim = c(0,1),track.height = 0.05,
                       bg.border = NA)
circos.text(15, 4.5, "January", sector.index="January",facing = "bending.inside", cex = 1.3)#
circos.text(45, 4.5, "February", sector.index="February",facing = "bending.inside", cex = 1.3)
circos.text(75, 4.5, "March", sector.index="March",facing = "bending.inside", cex = 1.3)
circos.text(105, 4.3, "April", sector.index="April",facing = "bending.inside", cex = 1.3)
circos.text(137, 4.5, "May", sector.index="May",facing = "bending.inside", cex = 1.3)
circos.text(167, 4.5, "June", sector.index="June",facing = "bending.inside", cex = 1.3)
circos.text(199, 4.5, "July", sector.index="July",facing = "bending.inside", cex = 1.3)
circos.text(229, 4.5, "August", sector.index="August",facing = "bending.inside", cex = 1.3)
circos.text(260, 4.5, "September", sector.index="September",facing = "bending.inside", cex = 1.3)
circos.text(290, 4.5, "October", sector.index="October",facing = "bending.inside", cex = 1.3)
circos.text(320, 4.5, "November", sector.index="November",facing = "bending.inside", cex = 1.3)
circos.text(350, 4.5, "December", sector.index="December",facing = "bending.inside", cex = 1.3)
# Build the regions of track #1 

circos.trackPlotRegion(factors = data$factor, y=dP[,1],ylim = c(212.8,330), panel.fun = function(x, y) {
  circos.axis(labels.cex=0.8, labels.font=2, lwd=0.8)#
},track.height = 0.15)##
circos.yaxis( at=c(220,310),labels=c('220','310'),labels.cex=0.8,tick.length=0.5,side = "left",sector.index = "October",labels.col=rgb(0.1,0.5,0.8,1),labels.font=2)#指标"summer"画y轴，at刻度位置

# --> Add a scatterplot on it:
circos.trackPoints(data$factor, data$x, data$y1, col = rgb(0.1,0.5,0.8,0.3), pch=20,cex=2)


# Build the regions of track #2:
circlize::circos.trackPlotRegion(factors = data$factor, y=dP[,2],ylim = c(131.2,212.8), panel.fun = function(x, y) {
  circos.axis(labels=FALSE, major.tick=FALSE)
},track.height = 0.15)
circos.yaxis( at=c(140,200),labels=c('140','200'),labels.cex=0.8,tick.length=0.5,side = "left",sector.index = "October",labels.col=rgb(0.9,0.5,0.8,1),labels.font=2)
# --> Add a scatterplot on it
circos.trackPoints(data$factor, data$x, data$y2, col = rgb(0.9,0.5,0.8,0.3), pch=20, cex=2)

# Build the regions of track #3:
circlize::circos.trackPlotRegion(factors = data$factor, y=dP[,3],ylim = c(0,131.2), panel.fun = function(x, y) {
  circos.axis(labels=FALSE, major.tick=FALSE)
},track.height = 0.15)
circos.yaxis( at=c(40,120),labels=c('40','120'),labels.cex=0.8,tick.length=0.5,side = "left",sector.index = "October",labels.col=rgb(0.9,0.5,0.3,1),labels.font=2)
# --> Add a scatterplot on it
circos.trackPoints(data$factor, data$x, data$y3, col = rgb(0.9,0.5,0.3,0.3), pch=20, cex=2)

# Add track #4 --> don't forget you can custom the height of tracks!
circos.par("track.height" = 0.35)
circos.trackPlotRegion(factors = data$factor, y=dP[,4], panel.fun = function(x, y) {
  circos.axis(labels=FALSE, major.tick=FALSE)
})
circos.trackLines(data$factor, data$x, data$z, col = rgb(0.9,0.5,0.1,0.3), pch="-", cex=2, type="h",lwd=2)
circos.yaxis(labels.cex=0.8,tick.length=0.8,side = "left",sector.index = "April",labels.col=rgb(0.1,0.5,0.8,1),labels.font=2)
# and continue as long as needed!

title("(b) Rainfall index",mgp=c(1, 1, 0),cex.main=1.5)#

legend("topright",pch = c(20,20,20,NA),lty=c(0,0,0,1),lwd=c(2,2,2,2),
       pt.cex=c(2,2,2,2),col = c(rgb(0.1,0.5,0.8,0.3),rgb(0.9,0.5,0.8,0.3),
                                 rgb(0.9,0.5,0.3,0.3),rgb(0.9,0.5,0.1,0.3)),
       legend = c(expression(italic(R[w])~'>212.8'),expression(italic(R[w])~'(131.2~212.8)')
                  ,expression(italic(R[w])~'<131.2'),expression(italic(R[w])~'/mm')),
       inset = c(-0.05, -0.05)) # 
#846, 713

