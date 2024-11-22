###################################################################################################
##  All Code was Written by Yan Liu
## 
## Last updated: 2024.11.22
###################################################################################################

rm(list = ls())
library(tidyverse)
library(ggplot2)
library(cowplot)
library(lubridate)
library(ggsci)
library(dlnm)
library(tsModel)
library(data.table)
library(splines)
library(readxl)
library(MASS)
library(mgcv)
library(RColorBrewer)
library(pROC)
library(caret)

load(".../work_history.RData")

alpha=66.14;lamda=0.39;
lp <- qgamma(0.025,shape=alpha,rate = lamda)#131.1987
up <- qgamma(0.975,shape=alpha,rate = lamda)#212.8327

get_season <- function(month) {
  if (month %in% c(12, 1, 2)) {
    return("winter")
  } else if (month %in% c(3, 4, 5)) {
    return("spring")
  } else if (month %in% c(6, 7, 8)) {
    return("summer")
  } else {
    return("autumn")
  }
}
GZ2006_2015.daily <- read_csv(".../DLNM2006_2015.csv") %>%
  mutate(OnsetDate = as.Date(date), Year = year(OnsetDate), Month = month(OnsetDate), dow = weekdays(OnsetDate), 
         DOY = yday(OnsetDate)) %>%
  mutate(Season=sapply(Month, get_season),Subset = ifelse( Month %in% c(7:12), 1, 0))%>%  # data used in the analysis  ifelse(test, yes, no):根据给定test值的真假返回yes 或no 
  mutate(Outbreak=as.numeric(LocalCase>=3),Outbreak_1=tsModel::Lag(Outbreak,1)) %>%
  mutate(Import_1=tsModel::Lag(Import,1))
# subset - used for fitting model
GZ2006_2015.daily.subset <- GZ2006_2015.daily %>%
  filter(Subset == 1) %>%
  mutate(Time = 1:n())

lag.T <- 30*3 # 3+1
lag.R <- 30*6 #IndexR:4+1
lag.Import <- 30*2 #2+0
lag_DaynT <- tsModel::Lag(GZ2006_2015.daily$DaynT, k = 1:lag.T)[GZ2006_2015.daily$Subset == 1, ]

lag_P1wk <- tsModel::Lag(GZ2006_2015.daily$P1wk, k = 1:lag.R)[GZ2006_2015.daily$Subset == 1, ]
lag_Import <- tsModel::Lag(GZ2006_2015.daily$Import, k = 1:lag.Import)[GZ2006_2015.daily$Subset == 1, ]


n.knots=2

basis_P1wk <- crossbasis(lag_P1wk, 
                         argvar = list(fun = "ns",
                                       knots= equalknots(GZ2006_2015.daily.subset$P1wk, n.knots)), #fun = " "用于选择不同类型的基参数  自由度df定义了基的维度（其列的数量，基本上是转换变量的数量）“ns”自然样条，degree恒为3    对于“bs”B样条，degree默认为3，即3次B样条，df=knots+degree+0(无截距)+1(有截距)
                         arglag=list(fun = "ns", knots = n.knots)) 

formula <- Outbreak~Season +lag_DaynT+basis_P1wk+lag_Import+Outbreak_1  #AIC:621.1692   45  (7:12,3,6,2)       

set.seed(123)
model <- gam(formula,data = GZ2006_2015.daily.subset,family = binomial)
model$aic
plot(as.numeric(model$fitted.values>0.5),type='l',ylim=c(0,1))+points(GZ2006_2015.daily.subset$Outbreak)
sum(abs(as.numeric(model$fitted.values>0.5)-GZ2006_2015.daily.subset$Outbreak)>0)

#Calculate the area of the AUC, ROC curve 
roc_obj <- roc(GZ2006_2015.daily.subset$Outbreak, as.numeric(model$fitted.values>0.5))
roc_obj
#confusion matrix
confusionMatrix(as.factor(as.numeric(model$fitted.values>0.5)), as.factor(GZ2006_2015.daily.subset$Outbreak))



# get RR for P1wk
predP1wk <- crosspred(basis_P1wk, model,cen=0)
plot(predP1wk)

which(predP1wk$matRRfit==max(predP1wk$matRRfit)) #[1] 3592
predP1wk$matRRlow[3592] #[1] 1.569246
predP1wk$matRRhigh[3592] #[1] 2.391125
ss <- which(abs(predP1wk$matRRfit-1.5) <0.001)
predP1wk$matRRlow[ss]
predP1wk$matRRhigh[ss]

y <- predP1wk$predvar
x <- seq(1, lag.R)
z <- predP1wk$matRRfit

pal <- rev(brewer.pal(11, "PRGn"))
levels <- pretty(c(0, z, 2), 20)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))

par(mar = c(5, 6, 4, 5) + 0.1)
filled.contour(y, x, z,
               ylab = "Lag/day", xlab = "Total weekly rainfall "~italic(R[w])~"/mm", main='(a)',cex.main=2,cex.lab = 1.5,
               col = cols,levels = levels,family = "serif",
               plot.axes = { axis(2,cex.axis = 1.5, family = "serif") 
                 axis(1,cex.axis = 1.5,family = "serif")
                 abline(v=lp , lwd=2, col="black", lty = 2)
                 abline(v=up , lwd=2, col="black", lty = 2)},
                 key.axes = axis(4, cex.axis = 1.5) #  
               )
# 850,650


# define colours
col11 <-  '#800080' 
tcol11 <- do.call(rgb, c(as.list(col2rgb(col11)), alpha = 255/4, max = 255))
col12 <-  '#008000' 
tcol12 <- do.call(rgb, c(as.list(col2rgb(col12)), alpha = 255/4, max = 255))

#
par(mar = c(5, 4, 5, 5) + 0.1,family = "serif",cex=1.5)
plot(predP1wk, ptype = "slices", lag = 80,col = col11, type = "l", lwd = 2, 
     xlab = "", ylab = "", 
     ylim = c(0, 2.5),xlim=c(0,464.2), frame.plot = T, axes = F)
box()
axis(1, col ='#800080', col.axis ='#800080')# brewer.pal(11, "PRGn")[2]
axis(2, col = '#800080', col.axis = '#800080')
polygon(c(predP1wk$predvar, rev(predP1wk$predvar)), 
        c(predP1wk$matRRlow[,80], rev(predP1wk$matRRhigh[,80])), 
        col = tcol11, border = tcol11) 

# 
par(new = TRUE)
plot(predP1wk, ptype = "slices", var = 350, col = col12, type = "l", lwd = 2, 
     axes = F, xlab='',ylab='', ylim = c(0, 2.5),xlim=c(0,179), frame.plot = T)


axis(3, col ='#008000', col.axis = '#008000')
axis(4, col = '#008000', col.axis = '#008000')
polygon(c(seq(0,179), rev(seq(0,179))), 
        c(predP1wk$matRRlow[which(predP1wk$predvar==350),], 
          rev(predP1wk$matRRhigh[which(predP1wk$predvar==350),])), 
        col = tcol12, border = tcol12)

title ('(b) ', line = 3.5) 

mtext("Total weekly rainfall "~italic(R[w])~"/mm", side = 1, line = 3, col = "#800080", cex = 1.5)  # x轴标签
mtext("Relative risk", side = 2, line = 3, col = "#800080", cex = 1.5)       # y轴标签
mtext("Lag/day", side = 3, line = 2, col = "#008000", cex = 1.5)   # 第3轴的标签
mtext("Relative risk", side = 4, line = 2, col = "#008000", cex = 1.5)  # 第4轴的标签
legend("topright", legend = c("Lag = 80", italic(R[w])~'=350mm'), 
       col = c(col11, col12), lwd = 2, lty = 1, 
       text.col = c("#800080", "#008000"), 
       bty = "n", cex = 1)


