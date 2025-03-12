###################################################################################################
##  All Code was Written by Yan Liu
## 
## Last updated: 2025.3.20
###################################################################################################

rm(list = ls())
library(readr)
library(dplyr)
library(mgcv)
library(ggplot2)
library(grid)
library(pscl)
library(glmnet)
library(fitdistrplus)
library(boot)
library(data.table)
library(cowplot)
library(car)

alpha=66.14;lamda=0.39;
lp <- qgamma(0.025,shape=alpha,rate = lamda)#131.1987
up <- qgamma(0.975,shape=alpha,rate = lamda)#212.8327

sdatzh <- read_csv("D:/Rstudio_data/R_GZ_zeroinflated/data_sensitivity_analysis06_15.csv")
datzh <- sdatzh[,-1]
data <- read_csv("D:/Rstudio_data/R_GZ_zeroinflated/P1wk.csv")%>% #daily
  dplyr::mutate(OnsetDate = as.Date(date), Year = year(OnsetDate), Month = month(OnsetDate), 
         Day = yday(OnsetDate)) %>%
  dplyr::mutate(IndexR = ifelse(P1wk>=lp & P1wk<=up,1,0)+ifelse(P1wk>up,-1,0),
                IndexR1 = ifelse(P1wk>=lp-50 & P1wk<=up-50,1,0)+ifelse(P1wk>up-50,-1,0),
                IndexR2 = ifelse(P1wk>=lp-100 & P1wk<=up-100,1,0)+ifelse(P1wk>up-100,-1,0),
                IndexR3 = ifelse(P1wk>=lp+50 & P1wk<=up+50,1,0)+ifelse(P1wk>up+50,-1,0),
                IndexR4 = ifelse(P1wk>=lp+100 & P1wk<=up+100,1,0)+ifelse(P1wk>up+100,-1,0),
                IndexR5 = ifelse(P1wk>=lp+150 & P1wk<=362,1,0)+ifelse(P1wk>up+150,-1,0),
                
                )

optimal_range <- data.frame(name=c('up or down 0','dowm 50','dowm 100','up 50','up 100','up 150'),
                            lp=c(lp,lp-50,lp-100,lp+50,lp+100,lp+150),
                            up=c(up,up-50,up-100,up+50,up+100,up+150))  

dataIndexR <- data %>%
  dplyr::group_by(Year,Month) %>%
  dplyr:: summarise(IndexR=sum(IndexR),IndexR1=sum(IndexR1),IndexR2=sum(IndexR2),
                    IndexR3=sum(IndexR3),IndexR4=sum(IndexR4),IndexR5=sum(IndexR5))
 
  

dataIndexR4 <- data.frame(matrix(NA,117,8))
colnames(dataIndexR4) <- colnames(dataIndexR)
for (i in 1:117) {
  
  dataIndexR4[i,] <- apply(dataIndexR[i:(i+3),],2,mean)#1,2,3,4  ->4;   2,3,4,5  ->5
  
}
lagdataIndexR4 <- dplyr::lag(dataIndexR4,1)
delay4 <- 2
datIndexR <- scale(lagdataIndexR4[-c(1:delay4),3:8]) 

datzh <- cbind(datzh,datIndexR)


# Data preparation
LocalCase <- datzh$`ln(LocalCase)`
family <- "gaussian"
alpha0 <- 0.4

datazl_IndexR <- cbind(datzh[,c(2,3,9:12)],datzh$IndexR)  # IndexR  
datazl_IndexR1 <- cbind(datzh[,c(2,3,9:12)],datzh$IndexR1)  # IndexR1  
datazl_IndexR2 <- cbind(datzh[,c(2,3,9:12)],datzh$IndexR2)  # IndexR2  
datazl_IndexR3 <- cbind(datzh[,c(2,3,9:12)],datzh$IndexR3)  # IndexR3
datazl_IndexR4 <- cbind(datzh[,c(2,3,9:12)],datzh$IndexR4)  # IndexR4
datazl_IndexR5 <- cbind(datzh[,c(2,3,9:12)],datzh$IndexR5)  # IndexR5

model_IndexR <- Hurdle_Regularized_Regression(datazl=datazl_IndexR,LocalCase,family,alpha0)
model_IndexR1 <- Hurdle_Regularized_Regression(datazl=datazl_IndexR1,LocalCase,family,alpha0)
model_IndexR2 <- Hurdle_Regularized_Regression(datazl=datazl_IndexR2,LocalCase,family,alpha0)
model_IndexR3 <- Hurdle_Regularized_Regression(datazl=datazl_IndexR3,LocalCase,family,alpha0)
model_IndexR4 <- Hurdle_Regularized_Regression(datazl=datazl_IndexR4,LocalCase,family,alpha0)
model_IndexR5 <- Hurdle_Regularized_Regression(datazl=datazl_IndexR5,LocalCase,family,alpha0)

RMSE_pred <- data.table(Model=c('Base model+IndexR','Base model+IndexR1','Base model+IndexR2',
                                'Base model+IndexR3','Base model+IndexR4','Base model+IndexR5'),
                        RMSE=c(model_IndexR$rmse_pred,model_IndexR1$rmse_pred,model_IndexR2$rmse_pred,
                               model_IndexR3$rmse_pred,model_IndexR4$rmse_pred,model_IndexR5$rmse_pred))
RMSE <- data.table(Model=c('Base model+IndexR','Base model+IndexR1','Base model+IndexR2',
                                'Base model+IndexR3','Base model+IndexR4','Base model+IndexR5'),
                        RMSE=c(model_IndexR$rmse,model_IndexR1$rmse,model_IndexR2$rmse,
                               model_IndexR3$rmse,model_IndexR4$rmse,model_IndexR5$rmse))


## figure
Model <- list(model_IndexR,model_IndexR1,model_IndexR2,model_IndexR3,model_IndexR4,model_IndexR5)
Model_name=c('(a) Basic model+IndexR','(b) Basic model+IndexR1',
             '(c) Basic model+IndexR2','(d) Basic model+IndexR3',
             '(e) Basic model+IndexR4','(f) Basic model+IndexR5')

# 
for (i in 1:6) {
  
  m <-  Model[[i]]
  
  rmse_text_grob <- textGrob(label = bquote(
    atop( RMSE[2006-2015] == .(round(RMSE$RMSE[i], digits = 2)),
          RMSE[2014-2015] == .(round(RMSE_pred$RMSE[i], digits = 2))
    )), 
    x = unit(1, "npc"), y = unit(0.2, "npc"), 
    hjust = 1, vjust = 0, 
    gp = gpar(fontsize = 14, fontfamily = "serif"))
  
  
  p <- ggplot(datzh, aes(x = 1:115, y = LocalCase)) +
    geom_ribbon(aes(x = 1:115, ymin = m$predicted_df$Lower, 
                    ymax = m$predicted_df$Upper, fill = "95% CI"), alpha = 0.7) +  
    geom_line(aes(x = 1:115, y = m$predicted_df$s1, color = "Predicted"), lwd = 1) +  
    geom_point(aes(color = "Observed"), alpha = 1, size = 2) +  
    geom_vline(xintercept = 91, linetype = "dashed", color = "black", linewidth = 1) +  
    annotate("text", x = c(45, 110), y = c(max(datzh$`ln(LocalCase)`)+0.5, max(datzh$`ln(LocalCase)`)+0.5 ),
             label = c("2006-2013\nTraining Set", "2014-2015\nTest Set"), color = "black",
             size = 4.5, hjust = 0.5, family = "serif") +  
    labs(title =Model_name[i], x = "Month", y = "ln(LocalCase)") +
    theme_minimal() +
    theme(
      text = element_text(family = "serif"),  
      plot.title = element_text(size = 18, face = "bold"),  
      axis.title.y = element_text(size = 16),  
      axis.title.x = element_text(size = 16),  
      axis.text.x = element_text(size = 14),  
      axis.text.y = element_text(size = 14),  
      legend.title = element_blank(),  
      legend.text = element_text(size = 14),  
      legend.position = "right"  
    ) +
    scale_color_manual(values = c("Predicted" = "red", "Observed" = "blue")) +  
    scale_fill_manual(values = c("95% CI" = "grey"))+  
    ylim(c(-0.5, 12.5))  
  
  final_plot <- plot_grid(p, rmse_text_grob, ncol=2,rel_widths = c(1, 0.05))  # Allocate 30% width to RMSE text
  print(final_plot)
  
  #  }
}
# 830 350

