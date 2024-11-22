###################################################################################################
##  All Code was Written by Yan Liu
## 
## Last updated: 2024.11.22
###################################################################################################

load(".../work_history_of_ data_preprocessing.RData")
####################  main code  #############################
library(pscl)
library(glmnet)
library(fitdistrplus)
library(boot)
library(ggplot2)
library(grid)
library(data.table)
library(cowplot)
library(car)
## firstly,run these codes:  data_preprocessing.R ;  Hurdle_Regularized_Regression.R ;  Simulation_HRR.R

################# Test multicollinearity between indices   ####################
eigen_values <- eigen(cor(datzh[, c('ln(LocalCase)1_1',	'Import2_0',	'IndexR4_1',	'DaynR4_2',
                                    'SumR4_2', 'CvR4_2','NumberR4_2',	'IndexT3_1',	'DaynT3_1',	'MeanT3_1',	'MaxT3_1')]))$values
condition_number <- max(eigen_values) / eigen_values
print(condition_number)#


#################### Non-zero partial data distribution selection
##
data00 <- read_csv(".../month_index06_15.csv")
y00 <- data00$LocalCase[1:96] #2006-2015
# 
nonzero_values <- y00[y00 > 0]
hist(nonzero_values, main = "Histogram of Non-Zero Values", xlab = "Values", breaks = 20)

#                           log          original
summary(nonzero_values)#mean:2.844      72.72
sd(nonzero_values)^2 # var: 3.297       23203.63

fitnb <- fitdist(nonzero_values,'nbinom')
plot(fitnb)

fitpo <- fitdist(nonzero_values,'pois')
fitln <- fitdist(nonzero_values,'lnorm')
fitweb <- fitdist(nonzero_values,'weibull')
fitgam <- fitdist(nonzero_values,'gamma')
gofstat(list(fitnb, fitpo, fitln,fitweb,fitgam),
        fitnames=c("nbinom", "pois", "lognormal",'weibull','gamma'))

windows(width = 8, height = 6)
# 
empirical_cdf <- ecdf(nonzero_values)
par(family = "serif")
# 
plot(empirical_cdf, main = "Empirical and Theoretical CDFs", xlab = "Non-zero local cases", ylab = "CDF", 
     lwd = 2, col = adjustcolor("black", alpha.f = 0.8), cex.lab = 1.5, cex.main = 1.8)

# 
curve(pnbinom(x, size = fitnb$estimate['size'], mu = fitnb$estimate['mu']), 
      col = adjustcolor("green", alpha.f = 1), lwd = 2, add = TRUE, lty = 1)
curve(plnorm(x, meanlog = fitln$estimate['meanlog'], sdlog = fitln$estimate['sdlog']), 
      col = "red", lwd = 3, add = TRUE, lty = 5) # 
curve(pweibull(x, shape = fitweb$estimate['shape'], scale = fitweb$estimate['scale']), 
      col = adjustcolor("purple", alpha.f = 1), lwd = 2, add = TRUE, lty = 4)
curve(pgamma(x, shape = fitgam$estimate['shape'], rate = fitgam$estimate['rate']), 
      col = adjustcolor("orange", alpha.f = 1), lwd = 2, add = TRUE, lty = 3)


# 
legend_pos <- "bottomright"
legend(legend_pos, legend = c("Empirical", "Negative Binomial: AIC=317.3683",
                              "Log-normal: AIC=304.3588",
                              "Weibull: AIC=310.4914", "Gamma: AIC=315.4593"), 
       col = c("black", "green", "red", "purple", "orange"), 
       lty = c(1, 1, 5, 4, 3), lwd = c(2, 2, 2, 2, 2), 
       pch = c(NA, NA, NA, NA, NA),  # 
       pt.cex = 1, cex = 1.5, bty = "n", inset = 0.05)

# 
legend_coords <- legend(legend_pos, legend = c("Empirical"), plot = FALSE)
points(legend_coords$text$x[1] -575, legend_coords$text$y[1]+0.40, pch = 16, col = "black", cex = 1)

dev.off()


# Data preparation
LocalCase <- datzh$`ln(LocalCase)`
family <- "gaussian"
alpha0 <- seq(0,1,by=0.1);
allRMSE_fit=data.table(Model=c('Base model','Base model+IndexR','Base model+DaynR',
                           'Base model+SumR','Base model+CvR','Base model+NumberR',
                           'Base model+IndexR+DaynR','Base model+IndexR+DaynR+SumR',
                           'Base model+IndexR+DaynR+SumR+CvR','Base model+NumberR+DaynR',
                           'Base model+NumberR+DaynR+SumR', 'Base model+NumberR+DaynR+SumR+CvR'
                           ),
                   RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA)                                         
allRMSE_pred=data.table(Model=c(c('Base model','Base model+IndexR','Base model+DaynR',
                                  'Base model+SumR','Base model+CvR','Base model+NumberR',
                                  'Base model+IndexR+DaynR','Base model+IndexR+DaynR+SumR',
                                  'Base model+IndexR+DaynR+SumR+CvR','Base model+NumberR+DaynR',
                                  'Base model+NumberR+DaynR+SumR', 'Base model+NumberR+DaynR+SumR+CvR'
                                  )),
                       RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA)                                         
allRMSE=data.table(Model=c(c('Base model','Base model+IndexR','Base model+DaynR',
                                  'Base model+SumR','Base model+CvR','Base model+NumberR',
                                  'Base model+IndexR+DaynR','Base model+IndexR+DaynR+SumR',
                                  'Base model+IndexR+DaynR+SumR+CvR','Base model+NumberR+DaynR',
                                  'Base model+NumberR+DaynR+SumR', 'Base model+NumberR+DaynR+SumR+CvR'
)),
RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA,RMSE=NA)                                         

datazl_no_R <- datzh[,-c(1,4,5,6,7,8)] # -R
datazl_IndexR <- datzh[,-c(1,5,6,7,8)]   # IndexR
datazl_SumR <- datzh[,-c(1,4,5,7,8)]   # SumR
datazl_DaynR <- datzh[,-c(1,4,6,7,8)]   # DaynR
datazl_CvR <- datzh[,-c(1,4,5,6,8)]   # CvR
datazl_NumberR <- datzh[,-c(1,4,5,6,7)]   # NumberR

datazl_IndexR_DaynR <- datzh[,-c(1,6,7,8)] # IndexR DaynR
datazl_IndexR_DaynR_SumR <- datzh[,-c(1,7,8)] # IndexR DaynR SumR
datazl_R1 <- datzh[,-c(1,8)]             # IndexR DaynR SumR CvR

datazl_NumberR_DaynR <- datzh[,-c(1,6,7,4)] # NumberR DaynR
datazl_NumberR_DaynR_SumR <- datzh[,-c(1,7,4)] # NumberR DaynR SumR
datazl_R2 <- datzh[,-c(1,4)] # NumberR DaynR SumR CvR

alpha <- 0.4
model_no_R <- Hurdle_Regularized_Regression(datazl=datazl_no_R,LocalCase,family,alpha)
model_IndexR <- Hurdle_Regularized_Regression(datazl=datazl_IndexR,LocalCase,family,alpha)
model_DaynR <- Hurdle_Regularized_Regression(datazl=datazl_DaynR,LocalCase,family,alpha)
model_SumR <- Hurdle_Regularized_Regression(datazl=datazl_SumR,LocalCase,family,alpha)
model_CvR <- Hurdle_Regularized_Regression(datazl=datazl_CvR,LocalCase,family,alpha)
model_NumberR <- Hurdle_Regularized_Regression(datazl=datazl_NumberR,LocalCase,family,alpha)

model_IndexR_DaynR <- Hurdle_Regularized_Regression(datazl=datazl_IndexR_DaynR,LocalCase,family,alpha)
model_IndexR_DaynR_SumR <- Hurdle_Regularized_Regression(datazl=datazl_IndexR_DaynR_SumR,LocalCase,family,alpha)
model_R1 <- Hurdle_Regularized_Regression(datazl=datazl_R1,LocalCase,family,alpha)
model_NumberR_DaynR <- Hurdle_Regularized_Regression(datazl=datazl_NumberR_DaynR,LocalCase,family,alpha)
model_NumberR_DaynR_SumR <- Hurdle_Regularized_Regression(datazl=datazl_NumberR_DaynR_SumR,LocalCase,family,alpha)
model_R2 <- Hurdle_Regularized_Regression(datazl=datazl_R2,LocalCase,family,alpha)


RMSE_fit <- data.table(Model=c('Base model','Base model+IndexR','Base model+DaynR',
                               'Base model+SumR','Base model+CvR','Base model+NumberR',
                               'Base model+IndexR+DaynR','Base model+IndexR+DaynR+SumR',
                               'Base model+IndexR+DaynR+SumR+CvR','Base model+NumberR+DaynR',
                               'Base model+NumberR+DaynR+SumR', 'Base model+NumberR+DaynR+SumR+CvR'),
                   RMSE=c(model_no_R$rmse_fit,model_IndexR$rmse_fit,model_DaynR$rmse_fit,
                          model_SumR$rmse_fit,model_CvR$rmse_fit,model_NumberR$rmse_fit,
                          
                          model_IndexR_DaynR$rmse_fit,
                          model_IndexR_DaynR_SumR$rmse_fit,model_R1$rmse_fit,
                          
                          model_NumberR_DaynR$rmse_fit,
                          model_NumberR_DaynR_SumR$rmse_fit,model_R2$rmse_fit))

RMSE_pred <- data.table(Model=c('Base model','Base model+IndexR','Base model+DaynR',
                                'Base model+SumR','Base model+CvR','Base model+NumberR',
                                'Base model+IndexR+DaynR','Base model+IndexR+DaynR+SumR',
                                'Base model+IndexR+DaynR+SumR+CvR','Base model+NumberR+DaynR',
                                'Base model+NumberR+DaynR+SumR', 'Base model+NumberR+DaynR+SumR+CvR'),
                       RMSE=c(model_no_R$rmse_pred,model_IndexR$rmse_pred,model_DaynR$rmse_pred,
                              model_SumR$rmse_pred,model_CvR$rmse_pred,model_NumberR$rmse_pred,
                              
                              model_IndexR_DaynR$rmse_pred,
                              model_IndexR_DaynR_SumR$rmse_pred,model_R1$rmse_pred,
                              
                              model_NumberR_DaynR$rmse_pred,
                              model_NumberR_DaynR_SumR$rmse_pred,model_R2$rmse_pred))

RMSE <- data.table(Model=c('Base model','Base model+IndexR','Base model+DaynR',
                           'Base model+SumR','Base model+CvR','Base model+NumberR',
                           'Base model+IndexR+DaynR','Base model+IndexR+DaynR+SumR',
                           'Base model+IndexR+DaynR+SumR+CvR','Base model+NumberR+DaynR',
                           'Base model+NumberR+DaynR+SumR', 'Base model+NumberR+DaynR+SumR+CvR'),
                   RMSE=c(model_no_R$rmse,model_IndexR$rmse,model_DaynR$rmse,
                          model_SumR$rmse,model_CvR$rmse,model_NumberR$rmse,
                          
                          model_IndexR_DaynR$rmse,
                          model_IndexR_DaynR_SumR$rmse,model_R1$rmse,
                          
                          model_NumberR_DaynR$rmse,
                          model_NumberR_DaynR_SumR$rmse,model_R2$rmse))

#################################################################################

#switch 2014,2015 rainfall 'IndexR4_1',	'DaynR4_2','SumR4_2', 'CvR4_2','NumberR4_2'，
R2014 <- data[97:108,5:9];R2015 <- data[109:120,5:9]
R2006_2015 <- rbind(data[1:96,5:9],R2015,R2014)
R4_2 <- matrix(NA,117,5)
for (i in 1:117) {
  R4_2[i,] <- apply(R2006_2015[i:(i+3),],2,mean)#1,2,3,4  ->4;   2,3,4,5  ->5
}
IndexR4_1 <- scale(dplyr::lag(R4_2[,1],1)[-c(1:delay4)])#'IndexR4_1'
R4_2 <- scale(dplyr::lag(R4_2[,-1],2)[-c(1:delay4),])#'DaynR4_2','SumR4_2', 'CvR4_2','NumberR4_2'

datasim_IndexR <- cbind(datzh[,2:3],IndexR4_1,datzh[,9:12])
final_predictions_sim_IndexR <-Simulation_HRR(datasim_IndexR,alpha,data = datazl_IndexR)


#switch 2014,2015 temperature 'IndexT3_1',	'DaynT3_1',	'MeanT3_1',	'MaxT3_1'
T2014 <- data[97:108,10:13];T2015 <- data[109:120,10:13]
T2006_2015 <- rbind(data[1:96,10:13],T2015,T2014)
T3_1 <- matrix(NA,118,4)
for (i in 1:118) {
  T3_1[i,] <- apply(T2006_2015[i:(i+2),],2,mean)#1,2,3  ->3;   2,3,4  ->4
}
T3_1 <- scale(dplyr::lag(T3_1,1)[-c(1:delay3),])
datasim_T <- cbind(datzh[,-c(1,5:12)],T3_1)
final_predictions_sim_T <-Simulation_HRR(datasim_T,alpha,data = datazl_IndexR)

#switch 2014,2015 imported case  Import2_0
Import2006_2015 <- c(data$Import[1:96],data$Import[109:120],data$Import[97:108])
Import2_0 <- matrix(NA,119,1)
for (i in 1:119) {
  Import2_0[i,] <- mean(Import2006_2015[i:(i+1)])#1,2  ->2;   2,3  ->3
}
Import2_0 <- scale(Import2_0[-c(1:delay2)])
datasim_Import <- datzh[,-c(1,5,6,7,8)]   # IndexR
datasim_Import$Import2_0<- Import2_0[2:116]
final_predictions_sim_Import <-Simulation_HRR(datasim_Import,alpha,data = datazl_IndexR)


######################  figure     #######################

Model <- list(model_no_R,model_IndexR,model_DaynR, model_SumR,model_CvR,model_NumberR,
              model_IndexR_DaynR, model_IndexR_DaynR_SumR,model_R1,
              model_NumberR_DaynR,model_NumberR_DaynR_SumR,model_R2)
Model_name=c('(a) Basic model','(b) Basic model+IndexR',
             '(c) Basic model+DaynR','(d) Basic model+SumR',
             '(e) Basic model+CvR','(f) Basic model+NumberR',
             '(a) Base model+IndexR+DaynR','(b) Base model+IndexR+DaynR+SumR',
             '(c) Base model+IndexR+DaynR+SumR+CvR','(a) Base model+NumberR+DaynR',
             '(b) Base model+NumberR+DaynR+SumR', '(c) Base model+NumberR+DaynR+SumR+CvR')

# 
for (i in 1:12) {
  
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


## 
# switch rainfall

i=2
m <-  Model[[i]]
rmse_text_grob <- textGrob(label = bquote(
  atop( RMSE[2006-2015] == .(round(RMSE$RMSE[i], digits = 2)),
        RMSE[2014-2015] == .(round(RMSE_pred$RMSE[i], digits = 2))
  )), 
  x = unit(0.6, "npc"), y = unit(0.2, "npc"), 
  hjust = 1, vjust = 0, 
  gp = gpar(fontsize = 14, fontfamily = "serif"))

if (i==2) {
  p <- ggplot(datzh, aes(x = 1:115, y = LocalCase)) +
    geom_ribbon(aes(x = 1:115, ymin = m$predicted_df$Lower, 
                    ymax = m$predicted_df$Upper, fill = "95% CI"), alpha = 0.7) +  
    geom_line(aes(x = 1:115, y = m$predicted_df$s1, color = "Predicted"), lwd = 1) +  
    geom_line(aes(x = 1:115, y = final_predictions_sim_IndexR, color = "Switching R"), linetype = "dashed",lwd = 1) +
    geom_point(aes(color = "Observed"), alpha = 1, size = 2) +  
    geom_vline(xintercept = 91, linetype = "dashed", color = "black", linewidth = 1) +  
    annotate("text", x = c(45, 110), y = c(max(datzh$`ln(LocalCase)`)+0.5 , max(datzh$`ln(LocalCase)`)+0.5 ),
             label = c("2006-2013\nTraining Set", "2014-2015\nTest Set"), color = "black",
             size = 4.5, hjust = 0.5, family = "serif") +  
    labs(title ='(d) Basic model+IndexR', x = "Month", y = "ln(LocalCase)") +
    theme_minimal() +
    theme(
      text = element_text(family = "serif"),  
      plot.title = element_text(size = 18, face = "bold"),  
      axis.title.y = element_text(size = 16),  
      axis.title.x = element_text(size = 16),  
      axis.text.x = element_text(size = 16),  
      axis.text.y = element_text(size = 16),  
      legend.title = element_blank(),  
      legend.text = element_text(size = 16),  
      legend.position = "right"  
    ) +
    scale_color_manual(values = c("Predicted" = "red", "Observed" = "blue",
                                  "Switching R"="#008000")) +  
    scale_fill_manual(values = c("95% CI" = "grey"))+  
    ylim(c(-0.5, 12.5))  
  
  
  final_plot <- plot_grid(p, rmse_text_grob, ncol=2,rel_widths = c(1, 0.05))
  
  print(final_plot)
  
}




# switch temperature
i=2
m <-  Model[[i]]
rmse_text_grob <- textGrob(label = bquote(
  atop( RMSE[2006-2015] == .(round(RMSE$RMSE[i], digits = 2)),
        RMSE[2014-2015] == .(round(RMSE_pred$RMSE[i], digits = 2))
  )), 
                           x = unit(0.6, "npc"), y = unit(0.2, "npc"), 
                           hjust = 1, vjust = 0, 
                           gp = gpar(fontsize = 14, fontfamily = "serif"))

if (i==2) {
  p <- ggplot(datzh, aes(x = 1:115, y = LocalCase)) +
    geom_ribbon(aes(x = 1:115, ymin = m$predicted_df$Lower, 
                    ymax = m$predicted_df$Upper, fill = "95% CI"), alpha = 0.7) +  
    geom_line(aes(x = 1:115, y = m$predicted_df$s1, color = "Predicted"), lwd = 1) +  
    geom_line(aes(x = 1:115, y = final_predictions_sim_T, color = "Switching T"), linetype = "dashed",lwd = 1) +
    geom_point(aes(color = "Observed"), alpha = 1, size = 2) +  
    geom_vline(xintercept = 91, linetype = "dashed", color = "black", linewidth = 1) +  
    annotate("text", x = c(45, 110), y = c(max(datzh$`ln(LocalCase)`)+0.5 , max(datzh$`ln(LocalCase)`)+0.5 ),
             label = c("2006-2013\nTraining Set", "2014-2015\nTest Set"), color = "black",
             size = 4.5, hjust = 0.5, family = "serif") +  
    labs(title ='(e) Basic model+IndexR', x = "Month", y = "ln(LocalCase)") +
    theme_minimal() +
    theme(
      text = element_text(family = "serif"),  
      plot.title = element_text(size = 18, face = "bold"),  
      axis.title.y = element_text(size = 16),  
      axis.title.x = element_text(size = 16),  
      axis.text.x = element_text(size = 16),  
      axis.text.y = element_text(size = 16),  
      legend.title = element_blank(),  
      legend.text = element_text(size = 16),  
      legend.position = "right"  
    ) +
    scale_color_manual(values = c("Predicted" = "red", "Observed" = "blue",
                                  "Switching T"="#008000")) +  
    scale_fill_manual(values = c("95% CI" = "grey"))+  
    ylim(c(-0.5, 12.5))  
  
  
  final_plot <- plot_grid(p, rmse_text_grob, ncol=2,rel_widths = c(1, 0.05))
  
  print(final_plot)
  
}


# switch imported case
i=2
m <-  Model[[i]]
rmse_text_grob <- textGrob(label = bquote(
  atop( RMSE[2006-2015] == .(round(RMSE$RMSE[i], digits = 2)),
        RMSE[2014-2015] == .(round(RMSE_pred$RMSE[i], digits = 2))
  )),  
                           x = unit(0.6, "npc"), y = unit(0.15, "npc"), 
                           hjust = 1, vjust = 0, 
                           gp = gpar(fontsize = 14, fontfamily = "serif"))

if (i==2) {
  p <- ggplot(datzh, aes(x = 1:115, y = LocalCase)) +
    geom_ribbon(aes(x = 1:115, ymin = m$predicted_df$Lower, 
                    ymax = m$predicted_df$Upper, fill = "95% CI"), alpha = 0.7) + 
    geom_line(aes(x = 1:115, y = m$predicted_df$s1, color = "Predicted"), lwd = 1) +  
    geom_line(aes(x = 1:115, y = final_predictions_sim_Import, color = "Switching\nImport"), linetype = "dashed",lwd = 1) +
    geom_point(aes(color = "Observed"), alpha = 1, size = 2) +  
    geom_vline(xintercept = 91, linetype = "dashed", color = "black", linewidth = 1) +  
    annotate("text", x = c(45, 110), y = c(max(datzh$`ln(LocalCase)`)+0.5 , max(datzh$`ln(LocalCase)`)+0.5 ),
             label = c("2006-2013\nTraining Set", "2014-2015\nTest Set"), color = "black",
             size = 4.5, hjust = 0.5, family = "serif") +  
    labs(title ='(f) Basic model+IndexR', x = "Month", y = "ln(LocalCase)") +
    theme_minimal() +
    theme(
      text = element_text(family = "serif"),  
      plot.title = element_text(size = 18, face = "bold"),  
      axis.title.y = element_text(size = 16),  
      axis.title.x = element_text(size = 16),  
      axis.text.x = element_text(size = 16),  
      axis.text.y = element_text(size = 16),  
      legend.title = element_blank(),  
      legend.text = element_text(size = 16),  
      legend.position = "right"  
    ) +
    scale_color_manual(values = c("Predicted" = "red", "Observed" = "blue"
                                  ,"Switching\nImport"="#008000")) +  
    scale_fill_manual(values = c("95% CI" = "grey"))+  
    ylim(c(-0.5, 12.5))  
  
  
  final_plot <- plot_grid(p, rmse_text_grob, ncol=2,rel_widths = c(1, 0.05))#
  
  print(final_plot)
  
}

