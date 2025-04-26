###################################################################################################
##  All Code was Written by Yan Liu
## 
## Last updated: 2025.4.23
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
#############################################
load(".../RH_work_history.RData")
###############################################################
datzh_RH <- read_csv(".../data_sensitivity_analysis_RH_06_15.csv")
datzh <- datzh_RH[,-1]

# Data preparation
LocalCase <- datzh$`ln(LocalCase)`
family <- "gaussian"
alpha0 <- 0.4

datazl_IndexR <- datzh[,c(2:4,9:12)]  # IndexR  
datazl_RH <- datzh[,c(2,3,13,9:12)] # RH  
model_IndexR <- Hurdle_Regularized_Regression(datazl=datazl_IndexR,LocalCase,family,alpha0)
model_RH <- Hurdle_Regularized_Regression(datazl=datazl_RH,LocalCase,family,alpha0)

RMSE_pred <- data.table(Model=c('Base model+IndexR','Base model+RH'),
                        RMSE=c(model_IndexR$rmse_pred,model_RH$rmse_pred))
RMSE <- data.table(Model=c('Base model+IndexR','Base model+RH'),
                   RMSE=c(model_IndexR$rmse,model_RH$rmse))


## SI Fig
Model <- list(model_IndexR,model_RH)
Model_name=c('(a) Basic model+IndexR','(b) Basic model+RH')

# 
for (i in 1:2) {
  
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


