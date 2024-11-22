###################################################################################################
##  All Code was Written by Yan Liu
## 
## Last updated: 2024.11.22
###################################################################################################

rm(list = ls())
library(readr)
library(dplyr)
library(corrplot)
library(mgcv)
library(ggplot2)
library(grid)

data <- read_csv(".../month_index06_15.csv")

ggplot(data, aes(x = factor(Month), y = Year, fill = log(LocalCase+1))) +
  geom_tile() +
  scale_fill_gradient(low = "#f1f0f4", high = '#008000') +
  labs(x = "Month", y = "Year", title = "(a) ln(LocalCase)") +
  theme_minimal(base_family = "Times New Roman", base_size = 18) +  # Set font to Times New Roman and size to 1.5 (15 pt)
  theme(
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank(),   
    axis.title = element_text(face = "bold"),
    text = element_text(family = "serif"),  
    plot.title = element_text(hjust = 0, vjust = 1, size = 18, face = "bold"),  
    axis.title.y = element_text(size = 16),  
    axis.title.x = element_text(size = 16),  #
    axis.text.x = element_text(size = 14, margin = unit(c(-10, 0, 0, 0), "pt")),  
    axis.text.y = element_text(size = 14),  
    legend.title = element_blank(),  
    legend.text = element_text(size = 14),
    plot.margin = unit(c(5, 5, 5, 5), "pt")  # 
  ) +
  scale_y_continuous(breaks = seq(min(data$Year), max(data$Year), 1))  # Show only integers on y-axis
#836,549

ggplot(data, aes(x = factor(Month), y = Year, fill = Import)) +
  geom_tile() +
  scale_fill_gradient(low = "#f1f0f4", high = '#008000') +
  labs(x = "Month", y = "Year", title = "(b) Import case") +
  theme_minimal(base_family = "Times New Roman", base_size = 18) +  # Set font to Times New Roman and size to 1.5 (15 pt)
  theme(
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank(),   
    axis.title = element_text(face = "bold"),
    text = element_text(family = "serif"),  
    plot.title = element_text(hjust = 0, vjust = 1, size = 18, face = "bold"),  
    axis.title.y = element_text(size = 16),  
    axis.title.x = element_text(size = 16),  # 
    axis.text.x = element_text(size = 14, margin = unit(c(-10, 0, 0, 0), "pt")),  
    axis.text.y = element_text(size = 14),  
    legend.title = element_blank(),  
    legend.text = element_text(size = 14),
    plot.margin = unit(c(5, 5, 5, 5), "pt")  # 
  ) +
  scale_y_continuous(breaks = seq(min(data$Year), max(data$Year), 1))  # Show only integers on y-axis

ggplot(data, aes(x = factor(Month), y = Year, fill = SumR)) +
  geom_tile() +
  scale_fill_gradient(low = "#f1f0f4", high = '#008000', name = "T") +
  labs(x = "Month", y = "Year", title = "(c) Total monthly rainfall") +
  theme_minimal(base_family = "Times New Roman", base_size = 18) +  # Set font to Times New Roman and size to 1.5 (15 pt)
  theme(
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank(),   
    axis.title = element_text(face = "bold"),
    text = element_text(family = "serif"),  
    plot.title = element_text(hjust = 0, vjust = 1, size = 18, face = "bold"),  
    axis.title.y = element_text(size = 16),  
    axis.title.x = element_text(size = 16),  # 
    axis.text.x = element_text(size = 14, margin = unit(c(-10, 0, 0, 0), "pt")),  
    axis.text.y = element_text(size = 14),  
    legend.title = element_blank(),  
    legend.text = element_text(size = 14),
    plot.margin = unit(c(5, 5, 5, 5), "pt")  # 
  ) +
  scale_y_continuous(breaks = seq(min(data$Year), max(data$Year), 1))  # Show only integers on y-axis


ggplot(data, aes(x = factor(Month), y = Year, fill = MeanT)) +
  geom_tile() +
  scale_fill_gradient(low = "#f1f0f4", high = '#008000', name = "T") +
  labs(x = "Month", y = "Year", title = "(d) Monthly mean temperature") +
  theme_minimal(base_family = "Times New Roman", base_size = 18) + 
  theme(
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank(),   
    axis.title = element_text(face = "bold"),
    text = element_text(family = "serif"),  
    plot.title = element_text(hjust = 0, vjust = 1, size = 18, face = "bold"),  
    axis.title.y = element_text(size = 16),  
    axis.title.x = element_text(size = 16),  # 
    axis.text.x = element_text(size = 14, margin = unit(c(-10, 0, 0, 0), "pt")),  
    axis.text.y = element_text(size = 14),  
    legend.title = element_blank(),  
    legend.text = element_text(size = 14),
    plot.margin = unit(c(5, 5, 5, 5), "pt")  # 
  ) +
  scale_y_continuous(breaks = seq(min(data$Year), max(data$Year), 1))  # 

####################   Data preprocessing  ####################
data$LocalCase <- round(log(data$LocalCase+1))
data$CvR <- -data$CvR

dat <- data[,3:13]
data0 <- dat; 
colnames(data0) <- c('ln(LocalCase)',	'Import',	'IndexR',	'DaynR',	'SumR',	'CvR','NumberR',	'IndexT',	'DaynT',	'MeanT',	'MaxT')
corr0 <- cor(data0)
testp0 <- matrix(NA,11,11)
d0 <- sapply(data0, as.numeric)
for (i in 1:11) {
  for (j in 1:11) {
    
    p0 <- cor.test(d0[,i],d0[,j])
    testp0[i,j] <- p0$p.value
    
  } 
  
}


# 
color_custom <- function(corr, p.mat, sig.level = 0.05) {
  col_matrix <- matrix(NA, nrow = nrow(corr), ncol = ncol(corr))
  for (i in 1:nrow(corr)) {
    for (j in 1:ncol(corr)) {
      if (p.mat[i, j] < sig.level) {
        col_matrix[i, j] <- "black"
      } else {
        col_matrix[i, j] <- "#7F7FDF"
      }
    }
  }
  return(col_matrix)
}

# 
col_matrix0 <- color_custom(corr0, testp0)


# 
par(family = "serif",cex=1.5, oma = c(0, 0, 2, 0),xpd=T)  #
col <- colorRampPalette(c("#8AB186", "#B3C8CD", "#F2F7F3", "#C5DF56", "#96CA00"))#
corrplot(corr0, 
         method = "square",          # 
         addCoef.col = col_matrix0,  # 
         # number.cex = 1.2,           # 
         col = col(200),             # 
         tl.col = "black",           # 
         tl.srt = 45,                # 
         tl.pos = "full",            # 
         mar = c(0,0,2,0))           


title ('(a) Correlation coefficient between variables', line = 0.5,outer=T) 
# 900,1000



#####  The indicator (the independent variable) granularity is filtered according to correlation : correlation(index,case)

###################################### Granularity = 1 month
data1 <- dat
colnames(data1) <- c('ln(LocalCase)',	'Import',	'IndexR',	'DaynR',	'SumR',	'CvR','NumberR',	'IndexT',	'DaynT',	'MeanT',	'MaxT')

corr1 <- matrix(NA,6,11);testp1 <- matrix(NA,6,11)

for (j in 1:11) {
  for (i in 1:6) {
    
    d1 <- data.frame(data1[,1],dplyr::lag(data1[,j],(i-1)))
    if (i==1){
      
      corr1[i,j] <- cor(d1[,1],d1[,2])
      p1 <- cor.test(d1[,1],d1[,2]);      testp1[i,j] <- p1$p.value
    } else{
      
      corr1[i,j] <- cor(d1[-c(1:(i-1)),1],d1[-c(1:(i-1)),2])
      p1 <- cor.test(d1[-c(1:(i-1)),1],d1[-c(1:(i-1)),2]);      testp1[i,j] <- p1$p.value
    }
    
  } 
  
}

colnames(corr1) <- c('ln(LocalCase)',	'Import',	'IndexR',	'DaynR',	'SumR',	'CvR','NumberR',	'IndexT',	'DaynT',	'MeanT',	'MaxT')
rownames(corr1) <- c('Lag 0','Lag 1','Lag 2','Lag 3','Lag 4','Lag 5' )
colnames(testp1) <- c('ln(LocalCase)',	'Import',	'IndexR',	'DaynR',	'SumR',	'CvR','NumberR',	'IndexT',	'DaynT',	'MeanT',	'MaxT')
rownames(testp1) <- c('Lag 0','Lag 1','Lag 2','Lag 3','Lag 4','Lag 5' )


# 
col_matrix1 <- color_custom(corr1, testp1)

# 
par(family = "serif", cex = 1.5)  # 
col <- colorRampPalette(c("#8AB186", "#B3C8CD", "#F2F7F3", "#C5DF56", "#96CA00"))
corrplot(corr1, 
         method = "square",          
         addCoef.col = col_matrix1,   
         #number.cex = 1.2,          
         col = col(200),            
         tl.col = "black",          
         tl.srt = 45,               
         tl.pos = "full",            
         mar = c(0,0,1,0))          


title ('(b) One month step', line = 1) 
#900,750


################################ Granularity = 2 month
data2 <- data.frame(matrix(NA,119,11))
colnames(data2) <- c('ln(LocalCase)',	'Import',	'IndexR',	'DaynR',	'SumR',	'CvR','NumberR',	'IndexT',	'DaynT',	'MeanT',	'MaxT')

for (i in 1:119) {
  
  data2[i,] <- apply(dat[i:(i+1),],2,mean)#1,2  ->2;   2,3  ->3
  
}

corr2 <- matrix(NA,6,11);testp2 <- matrix(NA,6,11)

for (j in 1:11) {
  for (i in 1:6) {
    
    d2 <- data.frame(data1[-1,1],dplyr::lag(data2[,j],(i-1)))
    if (i==1){
      
      corr2[i,j] <- cor(d2[,1],d2[,2])
      p2 <- cor.test(d2[,1],d2[,2]);      testp2[i,j] <- p2$p.value
    } else{
      
      corr2[i,j] <- cor(d2[-c(1:(i-1)),1],d2[-c(1:(i-1)),2])
      p2 <- cor.test(d2[-c(1:(i-1)),1],d2[-c(1:(i-1)),2]);      testp2[i,j] <- p2$p.value
    }
    
  } 
  
}
colnames(corr2) <- c('ln(LocalCase)',	'Import',	'IndexR',	'DaynR',	'SumR',	'CvR','NumberR',	'IndexT',	'DaynT',	'MeanT',	'MaxT')
rownames(corr2) <- c('Lag 0','Lag 1','Lag 2','Lag 3','Lag 4','Lag 5' )
colnames(testp2) <- c('ln(LocalCase)',	'Import',	'IndexR',	'DaynR',	'SumR',	'CvR','NumberR',	'IndexT',	'DaynT',	'MeanT',	'MaxT')
rownames(testp2) <- c('Lag 0','Lag 1','Lag 2','Lag 3','Lag 4','Lag 5' )


# 
col_matrix2 <- color_custom(corr2, testp2)

# 
par(family = "serif", cex = 1.5)  
col <- colorRampPalette(c("#8AB186", "#B3C8CD", "#F2F7F3", "#C5DF56", "#96CA00"))
corrplot(corr2, 
         method = "square",          
         addCoef.col = col_matrix2,   
                    
         col = col(200),             
         tl.col = "black",           
         tl.srt = 45,                
         tl.pos = "full",            
         #tl.cex = 1.2,
         mar = c(0,0,1,0))           


title ('(c) Two month step', line = 1) 

################################ Granularity = 3 month
data3 <- data.frame(matrix(NA,118,11))
colnames(data3) <- c('ln(LocalCase)',	'Import',	'IndexR',	'DaynR',	'SumR',	'CvR','NumberR',	'IndexT',	'DaynT',	'MeanT',	'MaxT')

for (i in 1:118) {
  
  data3[i,] <- apply(dat[i:(i+2),],2,mean)#1,2,3  ->3;   2,3,4  ->4
  
}

corr3 <- matrix(NA,6,11);testp3 <- matrix(NA,6,11)

for (j in 1:11) {
  for (i in 1:6) {
    
    d3 <- data.frame(data1[-c(1:2),1],dplyr::lag(data3[,j],(i-1)))
    if (i==1){
      
      corr3[i,j] <- cor(d3[,1],d3[,2])
      p3 <- cor.test(d3[,1],d3[,2]);      testp3[i,j] <- p3$p.value
    } else{
      
      corr3[i,j] <- cor(d3[-c(1:(i-1)),1],d3[-c(1:(i-1)),2])
      p3 <- cor.test(d3[-c(1:(i-1)),1],d3[-c(1:(i-1)),2]);      testp3[i,j] <- p3$p.value
    }
    
  } 
  
}
colnames(corr3) <- c('ln(LocalCase)',	'Import',	'IndexR',	'DaynR',	'SumR',	'CvR','NumberR',	'IndexT',	'DaynT',	'MeanT',	'MaxT')
rownames(corr3) <- c('Lag 0','Lag 1','Lag 2','Lag 3','Lag 4','Lag 5' )
colnames(testp3) <- c('ln(LocalCase)',	'Import',	'IndexR',	'DaynR',	'SumR',	'CvR','NumberR',	'IndexT',	'DaynT',	'MeanT',	'MaxT')
rownames(testp3) <- c('Lag 0','Lag 1','Lag 2','Lag 3','Lag 4','Lag 5' )


# 
col_matrix3 <- color_custom(corr3, testp3)

# 
par(family = "serif", cex = 1.5)  # 
col <- colorRampPalette(c("#8AB186", "#B3C8CD", "#F2F7F3", "#C5DF56", "#96CA00"))
corrplot(corr3, 
         method = "square",         
         addCoef.col = col_matrix3,  
         col = col(200),            
         tl.col = "black",          
         tl.srt = 45,               
         tl.pos = "full",            
         mar = c(0,0,1,0))        


title ('(d) Three month step', line = 1) 



################################ Granularity = 4 month
data4 <- data.frame(matrix(NA,117,11))
colnames(data4) <- c('ln(LocalCase)',	'Import',	'IndexR',	'DaynR',	'SumR',	'CvR','NumberR',	'IndexT',	'DaynT',	'MeanT',	'MaxT')

for (i in 1:117) {
  
  data4[i,] <- apply(dat[i:(i+3),],2,mean)#1,2,3,4  ->4;   2,3,4,5  ->5
  
}

corr4 <- matrix(NA,6,11);testp4 <- matrix(NA,6,11)

for (j in 1:11) {
  for (i in 1:6) {
    
    d4 <- data.frame(data1[-c(1:3),1],dplyr::lag(data4[,j],(i-1)))
    if (i==1){
      
      corr4[i,j] <- cor(d4[,1],d4[,2])
      p4 <- cor.test(d4[,1],d4[,2]);      testp4[i,j] <- p4$p.value
    } else{
      
      corr4[i,j] <- cor(d4[-c(1:(i-1)),1],d4[-c(1:(i-1)),2])
      p4 <- cor.test(d4[-c(1:(i-1)),1],d4[-c(1:(i-1)),2]);      testp4[i,j] <- p4$p.value
    }
    
  } 
  
}
colnames(corr4) <- c('ln(LocalCase)',	'Import',	'IndexR',	'DaynR',	'SumR',	'CvR','NumberR',	'IndexT',	'DaynT',	'MeanT',	'MaxT')
rownames(corr4) <- c('Lag 0','Lag 1','Lag 2','Lag 3','Lag 4','Lag 5' )
colnames(testp4) <- c('ln(LocalCase)',	'Import',	'IndexR',	'DaynR',	'SumR',	'CvR','NumberR',	'IndexT',	'DaynT',	'MeanT',	'MaxT')
rownames(testp4) <- c('Lag 0','Lag 1','Lag 2','Lag 3','Lag 4','Lag 5' )


# 
col_matrix4 <- color_custom(corr4, testp4)

# 
par(family = "serif", cex = 1.5)  # 
col <- colorRampPalette(c("#8AB186", "#B3C8CD", "#F2F7F3", "#C5DF56", "#96CA00"))
corrplot(corr4, 
         method = "square",         
         addCoef.col = col_matrix4,   
         
         col = col(200),            
         tl.col = "black",          
         tl.srt = 45,              
         tl.pos = "full",           
         mar = c(0,0,1,0))         


title ('(e) Four month step', line = 1) 




###########################################  


get_max_info <- function(column, col_index) {
  max_value <- max(column)
  max_row_index <- which(column == max_value)
  row_name <- rownames(corr1)[max_row_index]
  col_name <- colnames(corr1)[col_index]
  return(list(max_value = max_value, row_index = max_row_index, row_name = row_name, col_name = col_name))
}


####### Granularity = 1 month

results1 <- lapply(seq_len(ncol(corr1)), function(col_index) {
  get_max_info(corr1[, col_index], col_index)
})

# 
result_df1 <- do.call(rbind, lapply(results1, function(res) {
  data.frame(MaxValue = res$max_value,
             RowIndex = res$row_index,
             RowName = res$row_name,
             ColName = res$col_name)
}))


print(result_df1)


delay1 <- max(result_df1$RowIndex)-1
dat1 <- data.frame(data1$`ln(LocalCase)`,
                   dplyr::lag(data1$`ln(LocalCase)`,1), #The case of the current month is not used as an independent variable, so the lag of one month is taken
                   dplyr::lag(data1$Import,result_df1$RowIndex[2]-1),
                   dplyr::lag(data1$IndexR,result_df1$RowIndex[3]-1),
                   dplyr::lag(data1$DaynR,result_df1$RowIndex[4]-1),
                   dplyr::lag(data1$SumR,result_df1$RowIndex[5]-1),
                   dplyr::lag(data1$CvR,result_df1$RowIndex[6]-1),
                   dplyr::lag(data1$NumberR,result_df1$RowIndex[7]-1),
                   dplyr::lag(data1$IndexT,result_df1$RowIndex[8]-1),
                   dplyr::lag(data1$DaynT,result_df1$RowIndex[9]-1),
                   dplyr::lag(data1$MeanT,result_df1$RowIndex[10]-1),
                   dplyr::lag(data1$MaxT,result_df1$RowIndex[11]-1)
)

dat1 <-  dat1[-c(1:delay1),]
colnames(dat1) <- c('ln(LocalCase)','LocalCase_1',	'Import_0',	'IndexR_4',	'DaynR_4',
                    'SumR_4',	'CvR_4',	'NumberR_4','IndexT_2',	'DaynT_2',	'MeanT_2',	'MaxT_2')
mean_case1_1 <- mean(dat1$LocalCase_1)
sd_case1_1 <- sd(dat1$LocalCase_1)

dat1[,-1] <- scale(dat1[,-1])#
subdat1 <- dat1[1:92,];newdat1 <- dat1[93:116,]






####### Granularity = 2 month

# 
results2 <- lapply(seq_len(ncol(corr2)), function(col_index) {
  get_max_info(corr2[, col_index], col_index)
})

# 
result_df2 <- do.call(rbind, lapply(results2, function(res) {
  data.frame(MaxValue = res$max_value,
             RowIndex = res$row_index,
             RowName = res$row_name,
             ColName = res$col_name)
}))

# 
print(result_df2)

delay2 <- max(result_df2$RowIndex)-1
dat2 <- data.frame(data1$`ln(LocalCase)`[-1],
                   dplyr::lag(data1$`ln(LocalCase)`[-1],1), 
                   dplyr::lag(data2$Import,result_df2$RowIndex[2]-1),
                   dplyr::lag(data2$IndexR,result_df2$RowIndex[3]-1),
                   dplyr::lag(data2$DaynR,result_df2$RowIndex[4]-1),
                   dplyr::lag(data2$SumR,result_df2$RowIndex[5]-1),
                   dplyr::lag(data2$CvR,result_df2$RowIndex[6]-1),
                   dplyr::lag(data2$NumberR,result_df2$RowIndex[7]-1),
                   dplyr::lag(data2$IndexT,result_df2$RowIndex[8]-1),
                   dplyr::lag(data2$DaynT,result_df2$RowIndex[9]-1),
                   dplyr::lag(data2$MeanT,result_df2$RowIndex[10]-1),
                   dplyr::lag(data2$MaxT,result_df2$RowIndex[11]-1)
)

dat2 <-  dat2[-c(1:delay2),]
colnames(dat2) <- c('ln(LocalCase)','ln(LocalCase)_1',	'Import_0',	'IndexR_3',	'DaynR_3',	
                    'SumR_3',	'CvR_3','NumberR_3',	'IndexT_2',	'DaynT_2',	'MeanT_2',	'MaxT_1')
dat2[,2:12] <- scale(dat2[,2:12])
subdat2 <- dat2[1:92,];newdat2 <- dat2[93:116,]



####### Granularity = 3 month

# 
results3 <- lapply(seq_len(ncol(corr3)), function(col_index) {
  get_max_info(corr3[, col_index], col_index)
})

# 
result_df3 <- do.call(rbind, lapply(results3, function(res) {
  data.frame(MaxValue = res$max_value,
             RowIndex = res$row_index,
             RowName = res$row_name,
             ColName = res$col_name)
}))

# 
print(result_df3)

delay3 <- max(result_df3$RowIndex)-1
dat3 <- data.frame(data1$`ln(LocalCase)`[-c(1,2)],
                   dplyr::lag(data1$`ln(LocalCase)`[-c(1,2)],1), 
                   dplyr::lag(data3$Import,result_df3$RowIndex[2]-1),
                   dplyr::lag(data3$IndexR,result_df3$RowIndex[3]-1),
                   dplyr::lag(data3$DaynR,result_df3$RowIndex[4]-1),
                   dplyr::lag(data3$SumR,result_df3$RowIndex[5]-1),
                   dplyr::lag(data3$CvR,result_df3$RowIndex[6]-1),
                   dplyr::lag(data3$NumberR,result_df3$RowIndex[7]-1),
                   dplyr::lag(data3$IndexT,result_df3$RowIndex[8]-1),
                   dplyr::lag(data3$DaynT,result_df3$RowIndex[9]-1),
                   dplyr::lag(data3$MeanT,result_df3$RowIndex[10]-1),
                   dplyr::lag(data3$MaxT,result_df3$RowIndex[11]-1)
)

dat3 <- dat3[-c(1:delay3),]
colnames(dat3) <- c('ln(LocalCase)','ln(LocalCase)_1',	'Import_0',	'IndexR_2',	'DaynR_2',
                    'SumR_3',	'CvR_3','NumberR_3',	'IndexT_1',	'DaynT_1',	'MeanT_1',	'MaxT_1')
dat3[,2:12] <- scale(dat3[,2:12])
subdat3 <- dat3[1:91,];newdat3 <- dat3[92:115,]




####### Granularity = 4 month

# 
results4 <- lapply(seq_len(ncol(corr4)), function(col_index) {
  get_max_info(corr4[, col_index], col_index)
})

# 
result_df4 <- do.call(rbind, lapply(results4, function(res) {
  data.frame(MaxValue = res$max_value,
             RowIndex = res$row_index,
             RowName = res$row_name,
             ColName = res$col_name)
}))

# 
print(result_df4)

delay4 <- max(result_df4$RowIndex)-1
dat4 <- data.frame(data1$`ln(LocalCase)`[-c(1,2,3)],
                   dplyr::lag(data1$`ln(LocalCase)`[-c(1,2,3)],1), 
                   dplyr::lag(data4$Import,result_df4$RowIndex[2]-1),
                   dplyr::lag(data4$IndexR,result_df4$RowIndex[3]-1),
                   dplyr::lag(data4$DaynR,result_df4$RowIndex[4]-1),
                   dplyr::lag(data4$SumR,result_df4$RowIndex[5]-1),
                   dplyr::lag(data4$CvR,result_df4$RowIndex[6]-1),
                   dplyr::lag(data4$NumberR,result_df4$RowIndex[7]-1),
                   dplyr::lag(data4$IndexT,result_df4$RowIndex[8]-1),
                   dplyr::lag(data4$DaynT,result_df4$RowIndex[9]-1),
                   dplyr::lag(data4$MeanT,result_df4$RowIndex[10]-1),
                   dplyr::lag(data4$MaxT,result_df4$RowIndex[11]-1)
)

dat4 <-  dat4[-c(1:delay4),]
colnames(dat4) <- c('ln(LocalCase)','ln(LocalCase)_1',	'Import_0',	'IndexR_1',	'DaynR_2',
                    'SumR_2',	'CvR_2','NumberR_2',	'IndexT_1',	'DaynT_1',	'MeanT_1',	'MaxT_0')
dat4[,2:12] <- scale(dat4[,2:12])
subdat4 <- dat4[1:91,];newdat4 <- dat4[92:115,]





#######    Granularity = 1,2,3,4 month

datzh <- data.frame(          dat1[2:116,1],
                              dat1[2:116,2],#
                              dat2[2:116,3],
                              dat4[,c(4:8)],
                              dat3[,c(9:12)]
                              
)

colnames(datzh) <- c('ln(LocalCase)','ln(LocalCase)1_1',	'Import2_0',	'IndexR4_1',	'DaynR4_2',
                     'SumR4_2', 'CvR4_2','NumberR4_2',	'IndexT3_1',	'DaynT3_1',	'MeanT3_1',	'MaxT3_1')

subdatzh <- datzh[1:91,];newdatzh <- datzh[92:115,]

# 
zero_proportion <- apply(datzh, 2, function(col) {
  mean(col == 0)
})

# 
print(zero_proportion)





