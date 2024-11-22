###################################################################################################
##  All Code was Written by Yan Liu
## 
## Last updated: 2024.11.22
###################################################################################################


rm(list = ls())
library(tidyverse)
library(reactablefmtr)
library(reactable)
library(htmltools)
library(sysfonts)
library(showtext)
library(webshot2)
library(tiff)
library(htmlwidgets)
library(magick)
library("pagedown")


#  PhantomJS
#webshot::install_phantomjs()

load(".../work_history.RData")
df = read_csv(".../year_index_06_15.csv")

## Data preprocessing
df$Case=round(log(df$Case+1))

# Define a normalized function
normalize <- function(x) {
  return(round(x/sum(x),digits = 4))
}
df[,3:12] <- lapply(df[,3:12], normalize)


# create custom color palette for scale fill
pal_scale <- c("#F4FFFD", "#E9DAEC", "#A270E5", "#43009A")

# main body of reactable - note, I downloaded the sans-serif font locally from Google Fonts first!
R_year_index_rank_figure <- reactable(df,
                   theme = reactableTheme(
                     style = list(fontFamily = "Times New Roman",fontSize = "16px"),
                     borderColor = "#DADADA"
                   ),
                   defaultPageSize = 11,
                   defaultColDef = colDef(
                     vAlign = "center",
                     align = "center",
                     headerVAlign = "center",
                     style = color_scales(df, span = 3:12, colors = pal_scale),
                     headerStyle = list(fontFamily = "Times New Roman"),
                     width = 90
                   ),
                   columnGroups = list(
                     colGroup(name = "", columns = c("Year", "Case", "Import"), headerStyle = list(fontFamily = "Times New Roman"), align = "left"),
                     colGroup(name = "Rainfall index", columns = c("IndexR", "DaynR", "SumR", "CvR","NumberR"), headerStyle = list(fontFamily = "Times New Roman")),
                     colGroup(name = "Temperature index", columns = c("IndexT", "DaynT", "MeanT", "MaxT"), headerStyle = list(fontFamily = "Times New Roman"))
                   ),
                  
                   columns = list(
                     
                     Case = colDef(
                       name = "ln(LocalCase)",
                       width = 180,
                       class = "border-left",
                       align = "left",
                       cell = data_bars(df,
                                        fill_color = "#7814ff",
                                        text_position = "outside-end",
                                        bar_height = 10,
                                        text_size = 12,
                                        min_value = 0,
                                        max_value = 12,
                                        background = "transparent"
                       )
                     )
                   )
)

R_year_index_rank_figure

