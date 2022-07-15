
rm(list = ls())

getwd()
setwd("D:/00 ³í¹®/01 ¸ðµ¨/¸ðµ¨/SS_O/run/ncfiles/out_data")
getwd()


##### library
library(tidyverse)
library(ncdf4)
library(ncdf4.helpers)
# library(dplyr)
library(reshape2)
# library(tidyverse)
# library(ggplot2)
# library(gridExtra)
library(extrafont) 
library(abind)

##### load NetCDF
# targetVar0 <- c("TRACER_1") #sheet name 
targetVar  <- c("PAR_Extinction")
sheetName   <- c("sheet_top_1")

simNCnames  <- paste0(sheetName)
sNCfullPath <- paste0("../", simNCnames, ".nc")

# nSheet     <-2
viewZone <-c(  0, 180, # x: 0 ~ 466, y: 0 ~ 704
               0, 220) # xMin, xMax, yMin, yMax as I, J value
#218, 175

### make directory
makeDir <- paste0(sheetName, "_", targetVar) #"UVSPD") #targetVar, "_view1")
makeDirPath <- paste0("./", makeDir, "/")
if (!dir.exists(makeDir)) dir.create(makeDir)

# i <-1
# for (i in 1:nSheet){
ncListTxt      <- paste0("ncList",  " <- nc_open(sNCfullPath)")
ncVarTxt       <- paste0("ncVar",   " <- ncList",  "$var")
ncCwrJdayTxt   <- paste0("cwrJday", " <- ncvar_get(ncList", ", ncVar",  "$Ordinal_Dates)")
ncTargetVarTxt <- paste0("sVar",    " <- ncvar_get(ncList", ", ncVar",  "$", targetVar, ")")
eval(parse(text=ncListTxt))
eval(parse(text=ncVarTxt))
eval(parse(text=ncCwrJdayTxt))
eval(parse(text=ncTargetVarTxt))
# }

# ## conbine array 
# if (nSheet == 1){
#   cwrJday <- cwrJday1 
#   sVar    <- sVar1
# }else if (nSheet == 2){
#   cwrJday <- c(cwrJday1, cwrJday2) 
#   sVar    <- abind(sVar1, sVar2, along=3)
# }else if (nSheet == 3){
#   cwrJday <- c(cwrJday1, cwrJday2, cwrJday3) 
#   sVar    <- abind(sVar1, sVar2, along=3)
#   sVar    <- abind(sVar,  sVar3, along=3)
# }

sX <- ncList$dim[[6]]$vals
sY <- ncList$dim[[7]]$vals

# rm(list = c("ncList1",  "ncList2",  "ncList3",
#             "ncVar1",   "ncVar2",   "ncVar3",
#             "cwrJday1", "cwrJday2", "cwrJday3",
#             "sVar1",    "sVar2",    "sVar3"
#             ))



###### start: cwrJday to Date(string, YYYY-MM-DD 03H)
sJday <- cwrJday-2018000 ####+120/86400
sJday <- round(sJday, digits = 4)
sHour <- trunc((sJday-trunc(sJday))*24)
sDate <- as.Date(sJday, "2018-01-01")-1 
sDate_YMDH <- paste0(sDate," ", sprintf("%02d", sHour), "H")
###### end 

##### start: makee Grid value (top, down, right, left) for geom_rec  
# sX <- ncList1$dim[[6]]$vals
# sY <- ncList1$dim[[7]]$vals

sX_diff <- diff(sX)
sX_diff_s <-c(sX_diff[1], sX_diff)
sX_diff_e <-c(sX_diff, sX_diff[length(sX_diff)])

sY_diff <- diff(sY)
sY_diff_s <-c(sY_diff[1], sY_diff)
sY_diff_e <-c(sY_diff, sY_diff[length(sY_diff)])

xMinLeft <- sX - sX_diff_s/2
xMaxRight <- sX + sX_diff_e/2

yMaxTop <- sY + sY_diff_s/2
yMinBot <- sY - sY_diff_e/2
# ==========
sX_len <- length(sX)
sY_len <- length(sY)

xMinLeft_m  <- melt((matrix(rep(xMinLeft,  times= sY_len ), nrow = sX_len)))
xMaxRight_m <- melt((matrix(rep(xMaxRight, times= sY_len ), nrow = sX_len)))
yMinBot_m   <- melt((matrix(rep(yMinBot,   each = sX_len),  ncol = sY_len)))
yMaxTop_m   <- melt((matrix(rep(yMaxTop,   each = sX_len),  ncol = sY_len)))

xCenter <- ((matrix(rep(sX,  times= sY_len ), nrow = sX_len)))
yCenter <- ((matrix(rep(sY,   each = sX_len),  ncol = sY_len)))
##### end

##### start: select sJday
sJday_len <- length(sJday)

sJday_max <- max(sJday)
sJday_min <- min(sJday)
sJday_start <- sJday_min #134.04 # x
sJday_end   <- sJday_max #169    # 
selecJdaysIndex <- which(sJday >= sJday_start  & sJday <= sJday_end)
selecJdaysIndex_len <- length(selecJdaysIndex)
# selecJdaysIndex_len 
##### end 


##### start : plot 2d sheet by selected Jday


## 0927 sJday for paper
rJday <- seq(1.5, 365.5, 1.0)
rJday.n <- length(rJday)

i <-150
saveFileName <- vector()

for (i in 1: rJday.n){#selecJdaysIndex_len){  #sJday_len # starting for loop
  # jj <- selecJdaysIndex[kk]
  jj <- which(sJday == rJday[i])
  selectJday <- sJday[jj] 
  
  selectJday_char <- sprintf("%07.3f", selectJday)
  sVar_m <- melt((sVar[,, jj]))
  
  
  plotDf <- data.frame(x1 = xMinLeft_m$value, 
                       x2 = xMaxRight_m$value, 
                       y1 = yMinBot_m$value, 
                       y2 = yMaxTop_m$value, 
                       t  = sVar_m$value)
  plotDf <- filter( plotDf, !is.na(t))
  
  ######========================================================================
  ##  plot
  titlePlot <- paste0(sheetName, " [ ", targetVar, " ]" )
  subtitlePlot <- paste0("Jday: " , selectJday_char, "(Date: ", sDate_YMDH[jj], ")")
  ggplot()+
    geom_rect(data=plotDf,
              mapping=aes(xmin=y1,
                          xmax=y2,
                          ymin=x1,
                          ymax=x2,
                          fill=t
              ), linetype=0) + #linetype=1, color="red") + #linetype=0) +
    
    scale_x_continuous(name = "West-East (km)",
                       breaks = seq(viewZone[1]*100, viewZone[2]*100, 30*100),
                       labels = sprintf("%2.0f", seq(viewZone[1]*100, viewZone[2]*100, 30*100)/1000),
                       limits = c(viewZone[1]*100, viewZone[2]*100)
                       # breaks=seq(1*50, 200*50, 50*10),
                       # limits = c(1*50, 200*50)
    )+
    scale_y_reverse(name = "South-North (km)",
                    breaks = sort(seq(viewZone[3]*100, viewZone[4]*100, 30*100), decreasing=TRUE),
                    labels = sprintf("%2.0f", 
                                     sort(seq(viewZone[3]*100, viewZone[4]*100, 30*100)/1000, 
                                          decreasing=TRUE)),
                    limits = sort(c(viewZone[3]*100, viewZone[4]*100), decreasing=TRUE)
                    # trans = "reverse",
                    # breaks = seq(400*50, 500*50, 10*50),
                    # limits = c(400*50, 500*50)
    )+
    scale_fill_gradientn( "Extinction\n(/m)",
                          colors = c("#000066","#000066","#3300CC","#0050FA",
                                     "#0089F5","#1DBCEF","#73E0F1","#B7F4F7",
                                     "#E5FEFA","#99CC66","#99CC66","#99CC33",
                                     "#FFFF99","#FFFF66","#FFFF00","#FF6699",
                                     "#FF0066","#FF0000","#993366","#660066",
                                     "#330066"),
                          limits = c(0, 8), #trans = "sqrt", #trans = "sqrt", 
                          breaks = seq(0, 8, by = 2)#c(c(0, 1, 5, 10), seq( 0, 100, by = 20)),
                          # values = (1-sqrt(seq(1, 0, by=-0.2)))#position = "top",
                          # labels = seq(0, 100, by=10) #, guide = "colourbar"
    )+
    labs(title = titlePlot, subtitle=subtitlePlot) + #, subtitle = subtitlePlot)+
    theme_bw()+
    theme(text=element_text(family = "Consolas"), #size = 14
          plot.title =element_text(size=14),
          plot.subtitle =element_text(size=12),
          ## axis
          axis.title = element_text(size=12),#face="bold"),
          axis.text = element_text(size=10),#colour = "red", si  ze = rel(1.5)),
          # legend.justification = "top", 
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 6),
          legend.direction = "vertical", #"vertical", #"horizontal"
          legend.justification = c(0, 0), 
          legend.position = c(0.80, 0.50),
          legend.key.width =  unit(0.3, "cm"),
          legend.key.height = unit(0.8, "cm"),
          legend.background = element_rect(fill = "transparent"), #'fill = "gray50",  colour = "black",
          # legend.box.just = "right" ,
          legend.margin = margin(2, 2, 2, 2) # margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
    )
  
  # save plot
  # multiplot <- grid.arrange(a,b,c,d, e, nrow=5, ncol=1)
  saveFileName[i] <-paste0(makeDirPath,
                           sheetName,"_", targetVar, "_",#"UVSPD", "_",
                           selectJday_char,".png")
  
  ggsave(saveFileName[i], plot = last_plot(), device= "png",
         width = 100*1, height = 100*220/180,  units = "mm", dpi=200)
  # rm(list = ls())
  
} #i

