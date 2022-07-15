library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(ncdf4.helpers)
library(reshape2)
library(tidyverse)
library(extrafont)

rm(list=ls())

getwd()
setwd("D:/00 ³í¹®/01 ¸ðµ¨/¸ðµ¨/SS_O/run/ncfiles/out_data")
getwd()



###########################################################################################
# make directory =======
ncFName <- c("curtain_GGR2DCD")
ncFName.abbr  <- c("GGR to DCD")

targetVAR <-c("SSOL1")
makeDir <- paste0(ncFName, "_", targetVAR)
makeDirPath <- paste0("./", makeDir, "/")
if (!dir.exists(makeDir)) dir.create(makeDir)
# ======================

# load Netcdf file 
simStaName <- ncFName
simFullPath <- paste0('../', simStaName, ".nc")

ncList <- nc_open(simFullPath)
ncVars <- ncList$var

cwrJday <- ncvar_get(ncList, ncVars$Ordinal_Dates)
sJday <- cwrJday-2018000##+120/86400 # 120sec/24h*60min*60sec
sJday <- round(sJday, digits = 4)
eval(parse(text = paste0("sTargetVAR_SSOL1 <- ncvar_get(ncList, ncVars$",targetVAR,")")))
###########################################################################################
# make directory =======
ncFName <- c("curtain_GGR2DCD")
ncFName.abbr  <- c("GGR to DCD")

targetVAR <-c("SSOL2")
makeDir <- paste0(ncFName, "_", targetVAR)
makeDirPath <- paste0("./", makeDir, "/")
if (!dir.exists(makeDir)) dir.create(makeDir)

# load Netcdf file 
simStaName <- ncFName
simFullPath <- paste0('../', simStaName, ".nc")

ncList <- nc_open(simFullPath)
ncVars <- ncList$var

cwrJday <- ncvar_get(ncList, ncVars$Ordinal_Dates)
sJday <- cwrJday-2018000##+120/86400 # 120sec/24h*60min*60sec
sJday <- round(sJday, digits = 4)
eval(parse(text = paste0("sTargetVAR_SSOL2 <- ncvar_get(ncList, ncVars$",targetVAR,")")))
###########################################################################################
# make directory =======
ncFName <- c("curtain_GGR2DCD")
ncFName.abbr  <- c("GGR to DCD")

targetVAR <-c("SSOL3")
makeDir <- paste0(ncFName, "_", targetVAR)
makeDirPath <- paste0("./", makeDir, "/")
if (!dir.exists(makeDir)) dir.create(makeDir)
# ======================

# load Netcdf file 
simStaName <- ncFName
simFullPath <- paste0('../', simStaName, ".nc")

ncList <- nc_open(simFullPath)
ncVars <- ncList$var

cwrJday <- ncvar_get(ncList, ncVars$Ordinal_Dates)
sJday <- cwrJday-2018000##+120/86400 # 120sec/24h*60min*60sec
sJday <- round(sJday, digits = 4)
eval(parse(text = paste0("sTargetVAR_SSOL3 <- ncvar_get(ncList, ncVars$",targetVAR,")")))

###########################################################################################
sTargetVAR_a<-sTargetVAR_SSOL1+sTargetVAR_SSOL2+sTargetVAR_SSOL3
sTargetVAR <-sTargetVAR_a/0.9276
###########################################################################################

sDate_Yr <- ncvar_get(ncList, ncVars$Year)
sDate_Mo <- ncvar_get(ncList, ncVars$Month)
sDate_Dy <- ncvar_get(ncList, ncVars$Day)
sDate_Ho <- ncvar_get(ncList, ncVars$Hour) #+1 # adding factor : minute error 
# sDate_Mi <- ncvar_get(ncList, ncVars$Minute)
sDate_YMD <- ISOdate(sDate_Yr, sDate_Mo, sDate_Dy)
sDate_YMD1 <- format(sDate_YMD, "%m/%d")
sDate_YMDH <- paste0(sDate_YMD1," ", sprintf("%02d", sDate_Ho), "H")
# sHSur <- ncvar_get(ncList, ncVars$HEIGHT)
# sHBot <- ncvar_get(ncList, ncVars$BOTTOM)
s_dH <- ncVars$DZ$dim[[1]]$vals

##################################################
# calculate distance by accumulated points
sGridX <- ncvar_get(ncList, ncVars$GRID_X)
sGridY <- ncvar_get(ncList, ncVars$GRID_Y)

sGridX_diff <-diff(sGridX) 
sGridY_diff <-diff(sGridY)

sGrid_dist <- sqrt(sGridX_diff^2 + sGridY_diff^2)
sGrid_len <- length(sGrid_dist)

sGrid_dist_accum <-  cumsum(sGrid_dist)
sGrid_dist_accum <- c(0, sGrid_dist_accum)
sGrid_dist_accum <- round(sGrid_dist_accum, digits = 4)
sDist_min <- min(sGrid_dist_accum)
sDist_max <- max(sGrid_dist_accum)
sDist_len <- length(sGrid_dist_accum)
##################################################
# make rect. 

sGrid_dist_accum_len <- length(sGrid_dist_accum)

sGrid_dist_accum_diff <- diff(sGrid_dist_accum)
sGrid_dist_accum_diff_L <-c(sGrid_dist_accum_diff[1], sGrid_dist_accum_diff)
sGrid_dist_accum_diff_R <-c(sGrid_dist_accum_diff, 
                            sGrid_dist_accum_diff[length(sGrid_dist_accum_diff)])
XminLeft  <- sGrid_dist_accum - sGrid_dist_accum_diff_L/2
XmaxRight <- sGrid_dist_accum + sGrid_dist_accum_diff_R/2
YminBot <-round(s_dH, digits = 1)-0.5 # half of thickness
YmaxTop <-round(s_dH, digits = 1)+0.5
ndH <- length(s_dH) #30

XminLeft_m  <- melt(matrix(rep(XminLeft,  times = ndH), nrow = sGrid_dist_accum_len))
XmaxRight_m <- melt(matrix(rep(XmaxRight, times = ndH), nrow = sGrid_dist_accum_len))
YminBot_m   <- melt(matrix(rep(YminBot,   each = sGrid_dist_accum_len), ncol = ndH))
YmaxTop_m   <- melt(matrix(rep(YmaxTop,   each = sGrid_dist_accum_len), ncol = ndH))

############################################################
#JJ #############
sJday_len <- length(sJday)

sJday_max <- max(sJday)
sJday_min <- min(sJday)
sJday_start <- sJday_min #134.04 # x
sJday_end   <- sJday_max #169    # 
selecJdaysIndex <- which(sJday >= sJday_start  & sJday <= sJday_end)
selecJdaysIndex_len <- length(selecJdaysIndex)

## 0927 sJday for paper
rJday <- seq(1.5, 365.5, 1.0)
rJday.n <- length(rJday)

kk <-1

saveFileName <- vector()

for (kk in 1: rJday.n){#selecJdaysIndex_len){  #sJday_len # starting for loop
  # jj <- selecJdaysIndex[kk]
  jj <-  which(sJday==rJday[kk])
  
  selectJday <- sJday[jj] 
  
  selectJday_char <- sprintf("%07.3f", selectJday)
  
  sTargetVAR_J <- sTargetVAR[ , , jj]
  nr_sTargetVAR_J <- nrow(sTargetVAR_J) # distance(upper -> paldang dam)
  nc_sTargetVAR_J <- ncol(sTargetVAR_J) # depth_height(bottom -> top)
  rownames( sTargetVAR_J) <- round(sGrid_dist_accum, digits = 2)
  colnames( sTargetVAR_J) <- round(s_dH, digits = 2)
  
  sTargetVAR_J_melt <- melt(sTargetVAR_J)
  colnames(sTargetVAR_J_melt) <- c("Distance", "Elevation", "WaterTemp")
  
  ########################################
  #plot 
  
  ### 
  d=data.frame(x1=XminLeft_m$value, 
               x2=XmaxRight_m$value, 
               y1=YminBot_m$value, 
               y2=YmaxTop_m$value, 
               t=sTargetVAR_J_melt$WaterTemp)
  d <- filter(d, !is.na(t))
  
  ### ploting 
  
  subtitlePlot <- paste0("Jday:" , selectJday_char, "(", sDate_YMDH[jj], ")")
  titlePlot <- paste0(ncFName.abbr, ", ",targetVAR, ", ", subtitlePlot)
  
  ggplot() + 
    geom_rect(data=d, 
              mapping=aes(xmin=x1/1000, 
                          xmax=x2/1000, 
                          ymin=y1, 
                          ymax=y2, 
                          fill=t), linetype=0) + #linetype=1, color="red") +
    scale_x_continuous(name = "Distance (km)", #from Paldang Dam (km)",
                       breaks = seq(0, 80, by=10),
                       limits = c(0, 80))+
    scale_y_continuous(name = "Water level (EL. m)",
                       breaks = seq(20, 80, by=10), 
                       limits = c(20, 80))+
    scale_fill_gradientn( "Tur(NTU)",
                          colors = c("#000066","#000066","#3300CC","#0050FA",
                                     "#0089F5","#1DBCEF","#73E0F1","#B7F4F7",
                                     "#E5FEFA","#99CC66","#99CC66","#99CC33",
                                     "#FFFF99","#FFFF66","#FFFF00","#FF6699",
                                     "#FF0066","#FF0000","#993366","#660066",
                                     "#330066"),
                          limits = c(0, 100), #trans = "sqrt", #trans = "sqrt", 
                          breaks = seq( 0, 100, by = 20)
                          # breaks = c(c(0, 1, 5, 10), seq( 0, 400, by = 50)),
                          # values = (1-sqrt(seq(1, 0, by=-0.2)))#position = "top",
                          # labels = seq(0, 100, by=10) #, guide = "colourbar"
    )+
    labs(title = ncFName.abbr, subtitle = subtitlePlot)+
    theme_bw()+
    theme(text=element_text(family = "Consolas", size = 10), #size = 14
          ## axis
          axis.title = element_text(size=12),#face="bold"),
          axis.text = element_text(size=10),#colour = "red", size = rel(1.5)),
          ##Legend
          # legend.position = "none",
          # legend.justification = "top",
          legend.title = element_text(size = 8),
          # legend.direction = "horizontal", #"vertical", #
          # legend.position = c(0.70, 0.15),
          # legend.key.width =  unit(1.2, "cm"),
          # legend.key.height = unit(0.6, "cm"),
          # legend.title =  element_text(size = 12),
          legend.direction = "vertical",
          legend.justification = c(0.0, 0.0),
          legend.position = c(1.0, 0.0), #"right", #
          legend.key.width =  unit(0.5, "cm"),
          legend.key.height = unit(0.9, "cm"),
          legend.background = element_rect(fill = "transparent"), #'fill = "gray50",  colour = "black",
          # legend.box.just = "right" ,
          # legend.margin = margin(2,2,2,2),
          plot.margin = margin(0.1, 1.5, 0.1, 0.1, unit = "cm") # Top, Right, Bottom, Left
    )+
    # annotate("point", 
    #          label = paste0(seq(1:sGrid_dist_accum_len)), 
    #          x =  sGrid_dist_accum/1000,  y = rep(5, times=sGrid_dist_accum_len), colour = "blue")+
    # annotate("segment", x = 8.0, xend = 11.5, y = 29.0, yend = 29.0, 
    #          colour = "blue", arrow = arrow(length = unit(0.1,"cm"), ends="first"), size = 1)+
    # annotate("text", label = "[DCD]", x =  0.0, y = 80.0, vjust=1, hjust=0, size=3) + #, color = "white")
    # # annotate("text", label = "[GGR]", x = 75.0, y = 80.0, vjust=1, hjust=0, size=3) +
    annotate("text", label = "+DC2",   x = sGrid_dist_accum[6]/1000,    y = 20, vjust=0, hjust=0, size=3)+
    annotate("text", label = "-JWC",   x = sGrid_dist_accum[113]/1000,  y = 25, vjust=0, hjust=0, size=3)+
    annotate("text", label = "+DC5HN", x = sGrid_dist_accum[191]/1000,  y = 20, vjust=0, hjust=0, size=3)+
    annotate("text", label = "+DC6DJ", x = sGrid_dist_accum[302]/1000,  y = 20, vjust=0, hjust=0, size=3)+
    annotate("text", label = "-SHC",   x = sGrid_dist_accum[330]/1000,  y = 25, vjust=0, hjust=0, size=3)+
    annotate("text", label = "+DC4JG", x = sGrid_dist_accum[443]/1000,  y = 20, vjust=0, hjust=0, size=3)+
    geom_vline(data=data.frame(Xselc= c(sGrid_dist_accum[6]/1000, 
                                        sGrid_dist_accum[113]/1000,
                                        sGrid_dist_accum[191]/1000,
                                        sGrid_dist_accum[302]/1000,
                                        sGrid_dist_accum[330]/1000,
                                        sGrid_dist_accum[443]/1000)),
               mapping=aes(xintercept=Xselc), color="red", linetype = "dotted")
  # save plot
  # multiplot <- grid.arrange(a,b,c,d, e, nrow=5, ncol=1)
  saveFileName[kk] <-paste0(makeDirPath,
                            simStaName,"_",targetVAR,"_",
                            selectJday_char,".png")
  ggsave(saveFileName[kk], plot = last_plot(), device= "png",
         width = 210, height = 300*1/4,  units = "mm")
  
} #JJ #############

