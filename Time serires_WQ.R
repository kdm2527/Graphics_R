library(tidyverse);library(reshape2);library(ncdf4);library(latex2exp);library(lubridate);library(extrafont);library(readr)
library(pander)
rm(list=ls())
getwd()

######## OBSERVED ######## 

path.main <-c("D:/00 연구실과제/04 환기조/00 모델/00 입력자료처리/data_processing/02_obs_data")
setwd(path.main)
#data<-read_csv("WQ_DC_reservoir_obs_2017-18.csv",skip=1); data <-  data %>% filter(jday < 365)
data<-read_csv("WQ_DC_reservoir_obs_2018.csv",skip=1); data <-  data %>% filter(jday < 365)
data.obs <-cbind(data[,2:4],data$jday,data$TN,data$TP,data$NO3N,data$PO4P,data$NH4N,data$TOC,data$Chla,data$DO)
colname<-c("station","date","code","Jday","TN","TP","NO3N","PO4P","NH4N","TOC","Chla","DO")
colnames(data.obs)<-colname

obs.DC1 <- subset(subset(data.obs,station == "DC1") ,code == "top")
obs.DC2 <- subset(subset(data.obs,station == "DC2") ,code == "top")
obs.DC3 <- subset(subset(data.obs,station == "DC3") ,code == "top")
obs.DC4 <- subset(subset(data.obs,station == "DC4") ,code == "top")
obs.DC5 <- subset(subset(data.obs,station == "DC5") ,code == "top")
obs.DC6 <- subset(subset(data.obs,station == "DC6") ,code == "top")

obs.list<-list(obs.DC1,obs.DC2,obs.DC3,obs.DC4,obs.DC5,obs.DC6)
rm(obs.DC1);rm(obs.DC2);rm(obs.DC3);rm(obs.DC4);rm(obs.DC5);rm(obs.DC6)


######## SIMULATED 2018 ######## 
dc.List <- list(nPos= c(6), Names = c("ts_DC1","ts_DC2","ts_DC3","ts_DC4","ts_DC5","ts_DC6")) 
path.model <-paste0("D:/00 논문/01 모델/모델/SS_O/run/tsfiles")   

setwd(path.model);setwd("./ts_DC1/");sim.DC1<-read_csv("AllVariables_WC.csv");sim.DC1<-sim.DC1[-1,]
sim.DC1 <- data.frame(cbind(sim.DC1$Timestep/240+1,sim.DC1$TN,sim.DC1$TP,sim.DC1$NO3,sim.DC1$PO4,
                            sim.DC1$NH4,sim.DC1$TOC,sim.DC1$TCHLA,sim.DC1$DO))
for(i in 1:9){sim.DC1[,i] <-as.numeric(as.character(sim.DC1[,i]))} 

setwd(path.model);setwd("./ts_DC2/");sim.DC2<-read_csv("AllVariables_WC.csv");sim.DC2<-sim.DC2[-1,]
sim.DC2 <- data.frame(cbind(sim.DC2$Timestep/240+1,sim.DC2$TN,sim.DC2$TP,sim.DC2$NO3,sim.DC2$PO4,
                            sim.DC2$NH4,sim.DC2$TOC,sim.DC2$TCHLA,sim.DC2$DO))
for(i in 1:9){sim.DC2[,i] <-as.numeric(as.character(sim.DC2[,i]))} 

setwd(path.model);setwd("./ts_DC3/");sim.DC3<-read_csv("AllVariables_WC.csv");sim.DC3<-sim.DC3[-1,]
sim.DC3 <- data.frame(cbind(sim.DC3$Timestep/240+1,sim.DC3$TN,sim.DC3$TP,sim.DC3$NO3,sim.DC3$PO4,
                            sim.DC3$NH4,sim.DC3$TOC,sim.DC3$TCHLA,sim.DC3$DO))
for(i in 1:9){sim.DC3[,i] <-as.numeric(as.character(sim.DC3[,i]))} 


setwd(path.model);setwd("./ts_DC4/");sim.DC4<-read_csv("AllVariables_WC.csv");sim.DC4<-sim.DC4[-1,]
sim.DC4 <- data.frame(cbind(sim.DC4$Timestep/240+1,sim.DC4$TN,sim.DC4$TP,sim.DC4$NO3,sim.DC4$PO4,
                            sim.DC4$NH4,sim.DC4$TOC,sim.DC4$TCHLA,sim.DC4$DO))
for(i in 1:9){sim.DC4[,i] <-as.numeric(as.character(sim.DC4[,i]))}


setwd(path.model);setwd("./ts_DC5/");sim.DC5<-read_csv("AllVariables_WC.csv");sim.DC5<-sim.DC5[-1,]
sim.DC5 <- data.frame(cbind(sim.DC5$Timestep/240+1,sim.DC5$TN,sim.DC5$TP,sim.DC5$NO3,sim.DC5$PO4,
                            sim.DC5$NH4,sim.DC5$TOC,sim.DC5$TCHLA,sim.DC5$DO))
for(i in 1:9){sim.DC5[,i] <-as.numeric(as.character(sim.DC5[,i]))}


setwd(path.model);setwd("./ts_DC6/");sim.DC6<-read_csv("AllVariables_WC.csv");sim.DC6<-sim.DC6[-1,]
sim.DC6 <- data.frame(cbind(sim.DC6$Timestep/240+1,sim.DC6$TN,sim.DC6$TP,sim.DC6$NO3,sim.DC6$PO4,
                            sim.DC6$NH4,sim.DC6$TOC,sim.DC6$TCHLA,sim.DC6$DO))
for(i in 1:9){sim.DC6[,i] <-as.numeric(as.character(sim.DC6[,i]))} 

sim.list<-list(sim.DC1, sim.DC2, sim.DC3, sim.DC4, sim.DC5, sim.DC6)
rm(sim.DC1,sim.DC2,sim.DC3,sim.DC4,sim.DC5,sim.DC6)


######## functions ######## 
source("D:/00 연구실과제/04 환기조/00 모델/00 입력자료처리/data_processing/fCalStatsIndex3.R")
source("D:/00 연구실과제/04 환기조/00 모델/00 입력자료처리/data_processing/fJday2date.R")

######## error anlaysis WQ - 2017 ########
error <- data.frame(c("RMSE","MAPE","IOA")); names(error)<-"item"

i<-1; k<-1
for(i in 1:6){
  j<-1
  for(j in 1:8){
    a <- data.frame(obs.list[i]); obs<- cbind(a[,4],a[4+j]);colnames(obs)<-c("jday","data")
    b <-data.frame(sim.list[i]); c <-cbind(b[,1],b[1+j])
    
    selectedDay <- unique(obs$jday)
    
    c <- slice(b ,which(b$X1  %in% selectedDay))
    sim <- c[,j+1]
    ovar <- obs$data;svar<-sim
    statsIndices <- fCalStatsIndex(ovar, svar)
    statsIndices$values <- sprintf("%6.3f",statsIndices$values )
    d <- data.frame(statsIndices$values);colnames(d)<-paste0("DC",i,"item",j)
    error<-cbind(error,d)}}

#ploting
name.list <- list(st.names =
                    c("DC1_Chudong","DC2 (Front of dam)","DC3_Munui","DC4 (Jang-gye)","DC5 (Hoe-nam)","DC6_danjeong"),
                  wq.names = c("TN","TP","NO3-N","PO4-P","NH4-N","TOC","Chl-a","DO"),
                  wq.unit = c("mg/L","mg/L","mg/L","mg/L","mg/L","mg/L","mg/m3","mg/L"))

axis.df <-data.frame(
  TN = c(0,5),
  TP  =  c(0,0.15),
  NO3N = c(0,5),
  PO4P = c(0,0.1),
  NH4N = c(0,0.5),
  TOC =c(0,5),
  Chla = c(0,20),
  DO = c(4,20))

i<-1; k<-0

for(i in 1:6){
  j<-1
  for(j in 1:8){
    e <- data.frame(obs.list[i]); obs <- cbind(e[,4],e[,j+4])
    g <- data.frame(sim.list[i]); sim <- cbind(g[,1],g[,j+1])  
    
    k<-k+1
    
    sIndex.char <- 
      paste0(sprintf("%-05s",error[[1]][1]), ":", error[[k+1]][1], "  mg/L", "\n",
             sprintf("%-05s",error[[1]][2]), ":", error[[k+1]][2], " %", "\n",
             sprintf("%-05s",error[[1]][3]), ":", error[[k+1]][3]) 
    
    
    titlePlot    <- paste0(name.list$st.names[i],"_",name.list$wq.names[j])
    subtitlePlot <- paste0("[point(red) : obs., solid(black) : sim.]" )
    txtLabel1    <- sIndex.char
    yname<- paste0(name.list$wq.names[j]," (",name.list$wq.unit[j],")")
    ylim <-c(axis.df[1,j],axis.df[2,j])
    text.y <-c(axis.df[2,j])*0.8
    ggplot()+
      geom_path(aes(x=sim[,1] , y=sim[,2]), size=0.75)+
      geom_point(aes(x=obs[,1], y=obs[,2]), color="red", size=3.5, stroke=1.1)+
      ggtitle(titlePlot)+
      scale_x_continuous(name = "Month(2018y)", 
                         breaks = c(1,32,60,91,121,152,182,213,244,274,305,335,365),
                         limits = c(0,365),
                         labels= c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan"))+
      scale_y_continuous(name = yname, 
                         #breaks = seq(0, 5, by=0.5),
                         limits = ylim)+
           
      # ylab(expression(Chl-a ~ (mg/m^{"3"}))) + 
      geom_text(aes(x=0, y= text.y*0.95), label=txtLabel1, hjust=0, vjust=0,
                size=8, family = "Consolas", colour="grey10") +
      theme_classic()+
      theme(text=element_text(family = "Consolas", size = 25),
            axis.title = element_text(size=23),
            axis.text  = element_text(size=24)
            ) # top, right, bottom, left
    saveFileNamePng <-  paste0("D:/00 논문/01 모델/모델/SS_O/run/tsfiles/", name.list$st.names[i],"_",name.list$wq.names[j],".png")
    ggsave(saveFileNamePng, 
           plot = last_plot(), 
           device= "png", 
           width = 500*1/2, height = 350*1/2,  units = "mm")
    
  }}
