################################################################################
# Project: The Real Exchange Rate, Real Interest Rates, and the Risk Premium   #
# Program: for Table 2 -- Fama Regressions                                     #
# Dataset: dat73_1                                                             #
# Date: 02.14.2011                                                             #
# Author: Mian Zhu  <mian.zhu@gmail.com> 
# Updated: Kyle B. Lackinger
# Date: 07.09.2020
################################################################################

rm(list=ls())     # clear workspace

library(sandwich) # import package
library(vars)
library(plyr)

load("dat73_1")   # import dataset

#######
#Need to read in the updated dataset and append the new data after row 440
data=read.csv("dat73_New.csv",header= TRUE)

#Extend the existing country df's to easily append
CAN[(nrow(CAN)+(nrow(data)-nrow(CAN))),] <- NA
FRA[(nrow(FRA)+(nrow(data)-nrow(FRA))),] <- NA
DEU[(nrow(DEU)+(nrow(data)-nrow(DEU))),] <- NA
GBR[(nrow(GBR)+(nrow(data)-nrow(GBR))),] <- NA
ITA[(nrow(ITA)+(nrow(data)-nrow(ITA))),] <- NA
JPN[(nrow(JPN)+(nrow(data)-nrow(JPN))),] <- NA
G6[(nrow(G6)+(nrow(data)-nrow(G6))),] <- NA

#Update the columns in CAN df from data df
CAN <- within(CAN, rm(P.CAN, P.USA, i.CAN, i.USA))
CAN <- data.frame(data$i.USA, CAN)
CAN <- data.frame(data$i.CAN, CAN)
CAN <- data.frame(data$P.USA, CAN)
CAN <- data.frame(data$P.CAN, CAN)
CAN <- within(CAN, rm(S.CAN))
CAN <- data.frame(CAN, data$S.CAN)
CAN <- rename(CAN, c("data.P.CAN" = "P.CAN", "data.P.USA" = "P.USA", "data.i.CAN" = "i.CAN", "data.i.USA" = "i.USA", "data.S.CAN" = "S.CAN"))

#Update the columns in DEU df from data df
DEU <- within(DEU, rm(P.DEU, P.USA, i.DEU, i.USA))
DEU <- data.frame(data$i.USA, DEU)
DEU <- data.frame(data$i.DEU, DEU)
DEU <- data.frame(data$P.USA, DEU)
DEU <- data.frame(data$P.DEU, DEU)
DEU <- within(DEU, rm(S.DEU))
DEU <- data.frame(DEU, data$S.DEU)
DEU <- rename(DEU, c("data.P.DEU" = "P.DEU", "data.P.USA" = "P.USA", "data.i.DEU" = "i.DEU", "data.i.USA" = "i.USA", "data.S.DEU" = "S.DEU"))

#Update the columns in FRA df from data df
FRA <- within(FRA, rm(P.FRA, P.USA, i.FRA, i.USA))
FRA <- data.frame(data$i.USA, FRA)
FRA <- data.frame(data$i.FRA, FRA)
FRA <- data.frame(data$P.USA, FRA)
FRA <- data.frame(data$P.FRA, FRA)
FRA <- within(FRA, rm(S.FRA))
FRA <- data.frame(FRA, data$S.FRA)
FRA <- rename(FRA, c("data.P.FRA" = "P.FRA", "data.P.USA" = "P.USA", "data.i.FRA" = "i.FRA", "data.i.USA" = "i.USA", "data.S.FRA" = "S.FRA"))

#Update the columns in ITA df from data df
ITA <- within(ITA, rm(P.ITA, P.USA, i.ITA, i.USA))
ITA <- data.frame(data$i.USA, ITA)
ITA <- data.frame(data$i.ITA, ITA)
ITA <- data.frame(data$P.USA, ITA)
ITA <- data.frame(data$P.ITA, ITA)
ITA <- within(ITA, rm(S.ITA))
ITA <- data.frame(ITA, data$S.ITA)
ITA <- rename(ITA, c("data.P.ITA" = "P.ITA", "data.P.USA" = "P.USA", "data.i.ITA" = "i.ITA", "data.i.USA" = "i.USA", "data.S.ITA" = "S.ITA"))

#Update the columns in GBR df from data df
GBR <- within(GBR, rm(P.GBR, P.USA, i.GBR, i.USA))
GBR <- data.frame(data$i.USA, GBR)
GBR <- data.frame(data$i.GBR, GBR)
GBR <- data.frame(data$P.USA, GBR)
GBR <- data.frame(data$P.GBR, GBR)
GBR <- within(GBR, rm(S.GBR))
GBR <- data.frame(GBR, data$S.GBR)
GBR <- rename(GBR, c("data.P.GBR" = "P.GBR", "data.P.USA" = "P.USA", "data.i.GBR" = "i.GBR", "data.i.USA" = "i.USA", "data.S.GBR" = "S.GBR"))

#Update the columns in JPN df from data df
JPN <- within(JPN, rm(P.JPN, P.USA, i.JPN, i.USA))
JPN <- data.frame(data$i.USA, JPN)
JPN <- data.frame(data$i.JPN, JPN)
JPN <- data.frame(data$P.USA, JPN)
JPN <- data.frame(data$P.JPN, JPN)
JPN <- within(JPN, rm(S.JPN))
JPN <- data.frame(JPN, data$S.JPN)
JPN <- rename(JPN, c("data.P.JPN" = "P.JPN", "data.P.USA" = "P.USA", "data.i.JPN" = "i.JPN", "data.i.USA" = "i.USA", "data.S.JPN" = "S.JPN"))

#Update the columns in G6 df from data df
G6 <- within(G6, rm(P.G6, P.USA, i.G6, i.USA))
G6 <- data.frame(data$i.USA, G6)
G6 <- data.frame(data$i.G6, G6)
G6 <- data.frame(data$P.USA, G6)
G6 <- data.frame(data$P.G6, G6)
G6 <- within(G6, rm(S.G6))
G6 <- data.frame(G6, data$S.G6)
G6 <- rename(G6, c("data.P.G6" = "P.G6", "data.P.USA" = "P.USA", "data.i.G6" = "i.G6", "data.i.USA" = "i.USA", "data.S.G6" = "S.G6"))
#####################################

mylist<-c("CAN","FRA","DEU","ITA","JPN","GBR","G6") 
fama.result<-matrix(0,7,6)
for (cno in 1:7){
      init_date<-c(1973,3)
      start_date <- init_date+c(37,0)
      end_date<-c(2020,5) 
      country<-mylist[cno]
      Cdat<-data.frame(window(ts(get(country),start=init_date,freq=12),start=start_date, end=end_date))
      attach(Cdat)
      s<-log(get(paste("S",country,sep=".")))
      istar<-get(paste("i",country,sep="."))
      i<-100*(-(1+i.USA/100)^(1/12)+(1+istar/100)^(1/12))
      ni<-i[-length(i)]
      ex <- diff(100*s)+ni
      reg.fama<-lm(ex~ni)  # Fama regression
      detach(Cdat)
      reg.fama.nw<-matrix(coeftest(reg.fama,df=Inf,vcov=kernHAC(reg.fama,prewhite=F,bw=6,kernel="Bartlett")),2,4) # Calculate Newey-West standard errors
      coef0<-reg.fama.nw[1,1]
      std0<-reg.fama.nw[1,2]
      cf0<-c(-1.645*std0+coef0,1.645*std0+coef0)
      coef1<-reg.fama.nw[2,1]
      std1<-reg.fama.nw[2,2]
      cf1<-c(-1.645*std1+coef1,1.645*std1+coef1)
      fama.result[cno,]<-c(coef0,cf0,coef1,cf1)
}

# Output

write.csv(fama.result,"Table1.csv")
