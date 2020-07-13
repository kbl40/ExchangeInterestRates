################################################################################
# Project: Exchange Rates, Interest Rates, and the Risk Premium                #
# Program: for Figure 3                                                        #
# Dataset: dat73_1                                                             #
# Date: 01.24.2014                                                             #
# Author: Cheng-Ying Yang <anitac.c.yang@gmail.com>                            # 
################################################################################

rm(list=ls())           # clear workspace  

library(Biodem)         # import package 
library(sandwich)
library(vars)

load("dat73_1")         #import datset

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

#Function to process original data
mydat<-function(cno) {
       country<-mylist[cno]
       Cdat<-data.frame(window(ts(get(country),start=c(1973,3),freq=12),start=init_date, end=end_date))
       attach(Cdat)
       s<-log(get(paste("S",country,sep="."))) 
       p<-log(P.USA)
       pstar<-log(get(paste("P",country,sep=".")))
       pR<-100*(p-pstar)
       pi<-100*(diff(p,1)-diff(pstar,1))
       istar<-get(paste("i",country,sep="."))
       i<-100*((1+i.USA/100)^(1/12)-(1+istar/100)^(1/12))
       q<-100*(s+pstar-p)
       r<-i[-length(i)]-pi
       s<-100*s
       detach(Cdat)
       X1<-cbind(q,s,pR,i)
       colnames(X1)<-c("q","s","pR","i")
       X2<-cbind(q, c(0,pi), i)
       colnames(X2)<-c("q","pi","i")
       result<-list(X1<-X1,X2<-X2)
}

# Function for the estimation of ECM
ECMmodel<-function(x) {# x=Original data
          x=x
          q<-x[,1]
          iR<-x[,4]
          v<-cbind(diff(x[,2],1),diff(x[,3],1),diff(x[,4],1))
          colnames(v)<-c("ds","pi","di")
          obs<-dim(v)[1]
          nvar<-dim(v)[2]
          sample<-obs-nlag+1
          nlagecm<-nlag-1
          nbig<-nvar*nlagecm
          w<-matrix(0,sample,nbig)
          for (i in 1:nlagecm){
               for (n in 1:nvar){ 
                 w[,((i-1)*nvar+n)]<-v[(nlagecm-i+1):(obs-i),n]
               }
          }   
        wBig<-cbind(q[(nlagecm+1):obs],iR[(nlagecm+1):obs],w)
        yds<-v[(nlagecm+1):obs,1]
        ypi<-v[(nlagecm+1):obs,2]
        ydi<-v[(nlagecm+1):obs,3]
        eq1<-lm(yds~wBig)
        eq1.nw<-matrix(coeftest(eq1,df=Inf,vcov=NeweyWest(eq1)),12,4)
        eq2<-lm(ypi~wBig)   
        eq2.nw<-matrix(coeftest(eq2,df=Inf,vcov=NeweyWest(eq2)),12,4)     
        eq3<-lm(ydi~wBig)
        eq3.nw<-matrix(coeftest(eq3,df=Inf,vcov=NeweyWest(eq3)),12,4)
        A<-rbind(coef(eq1),coef(eq2),coef(eq3))
        U<-cbind(resid(eq1),resid(eq2),resid(eq3))
        Rsqu<-c(summary(eq1)$r.squared,summary(eq1)$adj.r.squared,summary(eq2)$r.squared,summary(eq2)$adj.r.squared,summary(eq3)$r.squared,summary(eq3)$adj.r.squared)
        result<-list(A<-A,wBig<-wBig)
        return(result)  
}

#Funtion for regression with Newey-West standard errors
model3<-function(A,x,w) {
    epi<-A[2,1]+w%*%(matrix(A[2,-1],11,1))
    er<-w[,2]-epi
    obs<-length(er)
    q<-w[,1]
    ds<-w[,3]
    nlagecm<-nlag-1
    lq<-x[nlagecm:(obs+2),1]
    li<-x[nlagecm:(obs+2),4]
    dq<-q-lq
    dsli<-ds-li 
    obsq<-length(dq) 

    q0<-q               
    q0hat<-q[1:(obsq-1)]
    er0<-er
    er0hat<-er[1:(obsq-1)] 
    qer0.result<-lm(q0~er0)  
    qerhat0.result<-lm(q0hat~er0hat)
    qer0.nw<-matrix(coeftest(qer0.result,df=Inf,vcov=NeweyWest(qer0.result)),2,4) #Newey-West standard errors
    qerhat0.nw<-matrix(coeftest(qerhat0.result,df=Inf,vcov=NeweyWest(qerhat0.result)),2,4) #Newey-West standard errors
    qer.nw.beta<-c(qerhat0.nw[2,1],qer0.nw[2,1],0,0)
    qer.nw.se<-c(qerhat0.nw[2,2],qer0.nw[2,2],0,0)

    for (k in 1:100){
        qk<-q[(k+1):obsq]
        qhatk<-q[(k+1):(obsq-1)]
        dqk<-dq[(k+1):obsq]
        dslik<-dsli[(k+1):obsq]
	  erk<-er[1:(obsq-k)]
        erhatk<-er[1:(obsq-k-1)]

        # Regression of q(t+k) on E(r)
        qerk.result<-lm(qk~erk)
        qerhatk.result<-lm(qhatk~erhatk)
        dqerk.result<-lm(dqk~erk) 
        dslierk.result<-lm(dslik~erk) 

        qerk.nw<-matrix(coeftest(qerk.result,df=Inf,vcov=NeweyWest(qerk.result)),2,4) #Newey-West standard errors	    
        dqerk.nw<-matrix(coeftest(dqerk.result,df=Inf,vcov=NeweyWest(dqerk.result)),2,4) #Newey-West standard errors
        qerhatk.nw<-matrix(coeftest(qerhatk.result,df=Inf,vcov=NeweyWest(qerhatk.result)),2,4) #Newey-West standard errors	     
        dslierk.nw<-matrix(coeftest(dslierk.result,df=Inf,vcov=NeweyWest(dslierk.result)),2,4) #Newey-West standard errors
        qer.nw.beta<-rbind(qer.nw.beta,c(qerhatk.nw[2,1],qerk.nw[2,1],dqerk.nw[2,1],dslierk.nw[2,1]))
        qer.nw.se<-rbind(qer.nw.se,c(qerhatk.nw[2,2],qerk.nw[2,2],dqerk.nw[2,2],dslierk.nw[2,2]))
        }            
        result<-list(qer.nw.beta<-qer.nw.beta,qer.nw.se<-qer.nw.se)
    return(result)
}

# Main Program

# Parameters

nlag<-4                # number of lags
tmax=1000              # number of bottstrap trials
set.seed(20)             # set random seed
init_date<-c(2010,3)
end_date<-c(2020,5) 

# Initialize variables to store the results
beta.true.all<-NULL

for (cno in 1:7){
    newdata<-mydat(cno) #Call program "mydat" to process original data
    X1<-newdata[[1]]
    ECM<-ECMmodel(X1)
    A<-ECM[[1]]
    wBig<-ECM[[2]]
    reg2<-model3(A,X1,wBig)  
    beta.true<-reg2[[1]][,4] #slop estimates
    nwse.true<-reg2[[2]][,4] #Newey West standard errors of slop estimates 

    beta.ci90<-cbind(-beta.true-1.645*nwse.true,-beta.true+1.645*nwse.true)
    
    mycoef<-cbind(-beta.true,beta.ci90)
    colnames(mycoef)<-c("beta","beta.l90","beta.u90") 

    write.csv(mycoef[-1,],file=paste("fig3",mylist[cno],"csv",sep="."),row.names=F) 

}

