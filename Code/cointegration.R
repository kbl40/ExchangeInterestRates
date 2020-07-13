################################################################################
# Project: Exchange Rates, Interest Rates, and the Risk Premium                #
# Program: test for cointgration of s and pR                                   #
# Dataset: dat73_1                                                             #
# Date: 09.23.2013                                                             #
# Author: Cheng-Ying Yang <anita.c.yang@gmail.com>                             # 
################################################################################

rm(list=ls())
 
library(car)
library(Biodem)         
library(sandwich)
library(vars)

load("dat73_1") 

#######
#Need to read in the updated dataset and append the new data after row 440
# data=read.csv("dat73_New.csv",header= TRUE)
# 
# #Extend the existing country df's to easily append
# CAN[(nrow(CAN)+(nrow(data)-nrow(CAN))),] <- NA
# FRA[(nrow(FRA)+(nrow(data)-nrow(FRA))),] <- NA
# DEU[(nrow(DEU)+(nrow(data)-nrow(DEU))),] <- NA
# GBR[(nrow(GBR)+(nrow(data)-nrow(GBR))),] <- NA
# ITA[(nrow(ITA)+(nrow(data)-nrow(ITA))),] <- NA
# JPN[(nrow(JPN)+(nrow(data)-nrow(JPN))),] <- NA
# G6[(nrow(G6)+(nrow(data)-nrow(G6))),] <- NA
# 
# #Update the columns in CAN df from data df
# CAN <- within(CAN, rm(P.CAN, P.USA, i.CAN, i.USA))
# CAN <- data.frame(data$i.USA, CAN)
# CAN <- data.frame(data$i.CAN, CAN)
# CAN <- data.frame(data$P.USA, CAN)
# CAN <- data.frame(data$P.CAN, CAN)
# CAN <- within(CAN, rm(S.CAN))
# CAN <- data.frame(CAN, data$S.CAN)
# CAN <- rename(CAN, c("data.P.CAN" = "P.CAN", "data.P.USA" = "P.USA", "data.i.CAN" = "i.CAN", "data.i.USA" = "i.USA", "data.S.CAN" = "S.CAN"))
# 
# #Update the columns in DEU df from data df
# DEU <- within(DEU, rm(P.DEU, P.USA, i.DEU, i.USA))
# DEU <- data.frame(data$i.USA, DEU)
# DEU <- data.frame(data$i.DEU, DEU)
# DEU <- data.frame(data$P.USA, DEU)
# DEU <- data.frame(data$P.DEU, DEU)
# DEU <- within(DEU, rm(S.DEU))
# DEU <- data.frame(DEU, data$S.DEU)
# DEU <- rename(DEU, c("data.P.DEU" = "P.DEU", "data.P.USA" = "P.USA", "data.i.DEU" = "i.DEU", "data.i.USA" = "i.USA", "data.S.DEU" = "S.DEU"))
# 
# #Update the columns in FRA df from data df
# FRA <- within(FRA, rm(P.FRA, P.USA, i.FRA, i.USA))
# FRA <- data.frame(data$i.USA, FRA)
# FRA <- data.frame(data$i.FRA, FRA)
# FRA <- data.frame(data$P.USA, FRA)
# FRA <- data.frame(data$P.FRA, FRA)
# FRA <- within(FRA, rm(S.FRA))
# FRA <- data.frame(FRA, data$S.FRA)
# FRA <- rename(FRA, c("data.P.FRA" = "P.FRA", "data.P.USA" = "P.USA", "data.i.FRA" = "i.FRA", "data.i.USA" = "i.USA", "data.S.FRA" = "S.FRA"))
# 
# #Update the columns in ITA df from data df
# ITA <- within(ITA, rm(P.ITA, P.USA, i.ITA, i.USA))
# ITA <- data.frame(data$i.USA, ITA)
# ITA <- data.frame(data$i.ITA, ITA)
# ITA <- data.frame(data$P.USA, ITA)
# ITA <- data.frame(data$P.ITA, ITA)
# ITA <- within(ITA, rm(S.ITA))
# ITA <- data.frame(ITA, data$S.ITA)
# ITA <- rename(ITA, c("data.P.ITA" = "P.ITA", "data.P.USA" = "P.USA", "data.i.ITA" = "i.ITA", "data.i.USA" = "i.USA", "data.S.ITA" = "S.ITA"))
# 
# #Update the columns in GBR df from data df
# GBR <- within(GBR, rm(P.GBR, P.USA, i.GBR, i.USA))
# GBR <- data.frame(data$i.USA, GBR)
# GBR <- data.frame(data$i.GBR, GBR)
# GBR <- data.frame(data$P.USA, GBR)
# GBR <- data.frame(data$P.GBR, GBR)
# GBR <- within(GBR, rm(S.GBR))
# GBR <- data.frame(GBR, data$S.GBR)
# GBR <- rename(GBR, c("data.P.GBR" = "P.GBR", "data.P.USA" = "P.USA", "data.i.GBR" = "i.GBR", "data.i.USA" = "i.USA", "data.S.GBR" = "S.GBR"))
# 
# #Update the columns in JPN df from data df
# JPN <- within(JPN, rm(P.JPN, P.USA, i.JPN, i.USA))
# JPN <- data.frame(data$i.USA, JPN)
# JPN <- data.frame(data$i.JPN, JPN)
# JPN <- data.frame(data$P.USA, JPN)
# JPN <- data.frame(data$P.JPN, JPN)
# JPN <- within(JPN, rm(S.JPN))
# JPN <- data.frame(JPN, data$S.JPN)
# JPN <- rename(JPN, c("data.P.JPN" = "P.JPN", "data.P.USA" = "P.USA", "data.i.JPN" = "i.JPN", "data.i.USA" = "i.USA", "data.S.JPN" = "S.JPN"))
# 
# #Update the columns in G6 df from data df
# G6 <- within(G6, rm(P.G6, P.USA, i.G6, i.USA))
# G6 <- data.frame(data$i.USA, G6)
# G6 <- data.frame(data$i.G6, G6)
# G6 <- data.frame(data$P.USA, G6)
# G6 <- data.frame(data$P.G6, G6)
# G6 <- within(G6, rm(S.G6))
# G6 <- data.frame(G6, data$S.G6)
# G6 <- rename(G6, c("data.P.G6" = "P.G6", "data.P.USA" = "P.USA", "data.i.G6" = "i.G6", "data.i.USA" = "i.USA", "data.S.G6" = "S.G6"))
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
       X<-cbind(s,pR,i)
       colnames(X)<-c("s","pR","i")
       result<-list(X<-X)
}

# Function for the estimation of restricted VAR
model4<-function(x) {# x=Original data
        x=x
        nlag<-4 
        obs=dim(x)[[1]]
        ds<-diff(x[,1],1)
        pi<-diff(x[,2],1)
        iR<-x[2:obs,3]
        xnew<-cbind(ds,pi,iR)
        colnames(xnew)<-c("ds","pi","i")
        obs<-dim(xnew)[1]
        nvar<-ncol(xnew)  # the number of equations in the system
        nbig<-nvar*(nlag+1)  #adjusted by lags
        sample<-obs-nlag
        v<-matrix(0,sample,nbig)
        for (i in 1:(nlag+1)){
             for (n in 1:nvar){ 
                 v[,((i-1)*nvar+n)]<-xnew[(nlag-i+2):(obs-i+1),n]
             }
        }   
        y1<-v[,1]
        y2<-v[,2] 
        y3<-v[,3]
        vnew<-cbind(v[,4:12],v[,15])
        eq1<-lm(y1~vnew)
        eq2<-lm(y2~vnew)        
        eq3<-lm(y3~vnew)
        A<-rbind(coef(eq1),coef(eq2),coef(eq3))
        A0<-A[,1]
        A1<-A[,2:4]
        A2<-A[,5:7]
        A3<-A[,8:10]
        A4<-matrix(0,nvar,nvar)
        A4[,1]<-matrix(0,3,1)
        A4[,2]<-matrix(0,3,1)
        A4[,3]<-A[,11]   
       
        B0<-A0
        B1<-K+A1
        B2<-A2-A1%*%K
        B3<-A3-A2%*%K
        B4<-A4-A3%*%K
        residuals<-cbind(resid(eq1),resid(eq2),resid(eq3))
        B<-cbind(B1,B2,B3,B4)   
        result<-list(B0<-B0,B<-B,residual<-residuals)
}

# Function to generate bootstrap random sample
mynx<-function(x,y){
       nlag<-4 
       nvar<-dim(x)[[2]]
       z<-matrix(0,nvar,nlag)
       for (n in 1:nlag){
       z[,n]<-x[(nlag-n+1),]
       }
       re<-y[[3]]
       Ac<-matrix(y[[1]],3,1)
       A<-list()
       for (n in 1:nlag){
       A[[n]]<-y[[2]][,((n-1)*nvar+1):(n*nvar)]
       }
       n.resid<-dim(re)[1]
       z.series<-NULL
       j<-1
       while(j<=865){
            random.re<-sample(1:n.resid,1,replace=T)
	      pseudo.re<-re[random.re,]   
	      nz<-Ac+A[[1]]%*%z[,1]+A[[2]]%*%z[,2]+A[[3]]%*%z[,3]+A[[4]]%*%z[,4]+matrix(pseudo.re,3,1)
            z<-cbind(nz,z[,(-nlag)])
	      z.series<-cbind(z.series,nz)
	      j=j+1
       }
       nx<-t(z.series[,-(1:500)])
       result<-list(nx<-nx)
}

# Main Program

# Parameters

tmax=2000              # number of bottstrap trials
set.seed(20)             # set random seed
init_date<-c(1979,6)
end_date<-c(2009,10) 
K<-rbind(c(1,0,0),c(0,1,0),c(0,0,0))

beta.ci.all<-NULL
tstat.ci.all<-NULL
beta.true.all<-NULL
tstat.true.all<-NULL
for (cno in 1:7){
    newdata<-mydat(cno) #Call program "mydat" to process original data
    X<-newdata[[1]]
    reg<-model4(X)

    q<-X[,1]-X[,2]
    dq<-diff(q,1)
    iR<-X[,3]
    ds<-diff(X[,1])
    pi<-diff(X[,2])
    di<-diff(X[,3])
    nxnew<-cbind(ds,pi,di)
    obs<-dim(nxnew)[1]
    nlag<-3
    nvar<-ncol(nxnew)  # the number of equations in the system
    nbig<-nvar*(nlag+1)  #adjusted by lags
    sample<-obs-nlag
    v<-matrix(0,sample,nbig)
    for (i in 1:(nlag+1)){
         for (n in 1:nvar){ 
              v[,((i-1)*nvar+n)]<-nxnew[(nlag-i+2):(obs-i+1),n]
         }
    }  
    vnew<-cbind(q[(nlag+1):obs],iR[(nlag+1):obs],v[,(nlag+1):nbig])
    y1<-v[,1]
    y2<-v[,2]
    y3<-dq[(nlag+1):obs]
    eq1<-lm(y1~vnew)
    eq1.nw<-matrix(coeftest(eq1,df=Inf,vcov=NeweyWest(eq1)),12,4)
    g11<-eq1.nw[2,1:2]
    g11.t<-eq1.nw[2,3]
    eq2<-lm(y2~vnew) 
    eq2.nw<-matrix(coeftest(eq2,df=Inf,vcov=NeweyWest(eq2)),12,4)
    g21<-eq2.nw[2,1:2]
    g21.t<-eq2.nw[2,3]
    eq3<-lm(y3~vnew)
    eq3.nw<-matrix(coeftest(eq3,df=Inf,vcov=NeweyWest(eq3)),12,4)
    h1<-eq3.nw[2,1:2]
    h1.t<-eq3.nw[2,3]
    beta.true<-cbind(g11,g21,h1)
    tstat.true<-c(g11.t,g21.t,h1.t) 
    
    #Generate 2000 times bootstrap results
    myresult<-array(0,dim=c(3,2,tmax))
   
    t<-1
    while (t<=tmax){
           mynx1<-mynx(X,reg)
           nx1<-mynx1[[1]]
           colnames(nx1)<-c("s","pR","iR")
           q<-nx1[,1]-nx1[,2]
           dq<-diff(q,1)
           iR<-nx1[,3]
           ds<-diff(nx1[,1])
           pi<-diff(nx1[,2])
           di<-diff(nx1[,3])
           nxnew<-cbind(ds,pi,di)
       
           obs<-dim(nxnew)[1]
           nlag<-3
           nvar<-ncol(nxnew)  # the number of equations in the system
           nbig<-nvar*(nlag+1)  #adjusted by lags
           sample<-obs-nlag
           v<-matrix(0,sample,nbig)
           for (i in 1:(nlag+1)){
               for (n in 1:nvar){ 
                   v[,((i-1)*nvar+n)]<-nxnew[(nlag-i+2):(obs-i+1),n]
               }
           }  
           vnew<-cbind(q[(nlag+1):obs],iR[(nlag+1):obs],v[,(nlag+1):nbig])
           y1<-v[,1]
           y2<-v[,2]
           y3<-dq[(nlag+1):obs]
           eq1<-lm(y1~vnew)
           eq1.nw<-matrix(coeftest(eq1,df=Inf,vcov=NeweyWest(eq1)),12,4)
           g11<-eq1.nw[2,1]
           g11.t<-eq1.nw[2,3]
           eq2<-lm(y2~vnew) 
           eq2.nw<-matrix(coeftest(eq2,df=Inf,vcov=NeweyWest(eq2)),12,4)
           g21<-eq2.nw[2,1]
           g21.t<-eq2.nw[2,3]
           eq3<-lm(y3~vnew)
           eq3.nw<-matrix(coeftest(eq3,df=Inf,vcov=NeweyWest(eq3)),12,4)
           h1<-eq3.nw[2,1]
           h1.t<-eq3.nw[2,3]
           myresult[,,t]<-rbind(c(g11,g11.t),c(g21,g21.t),c(h1,h1.t))
           t<-t+1  
     }

     # Combine bootstrap results 
     ci1<-c(0.01,0.05,0.10)
     ci2<-c(0.99,0.95,0.90)
     #the number of coefficient estimates is 3  
     beta.ci<-matrix(0,3,3)
     tstat.ci<-matrix(0,3,3)

     n<-1
     beta.q<-quantile(sort(myresult[n,1,]),probs=ci1) 
     tstat.q<-quantile(sort(myresult[n,2,]),probs=ci1) 
     beta.ci[n,]<-beta.q
     tstat.ci[n,]<-tstat.q       

     n<-n+1 
     beta.q<-quantile(sort(myresult[n,1,]),probs=ci2) 
     tstat.q<-quantile(sort(myresult[n,2,]),probs=ci2) 
     beta.ci[n,]<-beta.q
     tstat.ci[n,]<-tstat.q

     n<-n+1
     beta.q<-quantile(sort(myresult[n,1,]),probs=ci1) 
     tstat.q<-quantile(sort(myresult[n,2,]),probs=ci1) 
     beta.ci[n,]<-beta.q
     tstat.ci[n,]<-tstat.q 
     
     beta.true.all<-rbind(beta.true.all,beta.true)
     tstat.true.all<-rbind(tstat.true.all,tstat.true)
     beta.ci.all<-rbind(beta.ci.all,beta.ci) 
     tstat.ci.all<-rbind(tstat.ci.all,tstat.ci) 
}
write.csv(beta.true.all,"beta.true.cointegration.csv")
# write.csv(tstat.true.all,"tstat.true.cointegration.csv")
write.csv(beta.ci.all,"beta.ci.cointegration.csv")
# write.csv(tstat.ci.all,"tstat.ci.cointegration.csv")
