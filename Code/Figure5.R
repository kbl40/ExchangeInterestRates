################################################################################
# Project: Exchange Rates, Interest Rates, and the Risk Premium                #
# Program: for Figure 5 (Using VECM12/rstVAR13)                                #
# Dataset: dat73_1                                                             #
# Date: 02.04.2014                                                             #
# Author: Cheng-Ying Yang <anita.c.yang@gmail.com>                             # 
################################################################################

rm(list=ls())     # clear workspace

library(Biodem)   # import package
library(vars)
library(sandwich) # import dataset
library(foreign)

load("dat73_1")

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

#Function for the estimation 

model5<-function(x2, myk) {

        x2=x2
        obs<-dim(x2)[1]
        nvar<-ncol(x2)  # the number of equations in the system
        nbig<-nvar*nlag  #adjusted by lags
        sample<-obs-nlag+1
        q<-x2[,1]
        pi<-x2[2:obs,2]
        iR<-x2[,3] 
        qt<-q-mean(q)
        pit<-pi-mean(pi)
        it<-iR-mean(iR)
        xt<-cbind(qt,c(0,pit),it)#demean variables
        Xbig<-matrix(0,sample,nbig)  #Construct Xbig 
        Xtbig<-matrix(0,sample,nbig) #Construct Xtbig 
        for (n in 1:nlag){
             Xbig[,(nvar*(n-1)+1):(nvar*n)]<-as.matrix(x2[(nlag+1-n):(obs+1-n),])
             Xtbig[,(nvar*(n-1)+1):(nvar*n)]<-as.matrix(xt[(nlag+1-n):(obs+1-n),])
        }

        v<-matrix(0,(obs-nlag),nbig)
        for (i in 1:nlag){
             for (n in 1:nvar){ 
                 v[,((i-1)*nvar+n)]<-x2[(nlag-i+1):(obs-i),n]
             }
        }   
        v<-cbind(v[,1:(nbig-2)],v[,nbig])
        y1<-x2[(nlag+1):obs,1]
        y2<-x2[(nlag+1):obs,2] 
        y3<-x2[(nlag+1):obs,3]
        eq1<-lm(y1~v)
        eq2<-lm(y2~v)        
        eq3<-lm(y3~v)
        A<-rbind(coef(eq1),coef(eq2),coef(eq3))
        cons<-A[,1]
        Abig<-matrix(0,nvar,nbig)
        for (n in 1:(nlag-1)){
             Abig[,((n-1)*3+1):(n*3)]<-A[,((n-1)*3+2):(n*3+1)]
        }
        Abig[,(nlag*3-2)]<-A[,(nlag*3-1)]
        Abig[,(nlag*3-1)]<-matrix(0,3,1)
        Abig[,(nlag*3)]<-A[,(nlag*3)]

        I<-diag(nvar*(nlag-1))
        O<-matrix(0,nvar*(nlag-1),nvar)
        Ibig<-diag(nbig)
        Abig<-rbind(Abig,cbind(I,O))
        cons<-c(cons, rep(0,(nlag-1)*nvar))
        evAbig<-eigen(Abig)$values #compute eigenvalues of Abig
        I_A<-as.matrix(Ibig-Abig)
        I_Ainv<-as.matrix(solve(I_A))

	  e1<-c(1,rep(0,nbig-1))
        e2<-c(0,1,rep(0, nbig-2))
	  e3<-c(0,0,1,rep(0, nbig-3))
	  
        epi<-as.vector(e2%*%(Abig%*%t(Xtbig)))
	  er<-as.vector(e3%*%(t(Xtbig)))- epi
        ert<-as.vector((e3-e2%*%Abig)%*%mtx.exp(Abig,myk-1)%*%(t(Xtbig)))
	  edqt<-as.vector(e1%*%((Abig-Ibig)%*%mtx.exp(Abig,myk-1)%*%t(Xtbig)))
        elam<-edqt-ert
	  ert.lm<-lm(ert~0+er)   # regression of E(r_t+j-1) on r_t
        ert.lm.nw<-matrix(coeftest(ert.lm,df=Inf,vcov=NeweyWest(ert.lm)),1,2)
	  edqt.lm<-lm(edqt~0+er) # regression of E(d(q_t+j)) on r_t
        edqt.lm.nw<-matrix(coeftest(edqt.lm,df=Inf,vcov=NeweyWest(edqt.lm)),1,2)
	  elam.lm<-lm(elam~0+er) # regression of E(l_t+j-1) on r_t
        elam.lm.nw<-matrix(coeftest(elam.lm,df=Inf,vcov=NeweyWest(elam.lm)),1,2)
	  result<-list(elam.lm.nw<-elam.lm.nw, ert.lm.nw<-ert.lm.nw, edqt.lm<-edqt.lm.nw)       
        return(result)  
}

#Main program starts here!

nlag<-13                  # number of lags
init_date<-c(1979,6)
end_date<-c(2020,5) 

for (cno in 1:7){
     mycoef<-NULL
     newdata<-mydat(cno)
     X1<-newdata[[1]]
     X2<-newdata[[2]]

     for (k in 1:121){
          reg<-model5(X2,k)
          coef.ci.blj<-c(-reg[[1]][1],-reg[[1]][1]-1.645*reg[[1]][2],-reg[[1]][1]+1.645*reg[[1]][2])
          coef.ci.brj<-c(-reg[[2]][1],-reg[[2]][1]-1.645*reg[[2]][2],-reg[[2]][1]+1.645*reg[[2]][2])
          coef.ci.bqj<-c(-reg[[3]][1],-reg[[3]][1]-1.645*reg[[3]][2],-reg[[3]][1]+1.645*reg[[3]][2])
             
          mycoef<-rbind(mycoef,coef.ci.blj)
     }
     colnames(mycoef)<-c("blj","blj.l90","blj.u90")

     write.csv(mycoef,file=paste("fig5_VECM12",mylist[cno],"csv",sep="."),row.names=F)    
}

