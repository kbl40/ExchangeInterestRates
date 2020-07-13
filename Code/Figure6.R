################################################################################
# Project: Exchange Rates, Interest Rates, and the Risk Premium                #
# Program: for Figure 6 (with auxiliary_variables)                             #
# Dataset: dat73_1                                                             #
# Date: 02.04.2014                                                             #
# Author: Cheng-Ying Yang <anita.c.yang@gmail.com>                             # 
################################################################################

rm(list=ls())     # clear workspace

library(Biodem)   # import package
library(vars)
library(sandwich) # import dataset
library(foreign)

data=read.csv("dat73_New.csv",header= TRUE)

mylist<-c("CAN","FRA","DEU","ITA","JPN","GBR","G6")

#Function to process original data
mydat<-function(cno) {
       country<-mylist[cno]
       Cdat<-data.frame(data)
       attach(Cdat)
       s<-log(get(paste("S",country,sep="."))) 
       p<-log(P.USA)
       pstar<-log(get(paste("P",country,sep=".")))
       pR<-100*(p-pstar)
       pi<-100*(diff(p,1)-diff(pstar,1))
       istar<-get(paste("i",country,sep="."))
       i<-100*((1+i.USA/100)^(1/12)-(1+istar/100)^(1/12))
       bstar<-data[paste("b",country,sep=".")]/1200
       busa<-data["b.USA"]/1200
       b<-100*(busa-bstar)
       wusa<-data["w.USA"]
       wstar<-data[paste("w",country,sep=".")]   
       w<-log(wusa)-log(wstar)
       g<-log(g.World)
       l<-log(l.World)
       pi.g<-100*(diff(g,1))
       pi.l<-100*(diff(l,1))
       q<-100*(s+pstar-p)
       r<-i[-length(i)]-pi
       s<-100*s
       detach(Cdat)
       X1<-cbind(q,s,pR,i)
       colnames(X1)<-c("q","s","pR","i")
       X2<-cbind(q, c(0,pi), i, w, b, c(0,pi.g), c(0,pi.l))
       colnames(X2)<-c("q","pi","i","w","b","pi.g","pi.l")
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
        w<-x2[,4]
        b<-x2[,5]  
        pi.g<-x2[2:obs,6]
        pi.l<-x2[2:obs,7]
        qt<-q-mean(q)
        pit<-pi-mean(pi)
        it<-iR-mean(iR)
        wt<-w-mean(w)
        bt<-b-mean(b)
        pigt<-pi.g-mean(pi.g)
        pilt<-pi.l-mean(pi.l) 
        xt<-cbind(qt,c(0,pit),it,wt,bt,c(0,pigt),c(0,pilt))#demean variables
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
        v<-cbind(v[,1:(nbig-6)],v[,(nbig-4):(nbig-2)])
        y1<-x2[(nlag+1):obs,1]
        y2<-x2[(nlag+1):obs,2] 
        y3<-x2[(nlag+1):obs,3]
        y4<-x2[(nlag+1):obs,4]
        y5<-x2[(nlag+1):obs,5] 
        y6<-x2[(nlag+1):obs,6]
        y7<-x2[(nlag+1):obs,7]
        eq1<-lm(y1~v)
        eq2<-lm(y2~v)        
        eq3<-lm(y3~v)
        eq4<-lm(y4~v)
        eq5<-lm(y5~v)        
        eq6<-lm(y6~v)
        eq7<-lm(y7~v)
        A<-rbind(coef(eq1),coef(eq2),coef(eq3),coef(eq4),coef(eq5),coef(eq6),coef(eq7))
        cons<-A[,1]
        Abig<-matrix(0,nvar,nbig)
        for (n in 1:(nlag-1)){
             Abig[,((n-1)*nvar+1):(n*nvar)]<-A[,((n-1)*nvar+2):(n*nvar+1)]
        }
        Abig[,(nlag*nvar-6)]<-A[,(nlag*nvar-5)]
        Abig[,(nlag*nvar-5)]<-matrix(0,nvar,1)
        Abig[,(nlag*nvar-4)]<-A[,(nlag*nvar-4)]
        Abig[,(nlag*nvar-3)]<-A[,(nlag*nvar-3)]
        Abig[,(nlag*nvar-2)]<-A[,(nlag*nvar-2)]
        Abig[,(nlag*nvar-1)]<-matrix(0,nvar,1)
        Abig[,(nlag*nvar)]<-matrix(0,nvar,1)

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

nlag<-4                  # number of lags
init_date<-c(1979,6)
end_date<-c(2009,10) 

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

     write.csv(mycoef,file=paste("fig6_VECM_new",mylist[cno],"csv",sep="."),row.names=F)    
}

