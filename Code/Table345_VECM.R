################################################################################
# Project: Exchange Rates, Interest Rates, and the Risk Premium                #
# Program: for Table 3, 4, 45                                                  #
# Dataset: dat73_1                                                             #
# Date: 09.19.2013                                                             #
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

mylist<-c("CAN","DEU","FRA","G6","GBR","ITA","JPN")

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

#Function for the estimation of Table 2,3,4
model5<-function(x2) {

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
        Abig[,1:3]<-A[,2:4]
        Abig[,4:6]<-A[,5:7]
        Abig[,7:9]<-A[,8:10]
        Abig[,10]<-A[,11]
        Abig[,11]<-matrix(0,3,1)
        Abig[,12]<-A[,12]   
        I<-diag(nvar*(nlag-1))
        O<-matrix(0,nvar*(nlag-1),nvar)
        Ibig<-diag(nbig)
        Abig<-rbind(Abig,cbind(I,O))
        cons<-c(cons, rep(0,(nlag-1)*nvar))
        evAbig<-eigen(Abig)$values #compute eigenvalues of Abig
        I_A<-as.matrix(Ibig-Abig)
        I_Ainv<-as.matrix(solve(I_A))

	  Xq<-Xbig[,1]
        dXq<-diff(Xq)
	  Xqt<-Xtbig[,1]
	  e1<-c(1,rep(0,nbig-1))
        e2<-c(0,1,rep(0, nbig-2))
	  e3<-c(0,0,1,rep(0, nbig-3))
	  
        epi<-as.vector(e2%*%(cons+Abig%*%t(Xbig)))
        epit<-as.vector(e2%*%(Abig%*%t(Xtbig)))
	  er<-as.vector(e3%*%(t(Xbig)))- epi
        ert<-as.vector(e3%*%(t(Xtbig)))- epit
	  Rbig<-as.vector((e3-e2%*%Abig)%*%I_Ainv%*%t(Xtbig)) 
	  Pbig<--(Xqt+Rbig)
	  edq<-as.vector(e1%*%(cons+(Abig-Ibig)%*%t(Xbig)))
	  dXq<-diff(Xq)
	  ner<-er[-length(er)]
	  nepi<-epi[-length(epi)]
        
        # Table 2: regression of d(q) on E(r)
	      dXqner<-dXq-ner
            dqer.result<-lm(dXqner~ner)
            #dqer.nw<-matrix(coeftest(dqer.result,df=Inf,vcov=kernHAC(dqer.result,prewhite=F,bw=6,kernel="Bartlett")),2,4)
            dqer.nw<-matrix(coeftest(dqer.result,df=Inf,vcov=NeweyWest(dqer.result)),2,4)

        # Table 3: regression of q on E(r) 
            qer.result<-lm(Xq~er)
	      #qer.nw<-matrix(coeftest(qer.result,df=Inf,vcov=kernHAC(qer.result,prewhite=F,bw=6,kernel="Bartlett")),2,4)	      
            qer.nw<-matrix(coeftest(qer.result,df=Inf,vcov=NeweyWest(qer.result)),2,4)

        # Table 4: regression of Lam on r
            erP.result<-lm(Pbig~0+ert)
	      #erP.nw<-matrix(coeftest(erP.result,df=Inf,vcov=kernHAC(erP.result,prewhite=F,bw=6,kernel="Bartlett")),1,4)
	      erP.nw<-matrix(coeftest(erP.result,df=Inf,vcov=NeweyWest(erP.result)),1,4)

        residuals<-cbind(resid(eq1),resid(eq2),resid(eq3))
	  result<-list(dqer.nw<-dqer.nw,qer.nw<-qer.nw,erP.nw<-erP.nw,re<-residuals,cons<-cons[1:3], Abig<-Abig[1:3,],evAbig)
        
        return(result)  
}

# Function to generate bootstrap random sample
mynx<-function(x,y){
        nvar<-dim(x)[[2]] 
        obs<-dim(x)[[1]]
        q<-x[,1]
        pi<-x[2:obs,2]
        iR<-x[,3] 
        qmean<-mean(q)
        pimean<-mean(pi)
        imean<-mean(iR)
       z<-c(qmean,pimean,imean)
       re<-y[[4]]
       Ac<-matrix(y[[5]],3,1)
       A<-list()
       for (n in 1:nlag){
       A[[n]]<-y[[6]][,((n-1)*nvar+1):(n*nvar)]
       }
       n.resid<-dim(re)[1]
       z<-matrix(z,3,nlag)
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

coefVECM<-function(y){
    A0<-matrix(y[[5]],3,1)
    nvar<-length(y[[5]])
    A1<-y[[6]][,1:nvar]
    A2<-y[[6]][,(nvar+1):(nvar*2)]     
    A3<-y[[6]][,(nvar*2+1):(nvar*3)] 
    A4<-y[[6]][,(nvar*3+1):(nvar*4)] 
    G<-F_inv%*%(A1+A2+A3+A4-diag(3))%*%H%*%F
    C0<-F_inv%*%A0
    C1<-F_inv%*%(A1-(A1+A2+A3+A4)%*%H)%*%F
    C2<-F_inv%*%(A2-(A2+A3+A4)%*%H)%*%F
    C3<-F_inv%*%(A3-(A3+A4)%*%H)%*%F
    result<-list(G<-G,C0<-C0,C1<-C1,C2<-C2,C3<-C3)
}


#Main program starts here!

nlag<-4                  # number of lags
tmax=1000                # number of bottstrap trials
set.seed(20)             # set random seed
init_date<-c(2010,3)
end_date<-c(2020,5) 

F<-rbind(c(1,-1,0),c(0,1,0),c(0,0,1))
F_inv<-solve(F)
H<-rbind(c(1,0,0),c(0,0,0),c(0,0,1))

# Initialize variables to store the results
# Initialize variables to store the results
beta95.3<-NULL
beta95.4<-NULL
beta95.5<-NULL
beta90.3<-NULL
beta90.4<-NULL
beta90.5<-NULL
beta.co.3<-NULL
beta.co.4<-NULL
beta.co.5<-NULL
g.all<-NULL
it<-matrix(0,7,1)
ABS_all<-NULL

pECM.all<-NULL
rstA.all<-NULL

mean_median_bs<-matrix(0,35,2)

for (cno in 1:7){
    newdata<-mydat(cno) #Call program "mydat" to process original data
    X1<-newdata[[1]]
    X2<-newdata[[2]]
    reg2<-model5(X2)  #Regression results for Model 2
    VECM<-coefVECM(reg2)
    A0<-matrix(reg2[[5]],3,1)
    rstA<-cbind(A0,reg2[[6]])
    G<-VECM[[1]]
    C0<-VECM[[2]]
    C1<-VECM[[3]]
    C2<-VECM[[4]]  
    C3<-VECM[[5]]   
    pECM<-cbind(G,C0,C1,C2,C3)

    beta.true<-rbind(reg2[[1]][,1:2],reg2[[2]][,1:2],reg2[[3]][,1:2]) #estimate and standard error

    # Generate 1000 times bootstrap results

    myresult<-array(0,dim=c(5,2,tmax))
    q.result<-matrix(0,tmax,5)
    g.result<-matrix(0,tmax,2)

    it2<-0
    ABS2<-NULL
    re.t2<-NULL
    t2<-1
    while (t2<=tmax){     
           mynx2<-mynx(X2,reg2)
           nx2<-mynx2[[1]]
           colnames(nx2)<-c("q","pi","i")
          
           #use the pseudo-sample to estimate VAR and check whether any of the eigenvalues of the coefficient matrix are greater than one in absolute value 
           psbs2<-model5(nx2)
           while (any(abs(psbs2[[7]])>1)) {
                  it2<-it2+1
                  re.t2<-c(re.t2,t2)
                  ABS2<-rbind(ABS2,abs(psbs2[[7]]))
                  mynx2<-mynx(X2,reg2) #Call program "mynx" to generate another bootstrap data sample
                  nx2<-mynx2[[1]]
                  colnames(nx2)<-c("q","pi","i")
                  psbs2<-model5(nx2) #Use the new bootstrap data sample to estimate VAR
           }
           
           bs2<-psbs2
           bsVECM<-coefVECM(bs2)
           bsG<-bsVECM[[1]]
           g.result[t2,]<-c(bsG[1,1],bsG[1,1]-bsG[2,1])
           myresult[,,t2]<-rbind(bs2[[1]][,1:2],bs2[[2]][,1:2],bs2[[3]][,1:2])  
           q.result[t2,]<-(myresult[,1,t2]-beta.true[,1])/myresult[,2,t2] 
           t2<-t2+1
    }
    it[cno,]<-it2

    # Get mean and median of bootstrapping parameter estimates 

    for (n in 1:5){
        mean_median_bs[((cno-1)*5+n),]<-c(mean(myresult[n,1,]),median(myresult[n,1,]))
    }
     
    mean_median_bs[(cno-1)*5+2,]<--1*mean_median_bs[(cno-1)*5+2,]  

    # Combine bootstrap results 
      ci.95<-c(0.025,0.975) 
      ci.90<-c(0.050,0.950)
    
    #the number of coefficient estimates is 5  
      beta.ci90.1<-matrix(0,5,2)
      beta.ci90.2<-matrix(0,5,2)
      beta.ci95.1<-matrix(0,5,2)
      beta.ci95.2<-matrix(0,5,2)
      myresult.bs<-NULL

    for (n in 1:5) {
        ci95<-quantile(sort(myresult[n,1,]),probs=ci.95)   # Find quantile for 95% C.I.
        ci90<-quantile(sort(myresult[n,1,]),probs=ci.90)   # Find quantile for 90% C.I.
          
        myresult.bs<-cbind(myresult.bs,myresult[n,1,])
            
        q.25<-quantile(sort(q.result[,n]),probs=ci.95[1])  
        q.975<-quantile(sort(q.result[,n]),probs=ci.95[2])
        q.50<-quantile(sort(q.result[,n]),probs=ci.90[1])  
        q.950<-quantile(sort(q.result[,n]),probs=ci.90[2])

        beta.ci95.1[n,]<-ci95 #find the 25th and 975th beta estimates
	  beta.ci90.1[n,]<-ci90 #find the 50th and 950th beta estimates
	
        beta.ci95.2[n,]<-c(beta.true[n,1]-beta.true[n,2]*q.975,beta.true[n,1]-beta.true[n,2]*q.25) # A 95 percentile-t inerval bootstrapped C.I.
        beta.ci90.2[n,]<-c(beta.true[n,1]-beta.true[n,2]*q.950,beta.true[n,1]-beta.true[n,2]*q.50) # A 90 percentile-t inerval bootstrapped C.I.
    }

    beta.ci95.orig<-matrix(c(beta.true[,1]-1.960*beta.true[,2],beta.true[,1]+1.960*beta.true[,2]),5,2) #The 95 percent C.I. based on Newey-West standard error
    beta.ci90.orig<-matrix(c(beta.true[,1]-1.645*beta.true[,2],beta.true[,1]+1.645*beta.true[,2]),5,2) #The 90 percent C.I. based on Newey-West standard error
      
    beta.ci95.3<-cbind(2*beta.true[,1]-beta.ci95.1[,2],2*beta.true[,1]-beta.ci95.1[,1]) #A bias-corrected 95 percentile interval bootstrapped C.I.
    beta.ci90.3<-cbind(2*beta.true[,1]-beta.ci90.1[,2],2*beta.true[,1]-beta.ci90.1[,1]) #A bias-corrected 90 percentile interval bootstrapped C.I.
      
    # Table 3
    beta.out90.3<-matrix(t(rbind(beta.ci90.orig[1:2,],beta.ci90.3[1:2,],beta.ci90.2[1:2,])),3,4,byrow=T)
    beta.out90.3[,3]<-rbind(-beta.ci90.orig[2,2],-beta.ci90.3[2,2],-beta.ci90.2[2,2])  
    beta.out90.3[,4]<-rbind(-beta.ci90.orig[2,1],-beta.ci90.3[2,1],-beta.ci90.2[2,1])

    beta.out95.3<-matrix(t(rbind(beta.ci95.orig[1:2,],beta.ci95.3[1:2,],beta.ci95.2[1:2,])),3,4,byrow=T)
    beta.out95.3[,3]<-rbind(-beta.ci95.orig[2,2],-beta.ci95.3[2,2],-beta.ci95.2[2,2])  
    beta.out95.3[,4]<-rbind(-beta.ci95.orig[2,1],-beta.ci95.3[2,1],-beta.ci95.2[2,1])  
      
    # Table 4
    beta.out95.4<-matrix(t(rbind(-beta.ci95.orig[4,],-beta.ci95.3[4,],-beta.ci95.2[4,])),3,2,byrow=T)    
    beta.out90.4<-matrix(t(rbind(-beta.ci90.orig[4,],-beta.ci90.3[4,],-beta.ci90.2[4,])),3,2,byrow=T)
      
    # Table 5
    beta.out95.5<-matrix(t(rbind(-beta.ci95.orig[5,],-beta.ci95.3[5,],-beta.ci95.2[5,])),3,2,byrow=T)
    beta.out90.5<-matrix(t(rbind(-beta.ci90.orig[5,],-beta.ci90.3[5,],-beta.ci90.2[5,])),3,2,byrow=T)
      
    beta.coef.3<-beta.true[1:2,1]
    beta.coef.3[2]<--beta.coef.3[2]
    beta.coef.4<--c(beta.true[4,1])
    beta.coef.5<--c(beta.true[5,1])

    beta.nwse.3<-beta.true[1:2,2]
    beta.nwse.4<-c(beta.true[4,2])
    beta.nwse.5<-c(beta.true[5,2])

    # Stack each country's output     
    beta.co.3<-rbind(beta.co.3,beta.coef.3,beta.nwse.3) #Estimates of coefficients based on Model 1 and Model 2
    beta.co.4<-rbind(beta.co.4,beta.coef.4,beta.nwse.4)
    beta.co.5<-rbind(beta.co.5,beta.coef.5,beta.nwse.5)   

    beta95.3<-rbind(beta95.3,beta.out95.3) #95% C.I. of estimates based on Model 1 and Model 2
    beta90.3<-rbind(beta90.3,beta.out90.3) #90% C.I. of estimates based on Model 1 and Model 2
      
    beta95.4<-rbind(beta95.4,beta.out95.4)
    beta90.4<-rbind(beta90.4,beta.out90.4)

    beta95.5<-rbind(beta95.5,beta.out95.5)
    beta90.5<-rbind(beta90.5,beta.out90.5)

    pECM.all<-rbind(pECM.all,pECM) 
    rstA.all<-rbind(rstA.all,rstA)
    g.all<-cbind(g.all,g.result)  

    write.csv(myresult.bs,file=paste("Table345_bs",mylist[cno],"csv",sep=".")) 
}

#
mean_median_bs<-rbind(mean_median_bs[c(1:5,11:15,6:10,26:30,31:35,21:25,16:20),])


# Output #rearrange the order of currencies in output
beta.co.3<-rbind(beta.co.3[c(1,2,5,6,3,4,11,12,13,14,9,10,7,8),])
beta.co.4<-rbind(beta.co.4[c(1,2,5,6,3,4,11,12,13,14,9,10,7,8),])
beta.co.5<-rbind(beta.co.5[c(1,2,5,6,3,4,11,12,13,14,9,10,7,8),])

pECM.all<-rbind(pECM.all[c(1,2,3,7,8,9,4,5,6,16,17,18,19,20,21,13,14,15,10,11,12),])
rstA.all<-rbind(rstA.all[c(1,2,3,7,8,9,4,5,6,16,17,18,19,20,21,13,14,15,10,11,12),])

beta95.3<-rbind(beta95.3[c(2:3,8:9,5:6,17:18,20:21,14:15,11:12),])
beta95.4<-rbind(beta95.4[c(2:3,8:9,5:6,17:18,20:21,14:15,11:12),])
beta95.5<-rbind(beta95.5[c(2:3,8:9,5:6,17:18,20:21,14:15,11:12),])

beta90.3<-rbind(beta90.3[c(2:3,8:9,5:6,17:18,20:21,14:15,11:12),])
beta90.4<-rbind(beta90.4[c(2:3,8:9,5:6,17:18,20:21,14:15,11:12),])
beta90.5<-rbind(beta90.5[c(2:3,8:9,5:6,17:18,20:21,14:15,11:12),])

beta.3<-cbind(beta95.3[,1:2],beta90.3[,1:2],beta95.3[,3:4],beta90.3[,3:4])
beta.4<-cbind(beta95.4[,1:2],beta90.4[,1:2])
beta.5<-cbind(beta95.5[,1:2],beta90.5[,1:2])

g.all<-cbind(g.all[,c(1,2,5,6,3,4,11,12,13,14,9,10,7,8)])

write.csv(mean_median_bs,"Table345.mean.median.csv")

write.csv(g.all,"g.all.csv")

write.csv(beta.3, "Table3.ci.VECM.csv")
write.csv(beta.4, "Table4.ci.VECM.csv")
write.csv(beta.5, "Table5.ci.VECM.csv")

write.csv(beta.co.3, "Table3.coef.VECM.csv")
write.csv(beta.co.4, "Table4.coef.VECM.csv")
write.csv(beta.co.5, "Table5.coef.VECM.csv")

write.csv(pECM.all,"ECM.parameters.csv")
write.csv(rstA.all,"rstVAR.coef.csv")
