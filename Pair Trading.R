library(TTR) #Technical Trading Rules
options(warn=-1)

#stcu= cutoff probabilty of mean reversion to open trade
#clcu= cutoff probabilty of mean reversion to close trade
#mavn= no. of trading days considered in the moving average
#stpot= starting point(day) of trade
#enpot= ending point(day) of trade
#prnm= index of nth pair
#prnt= 1 if to print the trading data 


stcu1=0.5
clcu1=0.1
mavn1=90
frin=0.5 # fraction used for prediction from start
frtr=0.5 # fraction used for trading from end
pn=1
px=0
profall=0

wd="C:/Users/Kashyap/Downloads/"
pno=list("pepsi.csv","cocacola.csv","tcs.csv","infosys.csv","hdfc.csv","hdfcbank.csv","drreddy.csv","lupin.csv")
pnmes=list("Pepsi-Cocacola","TCS-Infosys","HDFC-HDFC Bank","DrReddy-Lupin")
redd=read.csv(paste(wd,pno[1],sep=""),header=FALSE,as.is = c(TRUE, FALSE))
lst=redd[1]

flea=c(1:4)
i1=2
while(i1<9){
  red=read.csv(paste(wd,pno[i1],sep=""),header=FALSE,as.is = c(TRUE, FALSE))
  flea[i1/2]=nrow(red)
  i1=i1+2
}
#Indexing of directory of pairs
rdir1=function(ar){
  ar=ar*2-1
  rv=paste(wd,pno[ar],sep="")
  return(rv) 
}
rdir2=function(ar){
  ar=ar*2
  rv=paste(wd,pno[ar],sep="")
  return(rv) 
}



trade=function(stcu,clcu,mavn,stpot,enpot,prnm,prnt,prntx){
  stpot=as.integer(stpot)
  enpot=as.integer(enpot)
  stdt=0    #Trade start day  
  endt=0    #Trade end day
  tr=0      #State of trade| Active:1 Inactive:0
  mod=0     #mode of trade| 0:Ratio>Mean
  inpr=0    #change in balance on opening day of a trade
  otpr=0    #change in balance on closing day of a trade
  stp=1000  #No. of stocks of first element of pair
  stc=1000  #No. of stocks of first element of pair
  pin=0.9   #Fraction of balance invested
  stlo=0.30  #Fraction of balance set as stoploss
  fle=0     #No. of trading days in the csv file imported
  mn=1000000  #Starting balance i.e. money invested
  ttd=0
  tpg=0
  trdno=1   #nth trade
  trpre=c(1:1000)   #array for storing %return on nth trade
  trlen=c(1:1000)   #No. of trading days in the nth trade     
  stdv=0    #standard deviation about moving average
  
  
  pps=read.csv(rdir1(prnm),header=FALSE,as.is = c(TRUE, FALSE))
  coc=read.csv(rdir2(prnm),header=FALSE,as.is = c(TRUE, FALSE))
  
  fle=flea[prnm]
  mna=c(1:fle)
  if(enpot==-1){enpot=fle}
  p=ts(pps[2])
  c=ts(coc[2])
  dat=ts(pps[1])
  r=p/c
  xy=data.frame(pps[2],coc[2])
  mav=TTR::EMA(r,mavn)
  for(i in c(1:mavn)){
    mav[i]=mav[mavn]
  }
  rdev=r-mav
  stdv=sd(rdev)
  for(i in c(stpot:enpot)){
    pr=pnorm(rdev[i],0,stdv)
    if(pr>0.5){pr=1-pr}
    pr=1-2*pr
    if(tr==0){
      if(pr>stcu){
        stp=as.integer(pin*mn/p[i])
        stc=as.integer(pin*mn/c[i])
        stdt=i
        tr=1
        if(rdev[i]>0){
          mod=0
          inpr=(p[i]*stp-c[i]*stc)
        }
        else{
          mod=1
          inpr=(c[i]*stc-p[i]*stp)
        }
      }
    }
    else{
      if(mod==0){
        otpr=(c[i]*stc-p[i]*stp)
      }
      else{
        otpr=(p[i]*stp-c[i]*stc)
      }
      if(pr<clcu){
        tr=0
        endt=i
        tpg=tpg+(inpr+otpr)/mn*100
        ttd=ttd+endt-stdt
        trpre[trdno]=(inpr+otpr)/mn*100
        trlen[trdno]=endt-stdt
        trdno=trdno+1
        mn=mn+inpr+otpr
        inpr=0
        otpr=0
        #cat(tpg,ttd,mn,"\n",sep="x")
        #cat(tr,mod,r[i],rdev[i],pr,mav[i],"\n",sep="x")
      }
      else{
        loss=stlo*mn+inpr+otpr
        if(loss<0){
          tr=0
          endt=i
          tpg=tpg+(inpr+otpr)/mn*100
          ttd=ttd+endt-stdt
          trpre[trdno]=(inpr+otpr)/mn*100
          trlen[trdno]=endt-stdt
          trdno=trdno+1
          mn=mn+inpr+otpr
          #print(inpr+otpr)
          inpr=0
          otpr=0
        }
      }
    }
    mna[i]=mn+inpr+otpr
    #cat(tr,mod,r[i],rdev[i],pr,mav[i],"\n",sep="x")
  }
  if(prnt==1){
    trdno=trdno-1
    trpre1=c(1:trdno)
    trlen1=c(1:trdno)
    for(i in c(1:trdno)){
      trpre1[i]=trpre[i]
      trlen1[i]=trlen[i]
    }
    cre=cor(xy, use="complete.obs", method="pearson")[2]
    cat("Starting Cutoff = ",stcu,"\n")
    cat("Closing Cutoff = ",clcu,"\n")
    cat("correlation = ",cre,"\n")
    cat("final balance = ",mn,"\n")
    cat("no. of trades = ",trdno,"\n")
    cat("started on = ",pps[stpot,1],"\n")
    mold=mean(trlen1)
    cat("mean hold = ",mold,"\n")
    profall <<- mn+profall-1000000
    if(prntx==1){
      for(vb in c(1:trdno)){
        cat(vb,":",trpre1[vb]," , ",trlen1[vb],"\n")
      }
    }
    ts.plot(p,c,gpars=list(xlab="Trading Days", ylab=pnmes[prnm], lty=c(1:2)),col=c(rep("blue",1),rep("red",1)))
    ts.plot(ts(mna),gpars=list(xlab="Trading Days", ylab=paste("Portfolio Value of ",pnmes[prnm],sep="")))
  }
  else{
    return(mn)
  }
}
gtclcf=function(stcf,mavn,ind){
  clcf=0.1
  v1=35
  if(stcf<0.36){v1=as.integer(stcf*100)}
  mx=trade(stcf,clcf,mavn,1,frin*flea[ind],ind,0,0)
  for(i in c(11:v1)){
    sr=trade(stcf,i/100,mavn,1,frin*flea[ind],ind,0,0)
    if(sr>mx){
      mx=sr
      clcf=i/100
    }
  }
  return(clcf)
}
gtstcf=function(clcf,mavn,ind){
  v1=26
  if(clcf>0.25){
    v1=as.integer(clcf*100)
  }
  stcf=v1-1
  mx=trade(stcf,clcf,mavn,1,frin*flea[ind],ind,0,0)
  for(i in c(v1:50)){
    sr=trade(i/100,clcf,mavn,1,frin*flea[ind],ind,0,0)
    if(sr>mx){
      mx=sr
      stcf=i/100
    }
  }
  return(stcf)
}




clcu1=gtclcf(stcu1,mavn1,1)
stcu1=gtstcf(clcu1,mavn1,1)
trade(stcu1,clcu1,mavn1,(1-frtr)*flea[1],-1,1,pn,px)
clcu1=gtclcf(stcu1,mavn1,2)
stcu1=gtstcf(clcu1,mavn1,2)
trade(stcu1,clcu1,mavn1,(1-frtr)*flea[2],-1,2,pn,px)
clcu1=gtclcf(stcu1,mavn1,3)
stcu1=gtstcf(clcu1,mavn1,3)
trade(stcu1,clcu1,mavn1,(1-frtr)*flea[3],-1,3,pn,px)
clcu1=gtclcf(stcu1,mavn1,4)
stcu1=gtstcf(clcu1,mavn1,4)
trade(stcu1,clcu1,mavn1,(1-frtr)*flea[4],-1,4,pn,px)

cat("net profit = ",profall,"\n")