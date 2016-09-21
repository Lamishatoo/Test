load("C:/Users/USER.PC_604/Dropbox/福山資料/墾丁樣區/kenting.full1.rdata")
load("C:/Users/USER.PC_604/Dropbox/福山資料/墾丁樣區/kenting.full2.rdata")
load("C:/Users/USER.PC_604/Dropbox/福山資料/墾丁樣區/kenting.full3.rdata")

library(dplyr)
data1 <- filter(kenting.full1, status=="A")
data2 <- filter(kenting.full2, status=="A")
data3 <- filter(kenting.full3, status=="A")

name <- unique(c(data1$sp,data2$sp,data3$sp))       
cut_data <- function(x){
  result <- list()
  k <- 1
  for(j in seq(0, 200, 50)){
    for(i in seq(0, 350, 50)){
      data<- rep(0,length(name))
      names(data) <- name
      tmp <- filter(x, gx<i+50 & gx>=i & gy<j+50 & gy>=j)
      data[names(table(tmp$sp))] <- as.vector(table(tmp$sp))
      result[[k]] <- data
      k=k+1
    }
  }
  return(result)
}
data1 <- cut_data(data1)
data2 <- cut_data(data2)
data3 <- cut_data(data3)

all <- list()
for(i in 1:40){
  all[[i]] <- cbind(data1[[i]],data2[[i]],data3[[i]])
}

Chat.Ind <- function(x, m)
{
  x <- x[x>0]
  n <- sum(x)
  f1 <- sum(x == 1)
  f2 <- sum(x == 2)
  if(f1>0 & f2>0)
  {
    a=(n - 1) * f1 / ((n - 1) * f1 + 2 * f2)
  }
  if(f1>1 & f2==0)
  {
    a=(n-1)*(f1-1) / ( (n-1)*(f1-1) + 2 )
  } 
  if(f1==1 & f2==0) {a=0}
  if(f1==0) {a=0} 
  
  Sub <- function(m){
    if(m < n) out <- 1-sum(x / n * exp(lchoose(n - x, m)-lchoose(n - 1, m)))
    if(m == n) out <- 1-f1/n*a
    if(m > n) out <- 1-f1/n*a^(m-n+1)
    out
  }
  sapply(m, Sub)		
}
Chat.Ind_Inc <- function(x, m)
{
  t <- x[1]
  x=x[-1]; x <- x[x>0]
  Q1 <- sum(x == 1)
  Q2 <- sum(x == 2)
  if(Q1>0 & Q2>0)
  {
    a=(t - 1) * Q1 / ((t - 1) * Q1 + 2 * Q2)
  }
  if(Q1>1 & Q2==0)
  {
    a=(t-1)*(Q1-1) / ( (t-1)*(Q1-1) + 2 )
  } 
  if(Q1==1 & Q2==0) {a=0}
  if(Q1==0) {a=0} 
  
  Sub <- function(m){
    if(m < t) out <- 1-sum(x / t * exp(lchoose(t - x, m)-lchoose(t - 1, m)))
    if(m == t) out <- 1-Q1/t*a
    if(m > t) out <- 1-Q1/t*a^(m-t+1)
    out
  }
  sapply(m, Sub)		
}
U2_equ=function(X,Y)
{
  n=sum(X)
  m=sum(Y)
  f1.=sum(X==1 & Y>0)
  p1barhat_1=p1bar_equ(X)
  out3=f1./n*(1-p1barhat_1)
  #############################
  f11=sum(X==1 & Y==1)
  p1barhat_2=p1bar_equ(Y)
  out4=f11/n/m*(1-p1barhat_1)*(1-p1barhat_2)/p1barhat_2
  output=out3+out4
  return(output) 
} 
U2_equ_Inc=function(X, Y)
{
  t1=X[1]
  t2=Y[1]
  X = X[-1]; Y = Y[-1]
  Q1.=sum(X==1 & Y>0)
  p1barhat_1=p1bar_equ_Inc(c(t1,X))
  out3=Q1./t1*(1-p1barhat_1)
  #############################
  Q11=sum(X==1 & Y==1)
  p1barhat_2=p1bar_equ_Inc(c(t2,Y))
  out4=Q11/t1/t2*(1-p1barhat_1)*(1-p1barhat_2)/p1barhat_2
  if(p1barhat_2==0){out4=0}
  output=out3+out4
  return(output) 
}
correct_obspi<- function(X)
{
  Sobs <- sum(X > 0)   
  n <- sum(X)		  	
  f1 <- sum(X == 1) 	
  f2 <- sum(X == 2)
  if(f1>0 & f2>0)
  {
    a=(n - 1) * f1 / ((n - 1) * f1 + 2 * f2) * f1 / n
  }
  if(f1>1 & f2==0)
  {
    a=(n-1)*(f1-1) / ( (n-1)*(f1-1) + 2 )*f1/n
  } 
  if(f1==1 & f2==0) {a=0}
  if(f1==0 ) {a=0} 	
  b <- sum(X / n * (1 - X / n) ^ n)
  w <- a / b  			
  Prob.hat <- X / n * (1 - w * (1 - X / n) ^ n)	
  Prob.hat
}
correct_obspi_Inc<- function(X)
{
  t = X[1]
  x = X[-1]
  Sobs <- sum(x > 0) 	
  
  f1 <- sum(x == 1) 	
  f2 <- sum(x == 2)
  if(f1>0 & f2>0)
  {
    a=(t - 1) * f1 / ((t - 1) * f1 + 2 * f2) * f1 / t
  }
  if(f1>1 & f2==0)
  {
    a=(t-1)*(f1-1) / ( (t-1)*(f1-1) + 2 )*f1/t
  } 
  if(f1==1 & f2==0) {a=0}
  if(f1==0 ) {a=0} 	
  b <- sum(x / t * (1 - x / t) ^ t)
  w <- a / b  			
  Prob.hat <- x / t * (1 - w * (1 - x / t) ^ t)	
  Prob.hat
}
p1bar_equ=function(X)
{
  n=sum(X)
  f1=sum(X==1)
  f2=sum(X==2)
  if(f1>0 & f2>0)
  {
    a=2*f2/( (n-1)*f1+2*f2  )
  }
  if(f1>1 & f2==0)
  {
    a=2/( (n-1)*(f1-1)+2      )
  }
  if(f1==1 &  f2==0){a=0}
  if(f1==0){a=0}
  return(a)
}
p1bar_equ_Inc=function(X)
{
  t = X[1]
  X = X[-1]
  
  Q1=sum(X==1)
  Q2=sum(X==2)
  a=1
  if(Q1>0 & Q2>0)
  {
    a=2*Q2/( (t-1)*Q1+2*Q2  )
  }
  if(Q1>1 & Q2==0)
  {
    a=2/( (t-1)*(Q1-1)+2      )
  }
  if(Q1==1 &  Q2==0){a=1}
  if(Q1==0){a=1}
  return(a)
}
Two_com_correct_obspi=function(X1,X2)
{
  n1=sum(X1)
  n2=sum(X2)
  f11=sum(X1==1)
  f12=sum(X1==2)
  f21=sum(X2==1)
  f22=sum(X2==2)
  C1=1-f11/n1*(n1 - 1) * f11 / ((n1 - 1) * f11 + 2 * f12)
  C2=1-f21/n2*(n2 - 1) * f21 / ((n2 - 1) * f21 + 2 * f22)
  
  PP1=correct_obspi(X1)
  PP2=correct_obspi(X2)
  D12=which(X1>0 & X2>0)
  
  f0hat_1=ceiling( ifelse(f12 == 0,  f11 * (f11 - 1) / 2,  f11 ^ 2/ 2 / f12)   )   
  f0hat_2=ceiling( ifelse(f22 == 0,  f21 * (f21 - 1) / 2,  f21 ^ 2/ 2 / f22)   )
  #-----------------------------------------------------------------------------
  
  r1=which(X1>0 & X2==0)
  f.1=length(which(X1>0 & X2==1))
  f.2=length(which(X1>0 & X2==2))
  f.0=ceiling(ifelse(f.2>0,f.1^2/2/f.2,f.1*(f.1-1)/2))
  #------------------------------------------------------------------------------
  r2=which(X1==0 & X2>0)
  f1.=length(which(X1==1 & X2>0))
  f2.=length(which(X1==2 & X2>0))
  f0.=ceiling(ifelse(f2.>0,f1.^2/2/f2.,f1.*(f1.-1)/2))
  #------------------------------------------------------------------------------
  t11=length(which(X1==1 & X2==1))
  t22=length(which(X1==2 & X2==2))
  f00hat=ceiling( ifelse(t22 == 0,  t11 * (t11 - 1) / 4,  t11 ^ 2/ 4 / t22)   )
  #------------------------------------------------------------------------------
  temp1=max(length(r1),f.0)-length(r1)+f0.+f00hat
  temp2=max(length(r2),f0.)-length(r2)+f.0+f00hat
  p0hat_1=(1-C1)/max(f0hat_1,temp1)
  p0hat_2=(1-C2)/max(f0hat_2,temp2)
  #------------------------------------------------------------------------------
  P1=PP1[D12]
  P2=PP2[D12]
  if(length(r1)> f.0)
  {
    P1=c(P1,PP1[r1])
    Y=c(rep(p0hat_2, f.0), rep(0,length(r1)-f.0))
    P2=c(P2,sample(Y,length(Y)) )
  }
  if(length(r1)< f.0)
  {
    P1=c(P1,PP1[r1],rep( p0hat_1,f.0-length(r1)))
    P2=c(P2,rep(p0hat_2, f.0) )
  }
  #----------------------------------------------------------------------------   
  if(length(r2)> f0.)
  {
    Y=c(rep(p0hat_1,f0.),rep(0,length(r2)- f0.))
    P1=c(P1,sample(Y,length(Y)))
    P2=c(P2,PP2[r2] )
  }
  if(length(r2)< f0.)
  {
    P1=c(P1,rep(p0hat_1,f0.))
    P2=c(P2,PP2[r2],rep( p0hat_2,f0.-length(r2)) )
  }
  P1=c(P1,rep( p0hat_1,f00hat))
  P2=c(P2,rep( p0hat_2,f00hat))
  P1=c(P1, rep(p0hat_1,max( f0hat_1-temp1,0)) , rep(    0  ,max( f0hat_2-temp2,0))         )
  P2=c(P2, rep(     0 ,max( f0hat_1-temp1,0)) , rep(p0hat_2,max( f0hat_2-temp2,0))         ) 
  #------------------------------------------------------------------------------------
  a=cbind(P1,P2)
  return(a)
}
Two_com_correct_obspi_Inc=function(X1,X2)
{
  x1=X1; x2=X2
  t1=X1[1]; X1=X1[-1]
  t2=X2[1]; X2=X2[-1]
  Q11=sum(X1==1)
  Q12=sum(X1==2)
  Q21=sum(X2==1)
  Q22=sum(X2==2)
  C1=Chat.Ind_Inc(x1,t1)
  C2=Chat.Ind_Inc(x2,t2)
  
  PP1=correct_obspi_Inc(x1)
  PP2=correct_obspi_Inc(x2)
  D12=which(X1>0 & X2>0)
  
  Q0hat_1=ceiling( (t1-1)/t1* ifelse(Q12 == 0,  Q11 * (Q11 - 1) / 2,  Q11 ^ 2/ 2 / Q12)   )   
  Q0hat_2=ceiling( (t2-1)/t2* ifelse(Q22 == 0,  Q21 * (Q21 - 1) / 2,  Q21 ^ 2/ 2 / Q22)   )
  #-----------------------------------------------------------------------------
  
  r1=which(X1>0 & X2==0)
  Q.1=length(which(X1>0 & X2==1))
  Q.2=length(which(X1>0 & X2==2))
  Q.0=ceiling((t2-1)/t2* ifelse(Q.2>0,Q.1^2/2/Q.2,Q.1*(Q.1-1)/2))
  #------------------------------------------------------------------------------
  r2=which(X1==0 & X2>0)
  Q1.=length(which(X1==1 & X2>0))
  Q2.=length(which(X1==2 & X2>0))
  Q0.=ceiling((t1-1)/t1*ifelse(Q2.>0,Q1.^2/2/Q2.,Q1.*(Q1.-1)/2))
  #------------------------------------------------------------------------------
  t11=length(which(X1==1 & X2==1))
  t22=length(which(X1==2 & X2==2))
  Q00hat=ceiling((t1-1)/t1*(t2-1)/t2* ifelse(t22 == 0,  t11 * (t11 - 1) / 4,  t11 ^ 2/ 4 / t22)   )
  #------------------------------------------------------------------------------
  temp1=max(length(r1),Q.0)-length(r1)+Q0.+Q00hat
  temp2=max(length(r2),Q0.)-length(r2)+Q.0+Q00hat
  p1_us_sum=min(U2_equ_Inc(x1,x2),1-C1)
  p1_us=p1_us_sum/temp1
  p2_us_sum=min(U2_equ_Inc(x2,x1),1-C2)
  p2_us=p2_us_sum/temp2
  if(Q0hat_1-temp1>0){p0_1= (1-C1-p1_us_sum)/(Q0hat_1-temp1) }
  if(Q0hat_1-temp1<=0){p0_1=0} 
  if(Q0hat_2-temp2>0){p0_2= (1-C2-p2_us_sum)/(Q0hat_2-temp2) }
  if(Q0hat_2-temp2<=0){p0_2=0}
  #------------------------------------------------------------------------------
  P1=PP1[D12]
  P2=PP2[D12]
  if(length(r1)> Q.0)
  {
    P1=c(P1,PP1[r1])
    Y=c(rep(p2_us, Q.0), rep(0,length(r1)-Q.0))
    P2=c(P2,sample(Y,length(Y)) )
  }
  if(length(r1)< Q.0)
  {
    P1=c(P1,PP1[r1],rep( p1_us,Q.0-length(r1)))
    P2=c(P2,rep(p2_us, Q.0) )
  }
  #----------------------------------------------------------------------------   
  if(length(r2)> Q0.)
  {
    Y=c(rep(p1_us,Q0.),rep(0,length(r2)- Q0.))
    P1=c(P1,sample(Y,length(Y)))
    P2=c(P2,PP2[r2] )
  }
  if(length(r2)< Q0.)
  {
    P1=c(P1,rep(p1_us,Q0.))
    P2=c(P2,PP2[r2],rep( p2_us,Q0.-length(r2)) )
  }
  P1=c(P1,rep( p1_us,Q00hat))
  P2=c(P2,rep( p2_us,Q00hat))
  P1=c(P1, rep(   p0_1,max( Q0hat_1-temp1,0)) , rep(    0  ,max( Q0hat_2-temp2,0))         )
  P2=c(P2, rep(     0 ,max( Q0hat_1-temp1,0)) , rep(   p0_2,max( Q0hat_2-temp2,0))         ) 
  #------------------------------------------------------------------------------------
  a=cbind(P1,P2)
  return(a)
}
Horn.Est=function(data,method=c("equal", "unequal"))
{ #data is a species*plot data matrix and w is a given weight vector. 
  N=ncol(data);data=data[rowSums(data)>0,];
  S=nrow(data);
  n=colSums(data);
  if(method == "unequal"){
    w <- n/sum(n)
  }else{w <- rep(1/N,N)}
  W=sum(-w*log(w));
  r.data=sapply(1:N,function(k) data[,k]/n[k]);
  r.pool=c(r.data%*%w); 
  
  U=numeric(N);K=numeric(N);
  for(i in 1:N){
    I=which(data[,i]*(rowSums(data)-data[,i])>0)
    is=data[,i][I];pools=rowSums(data)[I]-is;
    r.is=is/n[i];r.pools=r.pool[I];
    U1=sum(r.is);
    sf1=sum(pools==1);sf2=sum(pools==2);sf2=ifelse(sf2==0,1,sf2);
    U2=sum(r.is[pools==1])*(sf1/(2*sf2));
    U[i]=max(0,1-U1-U2)*(-w[i]*log(w[i]))
    K[i]=-sum(w[i]*r.is*log(r.pools/r.is)) 
  }
  Est=(sum(U)+sum(K))/W;   
  
  A=sum(sapply(1:N,function(k) -w[k]*sum(r.data[,k][r.data[,k]>0]*log(r.data[,k][r.data[,k]>0]))))
  G=sum(-r.pool[r.pool>0]*log(r.pool[r.pool>0]));
  Mle=(G-A)/W;
  return(c(1-Est,1-Mle))
}
Two_Horn_equ <- function(X1, X2, datatype="abundance", weight="equal",nboot=50, method="all")
{
  if(datatype=="abundance"){
    p <- Two_com_correct_obspi(X1 ,X2)
    commnunity1 <- rmultinom(nboot, sum(X1), p[,1])
    commnunity2 <- rmultinom(nboot, sum(X2), p[,2])
  }else{
    p <- Two_com_correct_obspi_Inc(X1 ,X2)
    commnunity1 <- t(sapply(1:nrow(p),FUN = function(i) rbinom(nboot, X1[1], p[i,1]) ))
    commnunity2 <- t(sapply(1:nrow(p),FUN = function(i) rbinom(nboot, X2[1], p[i,2]) ))
    X1 <- X1[-1]
    X2 <- X2[-1]
  }
  if(method=="all"){
    se <- apply(sapply(1:nboot, FUN = function(x){
      Horn.Est(data=cbind(commnunity1[,x] ,commnunity2[,x]),method=weight)
    }),MARGIN = 1, sd)
    value <- Horn.Est(data=cbind(X1, X2),method=weight)
    out <- c(value[1], se[1], max(0,value[1]-1.96*se[1]), min(1,value[1]+1.96*se[1]))
    out2 <- c(value[2], se[2],max(0,value[2]-1.96*se[2]), min(1,value[2]+1.96*se[2]))
    return(list(est=out,mle=out2))
  }
  if(method=="est"){
    se <-sd(sapply(1:nboot, FUN = function(x){
      Horn.Est(data=cbind(commnunity1[,x] ,commnunity2[,x]),method=weight)[1]
    }))
    value <- Horn.Est(data=cbind(X1, X2),method=weight)
    out <- c(value[1], se, max(0,value[1]-1.96*se), min(1,value[1]+1.96*se) )
    return(out)
  }
  if(method=="mle"){
    se <-sd(sapply(1:nboot, FUN = function(x){
      Horn.Est(data=cbind(commnunity1[,x] ,commnunity2[,x]),method=weight)[2]
    }))
    value <- Horn.Est(data=cbind(X1, X2),method=weight)
    out <- c(value[2], se, max(0,value[2]-1.96*se), min(1,value[2]+1.96*se) )
    return(out)
  }
}
BC.Est=function(data)
{  #data is species*plot data matrix 
  N=ncol(data);data=data[rowSums(data)>0,];
  n=colSums(data);Tn=sum(n);w=n/Tn;
  pool=rowSums(data);
  Mle=sum(abs(data-matrix(rep(pool/N,N),ncol=N)))/(2*(1-1/N)*Tn);
  temp=rep(0,N);
  for(i in 1:N){
    I=which(data[,i]*(pool-data[,i])>0);
    s.i=data[,i][I];s.pool=rowSums(data)[I]-s.i;
    sf1=sum(s.pool==1);sf2=sum(s.pool==2);sf2=ifelse(sf2==0,1,sf2);
    U1=sum(s.i)/Tn;
    U2=sf1/(2*sf2)*sum(s.i[s.pool==1])/Tn
    temp1=((N-1)/N)*max(0,w[i]-U1-U2) #(w[i]-U1-U2)#
    
    II=which(data[,i]==pool)
    temp2=sum(abs(data[,i][-II]-pool[-II]/N))/Tn;
    temp[i]=temp1+temp2;
  }
  Est=sum(temp)/(2-2/N);
  return(c(1-Est,1-Mle))
}
Two_BC_equ <- function(X1, X2, datatype="abundance", nboot)
{
  if(datatype=="abundance"){
    p <- Two_com_correct_obspi(X1 ,X2)
    commnunity1 <- rmultinom(nboot, sum(X1), p[,1])
    commnunity2 <- rmultinom(nboot, sum(X2), p[,2])
  }else{
    p <- Two_com_correct_obspi_Inc(X1 ,X2)
    commnunity1 <- t(sapply(1:nrow(p),FUN = function(i) rbinom(nboot, X1[1], p[i,1]) ))
    commnunity2 <- t(sapply(1:nrow(p),FUN = function(i) rbinom(nboot, X2[1], p[i,2]) ))
    X1 <- X1[-1]
    X2 <- X2[-1]
  }
  se <- apply(sapply(1:nboot, FUN = function(x){
    BC.Est(data=cbind(commnunity1[,x] ,commnunity2[,x]))
  }),MARGIN = 1, sd)
  value <- BC.Est(data=cbind(X1, X2))
  out <- c(value[1], se[1], max(0,value[1]-1.96*se[1]), min(1,value[1]+1.96*se[1]))
  out2 <- c(value[2], se[2],max(0,value[2]-1.96*se[2]), min(1,value[2]+1.96*se[2]))
  return(list(est=out,mle=out2))
}
SimilarityTwo<-function(X, q, datatype="abundance",method=c("equal weight","unequal weight"))
{ N=ncol(X);ni=colSums(X);n=sum(X);
pool=rowSums(X);
bX=apply(X,2,function(x) x/sum(x));pool.x=rowSums(bX)/N;
if(q==0){
  f1=apply(X,2,function(x) sum(x==1));
  f2=apply(X,2,function(x) sum(x==2));
  Sobs=apply(X,2,function(x) sum(x>0)); 
  Si=Sobs+sapply(1:N, function(k) ifelse(f2[k]==0, f1[k]*(f1[k]-1)/2,f1[k]^2/(2*f2[k]))); 
  Sa=mean(Si);
  UqN.mle=(1/N-mean(Sobs)/sum(pool>0))/(1/N-1);
  CqN.mle=(N-sum(pool>0)/mean(Sobs))/(N-1);
  F1=sum(pool==1);F2=sum(pool==2);
  Sg=sum(pool>0)+ifelse(F2==0,F1*(F1-1)/2,F1^2/(2*F2));
  UqN=min(1,(1/N-Sa/Sg)/(1/N-1));UqN=max(0,UqN);
  CqN=min(1,(N-Sg/Sa)/(N-1));CqN=max(0,CqN);
  out1=c(UqN.mle,UqN)
  out2=c(CqN.mle,CqN)
}
if(q==2){
  if(method=="equal weight"){
    a.mle=N/sum(bX^2)
    g.mle=1/sum(pool.x^2);
    b.mle=g.mle/a.mle;
    UqN.mle=(N-b.mle)/(N-1);
    CqN.mle=(1/N-1/b.mle)/(1/N-1);
    
    Ai=sapply(1:N,function(k) sum(X[,k]*(X[,k]-1)/(ni[k]*(ni[k]-1))));
    bX.1=apply(X,2,function(x) (x-1)/(sum(x)-1));
    temp=sapply(1:nrow(X),function(j) (sum(bX[j,]%*%t(bX[j,]))-sum(bX[j,]^2))+sum(bX[j,]*bX.1[j,]));
    G=1/(sum(temp)/N^2);
    
    B=G/(1/mean(Ai));
    UqN=min(1,(N-B)/(N-1));UqN=max(0,UqN);
    CqN=min(1,(1/N-1/B)/(1/N-1));CqN=max(0,CqN);
    
    out1=c(UqN.mle,UqN)
    out2=c(CqN.mle,CqN)
  }
  if(method=="unequal weight"){
    a.mle=1/(N*sum((X/n)^2));g.mle=1/sum((pool/n)^2);b.mle=g.mle/a.mle;
    UqN.mle=(N-b.mle)/(N-1);
    CqN.mle=(1/N-1/b.mle)/(1/N-1);
    
    A=(1/N)*(1/sum(X*(X-1)/(n*(n-1))));
    G=1/sum(pool*(pool-1)/(n*(n-1)));
    B=G/A;
    UqN=min(1,(N-B)/(N-1));UqN=max(0,UqN);    
    CqN=min(1,(1/N-1/B)/(1/N-1));CqN=max(0,CqN);
    
    out1=c(UqN.mle,UqN)
    out2=c(CqN.mle,CqN)
  }
}
return(rbind(out1,out2))
}
result12_0 <- vector(); result12_1 <- vector(); result12_2 <- vector()
result23_0 <- vector(); result23_1 <- vector(); result23_2 <- vector()
result13_0 <- vector(); result13_1 <- vector(); result13_2 <- vector()
for(i in 1:40){
  result12_0[i] <- SimilarityTwo(all[[i]][,-3],0,"abundance","unequal weight")[2,1]
  result12_1[i] <- Horn.Est(all[[i]][,-3],"unequal")[2]
  result12_2[i] <- SimilarityTwo(all[[i]][,-3],2,"abundance","unequal weight")[2,1]
  result23_0[i] <- SimilarityTwo(all[[i]][,-1],0,"abundance","unequal weight")[2,1]
  result23_1[i] <- Horn.Est(all[[i]][,-1],"unequal")[2]
  result23_2[i] <- SimilarityTwo(all[[i]][,-1],2,"abundance","unequal weight")[2,1]
  result13_0[i] <- SimilarityTwo(all[[i]][,-2],0,"abundance","unequal weight")[2,1]
  result13_1[i] <- Horn.Est(all[[i]][,-2],"unequal")[2]
  result13_2[i] <- SimilarityTwo(all[[i]][,-2],2,"abundance","unequal weight")[2,1]
}

library(ggplot2)
y <- rep(seq(50,250,50),each=8)
x <- rep(seq(50,400,50),5)
result12 <- data.frame(q0=result12_0,q1=result12_1,q2=result12_2,x=x,y=y)
ggplot(result12,aes(x,y)) + geom_raster(aes(fill = q0), hjust=0.5, vjust=0.5, interpolate=FALSE)
ggplot(result12,aes(x,y)) + geom_raster(aes(fill = q1), hjust=0.5, vjust=0.5, interpolate=FALSE)
ggplot(result12,aes(x,y)) + geom_raster(aes(fill = q2), hjust=0.5, vjust=0.5, interpolate=FALSE)

result23 <- data.frame(q0=result23_0,q1=result23_1,q2=result23_2,x=x,y=y)
ggplot(result23,aes(x,y)) + geom_raster(aes(fill = q0), hjust=0.5, vjust=0.5, interpolate=FALSE)
ggplot(result23,aes(x,y)) + geom_raster(aes(fill = q1), hjust=0.5, vjust=0.5, interpolate=FALSE)
ggplot(result23,aes(x,y)) + geom_raster(aes(fill = q2), hjust=0.5, vjust=0.5, interpolate=FALSE)

result13 <- data.frame(q0=result13_0,q1=result13_1,q2=result13_2,x=x,y=y)
ggplot(result13,aes(x,y)) + geom_raster(aes(fill = q0), hjust=0.5, vjust=0.5, interpolate=FALSE)
ggplot(result13,aes(x,y)) + geom_raster(aes(fill = q1), hjust=0.5, vjust=0.5, interpolate=FALSE)
ggplot(result13,aes(x,y)) + geom_raster(aes(fill = q2), hjust=0.5, vjust=0.5, interpolate=FALSE)

#=====================#


cut <- function(x){
  result <- list()
  k <- 1
  for(j in seq(0, 200, 50)){
    for(i in seq(0, 350, 50)){
      tmp <- filter(x, gx<i+50 & gx>=i & gy<j+50 & gy>=j)
      result[[k]] <- tmp
      k=k+1
    }
  }
  return(result)
}
year1 <- cut(kenting.full1)
year2 <- cut(kenting.full2)
year3 <- cut(kenting.full3)

calculate <- function(year1,year2,boot){
  D=A=P <- list()
  for(i in 1:40){
    A[[i]] <- table(year1[[i]][which(year1[[i]]$status=="A"),]$sp) 
    D[[i]] <- table(year2[[i]][which(year1[[i]]$status=="A" & year2[[i]]$status=="D"),]$sp)
    P[[i]] <- table(year2[[i]][which(year1[[i]]$status=="P" & year2[[i]]$status=="A"),]$sp)
  }
}

#1-2#
D=A=P <- list()
for(i in 1:40){
  A[[i]] <- table(year1[[i]][which(year1[[i]]$status=="A"),]$sp) 
  D[[i]] <- table(year2[[i]][which(year1[[i]]$status=="A" & year2[[i]]$status=="D"),]$sp)
  P[[i]] <- table(year2[[i]][which(year1[[i]]$status=="P" & year2[[i]]$status=="A"),]$sp)
}

year12 <- list()
all <- list()
for(i in 1:40){
  year12[[i]] <- data.frame(A=rep(0,length(as.vector(table(year2[[i]]$sp)))),D=rep(0,length(as.vector(table(year2[[i]]$sp)))),P=rep(0,length(as.vector(table(year2[[i]]$sp)))))
  rownames(year12[[i]]) <- names(table(year2[[i]]$sp))
  year12[[i]][names(A[[i]]),1] <- A[[i]]
  year12[[i]][names(D[[i]]),2] <- D[[i]]
  year12[[i]][names(P[[i]]),3] <- P[[i]]
  sum <- colSums(year12[[i]])[1]-colSums(year12[[i]])[2]
  choose <- rep(rownames(year12[[i]]),year12[[i]][,1])
  
  
  do <- function(x){
    tmp <- sample(choose,sum,replace=F)
    replace <- as.vector(table(tmp))
    names(replace) <- names(table(tmp))
    new <- rep(0,nrow(year12[[i]]))
    names(new) <- rownames(year12[[i]])
    new[names(replace)] <- replace
    new <- new + year12[[i]][,3]
    a <- SimilarityTwo(cbind(year12[[i]][,1],new),0,"abundance","unequal weight")[2,1]
    b <- Horn.Est(cbind(year12[[i]][,1],new),"unequal")[2]
    c <- SimilarityTwo(cbind(year12[[i]][,1],new),2,"abundance","unequal weight")[2,1]
    return(c(a,b,c))
  }
  all[[i]] <- t(apply(t(sapply(1:500,do)),MARGIN = 2,function(x) quantile(x,c(0.025,0.975))))
  print(i)
}
result <- c()
for(i in 1:40){
  result <- c(result,result12_0[i],result12_1[i],result12_2[i]) 

}
all<-do.call(rbind,all)
all <- data.frame(all, result,row.names = NULL)
sig <- apply(all,MARGIN = 1,function(x){
  if(x[1]<x[3] & x[2]>x[3]){
    return("N")
  }else{
    return("Y")
  }
})
y <- rep(rep(seq(50,250,50),each=8),each=3)
x <- rep(rep(seq(50,400,50),5),each=3)
q <- rep(c(0,1,2),40)
all <- data.frame(all, x,y,q,sig,row.names = NULL)
allq0 <- filter(all, q==0)
allq1 <- filter(all, q==1)
allq2 <- filter(all, q==2)
ggplot(allq0,aes(x,y)) + geom_raster(aes(fill = sig), hjust=0.5, vjust=0.5, interpolate=FALSE)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(text=element_text(size=18),legend.title = element_text(size=15,face="bold"))+
  scale_fill_manual(values = c("black","white"),name="significance",breaks=c("N", "Y"),labels=c("N", "Y"))
ggplot(allq1,aes(x,y)) + geom_raster(aes(fill = sig), hjust=0.5, vjust=0.5, interpolate=FALSE)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(text=element_text(size=18),legend.title = element_text(size=15,face="bold"))+
  scale_fill_manual(values = c("black","white"),name="significance",breaks=c("N", "Y"),labels=c("N", "Y"))
ggplot(allq2,aes(x,y)) + geom_raster(aes(fill = sig), hjust=0.5, vjust=0.5, interpolate=FALSE)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(text=element_text(size=18),legend.title = element_text(size=15,face="bold"))+
  scale_fill_manual(values = c("black","white"),name="significance",breaks=c("N", "Y"),labels=c("N", "Y"))

#2-3#
D=A=P <- list()
for(i in 1:40){
  A[[i]] <- table(year2[[i]][which(year2[[i]]$status=="A"),]$sp) 
  D[[i]] <- table(year3[[i]][which(year2[[i]]$status=="A" & year3[[i]]$status=="D"),]$sp)
  P[[i]] <- table(year3[[i]][which(year2[[i]]$status=="P" & year3[[i]]$status=="A"),]$sp)
}

year12 <- list()
all <- list()
for(i in 1:40){
  year12[[i]] <- data.frame(A=rep(0,length(as.vector(table(year3[[i]]$sp)))),D=rep(0,length(as.vector(table(year3[[i]]$sp)))),P=rep(0,length(as.vector(table(year3[[i]]$sp)))))
  rownames(year12[[i]]) <- names(table(year3[[i]]$sp))
  year12[[i]][names(A[[i]]),1] <- A[[i]]
  year12[[i]][names(D[[i]]),2] <- D[[i]]
  year12[[i]][names(P[[i]]),3] <- P[[i]]
  sum <- colSums(year12[[i]])[1]-colSums(year12[[i]])[2]
  choose <- rep(rownames(year12[[i]]),year12[[i]][,1])
  
  
  do <- function(x){
    tmp <- sample(choose,sum,replace=F)
    replace <- as.vector(table(tmp))
    names(replace) <- names(table(tmp))
    new <- rep(0,nrow(year12[[i]]))
    names(new) <- rownames(year12[[i]])
    new[names(replace)] <- replace
    new <- new + year12[[i]][,3]
    a <- SimilarityTwo(cbind(year12[[i]][,1],new),0,"abundance","unequal weight")[2,1]
    b <- Horn.Est(cbind(year12[[i]][,1],new),"unequal")[2]
    c <- SimilarityTwo(cbind(year12[[i]][,1],new),2,"abundance","unequal weight")[2,1]
    return(c(a,b,c))
  }
  all[[i]] <- t(apply(t(sapply(1:500,do)),MARGIN = 2,function(x) quantile(x,c(0.025,0.975))))
  print(i)
}
result <- c()
for(i in 1:40){
  result <- c(result,result12_0[i],result12_1[i],result12_2[i]) 
  
}
all<-do.call(rbind,all)
all <- data.frame(all, result,row.names = NULL)
sig <- apply(all,MARGIN = 1,function(x){
  if(x[1]<=x[3] & x[2]>=x[3]){
    return("N")
  }else{
    return("Y")
  }
})
y <- rep(rep(seq(50,250,50),each=8),each=3)
x <- rep(rep(seq(50,400,50),5),each=3)
q <- rep(c(0,1,2),40)
all <- data.frame(all, x,y,q,sig,row.names = NULL)
allq0 <- filter(all, q==0)
allq1 <- filter(all, q==1)
allq2 <- filter(all, q==2)
ggplot(allq0,aes(x,y)) + geom_raster(aes(fill = sig), hjust=0.5, vjust=0.5, interpolate=FALSE)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(text=element_text(size=18),legend.title = element_text(size=15,face="bold"))+
  scale_fill_manual(values = c("black","white"),name="significance",breaks=c("N", "Y"),labels=c("N", "Y"))
ggplot(allq1,aes(x,y)) + geom_raster(aes(fill = sig), hjust=0.5, vjust=0.5, interpolate=FALSE)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(text=element_text(size=18),legend.title = element_text(size=15,face="bold"))+
  scale_fill_manual(values = c("black","white"),name="significance",breaks=c("N", "Y"),labels=c("N", "Y"))
ggplot(allq2,aes(x,y)) + geom_raster(aes(fill = sig), hjust=0.5, vjust=0.5, interpolate=FALSE)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(text=element_text(size=18),legend.title = element_text(size=15,face="bold"))+
  scale_fill_manual(values = c("black","white"),name="significance",breaks=c("N", "Y"),labels=c("N", "Y"))


#1-3#
D=A=P <- list()
for(i in 1:40){
  A[[i]] <- table(year1[[i]][which(year1[[i]]$status=="A"),]$sp) 
  D[[i]] <- table(year3[[i]][which(year1[[i]]$status=="A" & year3[[i]]$status=="D"),]$sp)
  P[[i]] <- table(year3[[i]][which(year1[[i]]$status=="P" & year3[[i]]$status=="A" & year2[[i]]$status=="P"),]$sp)
}

year12 <- list()
all <- list()
for(i in 1:40){
  year12[[i]] <- data.frame(A=rep(0,length(as.vector(table(year2[[i]]$sp)))),D=rep(0,length(as.vector(table(year2[[i]]$sp)))),P=rep(0,length(as.vector(table(year2[[i]]$sp)))))
  rownames(year12[[i]]) <- names(table(year3[[i]]$sp))
  year12[[i]][names(A[[i]]),1] <- A[[i]]
  year12[[i]][names(D[[i]]),2] <- D[[i]]
  year12[[i]][names(P[[i]]),3] <- P[[i]]
  sum <- colSums(year12[[i]])[1]-colSums(year12[[i]])[2]
  choose <- rep(rownames(year12[[i]]),year12[[i]][,1])
  
  
  do <- function(x){
    tmp <- sample(choose,sum,replace=F)
    replace <- as.vector(table(tmp))
    names(replace) <- names(table(tmp))
    new <- rep(0,nrow(year12[[i]]))
    names(new) <- rownames(year12[[i]])
    new[names(replace)] <- replace
    new <- new + year12[[i]][,3]
    a <- SimilarityTwo(cbind(year12[[i]][,1],new),0,"abundance","unequal weight")[2,1]
    b <- Horn.Est(cbind(year12[[i]][,1],new),"unequal")[2]
    c <- SimilarityTwo(cbind(year12[[i]][,1],new),2,"abundance","unequal weight")[2,1]
    return(c(a,b,c))
  }
  all[[i]] <- t(apply(t(sapply(1:500,do)),MARGIN = 2,function(x) quantile(x,c(0.025,0.975))))
  print(i)
}
result <- c()
for(i in 1:40){
  result <- c(result,result12_0[i],result12_1[i],result12_2[i]) 
  
}
all<-do.call(rbind,all)
all <- data.frame(all, result,row.names = NULL)
sig <- apply(all,MARGIN = 1,function(x){
  if(x[1]<x[3] & x[2]>x[3]){
    return("N")
  }else{
    return("Y")
  }
})
y <- rep(rep(seq(50,250,50),each=8),each=3)
x <- rep(rep(seq(50,400,50),5),each=3)
q <- rep(c(0,1,2),40)
all <- data.frame(all, x,y,q,sig,row.names = NULL)
allq0 <- filter(all, q==0)
allq1 <- filter(all, q==1)
allq2 <- filter(all, q==2)
ggplot(allq0,aes(x,y)) + geom_raster(aes(fill = sig), hjust=0.5, vjust=0.5, interpolate=FALSE)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(text=element_text(size=18),legend.title = element_text(size=15,face="bold"))+
  scale_fill_manual(values = c("black","white"),name="significance",breaks=c("N", "Y"),labels=c("N", "Y"))
ggplot(allq1,aes(x,y)) + geom_raster(aes(fill = sig), hjust=0.5, vjust=0.5, interpolate=FALSE)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(text=element_text(size=18),legend.title = element_text(size=15,face="bold"))+
  scale_fill_manual(values = c("black","white"),name="significance",breaks=c("N", "Y"),labels=c("N", "Y"))
ggplot(allq2,aes(x,y)) + geom_raster(aes(fill = sig), hjust=0.5, vjust=0.5, interpolate=FALSE)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(text=element_text(size=18),legend.title = element_text(size=15,face="bold"))+
  scale_fill_manual(values = c("black","white"),name="significance",breaks=c("N", "Y"),labels=c("N", "Y"))


