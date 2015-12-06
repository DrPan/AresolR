library(Matrix)
library(mvtnorm)
library(pscl)
set.seed(1)


ptm <- proc.time()

### Set the parameter ###

n=110
p=15000
m=50
L=50

filename = paste(Sys.time(),'p=15000m=50z=100','.RData')

epi=10^-1 #you can change to set different N's!
max_z=100 #you can change to set the range of z's!

X=matrix(NA,nrow=n,ncol=p)

for(i in 1:n){
Xburn=array(NA,500) #can change the number of burning period
Xburn[1]=rnorm(1,0,1)
for(j in 2:500){
Xburn[j]=0.5*Xburn[j-1]+rnorm(1,0,1)}
X[i,1]=Xburn[500]
for(k in 2:p){
X[i,k]=0.5*X[i,(k-1)]+rnorm(1,0,1)}
}

SIG_beta=diag(m) #SIG(beta)
a1=6  #pi0
b1=1  #pi0

#Generate Y to use

sigma_true=1
gamma_true=rep(0,p)
gamma_true[1:5]=1
#gamma_true[1]=1
#gamma_true[2]=1
#gamma_true[3]=1
#gamma_true[4]=1
#gamma_true[5]=1

Y_gen=mvrnorm(1,X%*%gamma_true,sigma_true*diag(n))


## Generate PHI
dummy <- runif(m, 0.1, 1)
p1 <- dummy^2
p2 <- 2 * (1-dummy) * dummy
p3 <- (1-dummy)^2
PHI<-matrix(NA,nrow=m,ncol=p)

for (i in 1:m){
PHI_test <- runif(p,0,1)
PHI_temp <- array(NA,p)
PHI_temp[PHI_test <= p1[i]] <- -sqrt(1/dummy[i])
PHI_temp[(PHI_test <= (p1[i]+p2[i]))&(PHI_test>p1[i])] <- 0
PHI_temp[(PHI_test <= 1)&(PHI_test> (p1[i]+p2[i]))] <- sqrt(1/dummy[i])
 
PHI[i,]<- PHI_temp
}

Q <- matrix(0,p,m)
R <- matrix(0,m,m)

for (i in 1:m){
  v <- t(PHI[i,])
  if (i>1){
  for (j in 1:(i-1)){
    R[j,i] <- Q[,j] %*% PHI[i,]
    v <- v-R[j,i]*Q[,j]
  }
  }
  R[i,i] <- sum(abs(v)^2)^(1/2)
  Q[,i] <- v/R[i,i]
}

PHI = t(Q)

## Get H0(z)
Y=Y_gen

mode=b1/(a1+1)
#epi=10^-1  #you can change to set different N's!
#max_z=100 #you can change to set the range of z's!
sigma=seq(from=mode,to=mode+max_z,by=epi)
N=length(sigma)

F=function(sig){
A=1/sqrt(det(PHI%*%t(X)%*%X%*%t(PHI)+solve(SIG_beta)))/((2*pi)^(n/2)*sig^(n/2)
*(det(SIG_beta))^(1/2))

B=exp(-(t(Y)%*%(diag(n)-X%*%t(PHI)%*%solve(PHI%*%t(X)%*%X%*%t(PHI)+solve(SIG_beta))
%*%PHI%*%t(X))%*%Y)/(2*sig))

return(A*B)}

H0_temp=array(NA,length(sigma))
sum_temp=0
H0=array(NA,length(sigma))
for(i in 1:length(sigma)){
H0_temp[i]=F(sigma[i])
sum_temp=sum_temp+F(sigma[i])
H0[i]=1/i*(sum_temp)}
rm(sum_temp)

plot1name = paste('plot1',Sys.time(),'.png')
png(plot1name)
plot(sigma,H0,type="l",main="H0 function",col="red")      #the approximate H0(z) from alpha0 to alpha0+mam_z
dev.off()


## Get Hg(z)
#for each sigma, we get L betas.
#L=50
Beta_sig=function(sig){
sigbeta=solve((PHI%*%t(X)%*%X%*%t(PHI)+solve(SIG_beta))/sig)
meanbeta=solve(PHI%*%t(X)%*%X%*%t(PHI)+solve(SIG_beta))%*%PHI%*%t(X)%*%Y
return(mvrnorm(L,meanbeta,sigbeta))

}
#L*3 matrix

Insider=function(bet,sig){
AA=bet[1]/sqrt(det(PHI%*%t(X)%*%X%*%t(PHI)+solve(SIG_beta)))/
((2*pi*sig)^(n/2)*det(SIG_beta)^(1/2))*exp(-(t(Y)%*%(diag(n)-X%*%t(PHI)%*%solve(PHI%*%t(X)%*%X%*%t(PHI)+solve(SIG_beta))
%*%PHI%*%t(X))%*%Y)/(2*sig))
return(AA)}

Expect_bet=function(sig,L){
sum=0
for(j in 1:L){
sum=sum+Insider(Beta_sig(sig)[j,],sig)}
return(sum/L)}    #for a single x value

sum_temp=0
Hg=array(NA,length(sigma))

for(i in 1:length(sigma)){
sum_temp=sum_temp+Expect_bet(sigma[i],L)
Hg[i]=1/i*sum_temp}

plot2name = paste('plot2',Sys.time(),'.png')
png(plot2name)
plot(sigma,Hg,type="l")
dev.off()





## Get a
num=gamma(a1+n/2)/gamma(a1)*b1^a1
denom=(2*pi)^(n/2)*sqrt(det(SIG_beta)*det(PHI%*%t(X)%*%X%*%t(PHI)+solve(SIG_beta)))*(b1+t(Y)%*%(diag(n)-X%*%t(PHI)%*%solve(PHI%*%t(X)%*%X%*%t(PHI)+solve(SIG_beta))
%*%PHI%*%t(X))%*%Y/2)^(a1+n/2)

marginY=num/denom
epsilon=0.2
a=(1-epsilon)/epsilon*marginY

## Get a0
M=1000
betnow=mvrnorm(M,rep(0,m),diag(m))
FFF=function(bet){
bet[1]*sqrt(det(SIG_beta))*exp(t(bet)%*%solve(SIG_beta)%*%bet/2)*
gamma(a1+(n+m)/2)/gamma(a1+n/2)/(b1+(t(Y-X%*%t(PHI)%*%bet)%*%(Y-X%*%t(PHI)%*%bet)+
t(bet)%*%solve(SIG_beta)%*%bet)/2)^(a1+(n+m)/2)*
(b1+(t(Y)%*%(diag(n)-X%*%t(PHI)%*%solve(PHI%*%t(X)%*%X%*%t(PHI)+solve(SIG_beta))%*%PHI%*%
t(X))%*%Y)/2)^(a1+n/2)}

sum_temp=0
for(i in 1:M){
sum_temp=sum_temp+FFF(betnow[i,])}

a0=a*1/M*sum_temp
rm(sum_temp£©
 
a=as.numeric(a)
a0=as.numeric(a0)


#a= 0.0007337201
#a0=-2.649831e-05


#Get target function

target=array(NA,length(sigma))
for(i in 1:length(sigma)){
target[i]=(a0+Hg[i])/(a+H0[i])}

plot3name = paste('plot3',Sys.time(),'.png')
png(plot3name)
plot(sigma,target,type="l",col="blue",main="Posterior Mean of Beta[1]")
dev.off()

which.max(target)
target[which.max(target)]
sigma[which.max(target)]
which.min(target)
target[which.min(target)]
sigma[which.min(target)]

proc.time() - ptm

##continued.. for linear combination of gammas!

Insider=function(bet,sig){
AA=(t(PHI)[1,]%*%bet)/sqrt(det(PHI%*%t(X)%*%X%*%t(PHI)+solve(SIG_beta)))/
((2*pi*sig)^(n/2)*det(SIG_beta)^(1/2))*exp(-(t(Y)%*%(diag(n)-X%*%t(PHI)%*%solve(PHI%*%t(X)%*%X%*%t(PHI)+solve(SIG_beta))
%*%PHI%*%t(X))%*%Y)/(2*sig))
return(AA)}

Expect_bet=function(sig,L){
sum=0
for(j in 1:L){
sum=sum+Insider(Beta_sig(sig)[j,],sig)}
return(sum/L)}    #for a single x value

sum_temp=0
Hg=array(NA,length(sigma))

for(i in 1:length(sigma)){
sum_temp=sum_temp+Expect_bet(sigma[i],L)
Hg[i]=1/i*sum_temp}

plot4name = paste('plot4',Sys.time().'.png')
png(plot4name)
plot(sigma,Hg,type="l",main="Hg function")
dev.off()

#targetnew
targetnew=array(NA,length(sigma))
for(i in 1:length(sigma)){
target[i]=(a0+Hg[i])/(a+H0[i])}

plot5name = paste('plot5',Sys.time(),'.png')
png(plot5name)
plot(sigma,target,type="l",main="Function of Gamma[1]",col="blue")
dev.off()

which.max(targetnew)
target[which.max(targenew)]
sigma[which.max(targetnew)]
which.min(targetnew)
target[which.min(targetnew)]
sigma[which.min(targetnew)]

save.image(file = filename)







