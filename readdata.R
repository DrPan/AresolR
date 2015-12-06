library(MCMCpack)
library(maps)
library(fields)
library(mapproj)
library(MASS)
library(vars)
######################
F<-function(K)
{
R<-560
C<-300
r<-14
c<-10
S<-K[1:R,1:C]
M<-matrix(NA,nrow=R/r,ncol=C/c)
for (i in 1:(R/r))
{
for(j in 1:(C/c))
{
M[i,j]<-mean(S[((i-1)*r+1):(r*i),((j-1)*c+1):(c*j)])
}
}
return(M)

}
#######

c<-matrix(1,168861,51)

C.1960 <- matrix(scan("1960.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,1]<-as.vector(C.1960)
C.1961 <- matrix(scan("1961.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,2]<-as.vector(C.1961)
C.1962 <- matrix(scan("1962.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,3]<-as.vector(C.1962)
C.1963 <- matrix(scan("1963.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,4]<-as.vector(C.1963)
C.1964 <- matrix(scan("1964.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,5]<-as.vector(C.1964)
C.1965 <- matrix(scan("1965.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,6]<-as.vector(C.1965)
C.1966 <- matrix(scan("1966.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,7]<-as.vector(C.1966)
C.1967 <- matrix(scan("1967.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,8]<-as.vector(C.1967)
C.1968 <- matrix(scan("1968.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,9]<-as.vector(C.1968)
C.1969 <- matrix(scan("1969.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,10]<-as.vector(C.1969)
C.1970 <- matrix(scan("1970.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,11]<-as.vector(C.1970)
C.1971 <- matrix(scan("1971.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,12]<-as.vector(C.1971)
C.1972 <- matrix(scan("1972.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,13]<-as.vector(C.1972)
C.1973 <- matrix(scan("1973.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,14]<-as.vector(C.1973)
C.1974 <- matrix(scan("1974.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,15]<-as.vector(C.1974)
C.1975 <- matrix(scan("1975.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,16]<-as.vector(C.1975)
C.1976 <- matrix(scan("1976.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,17]<-as.vector(C.1976)
C.1977 <- matrix(scan("1977.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,18]<-as.vector(C.1977)
C.1978 <- matrix(scan("1978.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,19]<-as.vector(C.1978)
C.1979 <- matrix(scan("1979.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,20]<-as.vector(C.1979)
C.1980 <- matrix(scan("1980.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,21]<-as.vector(C.1980)
C.1981 <- matrix(scan("1981.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,22]<-as.vector(C.1981)
C.1982 <- matrix(scan("1982.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,23]<-as.vector(C.1982)
C.1983 <- matrix(scan("1983.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,24]<-as.vector(C.1983)
C.1984 <- matrix(scan("1984.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,25]<-as.vector(C.1984)
C.1985 <- matrix(scan("1985.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,26]<-as.vector(C.1985)
C.1986 <- matrix(scan("1986.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,27]<-as.vector(C.1986)
C.1987 <- matrix(scan("1987.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,28]<-as.vector(C.1987)
C.1988 <- matrix(scan("1988.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,29]<-as.vector(C.1988)
C.1989 <- matrix(scan("1989.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,30]<-as.vector(C.1989)
C.1990 <- matrix(scan("1990.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,31]<-as.vector(C.1990)
C.1991 <- matrix(scan("1991.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,32]<-as.vector(C.1991)
C.1992 <- matrix(scan("1992.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,33]<-as.vector(C.1992)
C.1993 <- matrix(scan("1993.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,34]<-as.vector(C.1993)
C.1994 <- matrix(scan("1994.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,35]<-as.vector(C.1994)
C.1995 <- matrix(scan("1995.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,36]<-as.vector(C.1995)
C.1996 <- matrix(scan("1996.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,37]<-as.vector(C.1996)
C.1997 <- matrix(scan("1997.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,38]<-as.vector(C.1997)
C.1998 <- matrix(scan("1998.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,39]<-as.vector(C.1998)
C.1999 <- matrix(scan("1999.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,40]<-as.vector(C.1999)
C.2000 <- matrix(scan("2000.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,41]<-as.vector(C.2000)
C.2001 <- matrix(scan("2001.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,42]<-as.vector(C.2001)
C.2002 <- matrix(scan("2002.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,43]<-as.vector(C.2002)
C.2003 <- matrix(scan("2003.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,44]<-as.vector(C.2003)
C.2004 <- matrix(scan("2004.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,45]<-as.vector(C.2004)
C.2005 <- matrix(scan("2005.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,46]<-as.vector(C.2005)
C.2006 <- matrix(scan("2006.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,47]<-as.vector(C.2006)
C.2007 <- matrix(scan("2007.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,48]<-as.vector(C.2007)
C.2008 <- matrix(scan("2008.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,49]<-as.vector(C.2008)
C.2009 <- matrix(scan("2009.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,50]<-as.vector(C.2009)
C.2010 <- matrix(scan("2010.txt", n = 301*561), 561, 301, byrow = TRUE)
c[,51]<-as.vector(C.2010)

#######
Ynew<-array(NA,c(1200,51))
Ynew[,1]<-F(C.1960)
Ynew[,2]<-F(C.1961)
Ynew[,3]<-F(C.1962)
Ynew[,4]<-F(C.1963)
Ynew[,5]<-F(C.1964)
Ynew[,6]<-F(C.1965)
Ynew[,7]<-F(C.1966)
Ynew[,8]<-F(C.1967)
Ynew[,9]<-F(C.1968)
Ynew[,10]<-F(C.1969)
Ynew[,11]<-F(C.1970)
Ynew[,12]<-F(C.1971)
Ynew[,13]<-F(C.1972)
Ynew[,14]<-F(C.1973)
Ynew[,15]<-F(C.1974)
Ynew[,16]<-F(C.1975)
Ynew[,17]<-F(C.1976)
Ynew[,18]<-F(C.1977)
Ynew[,19]<-F(C.1978)
Ynew[,20]<-F(C.1979)
Ynew[,21]<-F(C.1980)
Ynew[,22]<-F(C.1981)
Ynew[,23]<-F(C.1982)
Ynew[,24]<-F(C.1983)
Ynew[,25]<-F(C.1984)
Ynew[,26]<-F(C.1985)
Ynew[,27]<-F(C.1986)
Ynew[,28]<-F(C.1987)
Ynew[,29]<-F(C.1988)
Ynew[,30]<-F(C.1989)
Ynew[,31]<-F(C.1990)
Ynew[,32]<-F(C.1991)
Ynew[,33]<-F(C.1992)
Ynew[,34]<-F(C.1993)
Ynew[,35]<-F(C.1994)
Ynew[,36]<-F(C.1995)
Ynew[,37]<-F(C.1996)
Ynew[,38]<-F(C.1997)
Ynew[,39]<-F(C.1998)
Ynew[,40]<-F(C.1999)
Ynew[,41]<-F(C.2000)
Ynew[,42]<-F(C.2001)
Ynew[,43]<-F(C.2002)
Ynew[,44]<-F(C.2003)
Ynew[,45]<-F(C.2004)
Ynew[,46]<-F(C.2005)
Ynew[,47]<-F(C.2006)
Ynew[,48]<-F(C.2007)
Ynew[,49]<-F(C.2008)
Ynew[,50]<-F(C.2009)
Ynew[,51]<-F(C.2010)

N=1200
mean<-apply(Ynew,1,mean)
one<-matrix(1,1,51)
Xnew<-Ynew-mean%*%one

s<-svd(Xnew)


d<-s$d
sqd<-d*d
n<-seq(1:51)
va<-seq(1:51)
for (i in 1:51)
{va[i]<-sum(sqd[1:1:i])}
per.va<-va/sum(sqd)
#plot(n,per.va)

K=5
k=20
U<-s$u[,1:K] #first K columns of U matrix
Ur<-s$u[,(K+1):k]
Uleft<-s$u[,(k+1):51]

####get some idea about H and mu prior
a<-matrix(nrow=K,ncol=dim(Xnew)[2])   #define the coefficient wrt Ui which is K*51

for(i in 1:51)
{a[,i]<-t(U)%*%Xnew[,i]}  #solve for each a(t) as ols

lambda1<-matrix(nrow=K,ncol=dim(Xnew)[2])    #reverse a's order for modeling
for(i in 1:51)
{lambda1[,i]<-a[,52-i]}

##Try AR(1)
vars<-VAR(t(lambda1),p=1,type = "const") 
coeff<-Bcoef(vars)  #use VAR to solve for H
coef(vars)
round(Bcoef(vars),3)
fitted(vars) 
summary(vars)

##plot of fitted lambdas and true lse lambdas.
index<-1:50
plot(index,fitted(vars)[,1],type="l",col="red",ylim=c(-6000,6000))
lines(index,t(lambda1)[2:51,1],type="l",col="blue")
legend("topleft", c("Fitted","Least Square"), 
lty=c(1,1), lwd=c(1,1),col=c("red","blue"))

plot(fitted(vars)[,1],t(lambda1)[2:51,1])
cor(fitted(vars)[,5],t(lambda1)[2:51,5])

##try fewer years
vars<-VAR(t(lambda1)[1:10,],p=1,type = "const")
index<-1:9
plot(index,fitted(vars)[,1],type="l",col="red",ylim=c(-6000,6000))
lines(index,t(lambda1)[2:10,1],type="l",col="blue")
legend("topleft", c("Fitted","Least Square"), 
lty=c(1,1), lwd=c(1,1),col=c("red","blue"))

###Try AR(2)
vars2<-VAR(t(lambda1),p=2,type = "const")   
round(Bcoef(vars2),3)
coef(vars2)
fitted(vars2)
summary(vars2)
##Plots
index<-1:49
plot(index,fitted(vars2)[,2],type="l",col="red",ylim=c(-5000,5000))
lines(index,t(lambda1)[3:51,2],type="l",col="blue")
legend("topleft", c("Fitted","Least Square"), 
lty=c(1,1), lwd=c(1,1),col=c("red","blue"))

cor(fitted(vars2)[,1],t(lambda1)[3:51,1])
cor(fitted(vars2)[,5],t(lambda1)[3:51,5])


#H<-coeff[,1:5]
#mu<-t(t(coeff[,6]))     #H:K*K matrix, mu:K*1 vector
#########


########### prior ##################

d0=diag((s$d[(K+1):k])^2)
c0=sum(s$d[(k+1):51]*s$d[(k+1):51])
SIGMo<-(c0*diag(1,N)+Ur%*%d0%*%t(Ur)) ##the matrix sigma without the scalar sigmaM

########### initialize ################
set.seed(5)
T<-2000
B<-array(NA,c(K,K,T))
mu<-array(NA,c(K,T))

mu_mu=coeff[,6]
mu_var=160000*diag(K)
mu[,1]<-mvrnorm(1,mu_mu,mu_var) ##could change mean and var in the prior


H<-array(NA,c(K,K,T))
h1<-array(NA,c(K,T))
h2<-array(NA,c(K-1,T))
h3<-array(NA,c(K-2,T))
h4<-array(NA,c(K-3,T))
h5<-array(NA,c(K-4,T))

mu_h1=as.vector(c(0.1,-0.1,-0.15,0.15,0.1))
cov_h1=0.1*diag(K)
mu_h2=as.vector(c(0.1,-0.1,-0.1,-0.08))
cov_h2=0.1*diag(K-1)
mu_h3=as.vector(c(0,0.05,0))
cov_h3=0.1*diag(K-2)
mu_h4=as.vector(c(-0.25,-0.25))
cov_h4=0.1*diag(K-3)
mu_h5=as.vector(c(-0.1))
cov_h5=0.1*diag(K-4)

h1[,1]=mvrnorm(1,mu_h1,cov_h1)
h2[,1]=mvrnorm(1,mu_h2,cov_h2)
h3[,1]=mvrnorm(1,mu_h3,cov_h3)
h4[,1]=mvrnorm(1,mu_h4,cov_h4)
h5[,1]=mvrnorm(1,mu_h5,cov_h5)
H[,,1]=diag(h1[,1])+rbind(cbind(0,diag(h2[,1])),0)+
rbind(cbind(0,0,diag(h3[,1])),0,0)+rbind(cbind(0,0,0,diag(h4[,1])),0,0,0)+
rbind(cbind(0,0,0,0,as.matrix(h5[,1])),0,0,0,0)



## Inverse Gamma prior ##
sigmaM<-array(NA,T)
c<-6
d<-7
sigmaM[1]<-rinvgamma(1,c,d)


## Inverse Wishart prior 
v<-10
psy<-100000*diag(K)
B[,,1]<-riwish(v,psy)

## Normal prior on lambdaT##
AT<-100000*diag(K)
UT<-matrix(c(3000,-3800,400,-2200,1300),nrow=5,byrow=TRUE)
lambda<-array(NA,c(K,51,T))

lambda[,51,1]<-mvrnorm(1,UT,AT)

for(j in 1:50){
lambda[,51-j,1]<-mvrnorm(1,H[,,1]%*%lambda[,52-j,1]+mu[,1],B[,,1])
}




########### Gibbs sampling ###########
for(i in 2:T)
{
invB<-solve(B[,,i-1])
invAT<-solve(AT)
Sigma<-1/(sigmaM[i-1])*solve(SIGMo) ##inverse of sigma actually

## update lambda 51 ####
cov1<-solve(invAT+t(H[,,i-1])%*%invB%*%H[,,i-1]+t(U)%*%Sigma%*%U)
mean1<-cov1%*%(invAT%*%UT+t(H[,,i-1])%*%invB%*%(lambda[,50,i-1]-mu[,i-1])+t(U)%*%Sigma%*%Xnew[,51])
lambda[,51,i]<-mvrnorm(n=1,mean1,cov1,tol=1e-6,empirical=FALSE)

## update lambda 2,3,4,up to 50 ####i=3


for(j in 2:50) 
{
cov2<-solve(t(H[,,i-1])%*%invB%*%H[,,i-1]+invB+t(U)%*%Sigma%*%U)
mean2<-cov2%*%(t(H[,,i-1])%*%invB%*%(lambda[,51-j,i-1]-mu[,i-1])+invB%*%(H[,,i-1]%*%lambda[,53-j,i]+mu[,i-1])+t(U)%*%Sigma%*%Xnew[,52-j])
lambda[,52-j,i]<-mvrnorm(n=1,mean2,cov2,tol=1e-6,empirical=FALSE)
}

## update lambda 1 ####    !!!!cov3 becomes really large since i=3, even larger for i=4
cov3<-solve(invB+t(U)%*%Sigma%*%U)
mean3<-cov3%*%(invB%*%(H[,,i-1]%*%lambda[,2,i]+mu[,i-1])+t(U)%*%Sigma%*%Xnew[,1])
lambda[,1,i]<-mvrnorm(n=1,mean3,cov3,tol=1e-6,empirical=FALSE)

## update sigmaM ####large matrix
SIG<-solve(SIGMo)
temp2<-0
for (s in 1: 51)
{
temp2<-temp2+t(Xnew[,s]-U%*%lambda[,s,i])%*%SIG%*%(Xnew[,s]-U%*%lambda[,s,i])
}

cp<-c+N*51/2
dp<-d+temp2/2
sigmaM[i]<-rinvgamma(1,cp,dp)  

## update B ##           
temp3<-array(0,c(K,K))
for (j in 1:50)
{
temp3<-temp3+(lambda[,51-j,i]-H[,,i-1]%*%lambda[,52-j,i]-mu[,i-1])%*%t(lambda[,51-j,i]-H[,,i-1]%*%lambda[,52-j,i]-mu[,i-1])
}
vp<-50+v
##eigen<-eigen(temp3+psy) 
##psyp<-eigen$values[1]*eigen$vector[,1]%*%t(eigen$vector[,1])+eigen$values[2]*eigen$vector[,2]%*%t(eigen$vector[,2]) 
##Why truncating not work??

B[,,i]<-riwish(vp,temp3+psy) ##!!!Too large for B when i=3,then can't do it for i=4
UinvB=solve(B[,,i])
## updata mu
cov4=solve(50*UinvB+solve(mu_var))
temp4=rep(0,K)
for(j in 1:50)
{
temp4=temp4+(lambda[,51-j,i]-H[,,i-1]%*%lambda[,52-j,i]) 
}

mean4=cov4%*%(UinvB%*%temp4+solve(mu_var)%*%mu_mu)
mu[,i]=mvrnorm(1,mean4,cov4)

## update H ##
COV<-array(0,c(K,K,K,50))
for (l in 1:50){
for (j in 1:K) {
vector=as.vector(rbind(t(t(lambda[,(52-l),i][j:K])),array(0,c(j-1,1))))
COV[,,j,l]=diag(vector)  ##T(2010) corresponds to first year of COV
}}


#update h1
temp5=array(0,c(K,K))
for (j in 1:50){
temp5=temp5+t(COV[,,1,j])%*%UinvB%*%COV[,,1,j] }

temp6=array(0,c(K,1))

for (j in 1:50){
temp6=temp6+UinvB%*%(lambda[,51-j,i]-mu[,i]-COV[,,2,j]%*%rbind(t(t(h2[,i-1])),0)
      -COV[,,3,j]%*%rbind(t(t(h3[,i-1])),0,0)-COV[,,4,j]%*%rbind(t(t(h4[,i-1])),0,0,0)
      -COV[,,5,j]%*%rbind(t(t(h5[,i-1])),0,0,0,0))
}
Ucov_h1=solve(temp5+solve(cov_h1))
Umean_h1=Ucov_h1%*%(temp6+solve(cov_h1)%*%mu_h1)

h1[,i]=mvrnorm(1,Umean_h1,Ucov_h1)
#update h2
temp7=array(0,c(K,K))
for (j in 1:50){
temp7=temp7+t(COV[,,2,j])%*%UinvB%*%COV[,,2,j] }
temp7=temp7[1:4,1:4]    ##actually 1:(K-1)

temp8=array(0,c(K,1))
for (j in 1:50){
temp8=temp8+UinvB%*%(lambda[,51-j,i]-mu[,i]-COV[,,1,j]%*%t(t(h1[,i]))
      -COV[,,3,j]%*%rbind(t(t(h3[,i-1])),0,0)-COV[,,4,j]%*%rbind(t(t(h4[,i-1])),0,0,0)
      -COV[,,5,j]%*%rbind(t(t(h5[,i-1])),0,0,0,0)) }
temp8=temp8[1:4]       ##actually 1:(K-1)
 
Ucov_h2=solve(temp7+solve(cov_h2))
Umean_h2=Ucov_h2%*%(temp8+solve(cov_h2)%*%mu_h2)
h2[,i]=mvrnorm(1,Umean_h2,Ucov_h2)
#update h3
temp9=array(0,c(K,K))
for (j in 1:50){
temp9=temp9+t(COV[,,3,j])%*%UinvB%*%COV[,,3,j] }
temp9=temp9[1:3,1:3]       ##actually 1:(K-2)

temp10=array(0,c(K,1))
for (j in 1:50){
temp10=temp10+UinvB%*%(lambda[,51-j,i]-mu[,i]-COV[,,1,j]%*%t(t(h1[,i]))
      -COV[,,2,j]%*%rbind(t(t(h2[,i])),0)-COV[,,4,j]%*%rbind(t(t(h4[,i-1])),0,0,0)
      -COV[,,5,j]%*%rbind(t(t(h5[,i-1])),0,0,0,0)) }
temp10=temp10[1:3]     ##actually 1:(K-2)

Ucov_h3=solve(temp9+solve(cov_h3))
Umean_h3=Ucov_h3%*%(temp10+solve(cov_h3)%*%mu_h3)
h3[,i]=mvrnorm(1,Umean_h3,Ucov_h3)

#update h4
temp11=array(0,c(K,K))
for (j in 1:50){
temp11=temp11+t(COV[,,4,j])%*%UinvB%*%COV[,,4,j] }
temp11=temp11[1:2,1:2]       ##actually 1:(K-3)

temp12=array(0,c(K,1))
for (j in 1:50){
temp12=temp12+UinvB%*%(lambda[,51-j,i]-mu[,i]-COV[,,1,j]%*%t(t(h1[,i]))
      -COV[,,2,j]%*%rbind(t(t(h2[,i])),0)-COV[,,3,j]%*%rbind(t(t(h3[,i])),0,0)
      -COV[,,5,j]%*%rbind(t(t(h5[,i-1])),0,0,0,0)) }
temp12=temp12[1:2]     ##actually 1:(K-3)

Ucov_h4=solve(temp11+solve(cov_h4))
Umean_h4=Ucov_h4%*%(temp12+solve(cov_h4)%*%mu_h4)
h4[,i]=mvrnorm(1,Umean_h4,Ucov_h4)

#update h5
temp13=array(0,c(K,K))
for (j in 1:50){
temp13=temp13+t(COV[,,5,j])%*%UinvB%*%COV[,,5,j] }
temp13=temp13[1,1]       ##actually 1:(K-4)

temp14=array(0,c(K,1))
for (j in 1:50){
temp14=temp14+UinvB%*%(lambda[,51-j,i]-mu[,i]-COV[,,1,j]%*%t(t(h1[,i]))
      -COV[,,2,j]%*%rbind(t(t(h2[,i])),0)-COV[,,3,j]%*%rbind(t(t(h3[,i])),0,0)
      -COV[,,4,j]%*%rbind(t(t(h4[,i])),0,0,0)) }
temp14=temp14[1]     ##actually 1:(K-4)

Ucov_h5=solve(temp13+solve(cov_h5))
Umean_h5=Ucov_h5%*%(temp14+solve(cov_h5)%*%mu_h5)

h5[,i]=mvrnorm(1,Umean_h5,Ucov_h5)

H[,,i]=diag(h1[,i])+rbind(cbind(0,diag(h2[,i])),0)+
rbind(cbind(0,0,diag(h3[,i])),0,0)+rbind(cbind(0,0,0,diag(h4[,i])),0,0,0)+
rbind(cbind(0,0,0,0,as.matrix(h5[,i])),0,0,0,0)

}


plot(sigmaM,type="o",main="sigmaM square")
index<-9000:10000
par(mfrow=c(1,5))

sd(sigmaM[401:2000])
mean(sigmaM[401:2000])


par(mfrow=c(1,5))
plot(lambda[1,51,]) ##the lambda 2010's first entry
plot(lambda[2,51,]) ##the lambda 2010's second entry
plot(lambda[3,51,])
plot(lambda[4,51,])
plot(lambda[5,51,])

##Running Mean starting from 401
mean1_51<-rep(0,1600)
for (j in 1:1600)
{mean1_51[j]=mean(lambda[1,51,401:(400+j)])
}
mean2_51<-rep(0,1600)
for (j in 1:1600)
{mean2_51[j]=mean(lambda[2,51,401:(400+j)])
}
mean3_51<-rep(0,1600)
for (j in 1:1600)
{mean3_51[j]=mean(lambda[3,51,401:(400+j)])
}
mean4_51<-rep(0,1600)
for (j in 1:1600)
{mean4_51[j]=mean(lambda[4,51,401:(400+j)])
}
mean5_51<-rep(0,1600)
for (j in 1:1600)
{mean5_51[j]=mean(lambda[5,51,401:(400+j)])
}
par(mfrow=c(1,5))
plot(mean1_51) ##the lambda 2010's first entry
plot(mean2_51) ##the lambda 2010's second entry
plot(mean3_51)
plot(mean4_51)
plot(mean5_51)

sd(lambda[5,51,401:2000])
mean(lambda[5,51,401:2000])

par(mfrow=c(1,5))
plot(lambda[1,1,])
plot(lambda[2,1,]) 
plot(lambda[3,1,])
plot(lambda[4,1,])
plot(lambda[5,1,]) ##the lambda 1960's first entry


##Running Mean of size 100 starting from 401
mean1_1<-rep(0,1600)
for (j in 1:1600)
{mean1_1[j]=mean(lambda[1,1,401:(400+j)])
}
mean2_1<-rep(0,1600)
for (j in 1:1600)
{mean2_1[j]=mean(lambda[2,1,401:(400+j)])
}
mean3_1<-rep(0,1600)
for (j in 1:1600)
{mean3_1[j]=mean(lambda[3,1,401:(400+j)])
}
mean4_1<-rep(0,1600)
for (j in 1:1600)
{mean4_1[j]=mean(lambda[4,1,401:(400+j)])
}
mean5_1<-rep(0,1600)
for (j in 1:1600)
{mean5_1[j]=mean(lambda[5,1,401:(400+j)])
}
par(mfrow=c(1,5))
plot(mean1_1) 
plot(mean2_1) 
plot(mean3_1)
plot(mean4_1)
plot(mean5_1)
sd(lambda[3,1,401:2000])
mean(lambda[3,1,401:2000])
sd(lambda[4,1,401:2000])
mean(lambda[4,1,401:2000])
sd(lambda[5,1,401:2000])
mean(lambda[5,1,401:2000])


sd(lambda[2,2,401:2000])
mean(lambda[2,2,401:2000])
sd(lambda[3,2,401:2000])
mean(lambda[3,2,401:2000])
sd(lambda[4,2,401:2000])
mean(lambda[4,2,401:2000])
sd(lambda[5,2,401:2000])
mean(lambda[5,2,401:2000])



#1981
par(mfrow=c(1,5))
plot(lambda[1,22,])
plot(lambda[2,22,]) 
plot(lambda[3,22,])
plot(lambda[4,22,])
plot(lambda[5,22,]) 

##Running Mean starting from 401
mean1_22<-rep(0,1600)
for (j in 1:1600)
{mean1_22[j]=mean(lambda[1,22,401:(400+j)])
}
mean2_22<-rep(0,1600)
for (j in 1:1600)
{mean2_22[j]=mean(lambda[2,22,401:(400+j)])
}
mean3_22<-rep(0,1600)
for (j in 1:1600)
{mean3_22[j]=mean(lambda[3,22,401:(400+j)])
}
mean4_22<-rep(0,1600)
for (j in 1:1600)
{mean4_22[j]=mean(lambda[4,22,401:(400+j)])
}
mean5_22<-rep(0,1600)
for (j in 1:1600)
{mean5_22[j]=mean(lambda[5,22,401:(400+j)])
}
par(mfrow=c(1,5))
plot(mean1_22) 
plot(mean2_22) 
plot(mean3_22)
plot(mean4_22)
plot(mean5_22)


#############MU
mu[,2000]
mu[,1999]

par(mfrow=c(1,5))
plot(mu[1,])
plot(mu[2,])
plot(mu[3,])
plot(mu[4,])
plot(mu[5,])

##Running Mean for mu starting from 401
mean1<-rep(0,1600)
for (j in 1:1600)
{mean1[j]=mean(mu[1,401:(400+j)])
}
mean2<-rep(0,1600)
for (j in 1:1600)
{mean2[j]=mean(mu[2,401:(400+j)])
}
mean3<-rep(0,1600)
for (j in 1:1600)
{mean3[j]=mean(mu[3,401:(400+j)])
}
mean4<-rep(0,1600)
for (j in 1:1600)
{mean4[j]=mean(mu[4,401:(400+j)])
}
mean5<-rep(0,1600)
for (j in 1:1600)
{mean5[j]=mean(mu[5,401:(400+j)])
}
par(mfrow=c(1,5))
plot(mean1)
plot(mean2)
plot(mean3)
plot(mean4)
plot(mean5)
mean(mu[1,401:2000])
sd(mu[1,401:2000])
mean(mu[2,401:2000])
sd(mu[2,401:2000])
mean(mu[3,401:2000])
sd(mu[3,401:2000])
mean(mu[4,401:2000])
sd(mu[4,401:2000])
mean(mu[5,401:2000])
sd(mu[5,401:2000])
########
B[,,2000]
B[,,1999]
H[,,2000]
H[,,1999]

#####h1
par(mfrow=c(1,5))
plot(h1[1,])
plot(h1[2,])
plot(h1[3,])
plot(h1[4,])
plot(h1[5,])
##Running Mean for h1 of size 100 starting from 401
mean1<-rep(0,1600)
for (j in 1:1600)
{mean1[j]=mean(h1[1,401:(400+j)])
}
mean2<-rep(0,1600)
for (j in 1:1600)
{mean2[j]=mean(h1[2,401:(400+j)])
}
mean3<-rep(0,1600)
for (j in 1:1600)
{mean3[j]=mean(h1[3,401:(400+j)])
}
mean4<-rep(0,1600)
for (j in 1:1600)
{mean4[j]=mean(h1[4,401:(400+j)])
}
mean5<-rep(0,1600)
for (j in 1:1600)
{mean5[j]=mean(h1[5,401:(400+j)])
}
par(mfrow=c(1,5))
plot(mean1)
plot(mean2)
plot(mean3)
plot(mean4)
plot(mean5)

mean(h1[1,401:2000])
sd(h1[1,401:2000])
mean(h1[2,401:2000])
sd(h1[2,401:2000])
mean(h1[3,401:2000])
sd(h1[3,401:2000])
mean(h1[4,401:2000])
sd(h1[4,401:2000])
mean(h1[5,401:2000])
sd(h1[5,401:2000])


#####h2
par(mfrow=c(1,4))
plot(h2[1,])
plot(h2[2,])
plot(h2[3,])
plot(h2[4,])

##Running Mean for h2 of size 100 starting from 401
mean1<-rep(0,1501)
for (j in 1:1501)
{mean1[j]=mean(h2[1,(400+j):(499+j)])
}
mean2<-rep(0,1501)
for (j in 1:1501)
{mean2[j]=mean(h2[2,(400+j):(499+j)])
}
mean3<-rep(0,1501)
for (j in 1:1501)
{mean3[j]=mean(h2[3,(400+j):(499+j)])
}
mean4<-rep(0,1501)
for (j in 1:1501)
{mean4[j]=mean(h2[4,(400+j):(499+j)])
}

par(mfrow=c(1,4))
plot(mean1)
plot(mean2)
plot(mean3)
plot(mean4)


mean(h2[1,401:2000])
sd(h2[1,401:2000])
mean(h2[2,401:2000])
sd(h2[2,401:2000])
mean(h2[3,401:2000])
sd(h2[3,401:2000])
mean(h2[4,401:2000])
sd(h2[4,401:2000])

#####h3
par(mfrow=c(1,3))
plot(h3[1,])
plot(h3[2,])
plot(h3[3,])


##Running Mean for h3 of size 100 starting from 401
mean1<-rep(0,1501)
for (j in 1:1501)
{mean1[j]=mean(h3[1,(400+j):(499+j)])
}
mean2<-rep(0,1501)
for (j in 1:1501)
{mean2[j]=mean(h3[2,(400+j):(499+j)])
}
mean3<-rep(0,1501)
for (j in 1:1501)
{mean3[j]=mean(h3[3,(400+j):(499+j)])
}


par(mfrow=c(1,3))
plot(mean1)
plot(mean2)
plot(mean3)



mean(h3[1,401:2000])
sd(h3[1,401:2000])
mean(h3[2,401:2000])
sd(h3[2,401:2000])
mean(h3[3,401:2000])
sd(h3[3,401:2000])


#####h4
par(mfrow=c(1,2))
plot(h4[1,])
plot(h4[2,])



##Running Mean for h4 of size 100 starting from 401
mean1<-rep(0,1501)
for (j in 1:1501)
{mean1[j]=mean(h4[1,(400+j):(499+j)])
}
mean2<-rep(0,1501)
for (j in 1:1501)
{mean2[j]=mean(h4[2,(400+j):(499+j)])
}

par(mfrow=c(1,2))
plot(mean1)
plot(mean2)


mean(h4[1,401:2000])
sd(h4[1,401:2000])
mean(h4[2,401:2000])
sd(h4[2,401:2000])

#####h5
plot(h5[1,])



##Running Mean for h5 of size 100 starting from 401
mean1<-rep(0,1501)
for (j in 1:1501)
{mean1[j]=mean(h5[1,(400+j):(499+j)])
}

plot(mean1)



mean(h5[1,401:2000])
sd(h5[1,401:2000])


B[,,2000]
B[,,1999]

