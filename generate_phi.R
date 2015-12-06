m=3
dummy <- runif(m, 0.1, 1)
p1 <- dummy^2
p2 <- 2 * (1-dummy) * dummy
p3 <- (1-dummy)^2
p=10000
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


