## ----mwomen-------------------------------------------------------------------
str(women)
pairs(women)

## ----CO2----------------------------------------------------------------------
x = CO2[,4:5]
y = xtabs(data = x)
y
class(y)

## -----------------------------------------------------------------------------
set.seed(1111)
n <- 1000
u <- runif(n)
x <-2/sqrt(1-u) #F(x)=1-(2/x)^2, x>=2>0
hist(x,prob = TRUE, main = expression(f(x)==8/x^3))
y <- seq(2, 100, .1)
lines(y, 8/y^3)


## -----------------------------------------------------------------------------
f<-function(n){
  x<-numeric(n)
  u_1<- runif(n,-1,1)
  u_2<- runif(n,-1,1)
  u_3<- runif(n,-1,1)
  for(i in 1:n){
    if(abs(u_3[i])>=abs(u_2[i]) && abs(u_3[i])>=abs(u_1[i])){
      x[i]<-u_2[i]
    }
    else{
      x[i]<-u_3[i]
    }
  }
  return(x)
}
y<-f(100)#输入你想生成的随机数的个数即可
hist(y,prob = TRUE, main = expression(f(x)==3/4*(1-x**2)))
z<-seq(-1,1,0.01)
lines(z,3/4*(1-z**2),col='green')

## -----------------------------------------------------------------------------
n<- 1000
u<- runif(n)
y<-2/(1-u)**(1/4)-2 #F(y)=1-(2/(y+2)^4), y>=0
y
hist(y,prob = TRUE, main = expression(f(y)==64*(2+y)**(-5)))
x<-seq(0,100,0.1)
lines(x,64*(2+x)**(-5),col='red')


## -----------------------------------------------------------------------------
number<- 100000
uni<- runif(number,0,pi/3)
Giao<-mean(sin(uni))*(pi/3)
print(Giao)
print(paste('The true value is: ',1-cos(pi/3)))
delta=abs(Giao-(1-cos(pi/3)))
print(paste("We can see that the results is very colse, only a different of:",delta))

## -----------------------------------------------------------------------------
n<-1000000
Giao<-runif(n,0,1)
Trump_1<-exp(Giao)
slice<-Giao[0:n/2]
Trump_2<-(exp(slice)+exp(1-slice))/2
print(mean(Trump_1))
print(mean(Trump_2))
k<-(var(Trump_1)-var(Trump_2))/var(Trump_1)
print(paste('The empirical estimate of the percent reducation in variance using the antithetic variate is:',k))
print(paste('The true value is: ',1/2-(-exp(2)+3*exp(1)-1)/(exp(2)-1-2*(exp(1)-1)**2)))
delta=abs(1/2-(-exp(2)+3*exp(1)-1)/(exp(2)-1-2*(exp(1)-1)**2)-k)
print(paste("We can see that the results is very close, only a different of:",delta))

## -----------------------------------------------------------------------------
n<- 100000
hat_theta<-Var<-numeric(2)
g_x<-function(x){
  exp(-x^2/2)*x^2/sqrt(2*pi) * (x>1)
}
# Using f_1
x_1<-rnorm(n)
f_1g<-g_x(x_1)/(1/sqrt(2*pi)*exp(-(x_1**2)/2))
hat_theta[1]<-mean(f_1g)
Var[1]<-var(f_1g)

#Using f_2
u<-runif(n)
x_2<-sqrt(-2*log(1-u))#inverse transform method
f_2g<-g_x(x_2)/((x_2)*exp(-(x_2^2)/2))
hat_theta[2]<-mean(f_2g)
Var[2]<-var(f_2g)
rbind(hat_theta,Var)



## -----------------------------------------------------------------------------
n<- 10000
hat_theta<-se<-numeric(2)
g_x<-function(x){
  exp(-x-log(1+x^2)) * (x>0) * (x<1)
}
#f3, inverse transform method
u <- runif(n) 
x <- - log(1 - u * (1 - exp(-1)))
fg <- g_x(x) / (exp(-x) / (1 - exp(-1)))
hat_theta[1] <- mean(fg) 
se[1] <- sd(fg)

#Stratified importance sampling
k<-c(1,2,3,4,5)
mean_1<-numeric(5)
se_1<-numeric(5)

for(i in k){
  uni <- runif(n,(i-1)/5,i/5)
  x <- -log(1-uni*(1-exp(-1)))
  g_x<-function(x){
  exp(-x-log(1+x^2)) * (x>0) * (x<1) * (x> -log(1-(i-1)/5*(1-exp(-1)))) * (x< -log(1-i/5*(1-exp(-1))))
}
  fg <- g_x(x) /(5*exp(-x)/(1-exp(-1)))
  mean_1[i]<-mean(fg)
  se_1[i]<-var(fg)
}
hat_theta[2]<-sum(mean_1)
se[2]<-sqrt(sum(se_1))

rbind(hat_theta,se)


## -----------------------------------------------------------------------------
m<-20
alpha<-0.05
set.seed(2020)
LCL<-numeric(1000)
for(i in 1:1000){
  x<-rlnorm(m,meanlog = 0,sdlog = 1)
  y = log(x)
  LCL[i]<-mean(y)-sd(y)/sqrt(m)*qt(1-alpha/2,df = m-1)
}

UCL<-numeric(1000)
for(i in 1:1000){
  x<-rlnorm(m,meanlog = 0,sdlog = 1)
  y = log(x)
  UCL[i]<-mean(y)+sd(y)/sqrt(m)*qt(1-alpha/2,df = m-1)
}

mean(LCL<0 & UCL>0)


## -----------------------------------------------------------------------------
m<-20
alpha<-0.05
set.seed(2021)
LCL<-numeric(1000)
for(i in 1:1000){
  x<-rchisq(m,df = 2)
  LCL[i]<-mean(x)-sd(x)/sqrt(m)*qt(1-alpha/2,df = m-1)
}

UCL<-numeric(1000)
for(i in 1:1000){
  x<-rchisq(m,df = 2)
  UCL[i]<-mean(x)+sd(x)/sqrt(m)*qt(1-alpha/2,df = m-1)
}
mean(LCL<2 & UCL>2)

## -----------------------------------------------------------------------------
alpha <-0.1 
n<-30 
m <- 2000 
para <-seq(1,100,1) 
N <- length(para) 
power <- numeric(N) #critical value for the skewness test 
cv <- qnorm(1-alpha/2, 0, sqrt(6*(n-2) / ((n+1)*(n+3)))) 
sk <- function(x) {
#computes the sample skewness coeff.
  xbar <- mean(x)
  m3 <- mean((x - xbar)^3)
  m2 <- mean((x - xbar)^2)
  return( m3 / m2^1.5 )
}

#Beta(alpha,alpha)
for (j in 1:N) {
#for each parameter
  e <- para[j]
  sktests <- numeric(m)
  for (i in 1:m) {
#for each replicate
    x <- rbeta(n, e, e) 
    sktests[i] <- as.integer(abs(sk(x)) >= cv) }
  power[j] <- mean(sktests) }

#t(v)
para_t<-seq(1,100,1)
N1<-length(para_t)
power1 <- numeric(N1)
for (j in 1:N1) {
#for each parameter
  e <- para_t[j]
  sktests <- numeric(m)
  for (i in 1:m) {
#for each replicate
    x <- rt(n,e) 
    sktests[i] <- as.integer(abs(sk(x)) >= cv) }
  power1[j] <- mean(sktests) }
#plot power vs para
#para_t<-c(rep(NA,2),para_t)
#power1<-c(rep(NA,2),power1)
mydata<-data.frame(para,para_t,power,power1)
library(ggplot2)
ggplot(mydata,aes(x=paramenter,y=power))+
  coord_cartesian(xlim=c(0,100),y=c(0,1))+
  geom_point(data=mydata,aes(x=para,y=power),color = 'green')+
  geom_point(data=mydata,aes(x=para_t,y=power1),color = 'red')+
  geom_hline(aes(yintercept=0.1),colour='#990000',linetype='dashed')
 

## -----------------------------------------------------------------------------
count5test <- function(x, y) {
        X <- x - mean(x)
        Y <- y - mean(y)
        outx <- sum(X > max(Y)) + sum(X < min(Y))
        outy <- sum(Y > max(X)) + sum(Y < min(X))
        return(as.integer(max(c(outx, outy)) > 5))
}
set.seed(1027)
alpha.hat <- 0.055
n <- c(10, 20, 50, 100, 500, 1000)
mu1 <- mu2 <- 0
sigma1 <- 1
sigma2 <- 1.5
m <- 1e4
result <- matrix(0, length(n), 2)
for (i in 1:length(n)){
  ni <- n[i]
  tests <- replicate(m, expr={
    x <- rnorm(ni, mu1, sigma1)
    y <- rnorm(ni, mu2, sigma2)
    Fp <- var.test(x, y)$p.value
    Ftest <- as.integer(Fp <= alpha.hat)
    c(count5test(x, y), Ftest)
    })
  result[i, ] <- rowMeans(tests)
}
data.frame(n=n, C5=result[, 1], Fp=result[, 2])

## -----------------------------------------------------------------------------
k<-dimen<-2 #The dimension of the multivariate normal distribution.
cv<-qchisq(0.95,df=dimen*(dimen+1)*(dimen+2)/6)
sk<-function(x){    #Input sample matrix, where each row is a sample, and the column is the sample attribute
  m<-dim(x)[1]
  u<-matrix(0,m,m)
  for(i in 1:m){
    for(j in 1:m){
      yi<-x[i,]-colMeans(x)
      yj<-x[j,]-colMeans(x)
      u[i,j]<-(t(matrix(yi)) %*% solve((m-1)/m*cov(x)) %*% matrix(yj))^3/(m^2)
    }
  }
  sum(u)
}

m<-100 #the number of trials

n<-c(40,50,60,70,100)
p.reject<-numeric()
library('MASS')

for (j in 1:length(n)) {
  sktests<-numeric(m)
  for (i in 1:m) {
    y<-mvrnorm(n[j],rep(0,k),diag(k))
    sktests[i]<-as.integer(sk(y)*n[j]/6 >= cv)
  }
  p.reject[j]<-mean(sktests)
}
p.reject

## -----------------------------------------------------------------------------
## Jackknife estimate of the bias.
library(bootstrap)
#data(law, package = "bootstrap")
n <- nrow(law)
y <- law$LSAT
z <- law$GPA
theta.hat <- cor(y,z)
#print (theta.hat)
#compute the jackknife replicates, leave-one-out estimates.
theta.jack <- numeric(n)
for (i in 1:n)
  theta.jack[i] <- cor(y[-i],z[-i])
bias <- (n - 1) * (mean(theta.jack) - theta.hat)
print(paste('The Jackknife estimate of the bias of the correlation statistic is: ',bias))

## jackknife estimate of the standard error.

#compute the jackknife replicates, leave-one-out estimates
se <- sqrt((n-1) *mean((theta.jack - mean(theta.jack))^2))
print(paste('The Jackknife estimate of the standard error of the correlation statistic is: ',se))

## -----------------------------------------------------------------------------
library(boot)
data(aircondit, package = "boot")
boot.obj <- boot(aircondit, R = 2000,statistic = function(x,i){mean(x[i,1])})
n<-nrow(aircondit)
theta.hat<-1/mean(aircondit$hours)

print(boot.ci(boot.obj, type=c("basic","norm","perc","bca")))

## -----------------------------------------------------------------------------
library(bootstrap)
#data(scor, package = "bootstrap")
## Jackknife estimate of the bias.
n <- nrow(scor)
a<-cov(scor)#
ev<-eigen(a)
lambda_1<-max(ev$val)
theta.hat<-lambda_1/sum(ev$val)

#print (theta.hat)
#compute the jackknife replicates, leave-one-out estimates.
theta.jack <- numeric(n)
for (i in 1:n)
  theta.jack[i] <- max(eigen(cov(scor[-i,]))$val)/sum(eigen(cov(scor[-i,]))$val)
bias <- (n - 1) * (mean(theta.jack) - theta.hat)
print(paste('The Jackknife estimate of the bias of the correlation statistic is: ',bias))

## jackknife estimate of the standard error.

#compute the jackknife replicates, leave-one-out estimates
se <- sqrt((n-1) *mean((theta.jack - mean(theta.jack))^2))
print(paste('The Jackknife estimate of the standard error of the correlation statistic is: ',se))


## -----------------------------------------------------------------------------
library(DAAG)
#data(ironslag, package = "DAAG")
magnetic<-ironslag$magnetic
chemical<-ironslag$chemical
n <- length(magnetic) #in DAAG ironslag
e1 <- e2 <- e3 <- e4 <- matrix(1,n,n)
# for n-fold cross validation

# fit models on leave-two-out samples

for (i in 1:(n-1)) {
  for (k in (i+1):n) {
    y <- magnetic[-c(i,k)]
    x <- chemical[-c(i,k)]

    J1 <- lm(y ~ x)
    yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
    yhat1_ <- J1$coef[1] + J1$coef[2] * chemical[i]
    e1[i,k] <- magnetic[k] - yhat1
    e1[k,i] <- magnetic[i] - yhat1_

    
    J2 <- lm(y ~ x + I(x^2))
    yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +J2$coef[3] * chemical[k]^2
    yhat2_ <- J2$coef[1] + J2$coef[2] * chemical[i] +J2$coef[3] * chemical[i]^2
    e2[i,k] <- magnetic[k] - yhat2
    e2[k,i] <- magnetic[i] - yhat2_
  
    J3 <- lm(log(y) ~ x)
    logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
    yhat3 <- exp(logyhat3)
    logyhat3_ <- J3$coef[1] + J3$coef[2] * chemical[i]
    yhat3_ <- exp(logyhat3_)
    e3[i,k] <- magnetic[k] - yhat3
    e3[k,i] <- magnetic[i] - yhat3_
    
    J4 <- lm(log(y) ~ log(x))
    logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[k])
    yhat4 <- exp(logyhat4)
    e4[i,k] <- magnetic[k] - yhat4
    logyhat4_ <- J4$coef[1] + J4$coef[2] * log(chemical[i])
    yhat4_ <- exp(logyhat4_)
    e4[k,i] <- magnetic[i] - yhat4_
  }
}

e11<-e22<-e33<-e44<-numeric()
e11<-c(e1[which(upper.tri(e1))],e1[which(lower.tri(e1))])
e22<-c(e2[which(upper.tri(e2))],e2[which(lower.tri(e2))])
e33<-c(e3[which(upper.tri(e3))],e3[which(lower.tri(e3))])
e44<-c(e4[which(upper.tri(e4))],e4[which(lower.tri(e4))])
c(mean(e11^2), mean(e22^2), mean(e33^2), mean(e44^2))
 
lm(magnetic~chemical+I(chemical^2))

## -----------------------------------------------------------------------------
count5test <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
# return 1 (reject) or 0 (do not reject H0)
  return(as.integer(max(c(outx, outy)) > 5))
}
set.seed(2020)
count<- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
# return 1 (reject) or 0 (do not reject H0)
  return(as.integer((outx > 4) || (outy>7)))
}
R <- 1000 #number of replicates
 #pooled sample
K <- 1:50
D <- numeric(R) #storage for replicates

n1 <- 20
n2 <- 30
mu1 <- mu2 <- 0
sigma1 <- 1
sigma2 <- 1
m <- 100
Giao <- mean(replicate(m, expr={
  x <- rnorm(n1, mu1, sigma1)
  y <- rnorm(n2, mu2, sigma2)
  z <- c(x, y)
  for (i in 1:R) {
  k <- sample(K, size = 20, replace = FALSE)
  x1 <- z[k]
  y1 <- z[-k] #complement of x1
  D[i] <- count(x1, y1)
  }
  mean(D)
}))

Wude<-replicate(R,expr = {
  x=rnorm(n1,mu1,sigma1)
  y=rnorm(n2,mu2,sigma2)
  x=x-mean(x)
  y=y-mean(y)
  count5test(x,y)
})
Wude<-mean(Wude)
# print(Giao)
# print(Wude)
round(c(count5test=Wude,count_permutation=Giao),3)

## -----------------------------------------------------------------------------
library(RANN)
library(boot)
library(energy)
library(Ball)

set.seed(2020)

nn_test=function(x,y){
k <- c(x, y)
o <- rep(0, length(k))
z <- as.data.frame(cbind(k, o))
Tn3 <- function(z, ix, sizes) {
  n1 <- sizes[1]
  n2 <- sizes[2]
  n <- n1 + n2
  z <- z[ix, ]
  o <- rep(0, NROW(z))
  z <- as.data.frame(cbind(z, o))
  NN <- nn2(z, k=3)
  Giao1 <- NN$nn.idx[1:n1, ]
  Giao2 <- NN$nn.idx[(n1+1):n, ]
  i1 <- sum(Giao1 < n1 + .5)
  i2 <- sum(Giao2 > n1 + .5)
  return((i1 + i2) / (3 * n))
}
Wude <- c(length(x), length(y))
boot.obj <- boot(data = z, statistic = Tn3, sim = "permutation", R = 999, sizes = Wude)
tb <- c(boot.obj$t, boot.obj$t0)
mean(tb >= boot.obj$t0)
}
energy.test=function(x,y,R=length(x)+length(y)){
  Giao <- c(x, y)
  o <- rep(0, length(Giao))
  Giao <- as.data.frame(cbind(Giao, o))
  N <- c(length(x), length(y))
  eqdist.etest(Giao, sizes = N,R=R)$p.
}
Hessian=matrix(0,10,3)
for(i in 1:10){
  x=rnorm(100)
  y=rnorm(100)*(1+i/10)
  seed=.Random.seed
  Hessian[i,]=c(nn_test(x,y),energy.test(x,y),bd.test(x,y,R=length(x)+length(y))$p)
  .Random.seed=seed
}
plot(Hessian[,1],type='n',ylim=c(0,0.5),main="different variance")
for(i in 1:3)points(Hessian[,i],col=i+1)
for(i in 1:3)points(Hessian[,i],col=i+1,type='l')

## -----------------------------------------------------------------------------
##(2)Unequal variances and unequal expectations
Hessian=matrix(0,10,3)
for(i in 1:10){
  x=rnorm(100,i/10)
  y=rnorm(100)*(1+i/10)
  seed=.Random.seed
  Hessian[i,]=c(nn_test(x,y),energy.test(x,y),bd.test(x,y,R=length(x)+length(y))$p)
  .Random.seed=seed
}
plot(Hessian[,1],type='n',ylim=c(0,0.5),main="different mean and variance")
for(i in 1:3)points(Hessian[,i],col=i+1)
for(i in 1:3)points(Hessian[,i],col=i+1,type='l')

## -----------------------------------------------------------------------------
##Non-normal distributions: t distribution with 1 df (heavy-tailed
##distribution), bimodal distribution (mixture of two normal
##distributions)
Hessian=matrix(0,10,3)
for(i in 1:10){
  x=rt(1000,df=1)
  y=rt(1000,df=1+i/10)
  seed=.Random.seed
  Hessian[i,]=c(nn_test(x,y),energy.test(x,y),bd.test(x,y,R=length(x)+length(y))$p)
  .Random.seed=seed
}
plot(Hessian[,1],type='n',ylim=c(0,0.5),main="heavy-tail")
for(i in 1:3)points(Hessian[,i],col=i+1)
for(i in 1:3)points(Hessian[,i],col=i+1,type='l')

## -----------------------------------------------------------------------------
##Unbalanced samples 
Hessian=matrix(0,10,3)
for(i in 1:10){
  x=rnorm(100/i)
  y=rnorm(100*i,sd=1.5)
  seed=.Random.seed
  Hessian[i,]=c(nn_test(x,y),energy.test(x,y),bd.test(x,y,R=length(x)+length(y))$p)
  .Random.seed=seed
}
plot(Hessian[,1],type='n',ylim=c(0,0.5),main="unbalanced")
for(i in 1:3)points(Hessian[,i],col=i+1)
for(i in 1:3)points(Hessian[,i],col=i+1,type='l')

## -----------------------------------------------------------------------------
dLaplace<-function(x){
  return(0.5*exp(-abs(x)))
}

rw.Metropolis <- function(sigma, x0, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= (dLaplace(y) / dLaplace(x[i-1]))){
      x[i] <- y
    }
    else {
      x[i] <- x[i-1]
      k <- k + 1
      } 
    }
  return(list(x=x, k=k))
}
#Here we choose to generate 2000 samples.
N <- 2000
sigma <- c(.05, .5, 2, 10)
x0 <- 25
rw1 <- rw.Metropolis(sigma[1], x0, N)
rw2 <- rw.Metropolis(sigma[2], x0, N)
rw3 <- rw.Metropolis(sigma[3], x0, N)
rw4 <- rw.Metropolis(sigma[4], x0, N)
#number of candidate points rejected
print(c((2000-rw1$k)/2000, (2000-rw2$k)/2000, (2000-rw3$k)/2000, (2000-rw4$k)/2000))

## -----------------------------------------------------------------------------
Gelman.Rubin <- function(psi) {
# psi[i,j] is the statistic psi(X[i,1:j])
# for chain in i-th row of X
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi) #row means
  B <- n * var(psi.means) #between variance est.
  psi.w <- apply(psi, 1, "var") #within variances
  W <- mean(psi.w) #within est.
  v.hat <- W*(n-1)/n + (B/n) #upper variance est.
  r.hat <- v.hat / W #G-R statistic
  return(r.hat)
}
dLaplace<-function(x){
  return(0.5*exp(-abs(x)))
}

rw.Metropolis <- function(sigma, x0, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= (dLaplace(y) / dLaplace(x[i-1]))){
      x[i] <- y
    }
    else {
      x[i] <- x[i-1]
      k <- k + 1
      } 
    }
  return(list(x=x, k=k))
}

sigma <- sqrt(2) #parameter of proposal distribution
k <- 4 #number of chains to generate
n <- 10000 #length of chains
b <- 100 #burn-in length

x0 <- c(-10, -5, 5, 10)

#generate the chains
X <- matrix(0, nrow=k, ncol=n)
for (i in 1:k)
  X[i, ] <- rw.Metropolis(sigma, x0[i], n)$x

#compute diagnostic statistics
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi))
print(Gelman.Rubin(psi))

#plot psi for the four chains
par(mfrow=c(2,2))
for (i in 1:k)
  plot(psi[i, (b+1):n], type="l",xlab=i, ylab=bquote(psi))
par(mfrow=c(1,1)) #restore default

#plot the sequence of R-hat statistics

rhat <- rep(0, n)
for (j in (b+1):n){
  rhat[j] <- Gelman.Rubin(psi[,1:j])
}

plot(x=seq(b+1,n,1),rhat[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.2,col='green')


## -----------------------------------------------------------------------------
# f <- function(a,k=4){
#   qt(sqrt(a^2*(k-1)/(k-a^2)),df=k-1)-qt(sqrt(a^2*(k-1)/(k-a^2)),df=k)
# }
k<-c(4:25,100,500,1000)
root<-numeric()
for (i in 1:length(k)) {
  res <- uniroot(function(a){
    pt(sqrt(a^2*(k[i]-1)/(k[i]-a^2)),df=k[i]-1,log.p = T)-pt(sqrt(a^2*(k[i])/(k[i]+1-a^2)),df=k[i],log.p = T)
  },lower = 1e-5,upper = sqrt(k[i]-1e-5))
  root[i]<-unlist(res)[[1]]
}
root




## -----------------------------------------------------------------------------
##EM 
EM<-function(p.ini,n.obs){
  M=1e4 #maximum ierations
  tol=.Machine$double.eps #when to converge

  n=sum(n.obs)
  nA.=n.obs[1]
  nB.=n.obs[2]
  nOO=n.obs[3]
  nAB=n.obs[4]
  
  p=q=r=numeric(0)
  p[1]=p.ini[1]
  q[1]=p.ini[2]
  r[1]=1-p[1]-q[1]
  iter=1
  
  for(i in 2:M){
    p.old=p[i-1]
    q.old=q[i-1]
    r.old=r[i-1]
    
    nAA.t=nA.*p.old^2/(p.old^2+2*p.old*r.old)
    nAO.t=nA.*2*p.old*r.old/(p.old^2+2*p.old*r.old)
    nBB.t=nB.*q.old^2/(q.old^2+2*q.old*r.old)
    nBO.t=nB.*2*q.old*r.old/(q.old^2+2*q.old*r.old)
    nOO.t=nOO
    nAB.t=nAB
    
    p[i]=(2*nAA.t+nAO.t+nAB.t)/2/n
    q[i]=(2*nBB.t+nBO.t+nAB.t)/2/n
    r[i]=(2*nOO.t+nAO.t+nBO.t)/2/n
    iter=iter+1
    
    U=abs((p[i]-p.old)/p.old)<=tol
    V=abs((q[i]-q.old)/q.old)<=tol
    W=abs((r[i]-r.old)/r.old)<=tol
    if(U&&V&&W)
      break
  }
  list(p.mle.em=p[iter],q.mle.em=q[iter],r.mle.em=r[iter],iter=iter)
}
nObs=c(444,132,361,63)
p_Initial=c(1/3,1/3) #initial p,q value
em.result<-EM(p.ini=p_Initial,n.obs=nObs)
print(em.result)


## -----------------------------------------------------------------------------
EM.trend<-function(p.ini,n.obs){
  M=1e4 #maximum ierations
  tol=.Machine$double.eps #when to converge

  n=sum(n.obs)
  nA.=n.obs[1]
  nB.=n.obs[2]
  nOO=n.obs[3]
  nAB=n.obs[4]
  
  p=q=r=numeric(0)
  loglikelihood=numeric(0)
  p[1]=p.ini[1]
  q[1]=p.ini[2]
  r[1]=1-p[1]-q[1]
  loglikelihood[1]=0
  iter=1
  
  for(i in 2:M){
    p.old=p[i-1]
    q.old=q[i-1]
    r.old=r[i-1]
    
    nAA.t=nA.*p.old^2/(p.old^2+2*p.old*r.old)
    nAO.t=nA.*2*p.old*r.old/(p.old^2+2*p.old*r.old)
    nBB.t=nB.*q.old^2/(q.old^2+2*q.old*r.old)
    nBO.t=nB.*2*q.old*r.old/(q.old^2+2*q.old*r.old)
    nOO.t=nOO
    nAB.t=nAB
    
    p[i]=(2*nAA.t+nAO.t+nAB.t)/2/n
    q[i]=(2*nBB.t+nBO.t+nAB.t)/2/n
    r[i]=(2*nOO.t+nAO.t+nBO.t)/2/n
    iter=iter+1
    
    loglikelihood[i]=nAA.t*2*log(p[i])+nAO.t*log(2*p[i]*r[i])+nBB.t*2*log(q[i])+nBO.t*log(q[i]*r[i])+nOO.t*2*log(r[i])+nAB.t*log(2*p[i]*q[i])
    
    U=abs((p[i]-p.old)/p.old)<=tol
    V=abs((q[i]-q.old)/q.old)<=tol
    W=abs((r[i]-r.old)/r.old)<=tol
    if(U&&V&&W)
      break
  }
  list(p.mle.em=p[iter],q.mle.em=q[iter],r.mle.em=r[iter],iter=iter,p.mle.all=p,q.mle.all=q,loglikelihoods=loglikelihood)
}
nObs=c(444,132,361,63)
pInitial=c(0.4,0.3) #initial p,q value
em.result<-EM.trend(p.ini=pInitial,n.obs=nObs)

par(mfrow=c(1,2))
plot(em.result$p.mle.all,xlab = "iter",ylab = "p.mle",ylim = c(0,0.4))

plot(em.result$q.mle.all,xlab = "iter",ylab = "q.mle",ylim=c(0,0.4))


## -----------------------------------------------------------------------------
plot(em.result$loglikelihoods[-1],xlab = "iter",ylab = "loglikehood")

## -----------------------------------------------------------------------------

formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

##Use loops
for (i in 1:length(formulas)) {
  relation<-lm(formulas[[i]],mtcars)
  print(relation)
}

##Use lapply()
lapply(formulas, function(x) lm(data=mtcars,x))


## -----------------------------------------------------------------------------
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)
set.seed(2020)
##Use sapply
p_value<-sapply(trials,function(x) x$p.value)
p_value


## -----------------------------------------------------------------------------
#extra challenge: using [[ instead of anonymous function
sapply(trials,"[[",3)

## -----------------------------------------------------------------------------
f<-function(data,funct,output_type){
  new<-Map(funct,data)
  vapply(new,function(x) x ,output_type)
}

##Example
f(women,mean,double(1))

## -----------------------------------------------------------------------------
dLaplace<-function(x){
  return(0.5*exp(-abs(x)))
}

rw.Metropolis <- function(sigma, x0, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma)
    if (u[i] <= (dLaplace(y) / dLaplace(x[i-1]))){
      x[i] <- y
    }
    else {
      x[i] <- x[i-1]
      k <- k + 1
      } 
    }
  return(list(x=x, k=k))
}
#Here we choose to generate 2000 samples.
N <- 2000
sigma <- c(.05, .5, 2, 16)
x0 <- 25
rw1 <- rw.Metropolis(sigma[1], x0, N)
rw2 <- rw.Metropolis(sigma[2], x0, N)
rw3 <- rw.Metropolis(sigma[3], x0, N)
rw4 <- rw.Metropolis(sigma[4], x0, N)

Reject <- cbind(rw1$k, rw2$k, rw3$k, rw4$k)
Accpt <- round((N-Reject)/N,4)
rownames(Accpt) <- "Accept rates"
colnames(Accpt) <- paste("sigma",sigma)
knitr::kable(Accpt)

par(mfrow = c(2,2))  #display 4 graphs together
rw <- cbind(rw1$x, rw2$x, rw3$x,  rw4$x)
for (j in 1:4) {
  plot(rw[,j], type="l",
  xlab=bquote(sigma == .(round(sigma[j],3))),
  ylab="X", ylim=range(rw[,j]))
}

## -----------------------------------------------------------------------------
library(Rcpp) 
sourceCpp("../src/MetropolisCpp.cpp")

#test: rw=MetropolisCpp(2,25,2000)

N <- 2000
sigma <- c(.05, .5, 2, 16)
x0 <- 25
rw1_c <- MetropolisCpp(sigma[1],x0,N)
rw2_c <- MetropolisCpp(sigma[2],x0,N)
rw3_c <- MetropolisCpp(sigma[3],x0,N)
rw4_c <- MetropolisCpp(sigma[4],x0,N)
#number of candidate points rejected
Reject <- cbind(rw1_c$k, rw2_c$k, rw3_c$k, rw4_c$k)
Accpt <- round((N-Reject)/N,4)
rownames(Accpt) <- "Accept rates"
colnames(Accpt) <- paste("sigma",sigma)
knitr::kable(Accpt)

par(mfrow=c(2,2))  #display 4 graphs together
rw_c <- cbind(rw1_c$x, rw2_c$x, rw3_c$x,  rw4_c$x)

for (j in 1:4) {
  plot(rw_c[,j], type="l",
  xlab=bquote(sigma == .(round(sigma[j],3))),
  ylab="X", ylim=range(rw_c[,j]))
}

## -----------------------------------------------------------------------------
#Compare the two results with quantiles (qqplot)
k <- c(0.05,seq(0.1,0.9,0.1),0.95)
rw <- cbind(rw1$x, rw2$x, rw3$x,  rw4$x)
rw_c <- cbind(rw1_c$x, rw2_c$x, rw3_c$x,  rw4_c$x)
mc_1 <- rw[501:N,]
mc_2 <- rw_c[501:N,]
Q_rw <- apply(mc_1,2,function(x) quantile(x,k))
Q_rw_c <- apply(mc_2,2,function(x) quantile(x,k))
Q_table <- round(cbind(Q_rw,Q_rw_c),3)
colnames(Q_table) <- c("rw1","rw2","rw3","rw4","rw1_c","rw2_c","rw3_c","rw4_c")
Q_table

#qqplot
Giao <- ppoints(100)
QQ_rw3 <- quantile(rw3$x[501:N],Giao)
QQ_rw3_c <- quantile(rw3_c$x[501:N],Giao)
qqplot(QQ_rw3,QQ_rw3_c,main="",xlab="rw3 quantiles",ylab="rw3_c quantiles")
qqline(QQ_rw3_c)


## -----------------------------------------------------------------------------
#compare the computing time of the two functions with microbenchmark
library(microbenchmark)
microbenchmark(
  rw.Metropolis(sigma[3],x0,N),
  MetropolisCpp(sigma[3],x0,N))
print(22997.425/1011.025)


