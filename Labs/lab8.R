###Question 1======

x <- seq(-5,5,0.001)
plot(x,dnorm(x,0,1),ylim=c(0,0.45),type="l")
lines(x, 4*dunif(x,-5,5),type = "l",col=2)
lines(x, 1.15*dt(x,5),type='l',col=3)

x=runif(10000,-5,5)
alpha <- dnorm(x,0,1)/0.4
ru <- runif(10000)
x1 <- x[alpha>ru]
length(x1)
hist(x1)
mean(x1)
sd(x1)

x=rt(10000,5)
alpha <- dnorm(x,0,1)/1.15*dt(x,5)
ru <- runif(10000)
x2 <- x[alpha>ru]
length(x2)
hist(x2)
mean(x2)
sd(x2)

###Question 2====

##Uniform
x <- runif(1,-5,5)
ac <- 0
for(k in 2:1000) {
  xp=x[k-1]+runif(1,-5,5) 
  if(runif(1,0,1)<dnorm(xp,0,1)/dnorm(x[k-1],0,1)) {
    x[k]=xp
    ac=ac+1
    } else {
      x[k]=x[k-1]
    }
}

acf(x,500)
hist(x,15)
mean(x)
var(x)
shapiro.test(x)

##T-Distribution

x <- rt(1,5)
ac <- 0
for(k in 2:1000) {
  xp=x[k-1]+rt(1,5) 
  if(runif(1,0,1)<dnorm(xp,0,1)/dnorm(x[k-1],0,1)) {
    x[k]=xp
    ac=ac+1
  } else {
    x[k]=x[k-1]
  }
}

acf(x,500)
hist(x,15)
mean(x)
var(x)
shapiro.test(x)

###Question 3====
##Uniform
x <- runif(1,-5,5)
ac <- 0
for(k in 2:1000) {
  xp=runif(1,-5,5) 
  alp=dnorm(xp)/dnorm(x[k-1])*(dunif(x[k-1],-5,5)/dunif(xp,-5,5))
  if(runif(1,0,1)<alp){
    x[k]=xp
    ac=ac+1
  } else {
    x[k]=x[k-1]
  }
}

acf(x,500)
hist(x,15)
mean(x)
var(x)
shapiro.test(x)

##T-Distribution

x <- rt(1,5)
ac <- 0
for(k in 2:1000) {
  xp=rt(1,5) 
  alp=dnorm(xp)/dnorm(x[k-1])*(dt(x[k-1],5)/dt(xp,5))
  if(runif(1,0,1)<alp){x[k]=xp;ac=ac+1
  } else {
    x[k]=x[k-1]
    }
} 

acf(x,500)
hist(x,15)
mean(x)
var(x)
shapiro.test(x)

###Question 4====
##Metropolis
y <- c(0,1,3,5)
x <- c(-0.863,-0.296,-0.053,0.727)
sa <- 5
sb <- 5
alph <- rnorm(1,0,sa)
bet <- rnorm(1,0,sb) # these are starting values
d <- 100000
sxy <- sum(x*y)
sy <- sum(y)
accp <- 0
system.time({
for(j in 2:d) {
  ap=rnorm(1,alph[j-1],sa)
  bp=rnorm(1,bet[j-1],sb) 
  lalp=ap*sy+bp*sxy-5*sum(log(1+exp(ap+bp*x))) 
  lalp=lalp-(alph[j-1]*sy+bet[j-1]*sxy-5*sum(log(1+exp(alph[j-1]+bet[j-1]*x))))
  if(runif(1,0,1)<exp(lalp)) {
    alph[j]=ap
    bet[j]=bp
    accp=accp+1
    } else {
      alph[j]=alph[j-1]
      bet[j]=bet[j-1]
      }
  
  if(j>250) { 
    if(accp/j<0.2) {
      sa=sa*0.95
      sb=sb*0.95
    }
    if(accp/j>0.5) {
      sa=sa*1.05
      sb=sb*1.05
    }
  }}
})
plot(alph[101:d],bet[101:d])

hist(alph[101:d])
mean(alph[101:d])
quantile(alph[101:d],c(0.025,0.975))

hist(bet[101:d])
mean(bet[101:d])

quantile(bet[101:d],c(0.025,0.975))

##Metropolis-Hastings
rem <- function(a,b) { # this is a remainder function
  re = a/b-floor(a/b)
  return(re)
  }
y <- c(0,1,3,5)
x <- c(-0.863,-0.296,-0.053,0.727)
sa <- 5
sb <- 10
ma <- 1
mb <- 10
alph <- rnorm(1,0,sa)
bet <- rnorm(1,0,sb) # these are starting values
d <- 10000
sxy <- sum(x*y)
sy <- sum(y)
accp <- 0
system.time({
  for(j in 2:d) {
    ap=rnorm(1,ma,sa)
    bp=rnorm(1,mb,sb) 
    lalp=ap*sy+bp*sxy-5*sum(log(1+exp(ap+bp*x))) 
    lalp=lalp-(alph[j-1]*sy+bet[j-1]*sxy-5*sum(log(1+exp(alph[j-1]+bet[j-1]*x))))
    lalp=lalp+log(dnorm(alph[j-1],ma,sa))-log(dnorm(ap,ma,sa)) 
    lalp=lalp+log(dnorm(bet[j-1],mb,sb))-log(dnorm(bp,mb,sb)) 
    if(runif(1,0,1)<exp(lalp)) {
      alph[j]=ap
      bet[j]=bp
      accp=accp+1
    } else {
      alph[j]=alph[j-1]
      bet[j]=bet[j-1]
    }
    
    if(j>250&rem(j,100)==0) { 
      if(accp/j<0.2) {
        sa=sa*0.95
        sb=sb*0.95
      }
      if(accp/j>0.5) {
        sa=sa*1.05
        sb=sb*1.05
      }
    }}
})
plot(alph[101:d],bet[101:d])

hist(alph[101:d],20)
mean(alph[101:d])
quantile(alph[101:d],c(0.025,0.975))

hist(bet[101:d],20)
mean(bet[101:d])

quantile(bet[101:d],c(0.025,0.975))

##
y <- c(0,1,3,5) 
x <- c(-0.863,-0.296,-0.053,0.727) 
ma <- 2
sa <- 2
mb <- 15
sb <- 7
d <- 100000
alph <- rnorm(d,ma,sa)
bet <- rnorm(d,mb,sb) # proposed values 
sxy <- sum(x*y)
sy <- sum(y)
acalph <- 0 # sy and sxy in the posterior 
acbet <- 0
for(j in 2:d){ 
  logpalph=alph[j]*sy-5*sum(log(1+exp(alph[j]+bet[j-1]*x))) 
  logpalph=logpalph-(alph[j-1]*sy-5*sum(log(1+exp(alph[j-1]+bet[j-1]*x)))) 
  logpalph=logpalph+log(dnorm(alph[j-1],ma,sa))-log(dnorm(alph[j],ma,sa)) 
  if(runif(1,0,1)<exp(logpalph)) {
    alph[j] = alph[j]
    acalph = acalph + 1
  } else {
    alph[j] = alph[j-1]
    } 
  logpbet=bet[j]*sxy-5*sum(log(1+exp(alph[j]+bet[j]*x))) 
  logpbet=logpbet-(bet[j-1]*sxy-5*sum(log(1+exp(alph[j]+bet[j-1]*x)))) 
  logpbet=logpbet+log(dnorm(bet[j-1],mb,sb))-log(dnorm(bet[j],mb,sb))  
  if(runif(1,0,1)<exp(logpbet)) {
    bet[j] = bet[j]
    acbet = acbet + 1
  } else {
    bet[j] = bet[j-1]
    } 
} 
plot(alph[101:d],bet[101:d])

hist(alph[101:d],20)
mean(alph[101:d])
quantile(alph[101:d],c(0.025,0.975))

hist(bet[101:d],20)
mean(bet[101:d])

quantile(bet[101:d],c(0.025,0.975))
