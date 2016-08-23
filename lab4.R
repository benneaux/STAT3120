chicken <- read.delim("chicken.txt")
saveRDS(chicken, "chicken.rds")
chicken.data <- readRDS("chicken.rds")

yt <- na.omit(chicken.data$treatment)
yc <- na.omit(chicken.data$control)
mean.yt <- mean(yt)
mean.yc <- mean(yc)

var.yt <- var(yt)
var.yc <- var(yc)

CI.mean.yt.u <- mean.yt + qnorm(0.975)*sqrt(var.yt/length(yt))
CI.mean.yt.l <- mean.yt - qnorm(0.975)*sqrt(var.yt/length(yt))

CI.mean.yc.u <- mean.yc + qnorm(0.975)*sqrt(var.yc/length(yc))
CI.mean.yc.l <- mean.yc - qnorm(0.975)*sqrt(var.yc/length(yc))

x <- seq(0.5,1.5,0.005)
par(mfrow=c(2,1))
plot(x,dnorm(x,mean.yt,sqrt(var.yt/length(yt))), type='l', ylab="Density")
title("Treatment")
plot(x,dnorm(x,mean.yc,sqrt(var.yc/length(yc))), type='l',ylab="Density")
title("Control")

mu.t <- rnorm(1000,mean.yt,sqrt(var.yt/length(yt)))
mu.c <- rnorm(1000,mean.yc,sqrt(var.yc/length(yc)))

quantile(mu.t,c(0.025, 0.975))
quantile(mu.c,c(0.025,0.975))

1-pnorm(1,mean.yt, sqrt(var.yt/length(yt)))
1-pnorm(1,mean.yc, sqrt(var.yc/length(yc)))

mean(mu.t>1)
mean(mu.c>1)

range <- 1.8-0.4

mu.0 <- 1.8-range/2
sig2.0 <- (range/6)^2

mu.1t <- (length(yt)*(mean.yt)/var.yt + mu.0/sig2.0)/(length(yt)/var(yt) + 1/sig2.0)
mu.1c <- (length(yc)*(mean.yc)/var.yc + mu.0/sig2.0)/(length(yc)/var(yc) + 1/sig2.0)

var.1t <- 1/(1/sig2.0 + length(yt)/var.yt)
var.1c <- 1/(1/sig2.0 + length(yc)/var.yc)

CI.mu.1t.u <- mu.1t + qnorm(0.975)*sqrt(var.yt/length(yt))
CI.mu.1t.l <- mu.1t - qnorm(0.975)*sqrt(var.yt/length(yt))

CI.mu.1c.u <- mu.1c + qnorm(0.975)*sqrt(var.yc/length(yc))
CI.mu.1c.l <- mu.1c - qnorm(0.975)*sqrt(var.yc/length(yc))


x <- seq(0,2,0.005)
par(mfrow=c(4,1))
plot(x, dnorm(x,mu.0,sqrt(sig2.0)), type = 'l')
plot(x, dnorm(x, mu.1t, sqrt(var.1t)),type = 'l')
title("Treatment")
plot(x, dnorm(x,mu.0,sqrt(sig2.0)), type = 'l')
plot(x, dnorm(x,mu.1c,sqrt(var.1c)),type = 'l')
title("Control")

##Question 3 =====

n <- 672
mu.sample <- 0.07
var.sample <- 13.86^2
var.pop <- 14^2
var.un1 <- var.pop/n

#a) N(mu.sample, var.un1)

#b)
0.07+qnorm(0.95)*sqrt(var.un1)
0.07-qnorm(0.95)*sqrt(var.un1)

A <- n/2
B <- ((n-1)*var.sample + (n-1)*mu.sample^2)/2
x <- seq(100,300,0.01)
invgamma <- function(x,a,b){
  lg=a*log(b)-lgamma(a)-(a+1)*log(x)-b/x
  return(exp(lg))
}
#Q4
#note: we do not necessarily know the axis - x - already. We might need to play around to find it.
#a)
dev.off()
plot(x,invgamma(x,A,B), type = 'l')

#b)
1/qgamma(c(0.95,0.05), A,B)

#b) by simulation
g.sim <- rgamma(100000,A,B)
quantile(1/g.sim,c(0.05,0.95))

mean.est <- B/(A-1)
mode.est <- B/(A+1)

#c)

1-pgamma(1/160, A,B)
g.sim <- rgamma(1000000,A,B)
mean(g.sim > 1/160)

#c) with 170 instead of 160
hist(1/g.sim)
g.sim.rest <- g.sim[g.sim<1/170]
hist(1/g.sim.rest)
quantile(1/g.sim.rest,c(0.05,0.95))
