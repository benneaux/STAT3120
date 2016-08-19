
a <- 5
x <- seq(a/1000,a, by=a/1000)

samples <- c()
for (i in 1:1000){
  samples[i] <- max(sample(runif(20,0,a)))
}
dens <- density(samples)
par(mfrow=c(2,1))
plot(x,dunif(x,0,a))
plot(dens)
dev.off()
plot(dens)
lines(dunif(x,0,a))
max(samples)
