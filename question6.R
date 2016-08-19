y <- 5
n <- 20
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

theta <- seq(y+0.001,12, by = 0.001)
post.dens <- 1/(theta^(n))
dens <- as.data.frame(cbind(theta,post.dens))

theta.0 <- matrix(nrow=5001,ncol = 2, dimnames = list(c(),c("theta","post.dens")))
theta.0[,1] <- seq(0,y,by = 0.001)
theta.0[,2] <- 0
theta.0 <- as.data.frame(theta.0)
theta.comb <- rbind(theta.0, dens)

  ggplot2::ggplot(theta.comb) + 
    geom_line(aes(theta,post.dens)) + 
    geom_vline(xintercept=y, color= "red",linetype = 2) +
    geom_segment(x=0,xend=y,y=0,yend=0)
