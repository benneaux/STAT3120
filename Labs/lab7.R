## Lab Exercises 7

counts <- c(1,0,0,2,0,0)
total <- c(10,5,3,35,12,17)
ML <- c(0.1,0,0,0.057,0,0)
B_Est <- c(counts/total)
Data <- cbind(counts, total, ML, B_Est)
postmean <- c((counts+1)/(total+counts+1))
QIL <- qbeta(0.025, counts + 1, total - counts + 1)
QIU <- qbeta(0.975, counts + 1, total - counts + 1)
QI <- cbind(QIL,QIU)
Data <- cbind(Data, QI)

#Question 2 b - Empirical Bayes====

  a <- 0.345

  b <- 12.829
  a/(a+b)
  
  ebe <- (counts + a)/(total+a+b)
  Data <- cbind(Data, ebe)
  EQIL <- qbeta(0.025, counts + a, total - counts + b)
  EQIU <- qbeta(0.975, counts + a, total - counts + b)
  EQI <- cbind(EQIL, EQIU)
  Data <- cbind(Data, EQI)
  
  x <- seq(0,1,0.001)
  px <- dbeta(x,a,b)
  plot(x, px, type='l', ylim=c(0,30))
  for(k in 1:6) {
    lines(x, dbeta(x, counts[k] + a, total[k] - counts[k] + b), col=(k+2))
  }

  #Question 2 c - Expert Knowledge====  

    for(i in seq(0,10,0.1)) {
      a.0 <- 1
      a <- a.0 + (i*(0.5))
      b <- 19*a
      x <- pbeta(0.1,a,b) - pbeta(0.01,a,b)
      print(c(a,x))
    }
  
  #Question 3====

  rate <- c(58,66,78,56,70,71,54,101)
  MLE <- rate
  Be <- rate +1
  Data <- cbind(rate,MLE,Be)
  CIU <- vector(mode = 'numeric', length = 8)
  CIL <- vector(mode = 'numeric', length = 8)
  for(k in 1:8){
    CIL[k] <- qgamma(0.025, rate[k] +1, 1)
    CIU[k] <- qgamma(0.975, rate[k] +1, 1)
  }
  
  Data <- cbind(Data, CIL,CIU)
  Data

  b <- mean(rate)/var(rate)
  a <- b*mean(rate)
  
  x <- seq(0,150,0.001)
  px <- dgamma(x,a,b)
  plot(x, px, type='l', ylim = c(0,0.07))
  for(k in 1:8) {
    lines(x, dgamma(x, rate[k] + a, 1 + b), col=(k+2))
  }