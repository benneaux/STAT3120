### Question 1 =====

  q1.data <- matrix(
    c(0.5625,0.1875,0.1875,0.0625,0.25,0.25,0.25,0.25),
    nrow = 2,
    ncol = 4,
    byrow = TRUE,
    dimnames = list(
      c("A","B"),
      c("Type 1","Type 2","Type 3","Type 4")
      )
    )
  
  q1.results <- matrix(
    c(8,2,4,3),
    nrow = 1,
    ncol = 4,
    dimnames = list(
      c(),
      c("Type 1","Type 2","Type 3","Type 4")
      )
    )
  
  pr.hypA <- prod(q1.data[1,]^q1.results)
  
  pr.hypA
  
  pr.hypB <- prod(q1.data[2,]^q1.results)
  
  pr.hypB
  
  q1.BF.A <- pr.hypA/pr.hypB
  q1.BF.B <- pr.hypB/pr.hypA

### Question 2 ====

  q2.success.rate <- c(0,0.25,0.5,0.75,0.9,1)
  
  q2.prior.prob <- matrix(
    c(0,0.1,0.75,0.1,0.04,0.01),
    nrow = 1,
    ncol = 6,
    dimnames = list(
      c("Pr(p) (prior"),
      q2.success.rate
      )
    )
  
  q2.likelihood <- dbinom(
    7, # number of correct guesses
    10, # number of guesses
    q2.success.rate # vector of probabilities.
    )
  
  q2.post.prob <- q2.prior.prob*q2.likelihood
  
  q2.post.prob.norm <- q2.post.prob/sum(q2.post.prob)
  
  par(mfrow=c(3,1))
    plot(
      q2.success.rate,
      q2.prior.prob,
      type = 'l',
      main = "Prior",
      xlab = "Success Rate",
      ylab = ""
    )
    plot(
      q2.success.rate,
      q2.likelihood,
      type = 'l',
      main = "Likelihood",
      xlab = "Success Rate",
      ylab = ""
    )
    plot(
      q2.success.rate,
      q2.post.prob.norm,
      type = 'l',
      main = "Posterior",
      xlab = "Success Rate",
      ylab = ""
      )
    
  # Analysis repeated with flat - rather than informed prior
    
  q2.prior.prob.flat <- matrix(
    c(1/6,1/6,1/6,1/6,1/6,1/6),
    nrow = 1,
    ncol = 6,
    dimnames = list(
      c("Pr(p) (prior"),
      q2.success.rate
    )
  )
    
  q2.post.prob.flat <- q2.prior.prob.flat*q2.likelihood
    
  q2.post.prob.flat.norm <- q2.post.prob.flat/sum(q2.post.prob.flat)
    
  par(mfrow=c(3,1))
    plot(
      q2.success.rate,
      q2.prior.prob.flat,
      type = 'l',
      main = "Prior (flat)",
      xlab = "Success Rate",
      ylab = ""
    )
    plot(
      q2.success.rate,
      q2.likelihood,
      type = 'l',
      main = "Likelihood",
      xlab = "Success Rate",
      ylab = ""
    )
    plot(
      q2.success.rate,
      q2.post.prob.flat.norm,
      type = 'l',
      main = "Posterior (flat)",
      xlab = "Success Rate",
      ylab = ""
    )
  

### Question 3 ====  
  
  q3.success.rate <- seq(0,1,0.0001)
  n <- 100
  
  mu.j <- 0.85
  sig2.j <- 0.0025
  a.j <- mu.j*(mu.j*(1-mu.j)/sig2.j -1)
  b.j <- (1-mu.j)*(mu.j*(1-mu.j)/sig2.j -1) 
  prior.j <- dbeta(q3.success.rate, a.j, b.j)
  
  mu.o <- 0.5
  sig2.o <- 0.0025
  a.o <- mu.o*(mu.o*(1-mu.o)/sig2.o -1)
  b.o <- (1-mu.o)*(mu.o*(1-mu.o)/sig2.o -1) 
  prior.o <- dbeta(q3.success.rate, a.o, b.o)
  
  q3.jordan.success <- 89
  q3.ja <- q3.jordan.success
  q3.jb <- n - q3.jordan.success
  q3.likelihood.jordan <- dbeta(q3.success.rate, q3.ja + 1, q3.jb + 1)
  
  q3.oneal.success <- 40
  q3.oa <- q3.oneal.success
  q3.ob <- n - q3.oneal.success
  q3.likelihood.oneal <- dbeta(q3.success.rate, q3.oa + 1, q3.ob + 1)
  

  q3.posterior.Jordan <- dbeta(q3.success.rate, q3.ja + a.j, q3.jb + b.j)
  q3.posterior.ONeal <- dbeta(q3.success.rate, q3.oa + a.o, q3.ob + b.o)
  
  par(mfrow = c(3,2))
  plot(q3.success.rate, prior.j)
  plot(q3.success.rate, prior.o)
  plot(q3.success.rate, q3.likelihood.jordan)
  plot(q3.success.rate, q3.likelihood.oneal)
  
  plot(q3.success.rate, q3.posterior.Jordan, type = 'l', xlab = "x", ylab = "Posterior", main = "Jordan")
  plot(q3.success.rate, q3.posterior.ONeal, type = 'l', xlab = "x", ylab = "Posterior", main = "O'neal")
  
  q3.jordan.quantile <- qbeta(c(0.025,0.975), q3.ja, q3.jb)
  q3.jordan.post.quant <- qbeta(c(0.025,0.975), q3.ja + a.j - 1, q3.jb + b.j - 1)
  q3.oneal.quantile <- qbeta(c(0.025,0.975), q3.oa, q3.ob)
  q3.oneal.post.quant <- qbeta(c(0.025,0.975), q3.oa + a.o - 1, q3.ob + b.o - 1)
  
  
  pr.jordan.90.flat <- 1-pbeta(0.9, q3.ja + 1, q3.jb + 1)
  pr.jordan.90.sub <- 1-pbeta(0.9, q3.ja + a.j, q3.jb + b.j)
  
  pr.oneal.50.flat <- 1-pbeta(0.5, q3.oa + 1, q3.ob + 1)
  pr.oneal.50.sub <- 1-pbeta(0.5, q3.oa + a.o, q3.ob + b.o)
  