
rm(list = ls()) # clear the workspace.

mean2 <- function(x) sum(x)/length(x) # mean function in r is about 4 times slower
var2 <- function(x, len=length(x), sx=sum(x)) {sum(x*x)-{sx*sx/len}}/{len-1} # ditto

#  Also note that 'x*x' is faster than 'x^2' and '{}' is faster than '()' if the
# brackets are just grouping elements of an equation. You still need to use '()'
# for functions. E.g. '{x + y}' works but 'sum{x}' doesn't.

# the height function just automates what was in the for loop before.
htfunc <- function(y, kt,
                   y1set = y[kt == 1], y2set = y[kt == 2],
                   n1 = length(y1set), n2 = length(y2set), 
                   y1 =  y1set - sum(y1set)/n1, y2 = y2set - sum(y2set)/n2)
  
                  {-0.5*{log(n1) + log(n2)} + # function starts here.
                      lgamma({n1 - 1}/2) + 
                      lgamma({n2 - 1}/2) - 
                      {n1 - 1}/2*log(sum(y1*y1)) - 
                      {n2 - 1}/2 * log(sum(y2*y2))}

n  <-  200L # number of samples (L specifies integer)
d <- 1000L    # MCMC sample size
pkymax <- -99999

# just specifying template vectors to reduce code clutter.
n.vec <- vector("numeric", length=n)
d.vec <- vector("numeric",length=d)

y2 <- vector("numeric",length=0)
y3 <- vector("numeric", length=0)

# Defining all the variables needed in the code to save memory allocation time.

ktmax <- n.vec; ktold <- n.vec; mpkty <- n.vec
pyt <- n.vec; pyt2 <- n.vec
mu1 <- d.vec; mu2 <- d.vec
sig1 <- d.vec; sig2 <- d.vec
omeg <- d.vec; omeg[1] <- rbeta(1, 1, 1) # Initialises omeg

# Remove unnescessary variables.
rm(n.vec,d.vec)


# The following generates n samples from two different Normal distributions given 
# in the question, according to the specified mixture proportion.
  
# Predefining a condition for the for loop.
condition <- {runif(n, 0, 1)  < 0.25}

y  <-  rnorm(n, 15, sqrt(3)) # Initialises y as a sample of length n from one of 
                             # the distributions specified for the mixture. This 
                             # means that the for loop contains just an 'if'
                             # statement, not an 'if' and a 'then'. Simpler.

for(t in {1:n}[condition]) {
  if(condition[t]) { # 0.25 is value given in question statement for mixture prop.
    y[t] <- rnorm(1, 10, sqrt(2)) # N(10,2); the other mixture distribution.
  }
}
  
# The following creates an initial kt that we will update later. Again, by pre
# assing one of the possible values of kt ('2') to the entire vector, we simplify the
# for loop.
  
condition <- {runif(n,0,1) > 0.5} 
kt <- c(rep(2,n))  # pre assigning '2' to the entire vector.
  
for(k in (1:n)[condition]) {
  if(condition[k]) { # changes the value of each kt from '2' to '1' depending 
    kt[k] <- 1       # on the outcome of the 'condition' statement (T/F)
  }
}

# The number of '1's (n1) and '2's (n2) in kt. 
n1 <- length(y[kt == 1])
n2 <- n - n1

# The following updates the values in kt to equalise the variance of the two groups.
# Note: the 'vvar2(y[kt == 2]) > var2(y[kt == 1]))' statement is the part of the 
# code that takes the most time to run. It is repeated a couple of times further
# down as well. I can't yet figure out how to do it better.
  
while (var2(y[kt == 2]) > var2(y[kt == 1])) {
  condition <- {runif(n,0,1) > 0.5}
  for(k in (1:n)[condition]) {
    if(condition[k]) {
      kt[k] <- 1
    } 
  }
}

ktmax <- kt # creates a copy of kt for comparison later.

# Don't know why we do this. Testing two different variances.

# Variance 1
mu1[1] <- mean2(y) # mean of the initial sample
sig1[1] <- var2(y)*1.5 

# Variance 2
mu2[1] <- mean2(y) # mean of the initial sample
sig2[1] <- var2(y)*0.5

# The actual for loop; the inner loop (index = 't') creates the MCMC sample
# ('d' sample size). The outer loop samples the MCMC samples. 

# We utilise our own functions (mean2, var2, htfunc) to speed up or clean up
# the code.

for(j in 2:d) {
  ktold<-kt
  for(t in 1:n) {
    
    kt[t] <- 1
    ht1<- htfunc(y,kt) + log(omeg[j - 1])
    
    kt[t] <- 2
    ht2 <- htfunc(y,kt) + log(1 - omeg[j - 1])          
    
    pyk<- 1/{1 + exp(ht2 - ht1)}
    
    if(is.nan(pyk)) {
      
      if(is.nan(ht1)) {
        kt[t] <- 1
      }
      if(is.nan(ht2)) {
        kt[t] <- 2
      }
    } else {
      if(runif(1,0,1)<pyk) {
        kt[t] <- 1
        pyt2[t] <- pyt2[t]+dnorm(y[t],mu1[j-1],sqrt(sig1[j-1]))
      } else {
        kt[t] <- 2
        pyt2[t] <- pyt2[t]+dnorm(y[t],mu2[j-1],sqrt(sig2[j-1]))
      }
      
      mpkty[t] <- mpkty[t] + pyk
    }
    
    n1 <- length(y[kt == 1])
    n2 <- n - n1
    
    # Here we have combined two 'if' statements into one. Also note that
    # the 'var2(y[kt == 2]) > var2(y[kt == 1])' condition is very slow.
    if (n1 < 2 | n1 > {n - 3} | var2(y[kt == 2]) > var2(y[kt == 1])) {
      kt[t] <- ktold[t]
      n1 <- length(y[kt == 1])
      n2 <- n - n1
    }
    
    pyt[t] <- pyt[t] + 
      dnorm(y[t], mu1[j-1], sqrt(sig1[j-1]))*omeg[j-1] + 
      dnorm(y[t], mu2[j-1], sqrt(sig2[j-1]))*{1 - omeg[j-1]}
  
  } # End of the inner for-loop.
  
  y1 <- y[kt == 1]
  y2 <- y[kt == 2]
  
  condition <- {n1 < 2 | n1 > {n - 3} | var2(y2) > var2(y1)}
  
  if(condition) {
    kt <- ktold
    n1 <- length(y[kt == 1])
    n2 <- n - n1
  }
  
  lpyk <- htfunc(y,kt) + 
    n1*log(omeg[j-1]) + 
    n2*log(1 - omeg[j-1])
  
  if(lpyk > pkymax) {
    pkymax <- lpyk
    ktmax <- kt
  }
  
  mu1[j] <- rt(1, n1 - 1)*sqrt(var2(y1)/n1) + mean2(y1)
  mu2[j] <- rt(1, n2 - 1)*sqrt(var2(y2)/n2) + mean2(y2)
  
  ymu <- {y1 - mu1[j]}*{y1 - mu1[j]} # (y1 - mu1[j])^2 is slower
  D <- sum(ymu)
  
  # variance (sig1 or sig2) has an Inverse Gamma distribution.
  sig1[j] <- 1/rgamma(1, n1/2, D/2)
  
  ymu <- {y2 - mu2[j]}*{y2 - mu2[j]} # (y2 - mu2[j])^2 is slower
  D <- sum(ymu)
  
  sig2[j] <- 1/rgamma(1,n2/2,D/2) 
  
  a <- n1 + 1
  b <- n2 + 1
  omeg[j] <- rbeta(1, a, b)  # omega has a beta distribution.
}

# Comment out the next line if you don't want to remove unnescessary variables.

rm(list = setdiff(ls(),list("d","mpkty","pyt","pyt2","y")))

# Plotting the Results.

# Divide by the size of the MCMC sample for scale?
mpkty <- mpkty/d
pyt <- pyt/d
pyt2 <- pyt2/d

# Sort the samples but return each index to specify that samples original position.
# We then use the index to help plot the samples.
y.sort <- sort(y, index.return=T)

plot(y.sort$x, # Density of our MCMC samples.
     pyt[y.sort$ix],
     type = 'l',
     col = 'blue'
     )

lines(density(y), # Density of the original data
      col = 'red'
      )
