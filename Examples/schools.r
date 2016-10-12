data("schools")
J <- nrow(schools)

y <- schools$estimate
sigma.y <- schools$sd
data <- list("J","y","sigma.y")


inits <- function() {
  list(
    theta = rnorm(J, 0, 100),
    mu.theta = rnorm(1, 0, 100),
    sigma.theta = runif(1, 0, 100)
  )
}

schools.sim <- bugs(
  data,
  inits,
  model.file = "schools.txt",
  parameters = c("theta", "mu.theta", "sigma.theta"),
  n.chains = 3,
  n.iter = 1000,
  codaPkg = TRUE
  )

schools.coda <- read.bugs(schools.sim)

require(coda)
require(lattice)
xyplot(schools.coda)
densityplot(schools.coda)
acfplot(schools.coda)
gelman.diag(schools.coda)
gelman.plot(schools.coda)


schools.summary <- summary(
  schools.coda,
  q = c(0.025,0.975)
  )

schools.summary$stat["theta[1]",]
schools.summary$q["theta[1]",]
schools.summary
