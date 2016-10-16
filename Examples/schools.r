require(R2OpenBUGS)
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
    n.burnin = 100,
    n.thin = 1,
    codaPkg = TRUE,
    useWINE = TRUE,
    newWINE = TRUE,
    WINE="/Applications/Wine.app/Contents/MacOS/Wine",
    WINEPATH = "/Applications/Wine.app/Contents/Resources/bin/winepath",
    OpenBUGS.pgm="/Users/benjamin/Applications/wine/drive_c/Program Files/OpenBUGS/OpenBUGS323/"
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

validateInstallOpenBUGS(OpenBUGS.pgm="/Users/benjamin/Applications/wine/drive_c/ProgramFiles/OpenBUGS/OpenBUGS323/OpenBUGS.exe",
                        useWINE=TRUE,debug=TRUE,WINE = "/Application/Wine.app/Content/MacOS/Wine",newWINE=TRUE)
