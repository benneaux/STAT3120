### Import Packages ====

source("packages.R")

# Question 0 ----
#### Import Data ====

studentdata <- readRDS(file = "STAT3120/Data/studentdata.rds")
#or data(studentdata) # assuming 'LearnBayes' package installed.

### Review Data =====

studentdata[1,]
attach(studentdata)

### Investigating Studendata Drinks ====

table(Drink)

barplot(table(Drink), xlab="Drink", ylab = "Count", main = "Studentdata Drinks")

### Investigating studentdata Sleep ====

hours.of.sleep = WakeUp - ToSleep
summary(hours.of.sleep)

hist(hours.of.sleep, main= "Hoursof Sleep", xlab = "No. of Hours")

### Investigating studentdata Haircuts ====


female.Haircut <- Haircut[Gender == "female"]
male.Haircut <- Haircut[Gender == "male"]

summary(female.Haircut)
summary(male.Haircut)

### Studying relationships between Variables ====

arg <- hours.of.sleep ~ Gender

boxplot(arg, ylab = "Hours of Sleep")

plot(jitter(ToSleep), jitter(hours.of.sleep))

fit <- lm(hours.of.sleep~ToSleep)
fit
abline(fit)

# Question 1 ----

  # Basic figures 
    aj.Games <- 153
    aj.Wins <- 100
    aj.Losses <- aj.Games - aj.Wins

    knights.Games <- 223
    knights.Wins <- 135
    knights.Losses <- knights.Games - knights.Wins

  # Basic Probabilities  
    
    pr.aj.Plays <- aj.Games / knights.Games
    pr.aj.Wins <- aj.Wins / aj.Games
    pr.aj.Loses <- aj.Losses / aj.Games

    pr.knights.Win <- knights.Wins / knights.Games
    pr.knights.Lose <- knights.Losses / knights.Games

  # Probability tha AJ plays AND the Knights Win 
    
    pr.aj.Plays.knights.Win <- pr.aj.Plays + pr.knights.Win - pr.aj.Wins

  # Conditional Probabilites 
    
    pr.aj.Plays_knights.Win <- (pr.aj.Wins*pr.aj.Plays)/pr.knights.Win

    pr.aj.dnPlay_knights.Win <- (pr.knights.Win*(1 - pr.aj.Plays))/pr.knights.Win

    pr.knights.Win_aj.Plays <- (pr.aj.Plays_knights.Win*pr.knights.Win)/pr.aj.Plays

    pr.knights.Win_aj.dnPlay <- (pr.aj.dnPlay_knights.Win*pr.knights.Win)/(1 - pr.aj.Plays)

    pr.aj.Plays_knights.Lose <- (pr.aj.Loses*pr.aj.Plays)/pr.knights.Lose
    
### Question 2 ----
    
# A == you have HIV
# B == you have returned a positive test result.

    # Prior chances
    prior <- c(0.005, 0.995)
    
    # likelihood based on false-neg and false-pos
    likelihood <- c(0.97, 0.02)
    likelihood_f <- c(0.97^3, 0.02^3)
    
    # The product of the priors * the likelihood
    prod <- prior*likelihood
    prod_f <- prior*likelihood_f 
    
    # posterior probabilities.
    posterior <- prod/sum(prod)
    posterior_f <- prod_f/sum(prod_f)
    
    posterior
    posterior_f
    
    # the bayes factor = the magnitude of the effect of the test on the prior probabilities.
    bayes.factor <- posterior / prior
    bayes.factor_f <- posterior_f / prior
    
    bayes.factor
    bayes.factor_f
    

    
    for(i in seq(0.01, 0.1, by =  0.005)){
        # Prior chances
      likelihood.value <- matrix(nrow = 20)
        prior = c(i, 1-i)
        likelihood <- c(0.97, 0.02)
        prod = prior*likelihood
        posterior = prod/sum(prod)
        likelihood.value[i] <- posterior
    }
    
    choices <- as.numeric(seq(0.01, 0.1, by = 0.005))
    mymat = matrix(nrow=19, ncol=2) 
    for (j in 1:19) {
      for (i in choices) {
        prior = c(i, 1-i)
        # likelihood <- c(0.97, 0.02)
        # posterior = (prior*likelihood)/sum(prior*likelihood)
        # print(i, posterior)
        mymat[j] <- prior
        }
      }

        
        # the bayes factor = the magnitude of the effect of the test on the prior probabilities.
        bayes.factor <- posterior / prior
        bayes.factor_f <- posterior_f / prior
        
        bayes.factor
      }
    