graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!


# load libraries
library("tidyverse")
library("rjags")

# load data
load(file = "Phenology.RData")

# replace block in A and M site to 11:30
phenology %>% 
  spread(key = origSite, value = block) %>% 
  mutate(H = as.numeric(H), A = as.numeric(A) + 10, M = as.numeric(M) + 20) %>% 
  gather(key = origSite, value = block, H, A, M) %>% 
  filter(!is.na(block)) %>% distinct(origSite, block)

phenology %>% 
  mutate(block = plyr::mapvalues(block, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), c("11", "12", "13", "14", "15", "16", "17", "18", "19", "20")))
  distinct(origSite, block)

  mutate(origSite == factor(origSite, levels = c("H", "A", "M"))) %>% 
    mutate(block == factor(block, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))) %>% distinct(origSite, block) %>% pn
  
myData = phenology %>% filter(newTT != "Cold", year == "2017", origSite != "M", pheno.stage == "Flower", pheno.var == "peak") %>% select(value, newTT, species, origSite) %>% droplevels()

y = myData$value
newTT = as.numeric(myData$newTT)
species = as.numeric(myData$species)
origSite = as.numeric(myData$origSite)


Ntotal = length(y)
NnewTTLvl = nlevels(myData$newTT)
NSPTTLvl = nlevels(myData$species)

dataList <- list(y = y, newTT = newTT, origSite = origSite, species = species)

#------------------------------------------------------------------------------
# THE MODEL

### FLOWERING ~ TREATMENT + (1|SPECIES) + (1|Block) ###

sink('TEMPmodel.txt')
cat("
    model{
    # Likelihood
    for(i in 1:Ntotal){
    y[i] ~ dpois(lambda[i])
    
    # linear predictor
    lambda[i] <- exp(alpha + newTTCoeff[newTT[i]] + siteCoeff[origSite[i]] + spCoeff[species[i]])

    }

    # Prior for Fixed Effects
    for(i in 1:3){
    newTTCoeff[i] ~ dnorm(0, 0.0001)
    }  


    # Prior for Random Effects
    for(i in 1:25){
    spCoeff[i] ~ dnorm(0, spPrec)
    }

    spPrec ~ dgamma(0.001, 0.001) 
    alpha ~ dnorm(0, 0.0001) # Intercept
    siteCoeff ~ dnorm(0, 0.0001)

    }
    ", fill = TRUE)
sink()

inits.fn <- function() list(alpha = rnorm(1,0,1), spCoeff = rnorm(25, 0.3), newTTCoeff = rnorm(3, 0.2), siteCoeff = rnorm(2, 0.2))

jagsModel <- jags.model(file = "TEMPmodel.txt", data = dataList, inits = inits.fn, n.chains = 3, n.adapt = 5000)

# Specify parameters for which posterior samples are saved
para.names <- c("alpha", "spCoeff", "newTTCoeff")
# Continue the MCMC runs with sampling
Samples <- coda.samples(jagsModel , variable.names = para.names, n.iter = 5000)
summary(Samples)

library("lme4")
summary(glmer(y ~ newTT + (1|species), myData, family = "poisson"))



beta ~ dnorm(0, 0.001)
