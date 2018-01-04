# Negative Binomial Model

sink('TEMPmodel.txt')
cat("
    model{
    # Likelihood
    for(i in 1:Ntotal){
    
    # for poisson
    #y[i] ~ dpois(lambda[i]) 
    
    # for neg binom.
    y[i] ~ dnegbin(p[i], r)
    
    # linear predictor
    log(mu[i]) <- alpha + newTTCoeff[newTT[i]] + siteCoeff[origSite[i]] + spCoeff[species[i]] + blockCoeff[block[i]]
    
    # Transforms mu into p, which is used by the negative binomial distribution
    p[i] <- r/(r + mu[i])
    
    }
    
    ### PRIORS
    alpha ~ dmnorm(180, 30) # Intercept
    spPrec ~ dgamma(0.001, 0.001)
    blockPrec ~ dgamma(0.001, 0.001)
    r ~ dunif(0, 50) # for neg binom
    
    # Prior for Fixed Effects
    for(i in 1:NnewTTLvl){
    newTTCoeff[i] ~ dnorm(0, 0.0001)
    }  
    
    for(i in 1:NsiteLvl){
    siteCoeff[i] ~ dnorm(0, 0.0001)
    }
    
    # Prior for Random Effects
    for(i in 1:NSPLvl){
    spCoeff[i] ~ dnorm(0, spPrec)
    }
    
    for(i in 1:NBlockLvl){
    blockCoeff[i] ~ dnorm(0, blockPrec)
    }
    
    }
    ", fill = TRUE)
sink()