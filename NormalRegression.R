### FLOWERING ~ TREATMENT + ORIGINSITE + (1|SPECIES) + (1|Block) ###

model{
    # Likelihood
    for(i in 1:Ntotal){
    
    # normal distribution
    y[i] ~ dnorm(mu[i], tau)
    
    # linear predictor
    mu[i] <- alpha + newTTCoeff[newTT[i]] + siteCoeff[origSite[i]] + spCoeff[species[i]] + blockCoeff[block[i]]
    
    }
    
    ### PRIORS
    alpha ~ dunif(0, 360) # Intercept
    spPrec ~ dgamma(0.001, 0.001)
    blockPrec ~ dgamma(0.001, 0.001)
    
    #sigma ~ dunif(0, 100)
    #tau <- 1 / (sigma * sigma)
    tau ~ dgamma(0.001, 0.001)
    
    # Prior for Fixed Effects
    for(i in 2:NnewTTLvl){
    newTTCoeff[i] ~ dnorm(0, 1/10^2)
    }  
    newTTCoeff[1] <- 0    

    for(i in 1:(NsiteLvl-1)){
    siteCoeff[i] ~ dnorm(0, 1/10^2)
    }
    siteCoeff[NsiteLvl] <- 0

    
    # Prior for Random Effects
    for(i in 1:(NSPLvl-1)){
    spCoeff[i] ~ dnorm(0, spPrec)
    }
    spCoeff[NSPLvl] <- 0   

    for(i in 1:(NBlockLvl-1)){
    blockCoeff[i] ~ dnorm(0, blockPrec)
    }
    blockCoeff[NBlockLvl] <- 0 

}
