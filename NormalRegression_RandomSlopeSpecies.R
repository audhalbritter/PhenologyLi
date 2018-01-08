### FLOWERING ~ TREATMENT + ORIGINSITE + (1|SPECIES) + (1|Block) ###

model{
  # Likelihood
  for(i in 1:Ntotal){
    
    # normal distribution
    y[i] ~ dnorm(mu[i], tau)
    
    # linear predictor
    mu[i] <- newTTCoeff[species[i], newTT[i]] + siteCoeff[origSite[i]] + blockCoeff[block[i]]
    
  }
  
  ### PRIORS
  #alpha ~ dunif(0, 360) # Intercept
  #spPrec ~ dgamma(0.001, 0.001)
  blockPrec ~ dgamma(0.001, 0.001)
  
  #sigma ~ dunif(0, 100)
  #tau <- 1 / (sigma * sigma)
  tau ~ dgamma(0.001, 0.001)
  
  # Prior for Fixed Effects
  for(sp in 1:NSPLvl){  # need to loop through all species  
    for(t in 1:NnewTTLvl){
      
      newTTCoeff[sp,t] ~ dnorm(mean.treatment[sp,t], tau.slope[sp]) 
      mean.treatment[sp,t] ~ dnorm(0, 0.001) 
      
    }}
  
  for(i in 1:NsiteLvl){
    siteCoeff[i] ~ dnorm(0, 1/10^2)
  }
  
  
  # Prior for Random Effects
  
  for(i in 1:NBlockLvl){
    blockCoeff[i] ~ dnorm(0, blockPrec)
  }

  for(sp in 1:(NSPLvl)){  
    tau.slope[sp] ~ dgamma(0.001, 0.001)  
  }
  
  ## Treatment contrasts
  #for(sp in 1:NSPLvl){  
    #for(t1 in 1:NnewTTLvl){
      #for(t2 in 1:NnewTTLvl){
        #treatment.contrast[sp,t1,t2] <- mean.treatment[sp,t1] - mean.treatment[sp,t2]
      #}
      # treatment.contrast[sp,t1] <- mean.treatment[sp,t1] - mean.treatment[sp,1] # Or could just get contrasts with control
    #} }
  
  #diff1 <- newTTCoeff[2] - newTTCoeff[3]
  #diff2 <- newTTCoeff[2] - newTTCoeff[4]
  #diff3 <- newTTCoeff[3] - newTTCoeff[4]
}
