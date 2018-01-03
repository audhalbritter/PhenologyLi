# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!


# load libraries
library("tidyverse")
library("rjags")

# load data
load(file = "Phenology.RData")
myData = phenology %>% filter(newTT != "Cold", year == "2017", pheno.stage == "Flower", pheno.var == "peak") %>% select(value, newTT, species) %>% as_data_frame()
myData$newTT <- droplevels(myData$newTT)
myData$species <- droplevels(factor(myData$species))

y = myData$value
dataList = list(y = myData$value ,
  newTT = as.numeric(myData$newTT) ,
  species = as.numeric(myData$species),
  Ntotal = length(y) ,
  NnewTTLvl = nlevels(myData$newTT),
  NspLvl = nlevels(myData$species)
)

#------------------------------------------------------------------------------
# THE MODEL

### FLOWERING ~ TREATMENT + (1|SPECIES) ###

sink('TEMPmodel.txt')
cat("
    model{
    # Likelihood
    for(iterN in 1:Ntotal){
    y[iterN] ~ dpois(lambda[iterN])

    # linear predictor
    lambda[iterN] <- exp(alpha[species[iterN]] + betaNewTT*newTT[iterN])
    eps[iterN] ~ dnorm(0, tau.eps)
    }
    
# Prior for Fixed Effects - Treatment (factor)
    for(iternewTT in 1:NnewTTLvl){
    alpha[iternewTT] ~ dnorm(0, 0.0001) # Intercept 
    betaNewTT[iternewTT]  ~ dnorm(0, 0.0001) # Slope
    }

    # Priot for Random Effects - Species
    spPrec ~ dgamma(0.001, 0.001) # gamma distribution > 0    
    
    # use prior to set the random effect coefficient
    for(iterSP in 1:NspLvl){ 
    spCoeff[iterSP] ~ dnorm(0, spPrec)
    }
    }
    ", fill = TRUE)
sink()





inits.fn <- function() list(alpha = rnorm(1,0,1),
                            betaNewTT = rnorm(1,0,1),
                            spCoeff = rnorm(1,0,1),
                            tau.eps = runif(1,0,10)
)

# Compile the model and run the MCMC for an adaptation (burn-in) phase
jagsModel <- jags.model(file = "TEMPmodel.txt", data = dataList, inits = inits.fn, n.chains = 3, n.adapt = 5000)

# Specify parameters for which posterior samples are saved
para.names <- c('alpha', 'beta.newTT', 'sig.eps')
# Continue the MCMC runs with sampling
Samples <- coda.samples(jagsModel , variable.names = para.names, n.iter = 5000)

plot(Samples)

# convergence check
gelman.diag(Samples)

summary(Samples)

# Examine chains
source("DBDA2E-utilities.R")
diagMCMC( codaObject=Samples , parName=para.names )
plotMCMC( mcmcCoda , 
          datFrm=myData , yName=yName , xName=xName)







# initialize chain
numSavedSteps=50000
parameters = c( "mu" ,  "sigma")
adaptSteps = 500  # Number of steps to "tune" the samplers
burnInSteps = 1000
nChains = 4 
thinSteps = 1
nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )
# Create, initialize, and adapt the model:
jagsModel = jags.model( "TEMPmodel.txt" , data=dataList , n.chains=nChains , n.adapt=adaptSteps )

# run jags
jags.model(file = "TEMPmodel.txt", data = dataList)
jagsModel = jags.model( "TEMPmodel.txt" , data=dataList , inits=initsList, n.chains=nChains , n.adapt=adaptSteps )
# Burn-in:
cat( "Burning in the MCMC chain...\n" )
update( jagsModel , n.iter=burnInSteps )
# The saved MCMC chain:
cat( "Sampling final MCMC chain...\n" )
codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                            n.iter=nIter , thin=thinSteps )

# Examine chains
source("DBDA2E-utilities.R")
diagMCMC( codaObject=codaSamples , parName="theta" )



# Load the functions used below:
source("DBDA2E-utilities.R") # Must be in R's current working directory.
require(rjags)               # Must have previously installed package rjags.

fileNameRoot="Jags-ExampleScript" # For output file names.




# Define the model:
modelString = "

data {
  Ntotal <- length(y)
xm <- mean(x)
ym <- mean(y)
xsd <- sd(x)
ysd <- sd(y)
for ( i in 1:length(y) ) {
zx[i] <- ( x[i] - xm ) / xsd
zy[i] <- ( y[i] - ym ) / ysd
}
}

model {
  for ( i in 1:Ntotal ) {
zy[i]  ̃ dt( zbeta0[s[i]] + zbeta1[s[i]] * zx[i], 1/zsigmaˆu)
}
for ( j in 1:Nsubj ) {
zbeta0[j]  ̃ dnorm( zbeta0mu , 1/(zbeta0sigma)ˆ2 )
zbeta1[j]  ̃ dnorm( zbeta1mu , 1/(zbeta1sigma)ˆ2 )
}

# Priors vague on standardized scale:
zbeta0mu  ̃ dnorm( 0 , 1/(10)ˆ2 )
zbeta1mu  ̃ dnorm( 0 , 1/(10)ˆ2 )
zsigma  ̃ dunif( 1.0E-3 , 1.0E+3 )
zbeta0sigma  ̃ dunif( 1.0E-3 , 1.0E+3 )
zbeta1sigma  ̃ dunif( 1.0E-3 , 1.0E+3 )

# Transform to original scale:
for ( j in 1:Nsubj ) {
beta1[j] <- zbeta1[j] * ysd / xsd
beta0[j] <- zbeta0[j] * ysd  + ym - zbeta1[j] * xm * ysd / xsd
}
beta1mu <- zbeta1mu * ysd / xsd
beta0mu <- zbeta0mu * ysd  + ym - zbeta1mu * xm * ysd / xsd
sigma <- zsigma * ysd
}
" # close quote for modelString
writeLines( modelString , con="TEMPmodel.txt" )

# Initialize the chains based on MLE of data.
# Option: Use single initial value for all chains:
#  thetaInit = sum(y)/length(y)
#  initsList = list( theta=thetaInit )
# Option: Use function that generates random values for each chain:
initsList = function() {
  resampledY = sample( y , replace=TRUE )
  thetaInit = sum(resampledY)/length(resampledY)
  thetaInit = 0.001+0.998*thetaInit # keep away from 0,1
  return( list( theta=thetaInit ) )
}

# Run the chains:
jagsModel = jags.model( file="TEMPmodel.txt" , data=dataList , inits=initsList , 
                        n.chains=3 , n.adapt=500 )
update( jagsModel , n.iter=500 )
codaSamples = coda.samples( jagsModel , variable.names=c("theta") ,
                            n.iter=3334 )
save( codaSamples , file=paste0(fileNameRoot,"Mcmc.Rdata") )

# Examine the chains:
# Convergence diagnostics:
diagMCMC( codaObject=codaSamples , parName="theta" )
saveGraph( file=paste0(fileNameRoot,"ThetaDiag") , type="eps" )
# Posterior descriptives:
openGraph(height=3,width=4)
par( mar=c(3.5,0.5,2.5,0.5) , mgp=c(2.25,0.7,0) )
plotPost( codaSamples[,"theta"] , main="theta" , xlab=bquote(theta) )
saveGraph( file=paste0(fileNameRoot,"ThetaPost") , type="eps" )
# Re-plot with different annotations:
plotPost( codaSamples[,"theta"] , main="theta" , xlab=bquote(theta) , 
          cenTend="median" , compVal=0.5 , ROPE=c(0.45,0.55) , credMass=0.90 )
saveGraph( file=paste0(fileNameRoot,"ThetaPost2") , type="eps" )
