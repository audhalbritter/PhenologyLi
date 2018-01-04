################################################################################
#### DIFFERENCES #####
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# load libraries
library("tidyverse")
library("rjags")

# load data
load(file = "Phenology.RData")

differences <- phenology %>% 
  filter(year == 2017) %>% 
  select(turfID, species, origSite, block, newTT, pheno.stage, pheno.var, value) %>% 
  spread(key = newTT, value = value) %>% 
  
  # Calculate site mean value for controls
  group_by(species, origSite, pheno.stage, pheno.var) %>%
  mutate(ControlMeanSite = mean(Control, na.rm = TRUE), ControlSDSite = sd(Control, na.rm = TRUE)) %>%
  select(-turfID) %>% 
  gather(key = newTT, value = value, OTC, Warm, Cold) %>% 
  mutate(Diff = value - ifelse(!is.na(Control), Control, ControlMeanSite)) %>% 
  filter(!is.na(Diff))

myData <- differences %>% 
  # subset
  filter(pheno.stage == "Flower", pheno.var == "first") %>% 
  select(Diff, newTT, species, origSite, block) %>% 
  mutate(block = factor(block)) %>% droplevels()
myData <- as.data.frame(myData)

y = myData$Diff
newTT = as.numeric(factor(myData$newTT))
species = as.numeric(factor(myData$species))
origSite = as.numeric(factor(myData$origSite))
block = as.numeric(myData$block)

Ntotal = length(y)
NnewTTLvl = nlevels(factor(myData$newTT))
NsiteLvl = nlevels(factor(myData$origSite))
NSPLvl = nlevels(factor(myData$species))
NBlockLvl = nlevels(factor(myData$block))

# Making a data list
dataList <- list(y = y, 
                 newTT = newTT, 
                 origSite = origSite,
                 species = species, 
                 block = block, 
                 Ntotal = Ntotal, 
                 NnewTTLvl = NnewTTLvl, 
                 NsiteLvl = NsiteLvl,
                 NSPLvl = NSPLvl,
                 NBlockLvl = NBlockLvl
)


#------------------------------------------------------------------------------
# THE MODEL

### FLOWERING.DIFFERENCE ~ TREATMENT + ORIGINSITE + (1|SPECIES) + (1|BLOCK) ###


#source("NegativeBinomial.R")
source("NormalRegression.R")

# A function to generate initial values for each chain
inits.fn <- function() {
  list(newTTCoeff = rnorm(NnewTTLvl, 0.2), 
       siteCoeff = rnorm(NsiteLvl, 0.2),
       spCoeff = rnorm(NSPLvl, 0.1), 
       blockCoeff = rnorm(NBlockLvl, 0.2)
  )
}


jagsModel <- jags.model(file = "TEMPmodel.txt", data = dataList, inits = inits.fn, n.chains = 3, n.adapt = 100000)

# Specify parameters for which posterior samples are saved
para.names <- c("newTTCoeff", "siteCoeff", "spCoeff", "blockCoeff", "sigma")
#para.names <- c("alpha", "newTTCoeff", "siteCoeff")
# Continue the MCMC runs with sampling
Samples <- coda.samples(jagsModel , variable.names = para.names, n.iter = 100000)
summary(Samples)

png(file = "Traceplots%d.png", width = 1000, height = 1000)
plot(Samples)
dev.off()

gelman.diag(Samples)


library("lme4")
summary(lmer(y ~ newTT + origSite + (1|species) + (1|block), myData))


#Call jags function; specify number of chains, number of adaptive iterations,
#the length of the burn-in period, total iterations, and the thin rate.
outPeakFlower <- jags(data = dataList,
                      parameters.to.save = para.names,
                      model.file = "TEMPmodel.txt",
                      n.chains = 3,
                      n.adapt = 50000,
                      n.iter = 50000,
                      n.thin = 0)