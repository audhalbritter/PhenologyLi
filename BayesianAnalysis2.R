#------------------------------------------------------------------------------
# LOAD LIBRARIES


graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# load libraries
library("tidyverse")
library("rjags")
library("R2jags")

#######################
#### PHENO.VAR BUD ####
#######################
#------------------------------------------------------------------------------
# LOAD DATA
load(file = "Phenology.RData")

myData <- phenology %>% 
  # subset
  filter(year == "2017", pheno.stage == "Bud", pheno.var == "peak") %>% 
  select(value, newTT, species, origSite, block) %>% 
  mutate(block = factor(block), origSite = factor(origSite), species = factor(species)) %>%
  mutate(newTT = as.numeric(newTT), origSite = as.numeric(origSite), species = as.numeric(species), block = as.numeric(block))
myData <- as.data.frame(myData)

# Making a data list
y <- myData$value
newTT <- myData$newTT
origSite <- myData$origSite
species <- myData$species
block <- myData$block 
Ntotal <- length(y)
NnewTTLvl <-nlevels(factor(myData$newTT))
NsiteLvl <- nlevels(factor(myData$origSite))
NSPLvl <- nlevels(factor(myData$species))
NBlockLvl <- nlevels(factor(myData$block))

dataList <- list(y = myData$value, 
                 newTT = myData$newTT, 
                 origSite = myData$origSite,
                 species = myData$species, 
                 block = myData$block, 
                 Ntotal = length(y), 
                 NnewTTLvl = nlevels(factor(myData$newTT)), 
                 NsiteLvl = nlevels(factor(myData$origSite)),
                 NSPLvl = nlevels(factor(myData$species)),
                 NBlockLvl = nlevels(factor(myData$block))
                 )


#------------------------------------------------------------------------------
# SPECIFY PARAMETERS

n.iterations <- 100000      ## draws from posterior
n.burn <- 10000      ## draws to discard as burn-in
thin.rate <- 5    	## thinning rate
nc <- 3			## number of chains

# Specify parameters for which posterior samples are saved
para.names <- c("alpha", paste("newTTCoeff[", 2:4, "]", sep = ""), paste("siteCoeff[", 1:2, "]", sep = ""), paste("spCoeff[", 1:20, "]", sep = ""), paste("blockCoeff[", 1:19, "]", sep = ""), "tau")

#------------------------------------------------------------------------------
# RUN ANALYSIS

## Run model
mod1 <-jags(data = dataList, 
            parameters.to.save = para.names,
            n.thin = thin.rate, 
            n.chains = nc, n.burnin = n.burn, n.iter = n.iterations,
            model.file = "NormalRegression.R")

mod1
# use as.mcmmc to convert rjags object into mcmc.list
mod1.mcmc <- as.mcmc(mod1)

#------------------------------------------------------------------------------
# MODEL CHECK

pdf(file="mod1.JAGS.diagnostic.pdf", width = 12, height = 10)
par(mar=c(4,2,2,1))
plot(mod1)
plot(mod1.mcmc)
dev.off()

png(file = "Gelmanplots%d.png", width = 1000, height = 1000)
gelman.plot(mod1.mcmc)
dev.off()

png(file = "Traceplots%d.png", width = 1000, height = 1000)
plot(Samples)
dev.off()

# How test model assumptions (residuals)? monitor mu[i]!!!!

#------------------------------------------------------------------------------
# OUTPUT

names(mod1$BUGSoutput)
res <- data.frame(mod1$BUGSoutput$summary)
res %>% 
  rownames_to_column(var = "variable") %>% 
  filter(grepl("newTT", variable)) %>% 
  mutate(variable = plyr::mapvalues(variable, c("newTTCoeff[2]", "newTTCoeff[3]", "newTTCoeff[4]"), c("OTC", "Transplant warm", "Transplant cold"))) %>% 
  mutate(variable = factor(variable, levels = c("OTC", "Transplant warm", "Transplant cold"))) %>% 
  mutate(pheno.var = "Bud") %>% 
  ggplot(aes(x = variable, y = X50., ymin = X2.5., ymax = X97.5.)) +
  geom_point() +
  geom_errorbar(width = 0) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  labs(x = "", y = "Median and credible interval")



# check lme4 model
library("lme4")
summary(lmer(y ~ newTT + origSite + (1|species) + (1|block), myData))


