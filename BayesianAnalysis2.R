#------------------------------------------------------------------------------
# LOAD LIBRARIES


graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# load libraries
library("tidyverse")
library("rjags")
library("R2jags")
library("lme4")

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
para.names <- c("alpha", paste("newTTCoeff[", 2:4, "]", sep = ""), paste("siteCoeff[", 1:2, "]", sep = ""), paste("spCoeff[", 1:20, "]", sep = ""), paste("blockCoeff[", 1:19, "]", sep = ""), "tau", "diff1", "diff2", "diff3")

#------------------------------------------------------------------------------
# RUN ANALYSIS

## Run model
modPeakBud <-jags(data = dataList, 
            parameters.to.save = para.names,
            n.thin = thin.rate, 
            n.chains = nc, n.burnin = n.burn, n.iter = n.iterations,
            model.file = "NormalRegression.R")

modPeakBud
# use as.mcmmc to convert rjags object into mcmc.list
modPeakBud.mcmc <- as.mcmc(modPeakBud)

#------------------------------------------------------------------------------
# MODEL CHECK

pdf(file="ModelCheck/modPeakBud.JAGS.diagnostic.pdf", width = 12, height = 10)
par(mar=c(4,2,2,1))
plot(modPeakBud)
plot(modPeakBud.mcmc)
dev.off()

png(file = "ModelCheck/modPeakBudGelmanplots%d.png", width = 1000, height = 1000)
gelman.plot(modPeakBud.mcmc)
dev.off()

png(file = "ModelCheck/modPeakBudTraceplots%d.png", width = 1000, height = 1000)
plot(modPeakBud.mcmc)
dev.off()

# How test model assumptions (residuals)? monitor mu[i]!!!!

#------------------------------------------------------------------------------
# OUTPUT

names(modPeakBud$BUGSoutput)
res <- data.frame(modPeakBud$BUGSoutput$summary)
Pbud <- res %>% 
  rownames_to_column(var = "variable") %>% 
  filter(grepl("newTT", variable)) %>% 
  mutate(variable = plyr::mapvalues(variable, c("newTTCoeff[2]", "newTTCoeff[3]", "newTTCoeff[4]"), c("OTC", "Transplant warm", "Transplant cold"))) %>% 
  mutate(variable = factor(variable, levels = c("OTC", "Transplant warm", "Transplant cold"))) %>% 
  mutate(pheno.var = "Bud") %>% 
  ggplot(aes(x = variable, y = X50., ymin = X2.5., ymax = X97.5.)) +
  geom_point() +
  geom_errorbar(width = 0) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  labs(x = "", y = "Median and 95% credible interval")

save(Pbud, file = "Pbud.RData")

# check lme4 model
summary(lmer(y ~ newTT + origSite + (1|species) + (1|block), myData))

# Check contrasts
res %>% 
  rownames_to_column(var = "variable") %>% 
  filter(grepl("diff", variable)) %>%
  mutate(variable = plyr::mapvalues(variable, c("diff1", "diff2", "diff3"), c("OTC vs. Transplant warm", "OTC vs. Transplant cold", "Transplant warm vs. Transplant cold"))) %>% 
  ggplot(aes(x = variable, y = X50., ymin = X2.5., ymax = X97.5.)) +
  geom_point() +
  geom_errorbar(width = 0) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed")


##########################
#### PHENO.VAR FLOWER ####
##########################
#------------------------------------------------------------------------------

myData <- phenology %>% 
  # subset
  filter(year == "2017", pheno.stage == "Flower", pheno.var == "peak") %>% 
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
# RUN ANALYSIS

## Run model
modPeakFl <-jags(data = dataList, 
            parameters.to.save = para.names,
            n.thin = thin.rate, 
            n.chains = nc, n.burnin = n.burn, n.iter = n.iterations,
            model.file = "NormalRegression.R")

modPeakFl
# use as.mcmmc to convert rjags object into mcmc.list
modPeakFl.mcmc <- as.mcmc(modPeakFl)

#------------------------------------------------------------------------------
# MODEL CHECK

pdf(file="ModelCheck/modPeakFl.JAGS.diagnostic.pdf", width = 12, height = 10)
par(mar=c(4,2,2,1))
plot(modPeakFl)
plot(modPeakFl.mcmc)
dev.off()

png(file = "ModelCheck/modPeakFlGelmanplots%d.png", width = 1000, height = 1000)
gelman.plot(modPeakFl.mcmc)
dev.off()

png(file = "ModelCheck/modPeakFlTraceplots%d.png", width = 1000, height = 1000)
plot(modPeakFl.mcmc)
dev.off()

# How test model assumptions (residuals)? monitor mu[i]!!!!

#------------------------------------------------------------------------------
# OUTPUT

names(modPeakFl$BUGSoutput)
fl <- data.frame(modPeakFl$BUGSoutput$summary)
Pflower <- fl %>% 
  rownames_to_column(var = "variable") %>% 
  filter(grepl("newTT", variable)) %>% 
  mutate(variable = plyr::mapvalues(variable, c("newTTCoeff[2]", "newTTCoeff[3]", "newTTCoeff[4]"), c("OTC", "Transplant warm", "Transplant cold"))) %>% 
  mutate(variable = factor(variable, levels = c("OTC", "Transplant warm", "Transplant cold"))) %>% 
  mutate(pheno.var = "Flower") %>% 
  rbind(Pbud) %>% 
  ggplot(aes(x = variable, y = X50., ymin = X2.5., ymax = X97.5., color = variable)) +
  geom_point() +
  geom_errorbar(width = 0) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_color_manual(values = c("purple", "orange", "lightblue")) +
  labs(x = "", y = "Median and credible interval") +
  facet_grid(~ pheno.var) +
  theme(legend.position="none", text = element_text(size=20))

save(Pflower, file = "Pflower.RData")

# check lme4 model
summary(lmer(y ~ newTT + origSite + (1|species) + (1|block), myData))


# Check contrasts
fl %>% 
  rownames_to_column(var = "variable") %>% 
  filter(grepl("diff", variable)) %>%
  mutate(variable = plyr::mapvalues(variable, c("diff1", "diff2", "diff3"), c("OTC vs. Transplant warm", "OTC vs. Transplant cold", "Transplant warm vs. Transplant cold"))) %>% 
  ggplot(aes(x = variable, y = X50., ymin = X2.5., ymax = X97.5.)) +
  geom_point() +
  geom_errorbar(width = 0) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed")




##########################
#### PHENO.VAR SEED ####
##########################
#------------------------------------------------------------------------------

myData <- phenology %>% 
  # subset
  filter(year == "2017", pheno.stage == "Seed", pheno.var == "peak") %>% 
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
# RUN ANALYSIS

## Run model
modPeakSeed <-jags(data = dataList, 
                 parameters.to.save = para.names,
                 n.thin = thin.rate, 
                 n.chains = nc, n.burnin = n.burn, n.iter = n.iterations,
                 model.file = "NormalRegression.R")

modPeakSeed
# use as.mcmmc to convert rjags object into mcmc.list
modPeakSeed.mcmc <- as.mcmc(modPeakSeed)

#------------------------------------------------------------------------------
# MODEL CHECK

pdf(file="ModelCheck/modPeakSeed.JAGS.diagnostic.pdf", width = 12, height = 10)
par(mar=c(4,2,2,1))
plot(modPeakFl)
plot(modPeakFl.mcmc)
dev.off()

png(file = "ModelCheck/modPeakSeedGelmanplots%d.png", width = 1000, height = 1000)
gelman.plot(modPeakFl.mcmc)
dev.off()

png(file = "ModelCheck/modPeakSeedTraceplots%d.png", width = 1000, height = 1000)
plot(modPeakFl.mcmc)
dev.off()

# How test model assumptions (residuals)? monitor mu[i]!!!!

#------------------------------------------------------------------------------
# OUTPUT

names(modPeakSeed$BUGSoutput)
seed <- data.frame(modPeakSeed$BUGSoutput$summary)
Pseed <- seed %>% 
  rownames_to_column(var = "variable") %>% 
  filter(grepl("newTT", variable)) %>% 
  mutate(variable = plyr::mapvalues(variable, c("newTTCoeff[2]", "newTTCoeff[3]", "newTTCoeff[4]"), c("OTC", "Transplant warm", "Transplant cold"))) %>% 
  mutate(variable = factor(variable, levels = c("OTC", "Transplant warm", "Transplant cold"))) %>% 
  mutate(pheno.var = "Seed")

save(Pseed, file = "Pseed.RData")

PlotTreatmentEffects <- Pseed %>% 
  rbind(Pbud, Pflower) %>% 
  ggplot(aes(x = variable, y = X50., ymin = X2.5., ymax = X97.5., color = variable)) +
  geom_point(size = 5) +
  geom_errorbar(width = 0) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  scale_color_manual(values = c("purple", "orange", "lightblue")) +
  labs(x = "", y = "Median and credible interval") +
  facet_grid(~ pheno.var) +
  theme(legend.position="none", text = element_text(size=18))

ggsave(PlotTreatmentEffects, filename = "PlotTreatmentEffects.jpeg", height = 6, width = 15, dpi = 300)


# check lme4 model
summary(lmer(y ~ newTT + origSite + (1|species) + (1|block), myData))


# Check contrasts
seed %>% 
  rownames_to_column(var = "variable") %>% 
  filter(grepl("diff", variable)) %>%
  mutate(variable = plyr::mapvalues(variable, c("diff1", "diff2", "diff3"), c("OTC vs. Transplant warm", "OTC vs. Transplant cold", "Transplant warm vs. Transplant cold"))) %>% 
  ggplot(aes(x = variable, y = X50., ymin = X2.5., ymax = X97.5.)) +
  geom_point() +
  geom_errorbar(width = 0) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed")
