### FUNCTION TO RUN BAYESIAN ANALYSIS
# dat = data
# Year = year
# phenostage = pheno.stage
# phenovar = pheno.var
# niter = number of iterations
# nburn = burnin phase
# nthin = thining
# nchain = number of chains
# mod = model

RunBayesianAnalysis <- function(dat, Year, phenostage, phenovar, niter, nburn, nthin, nchain, mod){
  
  #------------------------------------------------------------------------------
  # LOAD DATA
  
  
  myData <- dat %>% 
    # subset
    filter(year == Year, pheno.stage == phenostage, pheno.var == phenovar) %>% 
    select(value, newTT, species, origSite, block) %>% 
    mutate(block = factor(block), origSite = factor(origSite), species = factor(species)) %>%
    mutate(newTT = as.numeric(newTT), origSite = as.numeric(origSite), species = as.numeric(species), block = as.numeric(block)) %>% 
    arrange(newTT, species)
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
  
  modname = paste("mod", Year, phenostage, phenovar, sep = "")
  
  #------------------------------------------------------------------------------
  # SPECIFY PARAMETERS
  
  n.iterations <- niter      ## draws from posterior
  n.burn <- nburn      ## draws to discard as burn-in
  thin.rate <- nthin    	## thinning rate
  nc <- nchain			## number of chains
  
  # Specify parameters for which posterior samples are saved
  #para.names <- c("alpha", paste("newTTCoeff[", 2:(NnewTTLvl), "]", sep = ""), paste("siteCoeff[", 1:(NsiteLvl-1), "]", sep = ""), paste("spCoeff[", 1:(NSPLvl-1), "]", sep = ""), paste("blockCoeff[", 1:(NBlockLvl-1), "]", sep = ""), "tau", "diff1", "diff2", "diff3", "diff4", "diff5")
  
  # random slope model
  para.names <- c("alpha", "newTTCoeff", "siteCoeff")
  
  
  #------------------------------------------------------------------------------
  # RUN ANALYSIS
  
  ## Run model
  mod <-jags(data = dataList, 
             parameters.to.save = para.names,
             n.thin = thin.rate, 
             n.chains = nc, n.burnin = n.burn, n.iter = n.iterations,
             model.file = mod)
  
  # use as.mcmmc to convert rjags object into mcmc.list
  mod.mcmc <- as.mcmc(mod)
  
  #------------------------------------------------------------------------------
  # MODEL CHECK
  
  png(file = paste("ModelCheck/", modname, "Gelmanplots%d.png", sep = ""), width = 1000, height = 1000)
  gelman.plot(mod.mcmc)
  dev.off()
  
  png(file = paste("ModelCheck/", modname, "Traceplots%d.png", sep = ""), width = 1000, height = 1000)
  plot(mod.mcmc)
  dev.off()
  
  #------------------------------------------------------------------------------
  # OUTPUT
  
  # model
  save(mod, file = paste("ModelOutput/", "model", modname, ".RData", sep = ""))
  
  # model summary
  res <- data.frame(mod$BUGSoutput$summary)
  res2 <- res %>% 
    rownames_to_column(var = "variable") %>% 
    #filter(grepl("newTT|diff", variable)) %>% 
    #mutate(var = variable) %>% 
    #mutate(variable = plyr::mapvalues(variable, c("newTTCoeff[2]", "newTTCoeff[3]", "newTTCoeff[4]", "diff1", "diff2", "diff3", "diff4", "diff5"), c("OTC", "Transplant warm", "Transplant cold", "OTC vs. Transplant warm", "OTC vs. Transplant cold", "Transplant warm vs. Transplant cold", "abs(OTC) vs. abs(Transplant cold)", "abs(Transplant warm) vs. abs(Transplant cold)"))) %>% 
    #mutate(variable = factor(variable, levels = c("OTC", "Transplant warm", "Transplant cold", "OTC vs. Transplant warm", "OTC vs. Transplant cold", "Transplant warm vs. Transplant cold", "abs(OTC) vs. abs(Transplant cold)", "abs(Transplant warm) vs. abs(Transplant cold)"))) %>% 
    mutate(pheno.var = phenovar) %>% 
    mutate(pheno.stage = phenostage) %>% 
    mutate(year = Year)
  
  save(res2, file = paste("ModelOutput/", modname, ".RData", sep = ""))
  
}
