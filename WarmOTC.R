#### COMAPARE PHENOLOGY BETWEEN OTC AND TRANSPLANT ####
load(file = "Phenology.RData", verbose = TRUE)
source(file = "PhenoFunctions.R")

library("tidyverse")
library("lubridate")


#### FIGURES ####
### Community Figures ###
MeanSE <- SpeciesMeanSE(phenology, "peak")
PlotCommunityData(MeanSE, "peak")

### Species Figures ###
MeanSE <- SpeciesMeanSE(phenology)
PlotSpeciesData2(dat = MeanSE, phenovar = "peak", Year = 2016)


head(phenology)
MeanSE %>% 
  left_join(AirTemp, by = c("origSite", "Treatment" = "newTT")) %>% 
  filter(pheno.var == "peak") %>% 
  mutate(origSite = factor(origSite, levels = c("H", "A", "M"))) %>% 
  ggplot(aes(x = meanTair, y = mean, color = Treatment)) +
  geom_jitter() +
  stat_smooth(method = "lm", formula = "y ~ x") +
  facet_grid(~ pheno.stage)


#### ANALYSIS ####
library("MuMIn")
#  change the default "na.omit" to prevent models from being fitted to different datasets in case of missing values.
options(na.action = "na.fail") # can also be put in the model
options(na.action = "na.omit") # change back
# Alternatively put it in the modle


# Function for model selection
ModelSelection <- function(dat){
  # fit model
  fit1 <- lmer(value ~ newTT + origSite + (1|species) + (1|block), dat)
  
  # Model selection using dredge
  
  model.set <- dredge(fit1, rank = "AICc", extra = "R^2")
  
  res <- data.frame(model.set)
  res$cumsum <- cumsum(res$weight)
  
  res <- res %>% 
    select(-logLik, -AICc) %>% 
    rename(R.square = R.2, delta.AIC = delta, akaikeWeight = weight) %>% 
    mutate(R.square = round(R.square, 3), delta.AIC = round(delta.AIC, 3), akaikeWeight = round(akaikeWeight, 3), cumsum = round(cumsum, 3))
    
    res
}



res <- phenology %>% 
  filter(year == "2017", pheno.stage != "Ripe") %>% 
  group_by(year, pheno.stage, pheno.var) %>% 
  select(value, newTT, species, origSite, block) %>% 
  droplevels() %>% 
  do(ModelSelection(.))

res %>%
  group_by(year, pheno.stage, pheno.var) %>%
  filter(cumsum < 0.95)


# test examples
ddd <- phenology %>% 
  filter(year == "2017", pheno.stage == "Seed", pheno.var == "duration")

fit1 <- lmer(value ~ newTT + origSite + (1|species) + (1|block), ddd)
summary(fit1)
ModelCheck(fit1)

model.set <- dredge(fit1, rank = "AICc", extra = "R^2")

mm <- data.frame(model.set)
mm$cumsum <- cumsum(mm$weight)
mm95 <- mm %>% filter(cumsum < 0.95)
averaged.model <- model.avg(model.set, cumsum(weight) <= percent.thresh)
res <- data.frame(summary(averaged.model)$coefmat.full)




# backtransform the data to get doy
newdat <- with(myData, expand.grid(
  newTT = c("Control", "OTC", "Warm", "Cold"),
  origSite = c("H", "A", "M"),
  value = 0
))


newdat$value <- predict(fit3, newdat, re.form = NA) # , type="response"
mm <- model.matrix(terms(fit3), newdat)

## or newdat$distance <- mm %*% fixef(fit1)

pvar1 <- diag(mm %*% tcrossprod(vcov(fit1), mm))
tvar1 <- pvar1 + VarCorr(fit1)$block[1]  ## must be adapted for more complex models
cmult <- 1.96 ## could use 1.96

newdat <- newdat %>% 
  mutate(value = exp(value)) %>% 
  mutate(plow = exp(value - cmult * sqrt(pvar1))) %>% 
  mutate(phigh = exp(value + cmult * sqrt(pvar1))) %>% 
  mutate(tlow = exp(value - cmult * sqrt(tvar1))) %>% 
  mutate(thigh = exp(value + cmult * sqrt(tvar1)))

#plot confidence
g0 <- ggplot(newdat, aes(x = newTT, y = value, ymin = plow, ymax = phigh, colour = species)) + 
  geom_point() +
  geom_errorbar(colour = "red") +
  facet_wrap(~ species)



########################################################################
# Maybe do not do this, data is not detailed enough to calc this !!!!
#### CALCULATE DAYS BETWEEN FIRST BUD AND FLOWER, FLOWER AND SEED ETC (PHENO.STAGES IN DAYS) ####
pheno.long <- pheno.long %>% 
  spread(key = pheno.stage, value = value) %>% 
  # calculate difference in days between peak bud-flower and flower-seed
  mutate(bf = ifelse(pheno.var == "peak", f-(b-1), NA), fs = ifelse(pheno.var == "peak", s-(f-1), NA), sr = ifelse(pheno.var == "peak", r-(s-1), NA)) %>%
  gather(key = pheno.stage, value = value, b, f, s, r, bf, fs, sr) %>%
  mutate(pheno.unit = ifelse(pheno.var == "duration", "days",
                             ifelse(pheno.var == "peak" & pheno.stage %in% c("bf", "fs", "sr"), "days", "doy"))) %>%
  filter(!is.na(value)) %>%  # remove empty rows
  mutate(value = ifelse(value < 0, NA, value)) # make negative values NA

########################################################################
