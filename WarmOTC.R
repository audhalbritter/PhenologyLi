#### COMAPARE PHENOLOGY BETWEEN OTC AND TRANSPLANT ####
load(file = "Phenology.RData")
source(file = "PhenoFunctions.R")

library("tidyverse")
library("lubridate")

# remove "Cold" treatment
phenology <- phenology %>% 
  filter(newTT != "Cold")

#### FIGURES ####
### Community Figures ###
MeanSE <- SpeciesMeanSE(phenology, "peak")
PlotCommunityData(MeanSE, "peak")

### Species Figures ###
MeanSE <- SpeciesMeanSE(phenology, "peak")
MeanSE <- MeanSE %>% 
  filter(year == "2017", pheno.stage != "Ripe", origSite != "M") 
#%>% filter(species %in% c("Cya.inc", "Kob.cap", "Kob.roy", "Car.atr.m", "Car.lae", "Ger.pyl", "Poa.spp", "Pol.viv", "Pot.leu"))
PlotSpeciesData(MeanSE)


phenology %>% 
  filter(year == "2017", pheno.stage == "Flower", pheno.var == "peak") %>% 
  ggplot(aes(x = newTT, y = value)) +
  geom_violin(draw_quantiles = c(0.5)) +
  geom_jitter(aes(colour = species)) +
  facet_wrap(~ origSite)


phenology %>% 
  select(-Warm, -Cold, -OTC, -sum, -pheno.unit) %>% 
  spread(key = pheno.var, value = value) %>% 
  ggplot(aes(x = newTT, y = duration, colour = species)) +
  geom_jitter() +
  facet_wrap(~ origSite)
  


#### ANALYSIS ####
dat <- phenology %>% 
  filter(year == "2017", pheno.stage == "Flower", pheno.var == "peak") %>% 
  filter(species %in% c("Cya.inc", "Kob.cap", "Kob.roy", "Car.atr.m", "Car.lae", "Ger.pyl", "Poa.spp", "Pol.viv", "Pot.leu"))
fit1 <- glmer(value ~ newTT + species + (1|origSite/block), dat, family = "poisson")
fit2 <- glmer(value ~ 1 + species + (1|origSite/block), dat, family = "poisson")
fit3 <- glmer(value ~ newTT + 1 + (1|origSite/block), dat, family = "poisson")
fit4 <- glmer(value ~ 1 + (1|origSite/block), dat, family = "poisson")
summary(fit1)
ModelCheck(fit1)
overdisp_fun(fit1)

modsel(list(fit1, fit2, fit3, fit4), 1000)


fit1 <- lmer(value ~ newTT + species + (1|origSite/block), dat)
fit2 <- lmer(value ~ newTT + (1|origSite/block), dat)
fit3 <- lmer(value ~ species + (1|origSite/block), dat)
fit4 <- lmer(value ~ 1 + (1|origSite/block), dat)
modsel(list(fit1, fit2, fit3, fit4), 1000)
summary(fit1)



# backtransform the data to get doy
newdat <- with(dat, expand.grid(
  newTT = c("Control", "OTC", "Warm"),
  species = c("Cya.inc", "Kob.pyg", "Car.A", "Car.bla", "Poa.sp","Pol.viv", "Pot.leu"),
  value = 0
))


newdat$value <- predict(fit1, newdat, re.form = NA) # , type="response"
mm <- model.matrix(terms(fit1), newdat)

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
