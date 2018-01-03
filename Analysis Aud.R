########################################
### ANALYSE PHENOLOGY DATA ###
########################################
library("aods3")

head(pheno.long)

### Select data
Dat <- pheno.long %>% 
  select(turfID, species, newtreat, origSite, block, value, pheno.var, pheno.stage, pheno.unit)

#### EXPLORE DATA
### Histograms
Dat %>% 
  filter(pheno.unit == "doy") %>% 
  ggplot(aes( x = value)) +
  geom_histogram() +
  facet_grid(pheno.stage ~ pheno.var)
# first, peak and end look kind of normally distributed, but earlier in the season data is more skewed
# duration variables are all skewed
# should use poisson distribution anyway for all variables


# just for one variable
dat <- Dat %>%
  filter(newtreat %in% c("Control", "OTC","Warm")) %>% 
  filter(pheno.var == "peak", pheno.stage =="BudFlower")
hist(dat$value)
# number for each observation to deal with overdispersion
dat$observation <- 1:nrow(dat)

# Make figure
ggplot(data = dat, aes(x = newtreat, y = value)) +
  geom_boxplot() +
  facet_wrap(~ origSite)

+ (1|observation)
# Mixed Effects Model
fit.glmm1 <- glmer(value ~ newtreat * origSite + (1|species) + (1|block), data = dat, family = poisson)
fit.glmm2 <- glmer(value ~ newtreat + origSite + (1|species) + (1|block), data = dat, family = poisson)
fit.glmm3 <- glmer(value ~ newtreat + (1|species) + (1|block), data = dat, family = poisson)
fit.glmm4 <- glmer(value ~ origSite + (1|species) + (1|block), data = dat, family = poisson)
fit.glmm5 <- glmer(value ~ 1 + (1|species) + (1|block), data = dat, family = poisson)
modsel(list(fit.glmm1, fit.glmm2, fit.glmm3, fit.glmm4, fit.glmm5), 1000)

summary(fit.glmm1)
ModelCheck(fit.glmm1)

# test for overdispersion
gof(fit.glmm1)
overdisp_fun(fit.glmm1)

ModelCheck(fit.glmm)



# backtransform the data to get doy
newdat <- with(dat, expand.grid(
  newtreat = c("Control", "OTC", "Warm"),
  origSite = c("H", "A"),
  value = 0
))

mm <- model.matrix(terms(fit.glmm), newdat)
newdat$value <- predict(fit.glmm, newdat, re.form = NA, type="response")

pvar1 <- diag(mm %*% tcrossprod(vcov(fit.glmm),mm))
tvar1 <- pvar1+VarCorr(fit.glmm)$block[1]  ## must be adapted for more complex models
cmult <- 1.96 ## could use 1.96
newdat <- data.frame(
  newdat
  , plo = newdat$value-cmult*sqrt(pvar1)
  , phi = newdat$value+cmult*sqrt(pvar1)
  , tlo = newdat$value-cmult*sqrt(tvar1)
  , thi = newdat$value+cmult*sqrt(tvar1)
)



# Test if treatment is important using model selection
fit.glmm1 <- glmer(value ~ newtreat * origSite + (1|block) + (1|species) + (1|observation), data = dat, family = "poisson")
fit.glmm2 <- glmer(value ~ newtreat + origSite + (1|block) + (1|species) + (1|observation), data = dat, family = "poisson")
fit.glmm3 <- glmer(value ~ newtreat + (1|block) + (1|species) + (1|observation), data = dat, family = "poisson")
fit.glmm4 <- glmer(value ~ origSite + (1|block) + (1|species) + (1|observation), data = dat, family = "poisson")
fit.glmm5 <- glmer(value ~ 1 + (1|block) + (1|species) + (1|observation), data = dat, family = "poisson")

modsel(list(fit.glmm1, fit.glmm2, fit.glmm3, fit.glmm4, fit.glmm5), 1000)




ddd <- pheno.long %>% 
  select(species, pheno.var, pheno.stage, origSite, pheno.unit, block, treatment, newtreat, value) %>% 
  filter(!newtreat %in% c("Cold"), pheno.var == "peak", pheno.unit == "doy", !pheno.stage %in% c("Ripe", "SeedRipe")) %>% 
  group_by(species, pheno.stage, origSite, block, treatment, newtreat, value) %>% 
  summarise(n = n(), mean = mean(value, na.rm = TRUE)) %>% 
  select(-n, -value, -treatment) %>% 
  spread(key = newtreat, value = mean) %>% # spread Treatments



  
