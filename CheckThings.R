### Check things

# Community Figures
pheno.long %>% 
  select(turfID, species, newtreat, origSite, block, value, pheno.var, pheno.stage) %>% 
  #filter(newtreat %in% c("Control", "OTC")) %>% 
  group_by(origSite, species, newtreat, pheno.var, pheno.stage) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  filter(pheno.var =="peak", origSite != "M") %>% 
  ggplot(aes(x = newtreat, y = mean)) +
  geom_boxplot() +
  facet_grid(pheno.stage ~origSite)


  

### TEST DIFFERENCE BETWEEN CONTROL AND LOCAL
# Simple regression
SimpleModel <- function(dd){
  mod01 <- lm(value ~ treatment, dd)
  ATables <- anova(mod01)
  return(ATables)
}

AnovaTables <- pheno.long %>%
  filter(treatment %in% c("Control", "Local")) %>% 
  group_by(pheno.var, pheno.stage) %>% 
  do(SimpleModel(.))

colnames(AnovaTables) <- c("pheno.var", "pheno.stage", "Df", "SumSq", "MeanSq", "Fvalue", "Pvalue")
AnovaTables %>% filter(Pvalue < 0.05 & Pvalue > 0)
# With simple Anova, only end r and s are different


### Mixed effect model accounting for block
## Check histgramms to see if log transformation is needed; only for duration
pheno.long %>%
  filter(treatment %in% c("Control", "Local"), pheno.stage == "r") %>% 
  group_by(pheno.var, pheno.stage) %>% 
  ggplot(aes(x = log(value))) +
  geom_histogram() +
  facet_wrap(~ pheno.var)

library("nlme")
MixedModel <- function(dd){
  if(unique(dd$pheno.var) != "duration"){
    mod01 <- lme(value ~ treatment, random = ~1| block, data = dd)
  }
  else if(unique(dd$pheno.var) == "duration"){
    mod01 <- lme(log(value) ~ treatment, random = ~1| block, data = dd)
  }
  SumTable <- anova.lme(mod01)
  return(SumTable)
}

SumTables <- pheno.long %>%
  filter(treatment %in% c("Control", "Local")) %>% 
  group_by(pheno.var, pheno.stage) %>% 
  do(MixedModel(.))

colnames(SumTables) <- c("pheno.var", "pheno.stage", "numDf", "denDF", "Fvalue", "Pvalue")
SumTables %>% filter(Pvalue < 0.05 & Pvalue > 0)
# difference for first fs; end r and end s

