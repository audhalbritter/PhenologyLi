### Check things


# How many species are the same/different per turfID
SameAndDiffSP <- pheno.long %>% 
  select(turfID, species, treatment, origSite, block, value, pheno.var, pheno.stage) %>% 
  filter(treatment %in% c("Control", "OTC")) %>% 
  group_by(origSite, species, treatment) %>% 
  summarize(mean = mean(value)) %>% 
  spread(key = treatment, value = mean) %>% 
  mutate(Diff = ifelse(is.na(Control), "different", ifelse(is.na(OTC), "different", "same"))) %>% 
  group_by(origSite, block, Diff) %>% 
  summarise(n = n()) %>% 
  mutate(SiteBlock = paste(origSite, block)) %>% 
  ggplot(aes(x = Diff, y = n)) +
  geom_point() +
  facet_wrap(~SiteBlock)
  
  

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

