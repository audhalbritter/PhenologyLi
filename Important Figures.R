# Figures Presentation

#### LIBRARIES ####
library("lme4")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("readxl")


# Community Figures
CommunityFig <- pheno.long %>% 
  select(turfID, species, newtreat, origSite, block, value, pheno.var, pheno.stage) %>% 
  #filter(newtreat %in% c("Control", "OTC")) %>% 
  group_by(origSite, species, newtreat, pheno.var, pheno.stage) %>% 
  summarize(mean = mean(value), sd = sd(value)) %>% 
  filter(pheno.var =="peak", origSite != "M") %>% 
  ggplot(aes(x = newtreat, y = mean)) +
  geom_boxplot() +
  facet_grid(pheno.stage ~origSite)


# Compare Treatments
Treat <- pheno.long %>% 
  #filter(pheno.var == "first", pheno.stage == "f", origSite == "H", species == "Gen.cra") %>% 
  select(turfID, species, newtreat, origSite, block, value, pheno.var, pheno.stage) %>% 
  group_by(origSite, species, pheno.var, pheno.stage, newtreat) %>% 
  summarize(mean = mean(value)) %>%
  spread(key = newtreat, value = mean) %>% 
  gather(key = newTT, value = newTTvalue,  -species, -origSite, -pheno.stage, -pheno.var, -Control) %>% # devide control and treamtents
  mutate(Diff = newTTvalue - Control) %>% 
  mutate(newTT = factor(newTT, levels=c("OTC", "Warm", "Cold"))) %>% 
  filter(!is.na(Diff)) %>% 
  filter(pheno.var == "peak", pheno.stage == "f")

Treat2 <- expand.grid(newTT=unique(Treat$newTT), species=unique(Treat$species), origSite = unique(Treat$origSite), pheno.var = unique(Treat$pheno.var)) %>% data.frame %>% left_join(Treat)

CompareTreat <- ggplot(data = Treat2, aes(y = Diff, x = species, fill = newTT)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  ylab("Treatment - Control") +
  xlab("") +
  ggtitle("Peak flowering") +
  facet_wrap(~ origSite)



