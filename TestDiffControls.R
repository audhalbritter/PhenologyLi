controls <- pheno.long %>%
  left_join(meta.pheno, by = "turfID") %>% 
  filter(treatment %in% c("C", "O"), pheno.var == "first")
# only end date is different


controls %>% 
  select(-turfID) %>% 
  spread(key = treatment, value = value) %>% 
  na.omit() %>%
  ggplot(aes(x = C, y = O)) +
  geom_point()


# Test if difference in phenological date between C and O
# accounting for overdispersion
fit0 <- glmer(value ~ treatment + (1|species) + (1|origSite/block), controls, family = "poisson")
fit1 <- glmer(value ~ 1 + (1|species) + (1|origSite/block), controls, family = "poisson")
controls$od <- as.factor(rnorm(nrow(controls)))
fit0 <- glmer(value ~ treatment + (1|species) + (1|origSite/block) + (1|od), controls, family = "poisson")
fit1 <- glmer(value ~ 1 + (1|species) + (1|origSite/block) + (1|od), controls, family = "poisson")

ModelCheck(fit0)
# overdispersion if p < 0.05 or overdispersion factor > 1.1
overdisp_fun(fit0)

modsel(list(fit0, fit1), 1000)
# no difference between the treatments!