# PhenoFunctions 
#### READ IN PHENOLOGY DATA 2016 ####
ReadInBodyPhenology <- function(datasheet, site){
  # import body of data
  dat <- read.csv(datasheet, header=FALSE, sep=";", skip=3, stringsAsFactors=FALSE)
  dat <- dat[dat$V2!="",] # get rid of empty lines, where no species
  dat <- dat[,-3] # get rid of chinese names
  dat$V2<-gsub(" ", "", dat$V2,fixed = TRUE) # get rid of space
  
  # loop to get turfID in all cells
  for (i in 2:nrow(dat)){
  if(nchar(dat$V1[i])==0){
      dat$V1[i] <- dat$V1[i-1]
    }
  }
  # import head of data set
  dat.h <- read.csv(datasheet, sep=";", header=FALSE, nrow=3, stringsAsFactors=FALSE)
  
  # merge data into long data table
  long.table <- lapply(seq(3,ncol(dat)-15,16),function(i){
    x <- dat[ ,c(1:2,i:(i+15))]
    names(x) <- c("turfID", "species", paste(rep(c("b", "f", "s", "r"), 4  ), rep(1:4, each=4), sep="."))
    x$date <- strsplit(dat.h[1,i+1], "_")[[1]][1]
    x$doy <- yday(ymd(x$date))
    x  
  })
  dat.long <- do.call(rbind,c(long.table, stingsAsFactors=FALSE))
  
  # Extract site
  dat.long$origSite <- substr(dat.long$turfID, 1,1)
  dat.long$destSite <- site
  dat.long$block <- substr(dat.long$turfID, 2,2)
  dat.long$treatment <- substr(dat.long$turfID, 4,nchar(dat.long$turfID))
  # if treatmen 1 or 2, remove species with a *sp* (not from original data set)
  dat.long  <-  dat.long[
    (dat.long$treatment %in% c("1", "2") & grepl("\\*.*\\*", as.character(dat.long$species), perl = TRUE)) | #if treatment 1 or 2 only *sp*
      !(dat.long$treatment %in% c("1", "2")), ] # if other treatment
  dat.long$species <- gsub("*", "", dat.long$species,fixed = TRUE) # get rid of *
  
  # convert to factor and numeric
  dat.long <- cbind(dat.long[,c(1:2,19:24)],sapply(dat.long[,c(3:18)],as.numeric))
  #sapply(dat.long[,c(4:7,9:12,14:17,19:22)],function(x)print(grep("\\D", x = x, value = TRUE))) # Check error messages
  dat.long <- dat.long[-1,]
  dat.long
  return(dat.long)
}


# Calculate the sum of buds, flowers and seeds per turf and species
CalcSums <- function(dat){
  dat$nr.b <- apply(dat[,c(seq(9,21,4))],1,sum, na.rm=TRUE)
  dat$nr.b[dat$nr.b == 0] <- NA
  dat$nr.f <- apply(dat[,c(seq(10,22,4))],1,sum, na.rm=TRUE)
  dat$nr.f[dat$nr.f == 0] <- NA
  dat$nr.s <- apply(dat[,c(seq(11,23,4))],1,sum, na.rm=TRUE)
  dat$nr.s[dat$nr.s == 0] <- NA
  dat$nr.r <- apply(dat[,c(seq(12,24,4))],1,sum, na.rm=TRUE)
  dat$nr.r[dat$nr.r == 0] <- NA
  return(dat)
}


#### FUNCTIONS FOR FIGURES ####

### GET MEAN AND SE BY SPECIES ###
SpeciesMeanSE <- function(dat, phenovar){
  # Calculate mean and se by species, pheno.stage, origSite, newTT
  MeanSE <- dat %>% 
    filter(pheno.var == phenovar) %>% 
    group_by(newTT, origSite, pheno.stage, species) %>% 
    summarise(N = sum(!is.na(value)), mean = mean(value, na.rm = TRUE), se = sd(value, na.rm = TRUE)/sqrt(N))
  
  # Calculate mean for difference between Control and Treatment
  #SPOnlyInOneTreatment
  SpeciesDifference <- MeanSE %>% 
    ungroup() %>% 
    select(-N) %>%  # remove site, because it causes problems
    unite(united, mean, se, sep = "_") %>% # unite mean and se
    spread(key = newTT, value = united) %>% # spread Treatments
    separate(col = Control, into = c("Control_mean", "Control_se"), sep = "_", convert = TRUE) %>% 
    separate(col = OTC, into = c("OTC_mean", "OTC_se"), sep = "_", convert = TRUE) %>% 
    separate(col = Warm, into = c("Transplant_mean", "Transplant_se"), sep = "_", convert = TRUE) %>% 
    mutate(OTC_mean = OTC_mean - Control_mean, Transplant_mean = Transplant_mean - Control_mean) %>% 
    mutate(OTC_se = sqrt(Control_se^2 + OTC_se^2), Transplant_se = sqrt(Control_se^2 + Transplant_se^2)) %>% 
    select(-Control_mean, -Control_se) %>% 
    unite(OTC, OTC_mean, OTC_se, sep = "_") %>% 
    unite(Transplant, Transplant_mean, Transplant_se, sep = "_") %>% 
    gather(key = Treatment, value = united, -origSite, -pheno.stage, -species) %>%
    separate(col = united, into = c("mean", "se"), sep = "_", convert = TRUE) %>% 
    filter(!is.na(mean))

  return(SpeciesDifference)
}    
    

### COMMUNITY DATA ###
PlotCommunityData <- function(dat, phenovar){    
    CommunityDifference <- dat %>% 
      mutate(newname = paste(origSite, Treatment, sep = "_")) %>% # paste Treatment and site, they are unique and can be renamed
      mutate(newname = plyr::mapvalues(newname, c("H_OTC", "A_OTC", "H_Transplant", "A_Transplant"), c("High alpine OTC", "Alpine OTC", "High alpine Transplant", "Alpine Transplant"))) %>% 
      mutate(newname = factor(newname, levels = c("High alpine OTC", "Alpine OTC", "High alpine Transplant", "Alpine Transplant"))) %>% 
      group_by(pheno.stage, newname) %>% 
      summarise(Difference = mean(mean, na.rm = TRUE), SE = mean(se, na.rm = TRUE)) %>% 
      mutate(Treatment = sub(".* ", "", newname))
  
  ggplot(CommunityDifference, aes(x = newname, y = Difference, color = Treatment, shape = Treatment, ymax = Difference + SE, ymin = Difference - SE)) +
    geom_hline(yintercept=0, color = "gray") +
    geom_point(size = 1.8) +
    labs(x = "", y = "Treatment - control in days") +
    scale_colour_manual(name = "", values = c("red", "purple")) +
    scale_shape_manual(name = "", values = c(16, 17)) +
    facet_grid(~ pheno.stage) +
    geom_errorbar(width=0.2) +
    scale_x_discrete(labels = c("High alpine OTC" = "High alpine", "Alpine OTC" = "Alpine", "High alpine Transplant" = "High alpine", "Alpine Transplant" = "Alpine", "High alpine OTC" = "High alpine", "Alpine OTC" = "Alpine", "High alpine Transplant" = "High alpine", "Alpine Transplant" = "Alpine", "High alpine OTC" = "High alpine", "Alpine OTC" = "Alpine", "High alpine Transplant" = "High alpine", "Alpine Transplant" = "Alpine", "High alpine OTC" = "High alpine", "Alpine OTC" = "Alpine", "High alpine Transplant" = "High alpine", "Alpine Transplant" = "Alpine")) +
    ggtitle(phenovar) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


### SPECIES DATA ###
PlotSpeciesData <- function(dat, phenovar){
  dat2 <- expand.grid(Treatment=unique(dat$Treatment), species=unique(dat$species), origSite = unique(dat$origSite), pheno.stage = unique(dat$pheno.stage)) %>% data.frame %>% left_join(dat)
  
  # Draw plot
  dat2 %>% 
    mutate(origSite = plyr::mapvalues(origSite, c("H", "A"), c("High Alpine", "Alpine"))) %>% 
    ggplot(aes(y = mean, x = species, fill = Treatment, ymin = mean - se, ymax = mean + se)) +
    geom_bar(position="dodge", stat="identity") +
    geom_errorbar(position = position_dodge(0.9), width = 0.2) +
    scale_fill_manual(name = "", values = c("red", "purple")) +
    labs(y = "Treatment - Control in days", x = "", title = phenovar) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    facet_grid(pheno.stage ~ origSite)
}


#### FUNCTIONS FOR ANALYSIS ####
#### Function to produce model-checking plots for the fixed effects of an lmer model
ModelCheck <- function(mod){		
  par(mfrow = c(1,2))
  # Residual plot: checking homogeneity of the variance and linerarity
  plot(fitted(mod), resid(mod)) #should have no pattern
  abline(h=0)
  # QQnorm plot: normal distribution of the residuals
  qqnorm(resid(mod), ylab="Residuals")		  #should be approximately straight line
  qqline(resid(mod))
  }


### Test overdispersion
# compare the residual deviance to the residual degrees of freedom
# these are assumed to be the same.

overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}


# Function to calculate QAICc
# NB, phi is the scaling parameter from the quasi-family model. If using e.g. a poisson family, phi=1 and QAICc returns AICc, or AIC if QAICc=FALSE.
QAICc <- function(mod, scale, QAICc = TRUE) {
  ll <- as.numeric(logLik(mod))
  df <- attr(logLik(mod), "df")
  n <- length(resid(mod))
  if (QAICc)
    qaic = as.numeric(-2 * ll/scale + 2 * df + 2 * df * (df + 1)/(n - df - 1))
  else qaic = as.numeric(-2 * ll/scale + 2 * df)
  qaic
}


# Model selection
modsel <- function(mods,x){	
  phi=1
  dd <- data.frame(Model=1:length(mods), K=1, QAIC=1)
  for(j in 1:length(mods)){
    dd$K[j] = attr(logLik(mods[[j]]),"df")
    dd$QAIC[j] = QAICc(mods[[j]],phi)
  }
  dd$delta.i <- dd$QAIC - min(dd$QAIC)
  dd <- subset(dd,dd$delta.i<x)
  dd$re.lik <- round(exp(-0.5*dd$delta.i),3)
  sum.aic <- sum(exp(-0.5*dd$delta.i))
  wi <- numeric(0)
  for (i in 1:length(dd$Model)){wi[i] <- round(exp(-0.5*dd$delta.i[i])/sum.aic,3)}; dd$wi<-wi
  print(dds <- dd[order(dd$QAIC), ])
  assign("mstable",dd,envir=.GlobalEnv)
}
