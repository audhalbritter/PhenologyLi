# PhenoFunctions 
#### READ IN PHENOLOGY DATA 2015 ####
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


#### FUNCTIONS FOR ANALYSIS ####
#### Function to produce model-checking plots for the fixed effects of an lmer model
ModelCheck <- function(mod){		
  par(mfrow = c(2,2))
  # Residual plot: checking homogeneity of the variance
  plot(fitted(mod),resid(mod))	            #should have no pattern
  abline(h=0)
  # test for relashionship between residuals and fitted values
  print(anova(lm(fitted(mod)~resid(mod))))	#should be non-significant
  # QQnorm plot: normal distribution of the residuals
  qqnorm(resid(mod), ylab="Residuals")		  #should be approximately straight line
  qqline(resid(mod))
  # Check the distribution of the residuals
  plot(density(resid(mod)))					        #should be roughly normally distributed
  rug(resid(mod))}


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