# Import climate data

load("clean_weather.Rdata", verbose = TRUE)
load("otcc_clean.Rdata", verbose = TRUE)
load("climate.Rdata", verbose = TRUE)
load("climate_month.Rdata", verbose = TRUE)

> 

head(distinct_weather)
head(otcc)

# plot the data
distinct_weather %>% 
  ggplot(aes(x = dateTime, y = windDirection, color = site)) +
  geom_line() +
  facet_wrap(~site)

otcc %>% 
  ggplot(aes(x = dateTime, y = Tsoil0, color = site)) +
  geom_line() +
  facet_wrap(~site)


# Check min and max values in each season: does not seem to have huge outliers
distinct_weather2 <- distinct_weather %>%
  mutate(nummonth = month(as.POSIXlt(dateTime, format="%Y/%m/%d %H/%m/%s"))) %>% 
  mutate(season = ifelse(nummonth %in% c(12,1,2), "Winter",
                         ifelse(nummonth %in% c(3,4,5), "Spring", 
                                ifelse(nummonth %in% c(6,7,8), "Summer", "Autumn")))) %>% 
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))) %>% 
  group_by(site, season, year(dateTime)) %>% summarise(min = min(Tsoil20, na.rm = TRUE), max = max(Tsoil20, na.rm = TRUE)) %>% 
  print(n=60)

otcc2 <- otcc %>%
  mutate(nummonth = month(as.POSIXlt(dateTime, format="%Y/%m/%d %H/%m/%s"))) %>% 
  mutate(season = ifelse(nummonth %in% c(12,1,2), "Winter",
                         ifelse(nummonth %in% c(3,4,5), "Spring", 
                                ifelse(nummonth %in% c(6,7,8), "Summer", "Autumn")))) %>% 
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))) %>% 
  group_by(site, season, year(dateTime)) %>% summarise(min = min(Tsoil0, na.rm = TRUE), max = max(Tsoil0, na.rm = TRUE)) %>% 
  print(n=60)


# fix Tsoil0 for OTC
otc <- otcc2 %>% 
  mutate(Tsoil0 = ifelse(season %in% c("Summer", "Autumn") & Tsoil0 < -3, NA, Tsoil0)) %>% # summer and autumn > -3
  mutate(Tsoil0 = ifelse(season %in% c("Summer", "Autumn") & Tsoil0 < -3, NA, Tsoil20)) %>% # summer and autumn > -3
  mutate(Tsoil0 = ifelse(season == "Spring" & Tsoil0 < -7.8, NA, Tsoil0)) %>% # spring > -7.9
  mutate(Tsoil0 = ifelse(season == "Winter" & Tsoil0 < -8.7, NA, Tsoil0)) %>% # winter > -8.8
  mutate(Tsoil0 = ifelse(site == "L" & month(dateTime) == 9 & year(dateTime) == 2013 & Tsoil0 < 2.5, NA, Tsoil0)) %>% # remove outlier in L site, sept 2013 < 2.5°C
  select(site, dateTime, Tsoil0, Tsoil5, Tsoil20, Tair30) %>% 
  rename(Tair = Tair30)
  
temperature <- distinct_weather2 %>% 
  mutate(Tsoil0 = ifelse(season == "Spring" & Tsoil0 < -15, NA, Tsoil0)) %>% # spring > -15
  mutate(Tsoil0 = ifelse(site == "H" & season == "Winter" & Tsoil20 < 2, NA, Tsoil20)) %>% # spring > -6.68
  select(site, dateTime, Tsoil0, Tsoil5, Tsoil20, Tair) %>% 
  left_join(otc, by = c("site", "dateTime"), suffix = c(".outside", ".otc")) %>% 
  gather(key = logger, value = value, -site, -dateTime) %>%  # gather all loggers in one column
  separate(logger, c("logger", "location"), sep = "\\.")
  



#####
library("data.table")
load(file = "~/Dropbox/Bergen/Transplant - China/climate/climate.RData")
climate <- setDT(climate)
climate <- setDT(climate)

### Calculate Daily Temperature
dailyTemperature <- climate %>% 
  select(site, dateTime, Tair, Tsoil0, Tsoil5, Tsoil20) %>% 
  gather(key = variable, value = value, - site, -dateTime) %>% 
  mutate(date = dmy(format(dateTime, "%d.%b.%Y"))) %>%
  group_by(site, variable, date) %>%
  summarise(n = n(), mean = mean(value), min = min(value), max = max(value))

  filter(n > 6) %>%
  select(-n)

# Get monthly mean for 2013
dailyTemperature %>% 
  filter(logger == "Tsoil0") %>% 
  mutate(month = month(dateDaily), year(dateDaily) == 2013) %>%
  group_by(site, month, location) %>% 
  summarise(n = n(), value = mean(value, na.rm = TRUE)) %>% 
  filter(!n < 25) %>% 
  #spread(key = location, value = value) %>% 
  #mutate(diff = otc - outside) %>% 
  ggplot(aes(x = month, y = value, color = location)) + geom_line() +
  labs(x = "", y = "Mean monthly temperature in °C") +
  facet_wrap(~ site)

dailyTemperature %>% 
  filter(logger == "Tsoil0") %>% 
  ggplot(aes(x = dateDaily, y = value, color = location)) +
  geom_line() +
  facet_wrap(~site)

 

#### CHECK REGRESSION BETWEEN TAIR IN A AND H SITE
### Regression for 10 min data
HAsite <- distinct_weather %>% 
  select(site, dateTime, Tair) %>% 
  filter(site %in% c("H", "A")) %>% 
  spread(key = site, value = Tair)

fit10 <- lm(H ~ A, data = HAsite)
summary(fit10)
plot(HAsite$A, HAsite$H)
abline(fit10, col = "red")



### Calculate Daily Temperature
dailyTemperature <- distinct_weather %>% 
  select(site, dateTime, Tair) %>% 
  filter(!is.na(dateTime)) %>% # remove NA in dateTime (2 rows)
  #mutate(dateHour = paste(format(dateTime, "%Y-%m-%d"), hour(dateTime), sep = "_")) %>% # for hourly data
  mutate(dateDaily = ymd(format(dateTime, "%Y-%m-%d"))) %>%
  group_by(site, dateDaily) %>% 
  summarise(n = n(), Tair = mean(Tair)) %>% 
  filter(!n < 144) # remove 30 days without all observations. SHOULD MAYBE LOWER TRESHOLD TO < 130 or 100?


### Regression for daily data -> better to use daily than 10min data
# only A and H sites
HAdaily <- dailyTemperature %>% 
  filter(site %in% c("H", "A")) %>% 
  spread(key = site, value = Tair) %>% 
  na.omit()

plot(HAdaily$A, HAdaily$H)
fitdaily <- lm(H ~ A, data = HAdaily)
summary(fitdaily)
abline(fitdaily)
# using daily values is better

# Check data for outliers, where colder at A site
distinct_weather %>%
  #select(site, dateTime, Tair, RH, PAR, solarRadiation) %>% 
  #filter(site %in% c("A", "H")) %>% 
  filter(dateTime > "2013-02-28 01:00:00" & dateTime < "2014-09-15 02:40:00") %>% 
  ggplot(aes(x = dateTime, y = Tsoil, color = site)) +
  geom_line()




# CREATE MODEL TO PREDICT MISSING VALUES CONTAINING ALL SITES
SiteTemp <- dailyTemperature %>% 
  select(site, dateDaily, Tair) %>% 
  spread(key = site, value = Tair)

fitA <- lm(H ~ A, data = SiteTemp)
fitAM <- lm(H ~ A + M, data = SiteTemp)
fitall <- lm(H ~ A + M + L, data = SiteTemp)
fitInt2 <- lm(H ~ A + M + L + A:M + M:L, data = SiteTemp)
fitInt3 <- lm(H ~ A * M * L, data = SiteTemp)

# compare models
AIC(fitA, fitAM, fitall, fitInt2, fitInt3)
summary(fitInt2)
ggplot(dailyTemperature, aes(x = dateDaily, y = Tair, color = site)) + geom_line() + facet_wrap(~ site)



#### FILL MISSING VALUES IN H-SITE USING REGRESSION MODEL
MissingHValues <- SiteTemp %>% 
  filter(is.na(H), year(dateDaily) > 2015) #%>% print(n = 450)
new.df <- data.frame(A = MissingHValues$A,
                     M = MissingHValues$M,
                     L = MissingHValues$L)
# use model to predict values
newH <- predict(fitall, new.df)

# replace NA's with predicted values
dailyTempPred <- SiteTemp %>% 
  mutate(mark = ifelse(year(dateDaily) > 2015 & is.na(H), "newvalue", "oldvalue")) %>% 
  mutate(H = ifelse(year(dateDaily) > 2015 & is.na(H), newH, H)) %>% 
  gather(key = site, value = Tair, -dateDaily, -mark) %>% 
  mutate(mark = ifelse(site == "H" & mark == "newvalue", "newvalue", "oldvalue")) %>% 
  mutate(site = factor(site, levels = c("H", "A", "M", "L")))

dailyTempPred %>% filter(year(dateDaily) > 2015) %>% 
  ggplot(aes(x = dateDaily, y = Tair, color = site)) + geom_line() + facet_wrap(~ site)


### CALCULATE CUMSUM
# Subset data for 2016, replace values below 5°C with 0, get cumsum
CumSum2016 <- dailyTempPred %>% 
  filter(year(ymd(dateDaily)) >= 2016) %>% # only 2016 data
  mutate(Tair5 = ifelse(Tair < 5, 0, Tair)) %>%  # replace temperature below 5°C with 0
  mutate(Tair5 = ifelse(is.na(Tair5), 0, Tair5)) %>%  # replace temperature below 5°C with 0
  mutate(doy = yday(ymd(dateDaily))) %>% # calculate day of the year
  group_by(site) %>% 
  mutate(cumTemp = cumsum(Tair5)) %>% 
  mutate(climateID = paste(site, doy, sep = "_"))

# plot cumulative air temperature
CumSum2016 %>% 
  ggplot(aes(x = ymd(dateDaily), y = cumTemp, color = site)) +
  geom_line()



head(climate_month)
climate_month %>% 
  mutate(mnr = month(month)) %>% 
  filter(variable == "Tair", mnr %in% c("5", "6", "7", "8", "9")) %>% 
  group_by(site, logger) %>% 
  summarise(mean = round(mean(value, na.rm = TRUE), 2), n = n())
