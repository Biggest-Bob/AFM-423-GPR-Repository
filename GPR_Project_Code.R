library(tidyverse)                      # Activate the data science package
library(lubridate)                      # Activate the date management package
library(readxl)
library(quantmod)

#https://www.cboe.com/tradable_products/vix/
#https://fred.stlouisfed.org/series/SP500#0

#####
#Daily

data_spday <- read_csv('dayMetaData.csv')
data_spday <- data_spday %>%
  mutate(Date=as.Date(Date, format = "%m/%d/%Y"))

### Base Portfolio (Invest at start and hold)
data_spday <- data_spday %>%
  mutate(BasePortfolio=0)

for (i in 1:5000) {
  data_spday <- data_spday %>%
    mutate(BasePortfolio= if_else(Date == "2003-01-21", 1000,
                                  lag(BasePortfolio)+(lag(BasePortfolio)*DailyReturn/100)))
}

###Portfolio Decision Based on Standard Deviation (Price Momentum)
start_timeD <- Sys.time()
#Standard Deviation and Buy/Hold/Sell (+1/0/-1) Decision Calculation
spday_bar <- median(data_spday$DailyReturn)
data_spday <- data_spday %>%
  mutate(neg_change = -sd(DailyReturn),
         pos_change = sd(DailyReturn))
data_spday <- data_spday %>%
  mutate(PortDecision = if_else(DailyReturn < neg_change, -1, 
                                if_else(DailyReturn > pos_change, 1,0)),
         PortDecision = as.factor(PortDecision))

#Determining Ownership for Portfolio Calculation Purposes
data_spday <- data_spday %>%
  mutate(Owned=0)

for (i in 1:5000) {
data_spday <- data_spday %>%
  mutate(Owned = ifelse(Date == "2003-01-21", 0,
                        ifelse(lag(Owned) == 0 & (lag(PortDecision) == 0 | lag(PortDecision == -1)), 0,
                               ifelse(lag(Owned) == 0 & lag(PortDecision) == 1, 1,
                                      ifelse(lag(Owned) == 1 & lag(PortDecision) == -1, 0, 1)))))
}
                                                 
#Applying Returns if Index is 'owned' otherwise no change
data_spday <- data_spday %>%
  mutate(MomentumPortfolio=0)

for (i in 1:5000) {
  data_spday <- data_spday %>%
    mutate(MomentumPortfolio= if_else(Date == "2003-01-21", 1000,
                                           if_else(Owned==1,lag(MomentumPortfolio)+(lag(MomentumPortfolio)*DailyReturn/100),
                                                   lag(MomentumPortfolio))))
}
end_timeD <- Sys.time()
net_timeD <- end_timeD - start_timeD
net_timeD

### Portfolio decision based on volatility neural network 
data_spday <- data_spday %>%
  mutate(Date=as.Date(Date, format = "%m/%d/%Y"))


getSymbols.FRED("VIXCLS",
                env = ".GlobalEnv",
                return.class = "xts")
vix <- fortify(VIXCLS)
colnames(vix) <- c("Date", "vix")

data_vixday <- merge(data_spday, vix, by = "Date")

delta <- 0.5
vix_bar <- median(data_vixday$vix)
data_vixday <- data_vixday %>%
  dplyr::select(Date, DailyReturn) %>%
  mutate(r_minus = (-0.02) * exp(-delta*(data_vixday$vix-vix_bar)),
         r_plus = 0.02 * exp(delta*(data_vixday$vix-vix_bar)))
data_vixday <- data_vixday %>%
  mutate(PortDecisionVix = if_else(DailyReturn < r_minus, -1, if_else(DailyReturn > r_plus, 1,0)),
         PortDecisionVix = as.factor(PortDecisionVix))

# Determining Ownership for Portfolio Calculation Purposes
data_vixday <- data_vixday %>%
  mutate(Owned=0)

for (i in 1:5000) {
  data_vixday <- data_vixday %>%
    mutate(Owned = ifelse(Date == "2003-01-21", 0,
                          ifelse(lag(Owned) == 0 & (lag(PortDecisionVix) == 0 | lag(PortDecisionVix == -1)), 0,
                                 ifelse(lag(Owned) == 0 & lag(PortDecisionVix) == 1, 1,
                                        ifelse(lag(Owned) == 1 & lag(PortDecisionVix) == -1, 0, 1)))))
}

#Applying Returns if Index is 'owned' otherwise no change
data_vixday <- data_vixday %>%
  mutate(VixPortfolio=0)

for (i in 1:5000) {
  data_vixday <- data_vixday %>%
    mutate(VixPortfolio= if_else(Date == "2003-01-21", 1000,
                                           if_else(Owned==1,lag(VixPortfolio)+(lag(VixPortfolio)*DailyReturn/100),
                                                   lag(VixPortfolio))))
}

DayPortfolios <- merge(data_spday, data_vixday, by = "Date")

DayPortfolios <- DayPortfolios %>%
  select(Date, MomentumPortfolio, BasePortfolio, VixPortfolio)

ggplot(DayPortfolios, aes(x = Date)) +
  geom_line(aes(y = MomentumPortfolio, color = "Std")) +
  geom_line(aes(y = BasePortfolio, color = "Base")) +
  geom_line(aes(y = VixPortfolio, color = "Volatility")) +
  labs(x = "Date", y = "Value", color = "Legend")+
  geom_hline(yintercept=1000, linetype="dotted")+
  geom_vline(xintercept = as.numeric(as.Date("2008-09-15")), color = "Orange", linetype = "dashed")+
  geom_vline(xintercept = as.numeric(as.Date("2020-02-20")), color = "Purple", linetype = "dashed")


#####
# Weekly
#Data Processing
weekmetadata <- read_csv('Weekmetadata.csv')
weekmetadata <- weekmetadata[-c(1,2),]
weekmetadata <- weekmetadata %>%
  mutate(Date=as.Date(Date, format = "%m/%d/%Y"))%>%
  mutate(Date=Date-2) #The data downloaded ends each week on the sunday, to make the merge with VIX later easier, we subtract two to make it end on Fridays

### Base Portfolio
data_spweek <- weekmetadata %>%
  mutate(BasePortfolio=0)

for (i in 1:5000) {
  data_spweek <- data_spweek %>%
    mutate(BasePortfolio= if_else(Date == "2003-01-24", 1000,
                                  lag(BasePortfolio)+(lag(BasePortfolio)*WeeklyReturn/100)))
}


### Portfolio Decision Based on Standard Deviation (Price Momentum)
start_timeW <- Sys.time()
sp_barweek <- median(data_spweek$WeeklyReturn)
data_spweek <- data_spweek %>%
  mutate(neg_change = -sd(WeeklyReturn),
         pos_change = sd(WeeklyReturn))
data_spweek <- data_spweek %>%
  mutate(PortDecision = if_else(WeeklyReturn < neg_change, -1, 
                                if_else(WeeklyReturn > pos_change, 1,0)),
         PortDecision = as.factor(PortDecision))

data_spweek <- data_spweek %>%
  mutate(Owned=0)

for (i in 1:5000) {
  data_spweek <- data_spweek %>%
    mutate(Owned = ifelse(Date == "2003-01-24", 0,
                          ifelse(lag(Owned) == 0 & (lag(PortDecision) == 0 | lag(PortDecision == -1)), 0,
                                 ifelse(lag(Owned) == 0 & lag(PortDecision) == 1, 1,
                                        ifelse(lag(Owned) == 1 & lag(PortDecision) == -1, 0, 1)))))
}

data_spweek <- data_spweek %>%
  mutate(MomentumPortfolio=0)

for (i in 1:5000) {
  data_spweek <- data_spweek %>%
    mutate(MomentumPortfolio= if_else(Date == "2003-01-24", 1000,
                                           if_else(Owned==1,lag(MomentumPortfolio)+(lag(MomentumPortfolio)*WeeklyReturn/100),
                                                   lag(MomentumPortfolio))))
}

end_timeW <- Sys.time()
net_timeW <- end_timeW - start_timeW
net_timeW

### Portfolio decision based on volatility neural network 
getSymbols.FRED("VIXCLS",
                env = ".GlobalEnv",
                return.class = "xts")
vix <- fortify(VIXCLS)
colnames(vix) <- c("Date", "vix")

for (i in 1:8687) {
  vix <- vix %>%
    mutate(vix= if_else(is.na(vix), lag(vix), vix))
}

data_vixweek <- merge(data_spweek, vix, by = "Date")



delta <- 0.5
vix_bar <- median(data_vixweek$vix)
data_vixweek <- data_vixweek %>%
  dplyr::select(Date, WeeklyReturn) %>%
  mutate(r_minus = (-0.02) * exp(-delta*(data_vixweek$vix-vix_bar)),
         r_plus = 0.02 * exp(delta*(data_vixweek$vix-vix_bar)))
data_vixweek <- data_vixweek %>%
  mutate(PortDecisionVix = if_else(WeeklyReturn < r_minus, -1, if_else(WeeklyReturn > r_plus, 1,0)),
         PortDecisionVix = as.factor(PortDecisionVix))


data_vixweek <- data_vixweek %>%
  mutate(Owned=0)

for (i in 1:5000) {
  data_vixweek <- data_vixweek %>%
    mutate(Owned = ifelse(Date == "2003-01-24", 0,
                          ifelse(lag(Owned) == 0 & (lag(PortDecisionVix) == 0 | lag(PortDecisionVix == -1)), 0,
                                 ifelse(lag(Owned) == 0 & lag(PortDecisionVix) == 1, 1,
                                        ifelse(lag(Owned) == 1 & lag(PortDecisionVix) == -1, 0, 1)))))
}


data_vixweek <- data_vixweek %>%
  mutate(VixPortfolio=0)

for (i in 1:5000) {
  data_vixweek <- data_vixweek %>%
    mutate(VixPortfolio= if_else(Date == "2003-01-24", 1000,
                                 if_else(Owned==1,lag(VixPortfolio)+(lag(VixPortfolio)*WeeklyReturn/100),
                                         lag(VixPortfolio))))
}

WeekPortfolios <- merge(data_spweek, data_vixweek, by = "Date")

WeekPortfolios<-WeekPortfolios %>%
  select(Date, MomentumPortfolio, BasePortfolio, VixPortfolio)

ggplot(WeekPortfolios, aes(x = Date)) +
  geom_line(aes(y = MomentumPortfolio, color = "Std")) +
  geom_line(aes(y = BasePortfolio, color = "Base")) +
  geom_line(aes(y = VixPortfolio, color = "Volatility")) +
  labs(x = "Date", y = "Value", color = "Legend")+
  geom_hline(yintercept=1000, linetype="dotted")+
  geom_vline(xintercept = as.numeric(as.Date("2008-09-15")), color = "Orange", linetype = "dashed")+
  geom_vline(xintercept = as.numeric(as.Date("2020-02-20")), color = "Purple", linetype = "dashed")



#####
# Monthly
monthlymetadata <- read_csv('Monthmetadata.csv')
monthlymetadata <- monthlymetadata %>%
  mutate(Date=as.Date(Date, format = "%m/%d/%Y"))

### Base Portfolio
data_spmonth <- monthlymetadata %>%
  mutate(BasePortfolio=0)

for (i in 1:5000) {
  data_spmonth <- data_spmonth %>%
    mutate(BasePortfolio= if_else(Date == "2003-02-01", 1000,
                                      lag(BasePortfolio)+(lag(BasePortfolio)*MonthlyReturn/100)))
}

### Portfolio Decision Based on Standard Deviation
start_timeM <- Sys.time()
### Standard Deviation Calculation


sp_barmonth <- median(data_spmonth$MonthlyReturn)
data_spmonth <- data_spmonth %>%
  mutate(neg_change = -sd(MonthlyReturn),
         pos_change = sd(MonthlyReturn))
data_spmonth <- data_spmonth %>%
  mutate(PortDecision = if_else(MonthlyReturn < neg_change, -1, 
                                if_else(MonthlyReturn > pos_change, 1,0)),
         PortDecision = as.factor(PortDecision))

data_spmonth <- data_spmonth %>%
  mutate(Owned=0)

for (i in 1:5000) {
  data_spmonth <- data_spmonth %>%
    mutate(Owned = ifelse(Date == "2003-02-01", 0,
                          ifelse(lag(Owned) == 0 & (lag(PortDecision) == 0 | lag(PortDecision == -1)), 0,
                                 ifelse(lag(Owned) == 0 & lag(PortDecision) == 1, 1,
                                        ifelse(lag(Owned) == 1 & lag(PortDecision) == -1, 0, 1)))))
}

data_spmonth <- data_spmonth %>%
  mutate(MomentumPortfolio=0)

for (i in 1:5000) {
  data_spmonth <- data_spmonth %>%
    mutate(MomentumPortfolio= if_else(Date == "2003-02-01", 1000,
                                            if_else(Owned==1,lag(MomentumPortfolio)+(lag(MomentumPortfolio)*MonthlyReturn/100),
                                                    lag(MomentumPortfolio))))
}

end_timeM <- Sys.time()
net_timeM <- end_timeM - start_timeM
net_timeM

### Portfolio decision based on volatility neural network 
getSymbols.FRED("VIXCLS",
                env = ".GlobalEnv",
                return.class = "xts")

# convert to monthly data
vix_monthly <- to.period(VIXCLS, period = "months", indexAt = "firstof", OHLC = FALSE)

# fortify and rename columns
vix_monthly <- fortify(vix_monthly)
colnames(vix_monthly) <- c("Date", "vix")

vix_monthly <- vix_monthly %>%
  mutate(Date=as.Date(Date, format = "%m/%d/%Y"))

data_vixmonth <- merge(data_spmonth, vix_monthly, data_spmonth = "Date")

#Just incase there are back-toback N/A
for (i in 1:8687) {
  vix_monthly <- vix_monthly %>%
    mutate(vix= if_else(is.na(vix), lag(vix), vix))
}


delta <- 0.5
vix_bar <- median(data_vixmonth$vix)
data_vixmonth <- data_vixmonth %>%
  dplyr::select(Date, MonthlyReturn) %>%
  mutate(r_minus = (-0.02) * exp(-delta*(data_vixmonth$vix-vix_bar)),
         r_plus = 0.02 * exp(delta*(data_vixmonth$vix-vix_bar)))
data_vixmonth <- data_vixmonth %>%
  mutate(PortDecisionVix = if_else(MonthlyReturn < r_minus, -1, if_else(MonthlyReturn > r_plus, 1,0)),
         PortDecisionVix = as.factor(PortDecisionVix))


data_vixmonth <- data_vixmonth %>%
  mutate(Owned=0)

for (i in 1:5000) {
  data_vixmonth <- data_vixmonth %>%
    mutate(Owned = ifelse(Date == "2003-02-01", 0,
                          ifelse(lag(Owned) == 0 & (lag(PortDecisionVix) == 0 | lag(PortDecisionVix == -1)), 0,
                                 ifelse(lag(Owned) == 0 & lag(PortDecisionVix) == 1, 1,
                                        ifelse(lag(Owned) == 1 & lag(PortDecisionVix) == -1, 0, 1)))))
}


data_vixmonth <- data_vixmonth %>%
  mutate(VixPortfolio=0)

for (i in 1:5000) {
  data_vixmonth <- data_vixmonth %>%
    mutate(VixPortfolio= if_else(Date == "2003-02-01", 1000,
                                 if_else(Owned==1,lag(VixPortfolio)+(lag(VixPortfolio)*MonthlyReturn/100),
                                         lag(VixPortfolio))))
}

MonthPortfolios <- merge(data_spmonth, data_vixmonth, by = "Date")

MonthPortfolios<-MonthPortfolios %>%
  select(Date, MomentumPortfolio, BasePortfolio, VixPortfolio)

ggplot(MonthPortfolios, aes(x = Date)) +
  geom_line(aes(y = MomentumPortfolio, color = "Std")) +
  geom_line(aes(y = BasePortfolio, color = "Base")) +
  geom_line(aes(y = VixPortfolio, color = "Volatility")) +
  labs(x = "Date", y = "Value", color = "Legend")+
  geom_hline(yintercept=1000, linetype="dotted")+
  geom_vline(xintercept = as.numeric(as.Date("2008-09-15")), color = "Orange", linetype = "dashed")+
  geom_vline(xintercept = as.numeric(as.Date("2020-02-20")), color = "Purple", linetype = "dashed")

summary(MonthPortfolios)

###
#Sensitivity Analysis
#Half Stdev Portfolio
data_sensitivity <- data_spmonth %>%
  mutate(neg_changeHalf = -0.5*sd(MonthlyReturn),
         pos_changeHalf = 0.5*sd(MonthlyReturn))
data_sensitivity <- data_sensitivity %>%
  mutate(PortDecisionHalf = if_else(MonthlyReturn < neg_changeHalf, -1, 
                                if_else(MonthlyReturn > pos_changeHalf, 1,0)),
         PortDecisionHalf = as.factor(PortDecisionHalf))

data_sensitivity <- data_sensitivity %>%
  mutate(OwnedHalfStd=0)

for (i in 1:5000) {
  data_sensitivity <- data_sensitivity %>%
    mutate(OwnedHalfStd = ifelse(Date == "2003-02-01", 0,
                          ifelse(lag(OwnedHalfStd) == 0 & (lag(PortDecisionHalf) == 0 | lag(PortDecisionHalf == -1)), 0,
                                 ifelse(lag(OwnedHalfStd) == 0 & lag(PortDecisionHalf) == 1, 1,
                                        ifelse(lag(OwnedHalfStd) == 1 & lag(PortDecisionHalf) == -1, 0, 1)))))
}

data_sensitivity <- data_sensitivity %>%
  mutate(HalfPortfolio=0)

for (i in 1:5000) {
  data_sensitivity <- data_sensitivity %>%
    mutate(HalfPortfolio= if_else(Date == "2003-02-01", 1000,
                                      if_else(OwnedHalfStd==1,lag(HalfPortfolio)+(lag(HalfPortfolio)*MonthlyReturn/100),
                                              lag(HalfPortfolio))))
}

#Double Stdev Portfolio
data_sensitivity <- data_sensitivity %>%
  mutate(neg_changeDbl = -2*sd(MonthlyReturn),
         pos_changeDbl = 2*sd(MonthlyReturn))
data_sensitivity <- data_sensitivity %>%
  mutate(PortDecisionDbl = if_else(MonthlyReturn < neg_changeDbl, -1, 
                                    if_else(MonthlyReturn > pos_changeDbl, 1,0)),
         PortDecisionDbl = as.factor(PortDecisionDbl))

data_sensitivity <- data_sensitivity %>%
  mutate(OwnedDblStd=0)

for (i in 1:5000) {
  data_sensitivity <- data_sensitivity %>%
    mutate(OwnedDblStd = ifelse(Date == "2003-02-01", 0,
                                 ifelse(lag(OwnedDblStd) == 0 & (lag(PortDecisionDbl) == 0 | lag(PortDecisionDbl == -1)), 0,
                                        ifelse(lag(OwnedDblStd) == 0 & lag(PortDecisionDbl) == 1, 1,
                                               ifelse(lag(OwnedDblStd) == 1 & lag(PortDecisionDbl) == -1, 0, 1)))))
}

data_sensitivity <- data_sensitivity %>%
  mutate(DblPortfolio=0)

for (i in 1:5000) {
  data_sensitivity <- data_sensitivity %>%
    mutate(DblPortfolio= if_else(Date == "2003-02-01", 1000,
                                  if_else(OwnedDblStd==1,lag(DblPortfolio)+(lag(DblPortfolio)*MonthlyReturn/100),
                                          lag(DblPortfolio))))
}

colnames(data_sensitivity)[13] = "SinglePortfolio"

sensitivityPortfolios<- data_sensitivity %>%
  select(Date, SinglePortfolio, HalfPortfolio, DblPortfolio)

ggplot(sensitivityPortfolios, aes(x = Date)) +
  geom_line(aes(y = SinglePortfolio, color = "Single")) +
  geom_line(aes(y = HalfPortfolio, color = "Half")) +
  geom_line(aes(y = DblPortfolio, color = "Double")) +
  labs(x = "Date", y = "Value", color = "Legend")+
  geom_hline(yintercept=1000, linetype="dotted")+
  geom_vline(xintercept = as.numeric(as.Date("2008-09-15")), color = "Orange", linetype = "dashed")+
  geom_vline(xintercept = as.numeric(as.Date("2020-02-20")), color = "Purple", linetype = "dashed")

#####
#Program Execution Table
#The resulting outputs can vary slightly, but multiple runs have seen similar results

runtime_table <- data.frame(Model=c('Daily','Weekly','Monthly'),
                            Runtime=c(net_timeD,net_timeW,net_timeM))
runtime_table

