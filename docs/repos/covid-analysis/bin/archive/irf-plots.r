library(ggplot2)
library(data.table)
library(tidyverse)
library(BGVAR)

irf = readRDS('../../results/global_irf_cases.RDS')
#df = read.csv('../../results/global_irf_posterior.csv')


#******************************************************************************************
#*EXPLANATORY TEXT
# first dim: country-variable (260 in all)
# second dim: time (0 - 30)
# third dim: shock
# fourth dim: quantiles


# first dim
names(irf$posterior[1:5,1,1,1])
#"AE.cases"       "AE.residential" "AE.workplaces"  "AE.transit"     "AE.grocery"

# quantiles
names(irf$posterior[1,1,1,])
#"Q5"  "Q10" "Q16" "Q50" "Q84" "Q90" "Q95"

# shocks
row.names(irf$posterior[1,1,,])
#"ZA.cases"   "ZA.transit" "US.cases"   "US.transit" "KR.cases"   "KR.transit" "DE.cases"   "DE.transit"

# To choose cases, e.g. 
cases_index = seq(1,260,5)

# To choose all transit:
transit_index = seq(4,260,5)
#******************************************************************************************

#tail(za_cases_irf)

irf_post <- irf$posterior

head(irf$posterior[cases_index,1,,1])

# EXAMPLE PLOT
# response of all country cases to ZA.cases shock:
za_cases_irf = irf$posterior[cases_index, , 7, ]
za_transit_irf = irf$posterior[transit_index, , 8, ]

us_cases_irf = irf$posterior[cases_index, , 1, ]
us_transit_irf = irf$posterior[transit_index, , 2, ]


za_cases_irf_q5 = melt(za_cases_irf[,,1])
za_cases_irf_q10 = melt(za_cases_irf[,,2])
za_cases_irf_q16 = melt(za_cases_irf[,,3])
za_cases_irf_q50 = melt(za_cases_irf[,,4])
za_cases_irf_q84 = melt(za_cases_irf[,,5])
za_cases_irf_q90 = melt(za_cases_irf[,,6])
za_cases_irf_q95 = melt(za_cases_irf[,,7])

za_transit_irf_q5  = melt(za_transit_irf[,,1])
za_transit_irf_q10 = melt(za_transit_irf[,,2])
za_transit_irf_q16 = melt(za_transit_irf[,,3])
za_transit_irf_q50 = melt(za_transit_irf[,,4])
za_transit_irf_q84 = melt(za_transit_irf[,,5])
za_transit_irf_q90 = melt(za_transit_irf[,,6])
za_transit_irf_q95 = melt(za_transit_irf[,,7])

melted_za_cases_irf = cbind(za_cases_irf_q5, za_cases_irf_q10$value, za_cases_irf_q16$value,
                            za_cases_irf_q50$value, za_cases_irf_q84$value, za_cases_irf_q90$value,
                            za_cases_irf_q95$value)
melted_za_transit_irf = cbind(za_transit_irf_q5, za_transit_irf_q10$value, za_transit_irf_q16$value,
                              za_transit_irf_q50$value, za_transit_irf_q84$value, za_transit_irf_q90$value,
                              za_transit_irf_q95$value)

colnames(melted_za_cases_irf) = c("Country","Day", "Q5", "Q10", "Q16", "Q50", "Q84", "Q90", "Q95" )
colnames(melted_za_transit_irf) = c("Country","Day", "Q5", "Q10", "Q16", "Q50", "Q84", "Q90", "Q95" )

melted_za_cases_irf$Country <- gsub(".cases", "", as.character(melted_za_cases_irf$Country))
melted_za_transit_irf$Country <- gsub(".transit", "", as.character(melted_za_transit_irf$Country))

head(melted_za_cases_irf)
#head(melted_za_transit_irf)

p <- ggplot(melted_za_cases_irf, aes(Day)) + 
    geom_line(aes(y=Q50), colour="blue") + 
    geom_ribbon(aes(ymin=Q5, ymax=Q95), alpha=0.2, fill='red')

p + facet_wrap(vars(Country)) #,nrow = 4,ncol = 13
ggsave(file="../../figures/irf_de_cases.png", width=10, height=8, dpi=300)

test <- subset(melted_za_cases_irf, Country %in% c("US", "FR", "ES") )

pnew <- ggplot(test, aes(Day)) + 
  geom_line(aes(y=Q50), colour="blue") + 
  geom_ribbon(aes(ymin=Q5, ymax=Q95), alpha=0.2, fill='red')
pnew + facet_wrap(vars(Country))

p1 <- ggplot(melted_za_transit_irf, aes(Day)) + 
  geom_line(aes(y=Q50), colour="blue") + 
  geom_ribbon(aes(ymin=Q5, ymax=Q95), alpha=0.2, fill='red')

p1 + facet_wrap(vars(Country))
ggsave(file="../../figures/irf_de_transit.png", width=10, height=8, dpi=300)