library(BGVAR)
library(reshape2)
library(panelvar)
library(dtw)
#library(pacman)
#p_load(tidyverse,panelvar)

#setwd('Projects/covid-analysis/')
df <- read.csv('../../data/tidy/cases_mobility_activity.csv')
df <- df[, c(2,4,8:186)] #36:95

# Infer missing Apple data
df[df$transportation_type=='driving','X5.11.2020'] = df[df$transportation_type=='driving','X5.10.2020']
df[df$transportation_type=='driving','X5.12.2020'] = df[df$transportation_type=='driving','X5.10.2020']
df[df$transportation_type=='walking','X5.11.2020'] = df[df$transportation_type=='walking','X5.10.2020']
df[df$transportation_type=='walking','X5.12.2020'] = df[df$transportation_type=='walking','X5.10.2020']
df[df$transportation_type=='transit','X5.11.2020'] = df[df$transportation_type=='transit','X5.10.2020']
df[df$transportation_type=='transit','X5.12.2020'] = df[df$transportation_type=='transit','X5.10.2020']


# Convert dataframe
melted.df <- melt(df, id.vars = c('region', 'transportation_type'))
m.data <- dcast(melted.df, region + variable~transportation_type)
colnames(m.data) = c('Country', 'Date', 'cov', 'car', 'groc', 'parks', 'home', 'reta', 'tran', 'tstop', 'walk', 'work' )

# Convert countries to factors
m.data$Country <- as.factor(m.data$Country)

# Convert numbers to numeric
for (i in seq(3, length(c('Country', 'Date', 'cov', 'car', 'groc', 'parks', 'home', 'reta', 'tran', 'tstop', 'walk', 'work' )  ) )) {
  m.data[,i] = as.numeric(m.data[,i], na.pass=TRUE)
}

# Correct Google (add 100 to baseline)
m.data[,c( 'groc', 'parks', 'home', 'reta',  'tstop', 'work' )] = m.data[,c( 'groc', 'parks', 'home', 'reta',  'tstop', 'work' )] + 100

# Remove Apple data
endovars <-  c('car', 'tran', 'walk', 'groc', 'parks', 'home', 'reta',  'tstop', 'work' )
m.data <- subset(m.data, select = c('Country', 'Date', endovars) )
m.data <- na.omit(m.data)
#m.data <- m.data[-1,]

#m.data <- m.data[m.data$cov >= 1,] # remove all zero covid cases
# m.data['day'] = 1 # initialize day column
# for (i in unique(m.data$Country)) {
#   for (j in seq(1,nrow(m.data[m.data$Country==i,]) ) ) {
#     m.data[m.data$Country==i,][j, 'day'] = j
#   }
# }
#m.data <- m.data[m.data$day <= 60,]


# Drop date column
m.data <- subset(m.data, select = -Date)


#Log and difference
#m.data[,endovars] = log(m.data[,endovars])
#diff(m.data[,endovars])

#ggplot(m.data, aes(x=Date,y=car,color=Country,group=Country)) + geom_point() + geom_line()
#ggplot(m.data, aes(x=Date,y=home,color=Country,group=Country)) + geom_point() + geom_line()


########################################
# DTW 
########################################
dm <- matrix(NA, nrow=length(unique(m.data$Country)), ncol =length(unique(m.data$Country))  )
diag(dm) <- 0
dm
rownames(dm) <- unique(m.data$Country)
colnames(dm) <- unique(m.data$Country)

ii = 0
for (i in unique(m.data$Country)) {
  ii = ii + 1
  jj = 0
  for (j in unique(m.data$Country)) {
    jj = jj + 1
    if (jj > ii) {
      dm[i,j] <- dtw(dist(m.data[m.data$Country==i,], m.data[m.data$Country==j,]), distance.only = T)$normalizedDistance
    }
  }
}

write.csv(dm,file='disimilarity-matrix-mobility.csv')

# mod.pvar1 <- pvargmm(
#   dependent_vars = endovars,
#   lags = 1,
#   transformation = "fd",
#   data = m.data,
#   panel_identifier=c("Country", "day"),
#   steps = c("twostep"),
#   system_instruments = FALSE,
#   collapse = FALSE
# )


#m.data <- subset(m.data, select = c(Country, Data, cov, car, groc, parks, home, reta, reta))
#l.data <- vector(mode = "list", length = length(unique(m.data$Country)))
# for (i in unique(m.data$Country)) {
#   l.data[[i]] <- subset(m.data, Country==i)
#   l.data[[i]] <- l.data[[i]][,3:12]
#   l.data[[i]] <- data.matrix(l.data[[i]], rownames.force = NA)
# }

#w.data <- read.csv('963652501_T_T100I_MARKET_ALL_CARRIER_2019_YEAR.csv')
#acast(w.data, ORIGIN_COUNTRY~DEST_COUNTRY, sum, value.var="PASSENGERS")
# 
# model.1<-bgvar(Data=df,
#                     W=W.trade0012,
#                     saves=100,
#                     burns=100,
#                     plag=2,
#                     prior="NG",
#                     SV=TRUE,
#                     thin=1,
#                     trend=TRUE,
#                     h=0,
#                     save.country.store=FALSE,
#                     multithread=TRUE,
#                     eigen=1.05
#                     )

