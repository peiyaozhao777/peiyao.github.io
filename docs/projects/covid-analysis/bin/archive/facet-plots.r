#library(BGVAR)
library(reshape2)
library(panelvar)
library(dtw)
library(ggplot2)
library(stringr)

#library(pacman)
#p_load(tidyverse,panelvar)

#setwd('Projects/covid-analysis/')
#setwd('C:/Users/nasko/dev/nars/covid-analysis/bin/rscripts')
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
# m.data <- dcast(melted.df, region + variable~transportation_type, value.var="value")
# colnames(m.data) = c('Country', 'Date', 'cov', 'car', 'groc', 'parks', 'home', 'reta', 'tran', 'tstop', 'walk', 'work' )

# Convert countries to factors
# m.data$Country <- as.factor(m.data$Country)


melted.df <- na.omit(melted.df)

# Rename transportation type
melted.df[c("transportation_type")][melted.df[c("transportation_type")]=='grocery_and_pharmacy_percent_change_from_baseline'] <- 'groc'
melted.df[c("transportation_type")][melted.df[c("transportation_type")]=='transit_stations_percent_change_from_baseline'] <- 'tran'
melted.df[c("transportation_type")][melted.df[c("transportation_type")]=='residential_percent_change_from_baseline'] <- 'home'
melted.df[c("transportation_type")][melted.df[c("transportation_type")]=='retail_and_recreation_percent_change_from_baseline'] <- 'reta'
melted.df[c("transportation_type")][melted.df[c("transportation_type")]=='workplaces_percent_change_from_baseline'] <- 'work'
melted.df[c("transportation_type")][melted.df[c("transportation_type")]=='parks_percent_change_from_baseline'] <- 'parks'

# Drop Apple variables
melted.df<-melted.df[!(melted.df$transportation_type=="driving" | melted.df$transportation_type=="walking" | melted.df$transportation_type=="transit"),]

# Convert numbers to numeric
# for (i in seq(3, length(c('Country', 'Date', 'cov', 'car', 'groc', 'parks', 'home', 'reta', 'tran', 'tstop', 'walk', 'work' )  ) )) {
#   m.data[,i] = as.numeric(m.data[,i], na.pass=TRUE)
# }

# Correct Google (add 100 to baseline)
# m.data[,c( 'groc', 'parks', 'home', 'reta',  'tstop', 'work' )] = m.data[,c( 'groc', 'parks', 'home', 'reta',  'tstop', 'work' )] + 100

# Remove Apple data
# endovars <-  c('car', 'tran', 'walk', 'groc', 'parks', 'home', 'reta',  'tstop', 'work' )
# m.data <- subset(m.data, select = c('Country', 'Date', endovars) )
# m.data <- na.omit(m.data)

# Plot Google data
colors = c( # '#ffff99', ##d8ac93', # '#ffff99', #or yellowversions 
  '#66c2a5',
  '#fc8d62',
  '#8da0cb',
  '#e78ac3',
  '#a6d854',
  '#ffd92f',
  '#e5c494')

# Change date format to POSIX
# m.data$Date <- as.POSIXct(str_remove(m.data1$Date, "X"),format="%m.%d.%Y")

google.names <- c("Work", "Transit Stops", "Groceries", "Retail", "Home", "Parks")
lims <- as.POSIXct(strptime(c("2020-02-15", "2020-07-15"), format = "%Y-%m-%d"))

# Create facet grid
melted.df$variable <- as.POSIXct(str_remove(melted.df$variable, "X"),format="%m.%d.%Y")
ggplot(data = melted.df, aes(x = variable, y = value, color = region, group = region)) +
  # geom_point(size=1, alpha=.4) + 
  theme(legend.position="none") +
  scale_x_datetime(limits = lims) +
  geom_smooth(method = "gam", size=.5, alpha=.25)+
  facet_grid(rows = vars(transportation_type), labeller = labeller(.cols = google.names))#, labeller = labeller(.cols = google.names))
  #facet_wrap( vars(transportation_type), ncol = 1 )

ggsave("../../results/facet-grid-test3.png", width = 14, height = 4, units="in", dpi="retina" )
  
# ggplot(data = m.data1, aes(x = Date, y = value)) + geom_line() + scale_x_datetime(limits = lims)
