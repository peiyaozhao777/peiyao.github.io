#library(BGVAR) #load library
library(ggplot2)
#library(GVAR)
#library(zoo)
#library(janitor) #rowtonames function
library(data.table)
#library(xts)
library(readxl)

flights = read_excel('../../data/raw/airsavvi_flight_data_20201215.xlsx')

# Read in the data files
df_covid = read.csv('../../data/tidy/cases_deaths_concat.csv')
df_google = read.csv('../../data/tidy/google_activity.csv')
df_interv = read.csv('../../data/tidy/govt_interventions.csv')
df_deter = read.csv('../../data/tidy/deterministic.csv')


# Here, we make all the names uniform
# Ideally, this should be done in the preprocessing script in python (make this a TODO)
names(df_covid)[3:(ncol(df_covid))] = gsub(x = names(df_covid)[3:201], pattern = "X", replacement = "")
names(df_google)[3:(ncol(df_google))] = names(df_covid)[3:(ncol(df_covid))]
names(df_interv)[3:(ncol(df_google))] = names(df_covid)[3:(ncol(df_covid))]
df_covid = cbind(df_covid[['iso']], df_covid[['X']], df_covid[,3:(ncol(df_covid))]) #reorder ISO and Variable columns TODO

#Standardize names of first 2 columns (This can/should also be done in python pre-processing script) (TODO)
names(df_covid)[1:2] = c('iso','variable')
names(df_google)[1:2] = c('iso','variable') 
names(df_interv)[1:2] = c('iso','variable')

# Change the variable names to be shorter (TODO)
# Save these new names in the Python pre-processing script. Once this is done, the next few lines should be obsolete
df_interv$variable <- gsub(x = df_interv$variable, pattern = "Public health measures", replacement = "phm")
df_interv$variable <- gsub(x = df_interv$variable, pattern = "Governance and socio-economic measures", replacement = "sem")
df_interv$variable <- gsub(x = df_interv$variable, pattern = "Social distancing", replacement = "sd")
df_interv$variable <- gsub(x = df_interv$variable, pattern = "Movement restrictions", replacement = "mr")
df_interv$variable <- gsub(x = df_interv$variable, pattern = "Lockdown", replacement = "ld")
df_interv$variable <- gsub(x = df_interv$variable, pattern = "Humanitarian exemption", replacement = "he")

#sort all df's by iso: (TODO)
sort_df_by_iso <- function(df) {
    df <- df[order(df[['iso']]), ]
    rownames(df)<-1:nrow(df)
    return(df)
}

df_covid = sort_df_by_iso(df_covid)
df_google = sort_df_by_iso(df_google)
df_interv = sort_df_by_iso(df_interv)
df_deter = sort_df_by_iso(df_deter)

### IMPORTANT: This removes other variables
df_covid = subset(df_covid, !(df_covid$variable %in% c('deaths'))) 
df_google = subset(df_google, !(df_google$variable %in% c('parks','retail'))) 
df_interv = subset(df_interv, !(df_interv$variable %in% c('phm', 'sem', 'mr', 'he'))) 

iso3iso2 = read.csv('../../data/raw/iso3_iso2_country_codes.csv')

head(df_covid)
head(df_google)
head(df_interv)
head(df_deter)

tail(iso3iso2)

change_iso3_to_iso2 <- function(iso_code) {
    if (nchar(iso_code) == 3) {
        iso_code = subset(iso3iso2, iso_3==iso_code)$iso_2
    }
    return(iso_code)
}

df_covid$iso = apply(df_covid[1], 1, change_iso3_to_iso2)
df_google$iso = apply(df_google[1], 1, change_iso3_to_iso2)
df_interv$iso = apply(df_interv[1], 1, change_iso3_to_iso2)
df_deter$iso = apply(df_deter[1], 1, change_iso3_to_iso2)

######################################################################################
# Exploratory plots for differencing
NCOLS = ncol(df_google)
options(repr.plot.width = 14, repr.plot.height = 5) 
par(mfrow=c(1,2))
plot(seq(3,NCOLS),df_google[1,3:NCOLS]) #quick exploratory plotting
plot(seq(4,NCOLS),matrixStats::rowDiffs(as.matrix(df_google[1,3:NCOLS]))) #quick exploratory plotting

par(mfrow=c(1,3))
plot(seq(4,NCOLS),matrixStats::rowDiffs(as.matrix(df_covid[1,3:NCOLS]))) #quick exploratory plotting
plot(seq(5,NCOLS),matrixStats::rowDiffs(as.matrix(df_covid[1,3:NCOLS]), differences=2L)) # twice differenced
plot(seq(3,NCOLS),df_covid[1,3:NCOLS]) #quick exploratory plotting

# Appropriately difference COVID cases and Google activity data
df_covid[, 5:201] = matrixStats::rowDiffs(as.matrix(df_covid[, 3:201]), differences=2L) # convert to daily cases and then difference
df_google[, 4:201] = matrixStats::rowDiffs(as.matrix(df_google[, 3:201])) # 1st difference of google activity/mobility
# More data transformations can/may need to be done here. 

# Check for consistency/completeness across country names
all(unique(df_google$iso) == unique(df_covid$iso))
all(unique(df_google$iso) == unique(df_interv$iso))
all(unique(df_google$iso) == unique(df_deter$iso))
print(paste0("Number of unique countries: ", length(unique(df_google$iso))))
unique(df_interv$country)[!unique(df_interv$country) %in% unique(df_covid$country)]
unique(df_interv$country)[!unique(df_interv$country) %in% unique(df_google$country)]
unique(df_covid$country)[!unique(df_covid$country) %in% unique(df_google$country)]
unique(df_interv$country)[!unique(df_interv$country) %in% unique(df_deter$iso)]
###############################################################################################################



#Convert endogenous data into list of dataframes; each list element corresponds to country
# For each country, rows = time obs; columns = endogenous variables
df_all <- rbind(df_covid[,1:NCOLS], df_google[,1:NCOLS])#, df_interv) #endogenous variables
formatted_dates = as.Date(colnames(df_all)[-c(1,2)], format = c("%m.%d.%Y"))
countries_with_missing_data = unique(df_all[rowSums(is.na(df_all)) > 0,]$iso)
df_all = subset(df_all, !(iso %in% countries_with_missing_data))

### Flight-based weights
flights_week1 = flights[65:128,2:66]
weight_matrix = as.matrix(flights_week1[,2:65])
rownames(weight_matrix) = flights_week1[,1][[1]]
weight_matrix = weight_matrix[unique(df_all$iso), unique(df_all$iso)]
weight_matrix[is.na(weight_matrix)] = 0
for ( i in rownames(weight_matrix)) {
    weight_matrix[i, ] = weight_matrix[i, ]/rowSums(weight_matrix)[i]
}
rowSums(weight_matrix)

countries_without_missing_data = setdiff(unique(df_all$iso), countries_with_missing_data)
countries_in_flight_data = names(flights_week1)[2:ncol(flights_week1)]
countries_notmissing_but_notin_flight_data = setdiff(countries_without_missing_data, countries_in_flight_data)


# Define the desired subset of country names with complete BGVAR data (52)
desired_subset <- c("AE", "AR", "AU", "AT", "BE", "BG", "BR", "CA", "CL", "CO", "CZ", "DE", "DK", "EG", "ES", "EE", "FI", "FR", "GB", "GR", "HR", "HU", "ID", "IN", "IE", "IL", "IT", "JP",
                    "KH", "KR", "LT", "LV", "MA", "MX", "MY", "NL", "NZ", "PH", "PT", "RU", "SA", "SG", "SK", "SI", "SE", "TH", "TR", "UA", "UY", "US", "VN", "ZA")

# Subset the ODX matrix for the desired subset of countries
subset_flights_week1 <- flights_week1[flights_week1$`Destination\r\n Origin` %in% desired_subset, c("Destination\r\n Origin", desired_subset)]

# Print the subset of the ODX matrix
head(subset_flights_week1)
saveRDS(subset_flights_week1, file = "../../results/flights_week1.RDS")


dfList = list()
for (i in unique(df_all$iso)) {
    country_df = transpose(df_all[df_all$iso==i, 2:NCOLS])
    colnames(country_df) = country_df[1,]
    country_df = country_df[-1, ] #c('cases', 'residential', 'workplaces', 'transit', 'grocery')]
    country_df = as.data.frame(sapply(country_df, as.numeric))
    country_df = ts(country_df, start = c(2020, as.numeric(format(formatted_dates[1], "%j"))), frequency = 365)
    #rownames(country_df) = formatted_dates
    #country_df = as.xts(country_df)
    #rownames(country_df) <- c()
    dfList[[i]] <- country_df 
}

head(dfList$AE)
 
#Check for consistency
names(dfList)
all(names(dfList) == row.names(weight_matrix))
 
## Finalize list of countries
#dfList = dfList[row.names(weight_matrix)]


 
#endo <- ord <- we <- d <- vector("list",length=length(row.names(weight_matrix)))
#names(endo) <- names(ord) <- names(we) <- names(d) <- row.names(weight_matrix)
#
#for (i in 1:length(endo)) {
#    endo[[i]] <- c(1:5)
#    ord[[i]] <- c(1:5)
#}
#
#for (i in 1:length(we))
#{
#    we[[i]] <- c(1)
#    #d[[i]] <- c(9,11) #sd, ld
#}
#
#
#res.GVAR <- GVAR(Data = dfList, p = 1,
#                q = 1,
#                weight = weight_matrix, 
#                Case = "I",
#                exo.var = FALSE,
#                #d = c('sd', 'ld')),
#                lex = 1,
#                endo = endo,
#                ord = ord,
#                we = we,
#                caseTest = False,
#                weTest = False
#) 
