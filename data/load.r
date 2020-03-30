
## get confirmed cases

confirmed <- read_csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

## get death cases

death <- read_csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")

## get recovered cases

recovered <- read_csv(file = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

## country data

#country_data <- read_csv(file = 'data/country_profile_variables.csv')


## temperatures

temperature_max <- read_csv('https://raw.githubusercontent.com/imantsm/COVID-19/master/csv/tMax.csv')

temperature_min <- read_csv('https://raw.githubusercontent.com/imantsm/COVID-19/master/csv/tMin.csv')          

## important dates

Important_dates <- read_csv('data/Important_Dates.csv')


### Portugal

Date <- as.Date()
Confirmed <- 1
Death <- 
Recovered <- 