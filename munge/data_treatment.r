library(sp)
library(rworldmap)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  #indices$continent   # returns the continent (6 continent model)
  indices$REGION   # returns the continent (7 continent model)
  #indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}


coords2continent(data.frame(lon=c(0), lat=c(52)))



confirmed <- confirmed %>% 
  ## convert the dataset from wide to long
  pivot_longer(., cols = 5:ncol(confirmed), names_to = "Date", values_to = "Confirmed") %>% 
  ## correct portugal values
  mutate(
    Confirmed = if_else((`Country/Region`=="Portugal") & (Date == "3/2/20"),2,Confirmed),
    Confirmed = if_else((`Country/Region`=="Portugal") & (Date == "3/3/20"),4,Confirmed),
    Confirmed = if_else((`Country/Region`=="Portugal") & (Date == "3/4/20"),6,Confirmed),
    Confirmed = if_else((`Country/Region`=="Portugal") & (Date == "3/5/20"),9,Confirmed),
    Confirmed = if_else((`Country/Region`=="Portugal") & (Date == "3/6/20"),13,Confirmed),
    Confirmed = if_else((`Country/Region`=="Portugal") & (Date == "3/7/20"),21,Confirmed),
    Confirmed = if_else((`Country/Region`=="Portugal") & (Date == "3/8/20"),30,Confirmed),
    Confirmed = if_else((`Country/Region`=="Portugal") & (Date == "3/9/20"),39,Confirmed),
    Confirmed = if_else((`Country/Region`=="Portugal") & (Date == "3/10/20"),41,Confirmed),
    Confirmed = if_else((`Country/Region`=="Portugal") & (Date == "3/11/20"),59,Confirmed),
    Confirmed = if_else((`Country/Region`=="Portugal") & (Date == "3/12/20"),78,Confirmed),
    Confirmed = if_else((`Country/Region`=="Portugal") & (Date == "3/13/20"),112,Confirmed),
    Confirmed = if_else((`Country/Region`=="Portugal") & (Date == "3/14/20"),169,Confirmed),
    Confirmed = if_else((`Country/Region`=="Portugal") & (Date == "3/18/20"),642,Confirmed),
    Confirmed = if_else((`Country/Region`=="Italy") & (Date == "3/12/20"),15113,Confirmed),
    Confirmed = if_else((`Country/Region`=="Spain") & (Date == "3/12/20"),3146,Confirmed),
    Confirmed = if_else((`Country/Region`=="France") & (`Province/State`=="France") & (Date == "3/12/20"),2876,Confirmed),
    Confirmed = if_else((`Country/Region`=="France") & (`Province/State`=="France") & (Date == "3/15/20"),5423,Confirmed),
    Confirmed = if_else((`Country/Region`=="United Kingdom") & (`Province/State`=="United Kingdom") & (Date == "3/12/20"),590,Confirmed),
    Confirmed = if_else((`Country/Region`=="Germany") & (Date == "3/12/20"),2745,Confirmed)
  ) %>% 
  ## Convert the date column to a date format
  mutate(Date = mdy(Date)) %>% 
  rename(
    State = `Province/State`,
    Country = `Country/Region`
  ) %>% 
  group_by(Country, State) %>% 
  arrange(Date) %>% 
  mutate(
    Confirmed_day = Confirmed - lag(Confirmed, default = 0),
    Continent = coords2continent(data.frame(lon=c(Long), lat=c(Lat)))) %>% 
  ungroup() %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    Continent = if_else(Country=="Australia","Australia",Continent),
    Continent = if_else(Country=="Canada","North America",Continent),
    Continent = if_else(Country=="China","Asia",Continent),
    Continent = if_else(Country=="Denmark","Europe",Continent),
    Continent = if_else(Country=="France","Europe",Continent),
    Continent = if_else(Country=="Netherlands","Europe",Continent),
    Continent = if_else(Country=="United Kingdom","Europe",Continent),
    Continent = if_else(Country=="US","North America",Continent),
    Continent = if_else(Country=="Monaco","Europe",Continent),
    Continent = if_else(Country=="Philippines","Asia",Continent),
    Continent = if_else(Country=="New Zealand","Europe",Continent),
    Continent = if_else(Country=="Saint Vincent and the Grenadine","North America",Continent)
         ) %>%
  select(Continent, State, Country, Lat, Long, Date, Confirmed, Confirmed_day) %>% 
  arrange(State, Continent, Country, Date) %>%
  data.frame()


death <- death %>% 
  ## convert the dataset from wide to long
  pivot_longer(., cols = 5:ncol(death), names_to = "Date", values_to = "Death") %>% 
  ## Convert the date column to a date format
  mutate(Date = mdy(Date)) %>% 
  rename(
    State = `Province/State`,
    Country = `Country/Region`
  ) %>% 
  group_by(Country, State) %>% 
  arrange(Date) %>% 
  mutate(Death_day = Death - lag(Death, default = 0),
         Continent = coords2continent(data.frame(lon=c(Long), lat=c(Lat)))) %>%
  ungroup() %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    Continent = if_else(Country=="Australia","Australia",Continent),
    Continent = if_else(Country=="Canada","North America",Continent),
    Continent = if_else(Country=="China","Asia",Continent),
    Continent = if_else(Country=="Denmark","Europe",Continent),
    Continent = if_else(Country=="France","Europe",Continent),
    Continent = if_else(Country=="Netherlands","Europe",Continent),
    Continent = if_else(Country=="United Kingdom","Europe",Continent),
    Continent = if_else(Country=="US","North America",Continent),
    Continent = if_else(Country=="Monaco","Europe",Continent),
    Continent = if_else(Country=="Philippines","Asia",Continent),
    Continent = if_else(Country=="New Zealand","Europe",Continent),
    Continent = if_else(Country=="Saint Vincent and the Grenadine","North America",Continent)
  ) %>%
  select(Continent, State, Country, Lat, Long, Date, Death, Death_day) %>% 
  arrange(State, Continent, Country, Date) %>% 
  ungroup() %>% 
  data.frame()

recovered <- recovered %>% 
  ## convert the dataset from wide to long
  pivot_longer(., cols = 5:ncol(recovered), names_to = "Date", values_to = "Recovered") %>% 
  ## Convert the date column to a date format
  mutate(Date = mdy(Date)) %>% 
  rename(
    State = `Province/State`,
    Country = `Country/Region`
  ) %>% 
  group_by(Country, State) %>% 
  arrange(Date) %>% 
  mutate(Recovered_day = Recovered - lag(Recovered, default = 0),
         Continent = coords2continent(data.frame(lon=c(Long), lat=c(Lat)))) %>%
  ungroup() %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    Continent = if_else(Country=="Australia","Australia",Continent),
    Continent = if_else(Country=="Canada","North America",Continent),
    Continent = if_else(Country=="China","Asia",Continent),
    Continent = if_else(Country=="Denmark","Europe",Continent),
    Continent = if_else(Country=="France","Europe",Continent),
    Continent = if_else(Country=="Netherlands","Europe",Continent),
    Continent = if_else(Country=="United Kingdom","Europe",Continent),
    Continent = if_else(Country=="US","North America",Continent),
    Continent = if_else(Country=="Monaco","Europe",Continent),
    Continent = if_else(Country=="Philippines","Asia",Continent),
    Continent = if_else(Country=="New Zealand","Europe",Continent),
    Continent = if_else(Country=="Saint Vincent and the Grenadine","North America",Continent)
  ) %>%
  select(Continent, State, Country, Lat, Long, Date, Recovered, Recovered_day) %>% 
  arrange(State, Country, Date) %>% 
  ungroup() %>% 
  data.frame()

active <- confirmed %>% 
  select(-Confirmed_day) %>% 
  left_join(death[, c("Country","Date","Lat","Long","Death")], 
            by=c("Country"="Country","Date"="Date","Lat"="Lat","Long"="Long")) %>% 
  left_join(recovered[, c("Country","Date","Lat","Long","Recovered")], 
            by=c("Country"="Country","Date"="Date","Lat"="Lat","Long"="Long")) %>% 
  mutate(
    Active = Confirmed - (Death + Recovered)
  ) %>% 
  group_by(Country, State) %>% 
  arrange(Date) %>% 
  mutate(Active_day = Active - lag(Active, default = 0)) %>% 
  select(Continent, State, Country, Lat, Long, Date, Active, Active_day) %>% 
  arrange(State, Country, Date) %>% 
  ungroup() %>% 
  data.frame()

# country_data <- country_data %>% 
#   rename(Country = country,
#          Population = `Population in thousands (2017)`) %>% 
#   mutate(Country = if_else(Country=="Iran (Islamic Republic of)","Iran",Country),
#          Country = if_else(Country=="Republic of Korea","Korea, South",Country),
#          Country = if_else(Country=="United States of America","US",Country),
#          Country = if_else(Country=="Viet Nam","Vietnam",Country),
#          Country = if_else(Country=="Russian Federation","Russia",Country),
#          Country = if_else(Country=="Brunei Darussalam","Brunei",Country),
#          Country = if_else(Country=="The former Yugoslav Republic of Macedonia","North Macedonia",Country),
#          Country = if_else(Country=="Republic of Moldova","Moldova",Country),
#          Country = if_else(Country=="Bolivia (Plurinational State of)","Bolivia",Country),
#          Country = if_else(Country=="Democratic Republic of the Congo","Congo (Kinshasa)",Country),
#          Population = Population*1000)

temperature_max <- temperature_max %>% 
  ## convert the dataset from wide to long
  pivot_longer(., cols = 5:ncol(temperature_max), names_to = "Date", values_to = "TempMax") %>% 
  ## Convert the date column to a date format
  mutate(Date = mdy(Date),
         Continent = coords2continent(data.frame(lon=c(Long), lat=c(Lat)))) %>% 
  rename(
    State = `Province/State`,
    Country = `Country/Region`
  ) %>% 
  ungroup() %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    Continent = if_else(Country=="Australia","Australia",Continent),
    Continent = if_else(Country=="Canada","North America",Continent),
    Continent = if_else(Country=="China","Asia",Continent),
    Continent = if_else(Country=="Denmark","Europe",Continent),
    Continent = if_else(Country=="France","Europe",Continent),
    Continent = if_else(Country=="Netherlands","Europe",Continent),
    Continent = if_else(Country=="United Kingdom","Europe",Continent),
    Continent = if_else(Country=="US","North America",Continent),
    Continent = if_else(Country=="Monaco","Europe",Continent),
    Continent = if_else(Country=="Philippines","Asia",Continent),
    Continent = if_else(Country=="New Zealand","Europe",Continent),
    Continent = if_else(Country=="Saint Vincent and the Grenadine","North America",Continent)
  ) %>%
  select(Continent, State, Country, Lat, Long, Date, TempMax) %>% 
  arrange(Continent, State, Country, Date) %>% 
  data.frame()

temperature_min <- temperature_min %>% 
  ## convert the dataset from wide to long
  pivot_longer(., cols = 5:ncol(temperature_min), names_to = "Date", values_to = "TempMin") %>% 
  ## Convert the date column to a date format
  mutate(Date = mdy(Date),
         Continent = coords2continent(data.frame(lon=c(Long), lat=c(Lat)))) %>% 
  rename(
    State = `Province/State`,
    Country = `Country/Region`
  ) %>% 
  ungroup() %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    Continent = if_else(Country=="Australia","Australia",Continent),
    Continent = if_else(Country=="Canada","North America",Continent),
    Continent = if_else(Country=="China","Asia",Continent),
    Continent = if_else(Country=="Denmark","Europe",Continent),
    Continent = if_else(Country=="France","Europe",Continent),
    Continent = if_else(Country=="Netherlands","Europe",Continent),
    Continent = if_else(Country=="United Kingdom","Europe",Continent),
    Continent = if_else(Country=="US","North America",Continent),
    Continent = if_else(Country=="Monaco","Europe",Continent),
    Continent = if_else(Country=="Philippines","Asia",Continent),
    Continent = if_else(Country=="New Zealand","Europe",Continent),
    Continent = if_else(Country=="Saint Vincent and the Grenadine","North America",Continent)
  ) %>%
  select(Continent, State, Country, Lat, Long, Date, TempMin) %>% 
  arrange(Continent, State, Country, Date) %>% 
  data.frame()
