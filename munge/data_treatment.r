
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
  mutate(Confirmed_day = Confirmed - lag(Confirmed, default = 0)) %>% 
  arrange(State, Country, Date) %>% 
  ungroup %>% 
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
  mutate(Death_day = Death - lag(Death, default = 0)) %>% 
  arrange(State, Country, Date) %>% 
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
  mutate(Recovered_day = Recovered - lag(Recovered, default = 0)) %>% 
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
