confirmed <- confirmed %>% 
  ## convert the dataset from wide to long
  pivot_longer(., cols = 5:ncol(confirmed), names_to = "Date", values_to = "Confirmed") %>% 
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
