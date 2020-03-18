### confirmed since day one

evolution_of_confirmed <- confirmed %>% 
  filter(!Confirmed==0) #%>% 
#group_by(Country, State) %>% 
#arrange(Date) %>% 
#mutate(day_after = row_number())


### China, Italy, Spain, Portugal, US, UK, France, Germany

countries <- c("China","Italy","Spain","Portugal","US", "United Kingdom", "Germany", "France")

evolution_of_confirmed_v2 <- evolution_of_confirmed %>% 
  group_by(Country, Date) %>% 
  summarise(Confirmed = sum(Confirmed), Confirmed_day = sum(Confirmed_day)) %>% 
  ungroup() %>% 
  group_by(Country) %>% 
  arrange(Date) %>% 
  mutate(day_after = row_number(),
         day_after_10 = if_else(Confirmed>10,1,0)) %>%
  ungroup() %>% 
  group_by(Country) %>% 
  arrange(Date) %>% 
  mutate(day_after_10 = cumsum(day_after_10),
         Confirmed = if_else((Country=="United Kingdom") & (Date == as.Date("2020-03-15")),1391,Confirmed))
#select(Country, Confirmed_day, day_after) #%>% 
#reshape2::dcast(., Country ~ day_after, value.var = "Confirmed_day")

## days after 10 cases



evolution_of_confirmed_v2 %>% 
  filter(Country %in% countries) %>% 
  mutate(Confirmed = log10(Confirmed)) %>% 
  filter(day_after_10 >0 ) %>% 
  plot_ly() %>% 
  add_trace(x=~day_after_10, y=~Confirmed, color=~Country, type = "scatter", mode = "lines") %>% 
  layout(xaxis = list(title = "Number of Days after 10th Case"),
         yaxis = list(title = "Number of Cases (logaritmic scale)"),
         title = "Evolution of Confirmed Cases after the 10th Confirmed Case") %>% 
  api_create(filename = "logaritmic_infected")


evolution_of_confirmed_v2 %>% 
  filter(Country %in% countries) %>% 
  mutate(Confirmed = log10(Confirmed)) %>% 
  plot_ly() %>% 
  add_trace(x=~day_after, y=~Confirmed, color=~Country, type = "scatter", mode = "lines")


evolution_of_confirmed_v2 %>% 
  mutate(Confirmed = Confirmed) %>% 
  filter(Country %in% countries) %>%
  #filter(!Country %in% c("China")) %>% 
  plot_ly() %>% 
  add_trace(x=~day_after, y=~Confirmed, color=~Country, type = "scatter", mode = "lines") %>% 
  layout(xaxis = list(title = "Number of Days after first Confirmated Case"),
         yaxis = list(title = "Number of Cases"),
         title = "Evolution of Confirmed Cases after the first Confirmed Case") %>% 
  api_create(filename = "evolution_infected")
### see countries with most cases


