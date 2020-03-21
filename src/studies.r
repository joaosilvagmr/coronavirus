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

### active cases by Continent

active %>% 
  filter(!is.na(Continent)) %>% 
  group_by(Date, Continent) %>% 
  summarise(active_total = sum(Active), active_day = sum(Active_day)) %>% 
  ungroup() %>% 
  reshape2::dcast(Date ~ Continent, value.var = c("active_total")) %>% 
  plot_ly() %>% 
  add_trace(x=~Date, y=~Asia, name = "Asia", type="scatter", mode="lines", line = list(color = 'rgb(222, 41, 16)', width = 4)) %>% 
  add_trace(x=~Date, y=~Europe, name = "Europe", type="scatter", mode="lines", line = list(color = 'rgb(0, 51, 153)', width = 4)) %>% 
  add_trace(x=~Date, y=~Australia, name = "Australia", type="scatter", mode="lines", line = list(color = 'rgb(255,205,0)', width = 4)) %>% 
  add_trace(x=~Date, y=~`North America`, name = "North America", type="scatter", mode="lines", line = list(color = 'rgb(60,59,110)', width = 4)) %>% 
  add_trace(x=~Date, y=~`South America`, name = "South America", type="scatter", mode="lines", line = list(color = 'rgb(0,156,59)', width = 4)) %>% 
  add_trace(x=~Date, y=~Africa, name = "Africa", type="scatter", mode="lines", line = list(color = 'rgb(0,0,0)', width = 4)) %>% 
  layout(xaxis = list(title = "Date"),
         yaxis = list(title = "Number of Active Cases"),
         title = "Evolution of Number of Active Cases of People with Covid-19") %>% 
  api_create(filename = 'Active-Cases')

### deaths

death %>% 
  filter(!is.na(Continent)) %>% 
  group_by(Date, Continent) %>% 
  summarise(death_total = sum(Death), death_day = sum(Death_day)) %>% 
  ungroup() %>% 
  reshape2::dcast(Date ~ Continent, value.var = c("death_total")) %>% 
  plot_ly() %>% 
  add_trace(x=~Date, y=~Asia, name = "Asia", type="scatter", mode="lines", line = list(color = 'rgb(222, 41, 16)', width = 4)) %>% 
  add_trace(x=~Date, y=~Europe, name = "Europe", type="scatter", mode="lines", line = list(color = 'rgb(0, 51, 153)', width = 4)) %>% 
  add_trace(x=~Date, y=~Australia, name = "Australia", type="scatter", mode="lines", line = list(color = 'rgb(255,205,0)', width = 4)) %>% 
  add_trace(x=~Date, y=~`North America`, name = "North America", type="scatter", mode="lines", line = list(color = 'rgb(60,59,110)', width = 4)) %>% 
  add_trace(x=~Date, y=~`South America`, name = "South America", type="scatter", mode="lines", line = list(color = 'rgb(0,156,59)', width = 4)) %>% 
  add_trace(x=~Date, y=~Africa, name = "Africa", type="scatter", mode="lines", line = list(color = 'rgb(0,0,0)', width = 4)) %>% 
  layout(xaxis = list(title = "Date"),
         yaxis = list(title = "Number of Deaths"),
         title = "Evolution of Number of Deaths by Covid-19") %>% 
  api_create(filename = 'Death-Cases')

### temperature max and evolution

top30 <- confirmed %>%
  group_by(Country) %>% 
  summarise(all_cases = sum(Confirmed_day)) %>% 
  arrange(desc(all_cases)) %>% 
  filter(row_number()<=30)

mean_temperature <- temperature_max %>% 
  filter(Country %in% top30$Country) %>%
  group_by(Country, Continent, Date) %>% 
  summarise(temp_max = mean(TempMax)) %>% 
  ungroup() %>% 
  mutate(Week = floor_date(Date, unit="week")) %>% 
  group_by(Country, Continent, Week) %>% 
  summarise(mean_temp = mean(temp_max))
  
weekly_evolution <- confirmed %>% 
  filter(Country %in% top30$Country) %>% 
  group_by(Country, Continent, Date) %>% 
  summarise(Confirmed_cases = sum(Confirmed_day)) %>% 
  ungroup() %>% 
  mutate(Week = floor_date(Date, unit="week")) %>% 
  group_by(Country, Continent, Week) %>% 
  summarise(cases = sum(Confirmed_cases)) %>% 
  ungroup() %>% 
  group_by(Country, Continent) %>% 
  arrange(Week) %>% 
  mutate(evolution = 100*((cases-lag(cases, default = 0))/lag(cases, default = 0))) %>% 
  filter(!is.nan(evolution),
         !is.infinite(evolution),
         evolution > 0)
  
## join data and plot

weekly_evolution %>% 
  left_join(mean_temperature, by=c("Week"="Week","Country"="Country", "Continent"="Continent")) %>% 
  ungroup() %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(
    evolution = round(evolution,2),
    text = paste("Continent: ", Continent, 
                 "\nCountry: ", Country, 
                 "\nWeek: ", Week, 
                 "\nNew Cases: ", cases, 
                 "\nEvolution (%): ", evolution, sep="")) %>% 
  plot_ly(x=~mean_temp, y=~evolution, color=~Continent, text =~text, type = 'scatter', mode = 'markers') %>% 
  layout(xaxis = list(title = "Average Temperature (ºC)"),
         yaxis = list(title = "Variation of Infected (%)"),
         title = "Distribution of Evolution of Infected Cases with the Average Temperature (ºC)") %>% 
  api_create(filename = "Temperature-Evolution")
  #ggplot(aes(x=mean_temp, y=evolution, color=Continent, text=text)) +
  #geom_point(alpha=0.7) +
  #scale_size(range = c(.1, 19), name="Continent") +
  #viridis::scale_color_viridis(discrete=TRUE, guide=FALSE, option="D") +
  #hrbrthemes::theme_ipsum() 

hum <- ggplotly(p, tooltip = "text") %>% 
  layout(legend = list(font = list(size=10)))
  #plot_ly(x=~mean_temp, y=~evolution, color=~Continent, text =~text, type = 'scatter', mode = 'markers') %>% 
  api_create(filename = "Country-Evolution-Temp")
