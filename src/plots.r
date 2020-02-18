### join to made a complete dataset
complete_dataset <- confirmed %>% 
  left_join(recovered %>% 
            select(-c(Lat, Long)), by=c("Date"="Date","State"="State","Country"="Country")) %>% 
  left_join(death %>%
              select(-c(Lat, Long)), by=c("Date"="Date","State"="State","Country"="Country"))

##### latest values by Country

day_evolution <- complete_dataset %>% 
  group_by(Date) %>% 
  summarise(
    Confirmed = sum(Confirmed_day), 
    Recovered = sum(Recovered_day), 
    Death = sum(Death_day)
    )

####

complete_dataset %>% 
  group_by(Date) %>% 
  summarise(
    Confirmed = sum(Confirmed),
    Recovered = sum(Recovered),
    Death = sum(Death)
  ) %>% 
  plot_ly() %>% 
  add_lines(x=~Date, y=~Confirmed, name = "Confirmed", type ="scatter", mode = "lines", line = list(color = 'rgb(0,191,255)')) %>% 
  add_lines(x=~Date, y=~Recovered, name = "Recovered", type ="scatter", mode = "lines", line = list(color = '	rgb(0,128,0)')) %>% 
  add_lines(x=~Date, y=~Death, name = "Death", type ="scatter", mode = "lines", line = list(color ='rgb(255,0,0)')) %>% 
  layout(
    title = "Total evolution of Infected, Recovered and Deaths caused by the Coronavirus",
    xaxis = list(title = ""),
    yaxis = list(title="Number of Occurences"),
    legend = list(orientation='h')
  )

day_evolution %>% 
  plot_ly() %>% 
  add_lines(x=~Date, y=~Confirmed, name = "Confirmed", type ="scatter", mode = "lines", line = list(color = 'rgb(0,191,255)')) %>% 
  add_lines(x=~Date, y=~Recovered, name = "Recovered", type ="scatter", mode = "lines", line = list(color = '	rgb(0,128,0)')) %>% 
  add_lines(x=~Date, y=~Death, name = "Death", type ="scatter", mode = "lines", line = list(color ='rgb(255,0,0)')) %>% 
  layout(
    title = "Daily evolution of Infected, Recovered and Deaths caused by the Coronavirus",
    xaxis = list(title = ""),
    yaxis = list(title="Number of Occurences"),
    legend = list(orientation='h')
  )

### death rate evolution

complete_dataset %>%
  group_by(Date) %>% 
  summarise(
    Confirmed = sum(Confirmed),
    Recovered = sum(Recovered),
    Death = sum(Death)
  ) %>% 
  mutate(death_rate = (Death/(Confirmed+Death))*100) %>% 
  plot_ly() %>% 
  add_lines(x=~Date, y=~death_rate, type ="scatter", mode = "lines", line = list(color ='rgb(255,0,0)')) %>% 
  layout(
    title = "Daily evolution of the Death Rate caused by the Coronavirus",
    xaxis = list(title = ""),
    yaxis = list(title="Percentage")
  )

### world map

g <- list(
  scope = 'world',
  showland = TRUE,
  showcountries = TRUE,
  showlakes = TRUE,
  lakescolor = toRGB("blue"),
  landcolor = toRGB("gray85"),
  countrycolor = toRGB("gray95"),
  subunitcolor = toRGB("white")
)


## world map

complete_dataset %>% 
  select(-c(Confirmed_day, Recovered_day, Death_day)) %>% 
  group_by(State, Country) %>% 
  slice(which.max(Date)) %>%
  mutate(text_plot=paste(
    "Country: ", Country, "\n", 
    "State: ", State, "\n",
    "Confirmed Cases: ", Confirmed, "\n",
    "Recovered: ", Recovered, "\n",
    "Deaths: ", Death, sep="")
  ) %>%
  plot_geo(locationmode = 'world', sizes = c(1, 250)) %>%
  add_markers(
    x = ~Long, y = ~Lat, size = ~Confirmed, hoverinfo = "text",
    text=~text_plot, marker = list(
      color = 'rgba(155, 0, 0,1)',
      line = list(color = 'rgba(155, 0, 0, 1)',
                  width = 2)
    )
  ) %>%
  layout(title = 'Coronavirus Distribution of Confirmed Cases (Worldwide)', geo = g)
  
  
  
## Asia

g <- list(
  scope = 'asia',
  showland = TRUE,
  showcountries = TRUE,
  showlakes = TRUE,
  lakescolor = toRGB("blue"),
  landcolor = toRGB("gray85"),
  countrycolor = toRGB("gray95"),
  subunitcolor = toRGB("white")
)


## world map

complete_dataset %>% 
  select(-c(Confirmed_day, Recovered_day, Death_day)) %>% 
  group_by(State, Country) %>% 
  slice(which.max(Date)) %>%
  mutate(text_plot=paste(
    "Country: ", Country, "\n", 
    "State: ", State, "\n",
    "Confirmed Cases: ", Confirmed, "\n",
    "Recovered: ", Recovered, "\n",
    "Deaths: ", Death, sep="")
  ) %>%
  plot_geo(locationmode = 'asia', sizes = c(1, 2500)) %>%
  add_markers(
    x = ~Long, y = ~Lat, size = ~Confirmed, hoverinfo = "text",
    text=~text_plot, marker = list(
      color = 'rgba(155, 0, 0,1)',
      line = list(color = 'rgba(155, 0, 0, 1)'))
  ) %>%
  layout(title = 'Coronavirus Distribution of Confirmed Cases (Asia)', geo = g)


