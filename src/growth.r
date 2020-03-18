# predictions after march 18 and 

today_cases <- evolution_of_confirmed_v2 %>% 
  filter(day_after_10 > 0 & !(Country %in% c("Portugal", "France", "US", "Germany"))) %>%
  bind_rows(evolution_of_confirmed_v2 %>% 
              filter(Country == "Portugal")) %>% 
  bind_rows(evolution_of_confirmed_v2 %>% 
              filter(Date > as.Date("2020-02-23") & Country %in% c("France", "US", "Germany"))) %>% 
  filter(Country %in% countries) %>% 
  group_by(Country) %>% 
  arrange(Date) %>% 
  mutate(day_after = row_number()) %>% 
  ungroup() %>% 
  select(Country, Date, Confirmed)

today_date <- as.Date(Sys.time())
n_days <- 3

for(i in 1:length(countries)) {
  country_selected <- countries[i]

  for(j in 1:n_days) {
  country_row <- today_cases %>% 
    filter(Country == country_selected & Date > today_date) %>% 
    tibble::add_row(Country = country_selected, Date = today_date+(j-1), Confirmed = NA)
  
  if(j == 1) country_test <- country_row else country_test <- country_test %>% bind_rows(country_row)
  }
  country_test <- country_test %>% 
    bind_rows(today_cases %>% 
                filter(Country == country_selected))
  
  if(i == 1 ) today_cases_v2 <- country_test else today_cases_v2 <- today_cases_v2 %>% bind_rows(country_test) %>% arrange(Country, Date)
}

## add day_after column (again)
today_cases_v3 <- today_cases_v2 %>% 
  group_by(Country) %>% 
  arrange(Date) %>% 
  mutate(day_after = row_number()) %>% 
  na_if(0) %>% 
  filter(Country != "China")

### for each country create an exponential model

countries_exp <-  unique(today_cases_v3$Country)

for(n in 1:length(countries_exp)) {

  
  train_data <- today_cases_v3 %>% 
    filter(Country == countries_exp[n])

exponential.model <- lm(log(Confirmed) ~ day_after, data = train_data)

predicted_data <- data.frame(exp(predict(object = exponential.model, newdata = train_data))) %>%
  rename(predict = 1) %>% 
  bind_cols(train_data) %>% 
  select(Country, Date, day_after, Confirmed, predict) %>% 
  mutate(predict = floor(predict)) #%>% 

if(n == 1) predicted_cases <- predicted_data else predicted_cases <- predicted_cases %>% bind_rows(predicted_data)
}


plot_all_countries <- predicted_cases %>% 
  select(-c(day_after)) %>% 
  mutate(#march 18 values source ("https://www.worldometers.info/coronavirus/")
         Confirmed = if_else((Country=="Portugal") & Date==as.Date("2020-03-18"),642,Confirmed),
         Confirmed = if_else((Country=="Spain") & Date==as.Date("2020-03-18"),14769,Confirmed),
         Confirmed = if_else((Country=="Italy") & Date==as.Date("2020-03-18"),35713,Confirmed),
         Confirmed = if_else((Country=="Germany") & Date==as.Date("2020-03-18"),12327,Confirmed),
         Confirmed = if_else((Country=="France") & Date==as.Date("2020-03-18"),9134,Confirmed),
         Confirmed = if_else((Country=="United Kingdom") & Date==as.Date("2020-03-18"),2626,Confirmed),
         Confirmed = if_else((Country=="US") & Date==as.Date("2020-03-18"),8893,Confirmed)) %>% 
  na_if(0) %>% 
  reshape(direction  = "wide", idvar="Date", timevar="Country") %>% 
  plot_ly() %>% 
  add_trace(x=~Date, y=~predict.Portugal, name = "Prediction Portugal", type = 'scatter', mode = 'lines', line = list(color = 'rgb(0, 255, 0)', width = 2.5, dash = 'dot')) %>% 
  add_trace(x=~Date, y=~Confirmed.Portugal, name = "Real Portugal", type = 'scatter', mode = 'lines', line = list(color = 'rgb(0, 255, 0)', width = 2.5)) %>%
  add_trace(x=~Date, y=~predict.Spain, name = "Prediction Spain", type = 'scatter', mode = 'lines', line = list(color = 'rgb(255,215,0)', width = 2.5, dash = 'dot')) %>% 
  add_trace(x=~Date, y=~Confirmed.Spain, name = "Real Spain", type = 'scatter', mode = 'lines', line = list(color = 'rgb(255,215,0)', width = 2.5)) %>% 
  add_trace(x=~Date, y=~predict.France, name = "Prediction France", type = 'scatter', mode = 'lines', line = list(color = 'rgb(255, 0, 0)', width = 2.5, dash = 'dot')) %>% 
  add_trace(x=~Date, y=~Confirmed.France, name = "Real France", type = 'scatter', mode = 'lines', line = list(color = 'rgb(255, 0, 0)', width = 2.5)) %>% 
  add_trace(x=~Date, y=~predict.Germany, name = "Prediction Germany", type = 'scatter', mode = 'lines', line = list(color = 'rgb(139,69,19)', width = 2.5, dash = 'dot')) %>% 
  add_trace(x=~Date, y=~Confirmed.Germany, name = "Real Germany", type = 'scatter', mode = 'lines', line = list(color = 'rgb(139,69,19)', width = 2.5)) %>% 
  add_trace(x=~Date, y=~predict.Italy, name = "Prediction Italy", type = 'scatter', mode = 'lines', line = list(color = 'rgb(0,100,0)', width = 2.5, dash = 'dot')) %>% 
  add_trace(x=~Date, y=~Confirmed.Italy, name = "Real Italy", type = 'scatter', mode = 'lines', line = list(color = 'rgb(0,100,0)', width = 2.5)) %>% 
  add_trace(x=~Date, y=~`predict.United Kingdom`, name = "Prediction UK", type = 'scatter', mode = 'lines', line = list(color = 'rgb(25,25,112)', width = 2.5, dash = 'dot')) %>% 
  add_trace(x=~Date, y=~`Confirmed.United Kingdom`, name = "Real UK", type = 'scatter', mode = 'lines', line = list(color = 'rgb(25,25,112)', width = 2.5)) %>% 
  add_trace(x=~Date, y=~predict.US, name = "Prediction USA", type = 'scatter', mode = 'lines', line = list(color = 'rgb(105,105,105)', width = 2.5, dash = 'dot')) %>% 
  add_trace(x=~Date, y=~Confirmed.US, name = "Real USA", type = 'scatter', mode = 'lines', line = list(color = 'rgb(105,105,105)', width = 2.5)) %>% 
  layout(xaxis = list(title = "Date"),
          yaxis = list(title = "Number of Cases"),
          title = "Prediction of future cases (using exponential curve)")
api_create(plot_all_countries, filename = "all_countries_exponential")

# plot_all_countries_log <- predicted_cases %>% 
#   select(-c(day_after)) %>% 
#   mutate(
#     Confirmed = if_else((Country=="Portugal") & Date==as.Date("2020-03-18"),642,Confirmed),
#     Confirmed = if_else((Country=="Spain") & Date==as.Date("2020-03-18"),14769,Confirmed),
#     Confirmed = if_else((Country=="Italy") & Date==as.Date("2020-03-18"),35713,Confirmed),
#     Confirmed = if_else((Country=="Germany") & Date==as.Date("2020-03-18"),12327,Confirmed),
#     Confirmed = if_else((Country=="France") & Date==as.Date("2020-03-18"),9134,Confirmed),
#     Confirmed = if_else((Country=="United Kingdom") & Date==as.Date("2020-03-18"),2626,Confirmed),
#     Confirmed = if_else((Country=="US") & Date==as.Date("2020-03-18"),8893,Confirmed),
#     predict = log10(predict),
#     Confirmed = log10(Confirmed)) %>% 
#   na_if(0) %>% 
#   reshape(direction  = "wide", idvar="Date", timevar="Country") %>% 
#   plot_ly() %>% 
#   add_trace(x=~Date, y=~predict.Portugal, name = "Prediction Portugal", type = 'scatter', mode = 'lines', line = list(color = 'rgb(0, 255, 0)', width = 2.5, dash = 'dot')) %>% 
#   add_trace(x=~Date, y=~Confirmed.Portugal, name = "Real Portugal", type = 'scatter', mode = 'lines', line = list(color = 'rgb(0, 255, 0)', width = 2.5)) %>%
#   add_trace(x=~Date, y=~predict.Spain, name = "Prediction Spain", type = 'scatter', mode = 'lines', line = list(color = 'rgb(255,215,0)', width = 2.5, dash = 'dot')) %>% 
#   add_trace(x=~Date, y=~Confirmed.Spain, name = "Real Spain", type = 'scatter', mode = 'lines', line = list(color = 'rgb(255,215,0)', width = 2.5)) %>% 
#   add_trace(x=~Date, y=~predict.France, name = "Prediction France", type = 'scatter', mode = 'lines', line = list(color = 'rgb(255, 0, 0)', width = 2.5, dash = 'dot')) %>% 
#   add_trace(x=~Date, y=~Confirmed.France, name = "Real France", type = 'scatter', mode = 'lines', line = list(color = 'rgb(255, 0, 0)', width = 2.5)) %>% 
#   add_trace(x=~Date, y=~predict.Germany, name = "Prediction Germany", type = 'scatter', mode = 'lines', line = list(color = 'rgb(139,69,19)', width = 2.5, dash = 'dot')) %>% 
#   add_trace(x=~Date, y=~Confirmed.Germany, name = "Real Germany", type = 'scatter', mode = 'lines', line = list(color = 'rgb(139,69,19)', width = 2.5)) %>% 
#   add_trace(x=~Date, y=~predict.Italy, name = "Prediction Italy", type = 'scatter', mode = 'lines', line = list(color = 'rgb(0,100,0)', width = 2.5, dash = 'dot')) %>% 
#   add_trace(x=~Date, y=~Confirmed.Italy, name = "Real Italy", type = 'scatter', mode = 'lines', line = list(color = 'rgb(0,100,0)', width = 2.5)) %>% 
#   add_trace(x=~Date, y=~`predict.United Kingdom`, name = "Prediction UK", type = 'scatter', mode = 'lines', line = list(color = 'rgb(25,25,112)', width = 2.5, dash = 'dot')) %>% 
#   add_trace(x=~Date, y=~`Confirmed.United Kingdom`, name = "Real UK", type = 'scatter', mode = 'lines', line = list(color = 'rgb(25,25,112)', width = 2.5)) %>% 
#   add_trace(x=~Date, y=~predict.US, name = "Prediction USA", type = 'scatter', mode = 'lines', line = list(color = 'rgb(105,105,105)', width = 2.5, dash = 'dot')) %>% 
#   add_trace(x=~Date, y=~Confirmed.US, name = "Real USA", type = 'scatter', mode = 'lines', line = list(color = 'rgb(105,105,105)', width = 2.5)) %>% 
#   layout(xaxis = list(title = "Date"),
#          yaxis = list(title = "Number of Cases"),
#          title = "Prediction of future cases (using exponential curve)")
# api_create(plot_all_countries_log, filename = "countries_logaritmic_growth")
