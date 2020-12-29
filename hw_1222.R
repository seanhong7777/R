# 한국 지도 데이터 다운로드
# http://www.gisdeveloper.co.kr/?p=2332

# https://github.com/nytimes/covid-19-data
# 미국의 Covid-19(코로나 바이러스) 데이터가 업데이트되는 github
# https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html
# 위의 github의 데이터로 시각화한 그래프들이 있는 NY Times 사이트 
# github의 설명을 잘 읽어보고,
# 미국의 주별 코로나 확진자 수를 지도 위에 시각화하기 위해 필요한 데이터를 찾아서
# 미국 지도 위에 현재 확진자 수를 시각화해보세요 (가장 마지막 날의 확진자 수)

# NY Times 의 여러 시각화 그래프들을 보시고,
# 같은 그래프들을 만들어 보세요

# 대한민국의 시도별 코로나 바이러스 확진자수 데이터를 찾을 수 있으면
# 대한민국의 지도 위에 확진자수를 시각화해보세요


library(tidyverse)
library(ggiraphExtra)
library(mapproj)
library(dplyr)
library(data.table)
library(gridExtra)
library(ggthemes)
library(plotly)
library(dygraphs)
library(xts)


################################################################################
################################################################################
# 전처리

# 지도 데이터 불러오기:
state_map <- map_data(map = 'state')
state_map <- rename(state_map, state = region)
head(state_map)
str(state_map)   # state는 region(소문자)
distinct(state_map, state)

# COVID 데이터 불러오기
us_covid <- read.csv('D:/Daddy/Data Science/lab/datasets/us-counties.csv')
us_covid
str(us_covid)    # state로 되어 있고 Title 형식 
us_covid <- us_covid %>% select(!fips) 

us_covid$state <- tolower(us_covid$state)
us_covid$date <- as.Date(us_covid$date)
head(us_covid)


################################################################################
################################################################################
# Daily graph

# Daily confirmed case 
daily_case <- us_covid %>% 
  group_by(date) %>% 
  summarize(sum_case = sum(cases), .groups = 'drop')
daily_case
summary(daily_case)

ggplot(data = daily_case, aes(x = date, y = sum_case))+
  geom_line(size = 1.6, alpha = 0.8, col = 'gray65')+
  geom_smooth(method = 'loess', color = 'firebrick3', size = 2.2, 
              formula = y ~ x, fill = 'firebrick4', alpha = 0.32) +
  theme_fivethirtyeight()+
  #scale_y_continuous(limits = c(0,1900000))+
  scale_x_date(date_labels = "%b %d", date_breaks = "months")+
  labs(x="", y="", title="Daily Confirmed Case", subtitle = "# of cases",
       caption = '[Month]')

# Daily death toll 
daily_death <- us_covid %>% 
  filter(!is.na(deaths)) %>% 
  group_by(date) %>% 
  summarize(sum_death = sum(deaths), .groups = 'drop')
daily_death
summary(daily_death)

ggplot(data = daily_death, aes(x = date, y = sum_death))+
  geom_line(size = 1.6, alpha = 0.8, col = 'gray65')+
  geom_smooth(method = 'loess', color = 'firebrick3', size = 2.2, 
              formula = y ~ x, fill = 'firebrick4', alpha = 0.32) +
  theme_fivethirtyeight()+
  #scale_y_continuous(limits = c(0,320000))+
  scale_x_date(date_labels = "%b %d", date_breaks = "months")+
  labs(x="", y="", title="Daily Confirmed Case", subtitle = "# of cases")
  
# dyGraph로 표현하기
confirmed_case <- xts(daily_case$sum_case, order.by = daily_case$date)
death <- xts(daily_death$sum_death, order.by = daily_death$date)

daily_dygraph <- cbind(confirmed_case, death)
colnames(daily_case) <- c("Confirmed_case", "Death")
head(daily_dygraph)

dygraph(daily_dygraph) %>% dyRangeSelector()


# State 별 Daily confirmed case
daily_state_case <- us_covid %>% 
  group_by(date, state) %>% 
  summarize(sum_case = sum(cases), .groups = 'drop')
daily_state_case
summary(daily_case)

ggplot(data = daily_state_case, aes(x = date, y = sum_case))+
  geom_line(size = 0.5, alpha = 0.5, col = 'gray65')+
  geom_smooth(method = 'loess', color = 'firebrick3', size = 2.2, 
              formula = y ~ x, fill = 'firebrick4', alpha = 0.32) +
  theme_fivethirtyeight()+
  facet_wrap( ~ state, ncol = 10) +
  scale_x_date(date_labels = "%b %d", date_breaks = "months")+
  labs(x="", y="", title="Daily Confirmed Case", subtitle = "# of cases")


################################################################################
# Confirmed case by county
state_case <- us_covid %>% 
  group_by(date, state) %>% 
  summarize(sum_case = sum(cases), .groups = 'drop') 
state_case

state_covid <- left_join(state_map, state_case, by = c('state' = 'state'))
state_covid

ggplot(data = state_covid, 
       mapping = aes(x = long, y = lat, group = group, fill = sum_case))+
  geom_polygon(color = 'black') +
  coord_quickmap() +
  scale_fill_continuous(low = 'white', high = 'darkred')

state_map

ggChoropleth(data = state_case, map = state_map, 
             mapping = aes(map_id = state, fill = sum_case))



