install.packages('devtools')
devtools::install_github('cardiomoon/kormaps2014')
install.packages('knitr')

search()

# install.packages('패키지이름'):
# R 패키지 중앙 저장소(CRAN: Comprehnesive R Archive Network)에 등록된 패니지를 다운받고 설치 
# https://cran.r-project.org/
# CRAN에 등록되어 있지 않고, 개인/단체 저장소(github)에 저장된 패키지를 설치할 필요가 있을 수 있음
# devtools::install_github() 함수를 사용해서 github의 패키지를 설치할 수 있음

# 필요한 패키지 메모리 로드
library(tidyverse)
library(ggiraphExtra)
library(kormaps2014)
search()

# maps 패키지의 world 지도에서 한국만 선택 지도 시각화
korea_map <- map_data(map = 'world',
                      region = c('South Korea'))
head(korea_map)
distinct(korea_map, region)
distinct(korea_map, subregion)

ggplot(data = korea_map, 
       mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(color = 'black', fill = 'white') +
  coord_quickmap()

# kormaps2014 패키지에 포함된 한국 지도 데이터 시각화 
head(kormap1)
str(kormap1)
ggplot(data = kormap1,
       mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = 'white', color = 'darkgray') +
  coord_quickmap()

## kormap2014 패키지: kormap1, kormap2, kormap3
str(kormap2)
ggplot(data = kormap2,
       mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = 'white', color = 'darkgray') +
  coord_quickmap()

# Kormap2 지도 데이터프레임을 사용. 서울 지도를 시각화
# sigungu_cd 변수(컬럼)의 값이 '11'로 시작하면 서울시의 구
# str_starts() 함수 사용 
code <- c('1101', '1102', '1201', '1202')
str_starts(code, '11')

# kormaps2에 서울 구 정보들로 이루어진 부분집합
seoul_map <- filter(kormap2, str_starts(sigungu_cd, "11")) 

ggplot(data = seoul_map,
       mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = 'white', color = 'darkgray') +
  coord_quickmap()

# 한국 지도(kormap1) 위에 인구 통계(korpop1) 데이터 시각화 
str(kormap1)
str(korpop1)
head(korpop1) 
# invalid multibyte string 에러 발생
# 데이터 프레임의 컬럼 이름에 인코딩 타입이 다른 문자열이 포함되어 있기 때문
str(changeCode(korpop1))
head(korpop1)

# kormap1과 korpop1를 join 
# kormap1$code, korpop1$code
distinct(kormap1, code)
distinct(korpop1, code)
# korpop1 데이터프레임에서 한글 컬럼이름을 영어로 변경, 필요한 컬럼만 선택
korpop1_df <- korpop1 %>% 
  changeCode() %>% 
  rename(sido_name = 행정구역별_읍면동, pop = 총인구_명) %>% 
  select(code, sido_name, pop)
head(korpop1_df)
str(korpop1_df)

korpop1_df$pop <- as.numeric(korpop1_df$pop)

kormap_pop <- left_join(kormap1, korpop1_df, by = 'code')
head(kormap_pop)
tail(kormap_pop)

# 지도 & 총인구 데이터 시각화 
ggplot(data = kormap_pop,
       mapping = aes(x = long, y = lat, group = group, fill = pop)) +
  geom_polygon(color = 'darkgray') +
  coord_quickmap() + 
  scale_fill_continuous(low = 'white', high = 'darkorange')

kormap1 %>% 
  group_by(code, name1) %>% 
  summarize()

ggChoropleth(data = korpop1_df,  # 통계 데이터 프레임
             map = kormap1,      # 지도 데이터 프레임 
             mapping = aes(fill = pop, map_id = code))

# korpop2(인구 통계 데이터), kormap2(지도 데이터) 사용
# 서울의 구별 총 인구를 지도 위에 시각화 
# 1) ggplot 직접 사용
# 2) ggChoropleth 사용 


# 1) ggplot 직접 사용
head(seoul_map)
str(seoul_map)

head(korpop2)
str(changeCode(korpop2))

seoul_pop <- korpop2 %>% 
  changeCode() %>% 
  rename(gu_name = 행정구역별_읍면동, population = 총인구_명) %>% 
  filter(str_starts(code, '11')) %>% 
  select(code, gu_name, population)
seoul_pop
str(seoul_pop)
seoul_pop$population <- as.numeric(seoul_pop$population)

seoul_map_pop <- left_join(seoul_map, seoul_pop, by = 'code')
head(seoul_map_pop)

ggplot(data = seoul_map_pop,
       mapping = aes(x = long, y = lat, group = group, fill = population)) +
  geom_polygon(color = 'darkgray') +
  coord_quickmap() +
  scale_fill_continuous(low = 'white', high = 'darkorange')

# 2) ggChoropleth 사용
ggChoropleth(data = seoul_pop,
             map = seoul_map,
             mapping = aes(fill = population, map_id = code))

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





