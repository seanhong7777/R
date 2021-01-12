# 12월 15일 과제

## ggplot2:midwest 데이터셋 - Help 페이지에서 내용 확인
# poptotal(인구수), popadults(성인 인구수), popasian(아시아계 인구수)
# 1) '인구 대비 미성년 인구 백분율' 파생 변수를 추가
midwest %>% 
  mutate(perc_child = (poptotal - popadults) / poptotal)


# 2) 미성년 인구 백분율이 높은 상위 5개 county와 백분율을 출력
midwest %>% 
  mutate(perc_child = (poptotal - popadults) / poptotal) %>% 
  group_by(county) %>% 
  summarize(mean_percchild = sum(perc_child)) %>% 
  arrange(desc(mean_percchild)) %>% 
  head(n = 5)


# 3) 미성년 인구 비율 등급 파생 변수를 추가
# 40% 이상 large, 30 ~ 40% 미만 middle, 30% 미만 small
midwest <- midwest %>% 
  mutate(perc_child = (poptotal - popadults) / poptotal,
         grade = ifelse(perc_child >= 0.4, 'large', 
                        ifelse(perc_child >= 0.3, 'middle', 'small'))) 

# 4) '인구 대비 아시아계 인구 백분율' 파생 변수를 추가하고, 상위 10개 지역의 
# state, county, 아시아계 인구 비율을 출력 
midwest %>% 
  mutate(perc_asian = (popasian / poptotal) * 100) %>% 
  group_by(state, county) %>% 
  summarise(perc_asian, percasian) %>% 
  arrange(desc(perc_asian)) %>% 
  head(n = 10) %>% 
  select(state, county, perc_asian, percasian)


