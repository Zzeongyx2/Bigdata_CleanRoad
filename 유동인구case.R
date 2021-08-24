# rm(list = ls())

# 패키지
# install.packages('dplyr')
library(dplyr)
# install.packages('data.table')
library(data.table)
# install.packages('stringr')
library(stringr)



# case유동인구

# 생활인구 동별
setwd('C:/Users/user/Desktop/클린로드/DATA/유동인구')
listfile <- list.files()
life <- list()
for(i in 1:(length(listfile))) {
  life[[i]] <- data.table::fread(listfile[i], stringsAsFactors = F,
                                 encoding = 'UTF-8')
}
str(life)
life <- bind_rows(life)

# 행정동별 생활인구 합
summary(life)
life1 <- life %>% group_by(행정동코드, 기준일ID) %>%
  summarise(일생활인구 = sum(총생활인구수))

life2 <- life1 %>% group_by(행정동코드) %>%
  summarise(생활인구합 = sum(일생활인구),
            일평균생활인구 = mean(일생활인구))

code <- read.csv('C:/Users/user/Desktop/클린로드/DATA/코드정보.csv',stringsAsFactors=F)
code <- code %>% dplyr::rename(행정동코드1 = 행정동코드)
head(code)

code$행정동코드 <- paste0(code$시군구코드, code$행정동코드1)
code$행정동코드 <- as.integer(code$행정동코드)

# 행정동명 합치기
code1 <- code %>% filter(시도명 == '서울특별시')
head(code1)

# 제 바꾸기(제1동 , 제2동)
code1[str_detect(code1$행정동명,'(제)'),]

je<- str_split_fixed(code1$행정동명, '(제)', 2)

je<- as.data.frame(je)

je<- paste0(je$V1, je$V2)

je<- gsub("[\\$.@]", '/', je)

je[str_detect(je, '제')]

code1$행정동명 <- je

# 행정동 코드로 행정동명 넣기
life3 <- life2 %>% left_join(code1 %>% dplyr::select(행정동코드, 행정동명))

# 중복값 제거
life3 <- life3[-which(duplicated(life3$행정동코드)),]
table(life3$행정동명)
# csv저장
write.csv(life3, 'C:/Users/user/Desktop/클린로드/DATA/동별생활인구.csv')

# 승하차 : 지하철 승하차 , 버스 승하차 따로 or 두 개 합
# 1. 지하철 승하차
# 서울 내 전철역 위치 구하기
subway <- read.csv('C:/Users/user/Desktop/클린로드/DATA/전국도시철도역사정보표준데이터.csv',stringsAsFactors=F)
str(subway)
subway <- subway %>% dplyr::select(역번호, 역사명, 역위도, 역경도, 역사도로명주소)

# 서울만 고르기
subway <- subway %>% filter(substr(역사도로명주소, 1, 2) %in% '서울')

# 역 명에 괄호 제거
subway$역사명 <- str_trim(subway$역사명)
subway$역사명 <- gsub('\\(.*?\\)', '', subway$역사명)

# 역 제거
subway[str_detect(subway$역사명,'(역)'),]
subway$역사명 <- ifelse(str_sub(subway$역사명, start = -1) =='역', 
                     substr(subway$역사명, 1, nchar(subway$역사명)-1),
                     subway$역사명)
subway[str_detect(subway$역사명,'(역)'),]

# 중복역 제거
subway <- subway[-which(duplicated(subway$역사명)),]
table(subway$역사명)

# csv 저장
write.csv(subway, 'C:/Users/user/Desktop/클린로드/DATA/서울지하철위치.csv')

# 행정동shp 파일 이름 바꾸기
dong <- read.csv('C:/Users/user/Desktop/클린로드/DATA/행정동.csv',stringsAsFactors=F)
dong$name <- str_trim(dong$name)
dong$name <- gsub("[\\$·@]", '/', dong$name)
table(dong$name)

# 다시 csv로 저장
write.csv(dong, 'C:/Users/user/Desktop/클린로드/DATA/행정동.csv')

# 지하철 승하차
subway_p <- read.csv('C:/Users/user/Desktop/클린로드/DATA/지하철일별승하차승객수.csv',stringsAsFactors=F)
summary(subway_p)

# 역명 괄호제거
subway_p <- subway_p %>% dplyr::rename(역사명 = 역명)
subway_p$역사명 <- str_trim(subway_p$역사명)
subway_p$역사명 <- gsub('\\(.*?\\)', '', subway_p$역사명)
subway_p$역사명 <- str_trim(subway_p$역사명)

# 역 제거
subway_p[str_detect(subway_p$역사명,'(역)'),]
subway_p$역사명 <- ifelse(str_sub(subway_p$역사명, start = -1) =='역', 
                     substr(subway_p$역사명, 1, nchar(subway_p$역사명)-1),
                     subway_p$역사명)
subway_p[str_detect(subway_p$역사명,'(역)'),]

# 역별 승하차수 구하기
subway_p2 <- subway_p %>% group_by(역사명) %>%
  summarise(전철승하차합 = sum(승하차총승객수),
            전철승하차평균 = mean(승하차총승객수))

# 다시저장
write.csv(subway_p2, 'C:/Users/user/Desktop/클린로드/DATA/전철승하차합.csv')


# 전철역_행정동 불러오기
subway_dong <- read.csv('C:/Users/user/Desktop/클린로드/DATA/서울지하철위치_행정동.csv',stringsAsFactors=F)
subway_dong1 <- subway_dong %>% left_join(subway_p2)
summary(subway_dong1)

subway_dong2 <- subway_dong1 %>% na.omit() %>% group_by(name) %>%
  summarise(전철승하차합 = sum(전철승하차합),
            전철승하차평균 = mean(전철승하차평균))

subway_dong2 <- subway_dong2 %>% dplyr::rename(행정동명 = name)

# 생활인구 데이터에 합치기
life4 <- life3 %>% left_join(subway_dong2)
summary(life4)
# 여기 na 값 226개는 전철역이 없는 행정동...

# R 데이터 저장
save(list = ls(), file = 'C:/Users/user/Desktop/클린로드/DATA/유동인구.RData')
load('C:/Users/user/Desktop/클린로드/DATA/유동인구.RData')

# 버스승하차합, 버스승하차 평균 구하기
bus_p <- data.table::fread('C:/Users/user/Desktop/클린로드/DATA/버스일별승하차승객수.csv',stringsAsFactors=F)

# na제거
bus_p <- bus_p %>% na.omit()

# 버스정류장별 일승하차합, 일승하차평균
bus_p2 <- bus_p %>% dplyr::group_by(버스정류장ARS번호) %>%
  summarise(버스승하차합 = sum(일승하차승객수),
            버스승하차평균 = mean(일승하차승객수))

# csv저장
write.csv(bus_p2, 'C:/Users/user/Desktop/클린로드/DATA/버스승하차.csv')

# 동별 버스 승하차 합 평균
bus_dong <- read.csv('C:/Users/user/Desktop/클린로드/DATA/서울버스정류장위치_행정동.csv',stringsAsFactors=F)
bus_dong <- bus_dong %>% na.omit()

bus_dong <- bus_dong %>% dplyr::rename(행정동명 = name)

bus_dong2 <- bus_dong %>% group_by(행정동명) %>%
  summarise(버스승하차합 = sum(bus_sum),
            버스승하차평균 = mean(bus_mean))

life5 <- life4 %>% left_join(bus_dong2)
summary(life5)

# 대중교통승하차합
life5_1 <- life5
life5_1$전철승하차합 <- ifelse(is.na(life5_1$전철승하차합) == T, 0, life5_1$전철승하차합)
life5_1$전철승하차평균 <- ifelse(is.na(life5_1$전철승하차평균) == T, 0, life5_1$전철승하차평균)
life5_1$버스승하차합 <- ifelse(is.na(life5_1$버스승하차합) == T, 0, life5_1$버스승하차합)
life5_1$버스승하차평균 <- ifelse(is.na(life5_1$버스승하차평균) == T, 0, life5_1$버스승하차평균)
life5_1$대중교통승하차합 <- life5_1$전철승하차합+life5_1$버스승하차합
life5_1$대중교통승하차평균 <- (life5_1$전철승하차합+life5_1$버스승하차합)/2
summary(life5_1)
life6 <- life5 %>% left_join(life5_1 %>% dplyr::select(행정동코드, 대중교통승하차합, 대중교통승하차평균))
summary(life6)

# 버스정류장 개수
bus_dong3 <- bus_dong %>% group_by(행정동명) %>%
  summarise(버스정류장개수 = n())

life7 <- life6 %>% left_join(bus_dong3)
summary(life7)

# 건물 정보 가져오기
building <- read.csv('C:/Users/user/Desktop/클린로드/DATA/건물좌표_행정동.csv',stringsAsFactors=F)
building <- building %>% na.omit()
summary(building)

table(building$세부용도명)

building$세부용도명 <- str_trim(building$세부용도명)
# 동별 사무실 개수
building_sa <- building %>% filter(세부용도명 %in% c('기타공공업무시설',
                                                '기타일반업무시설',
                                                '기타판매및영업시설',
                                                '업무시설',
                                                '일반업무시설',
                                                '판매및영업시설',
                                                '판매시설'))

building_sic <- building %>% filter(세부용도명 %in% c('일반음식점',
                                                '휴게음식점'))

building_sang <- building %>% filter(세부용도명 %in% c('백화점',
                                                  '도매시장',
                                                  '기타소매시장',
                                                  '기타판매시설',
                                                  '상점',
                                                  '소매점',
                                                  '쇼핑센터',
                                                  '슈퍼마켓',
                                                  '시장'))

building_sa2 <- building_sa %>% group_by(name) %>%
  summarise(사무실수 = n())

building_sic2 <- building_sic %>% group_by(name) %>%
  summarise(식당수 = n())

building_sang2 <- building_sang %>% group_by(name) %>%
  summarise(상점수 = n())

building_sa2 <- building_sa2 %>% dplyr::rename(행정동명 = name)
building_sic2 <- building_sic2 %>% dplyr::rename(행정동명 = name)
building_sang2 <- building_sang2 %>% dplyr::rename(행정동명 = name)


life8 <- life7 %>% left_join(building_sa2) %>% 
  left_join(building_sic2) %>% left_join(building_sang2)
summary(life8)

# 신호등 개수
sin <- read.csv('C:/Users/user/Desktop/클린로드/DATA/신호등_행정동.csv',stringsAsFactors=F)
summary(sin)
sin2 <- sin %>% group_by(name) %>% summarise(신호등수 = n())
sin2 <- sin2 %>% dplyr::rename(행정동명 = name)

data <- life8 %>% left_join(sin2)

summary(data)

# min max 정규화
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}
summary(data)

data$사무실수 <- ifelse(is.na(data$사무실수)==T, 0, data$사무실수)
data$식당수 <- ifelse(is.na(data$식당수)==T, 0, data$식당수)
data$상점수 <- ifelse(is.na(data$상점수)==T, 0, data$상점수)
data$신호등수 <- ifelse(is.na(data$신호등수)==T, 0, data$신호등수)


# 회귀분석
summary(m <- lm(normalize(생활인구합) ~ 대중교통승하차합 + 버스정류장개수 + 사무실수 + 식당수 + 상점수 + 신호등수, data))
summary(m1 <- lm(normalize(생활인구합) ~ 대중교통승하차합 + 버스정류장개수 + 사무실수 + 신호등수, data))

# 유동인구_score 구하기
summary(d <- predict(m1, data %>% dplyr::select(대중교통승하차합, 버스정류장개수, 사무실수, 신호등수)))
data$predict <- d

write.csv(data, 'C:/Users/user/Desktop/클린로드/DATA/유동인구case.csv')
save(m1, data, file = 'C:/Users/user/Desktop/클린로드/DATA/최종.RData')
load('C:/Users/user/Desktop/클린로드/DATA/최종.RData')
par(mfrow=c(2,2))
plot(m1)

# 날씨 가중치 구하기
nalc <- read.csv('C:/Users/user/Desktop/클린로드/DATA/finall.csv', stringsAsFactors = F)

summary(nalc)
nalc <- nalc %>% mutate(nalc_score = tem + hotday + mg)

nalc$nalc_score <- normalize(nalc$nalc_score) + 1

write.csv(nalc, 'C:/Users/user/Desktop/클린로드/DATA/날씨가중치.csv')

# 유동인구 순위 barplot
ud20 <- read.csv('C:/Users/user/Desktop/클린로드/리얼최종/유동인구_20.csv')
library(ggplot2)
ud20 <- ud20[order(ud20$ud_score, decreasing = T),]

ggplot(ud20, aes( reorder(역사명, ud_score), ud_score, fill = 역사명)) + 
  geom_bar(stat="identity", width=0.5) + 
  coord_flip() + scale_y_continuous(0,1.75) + 
  labs(x = "역명", y = "유동인구 score") + 
  ggtitle("         역별 유동인구 score") + 
  theme(plot.title = element_text(family = "serif", face = "bold")) +
  theme_minimal()