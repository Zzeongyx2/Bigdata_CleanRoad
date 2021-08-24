# rm(list = ls())
# 패키지
# install.packages('dplyr')
library(dplyr)
# install.packages('reshape')
library(reshape)
# install.packages('stringr')
library(stringr)

# 데이터 불러오기(종로구 없음)
d20 <- read.csv('C:/Users/user/Desktop/클린로드/DATA/날씨/구별날씨_2020.csv')
d19 <- read.csv('C:/Users/user/Desktop/클린로드/DATA/날씨/구별날씨_2019.csv')
d18 <- read.csv('C:/Users/user/Desktop/클린로드/DATA/날씨/구별날씨_2018.csv')
d17 <- read.csv('C:/Users/user/Desktop/클린로드/DATA/날씨/구별날씨_2017.csv')

# 이름변경
names(d20) <- c('no','name', 'date', 'tem', 'rain', 'hum')
names(d19) <- c('no','name', 'date', 'tem', 'rain', 'hum')
names(d18) <- c('no','name', 'date', 'tem', 'rain', 'hum')
names(d17) <- c('no','name', 'date', 'tem', 'rain', 'hum')

# 서울시 구만 추출
d20 <- d20 %>% filter(!name %in% c('한강', '현충원', '관악(레)', '남현', '북악산'))
table(d20$name)
d19 <- d19 %>% filter(!name %in% c('한강', '현충원', '관악(레)', '남현', '북악산'))
table(d20$name)
d18 <- d18 %>% filter(!name %in% c('한강', '현충원', '관악(레)', '남현', '북악산'))
table(d20$name)
d17 <- d17 %>% filter(!name %in% c('한강', '현충원', '관악(레)', '남현', '북악산'))
table(d20$name)

# 데이터 불러오기(종로구)
j20 <- read.csv('C:/Users/user/Desktop/클린로드/DATA/날씨/종로구_2020.csv')
j19 <- read.csv('C:/Users/user/Desktop/클린로드/DATA/날씨/종로구_2019.csv')
j18 <- read.csv('C:/Users/user/Desktop/클린로드/DATA/날씨/종로구_2018.csv')
j17 <- read.csv('C:/Users/user/Desktop/클린로드/DATA/날씨/종로구_2017.csv')

# 이름 변경
names(j20) <- c('no','name', 'date', 'tem', 'rain', 'hum')
names(j19) <- c('no','name', 'date', 'tem', 'rain', 'hum')
names(j18) <- c('no','name', 'date', 'tem', 'rain', 'hum')
names(j17) <- c('no','name', 'date', 'tem', 'rain', 'hum')

# 서울 통합 데이터 만들기(종로구 합치기)
d20 <- d20 %>% bind_rows(j20)
d19 <- d19 %>% bind_rows(j19)
d18 <- d18 %>% bind_rows(j18)
d17 <- d17 %>% bind_rows(j17)

# 불필요한 데이터 삭제
rm(j20, j19, j18, j17)

# 기상청 -> 동작구 / 서울 -> 종로구로 바꾸기
d20 <- d20 %>% mutate(name = ifelse(name == '기상청', '동작',
                                    ifelse(name == '서울', '종로', name)))
d19 <- d19 %>% mutate(name = ifelse(name == '기상청', '동작',
                                    ifelse(name == '서울', '종로', name)))
d18 <- d18 %>% mutate(name = ifelse(name == '기상청', '동작',
                                    ifelse(name == '서울', '종로', name)))
d17 <- d17 %>% mutate(name = ifelse(name == '기상청', '동작',
                                    ifelse(name == '서울', '종로', name)))

# 양끝 공백 제거
d20$name <- str_trim(d20$name)
d19$name <- str_trim(d19$name)
d18$name <- str_trim(d18$name)
d17$name <- str_trim(d17$name)

# 중구에만 '구' 붙으므로 이를 제거 후 모든 구 뒤에 '구' 붙이기
d20$name <- ifelse(d20$name == '중구', '중', d20$name)
d20$name <- paste0(d20$name, '구')
d19$name <- ifelse(d19$name == '중구', '중', d19$name)
d19$name <- paste0(d19$name, '구')
d18$name <- ifelse(d18$name == '중구', '중', d18$name)
d18$name <- paste0(d18$name, '구')
d17$name <- ifelse(d17$name == '중구', '중', d17$name)
d17$name <- paste0(d17$name, '구')

# 일자별 최고기온
d20_1 <- d20 %>% dplyr::mutate(date = substr(date, 1, 10))
d19_1 <- d19 %>% dplyr::mutate(date = substr(date, 1, 10))
d18_1 <- d18 %>% dplyr::mutate(date = substr(date, 1, 10))
d17_1 <- d17 %>% dplyr::mutate(date = substr(date, 1, 10))

# 678월 뽑기
d20_2 <- d20_1 %>% dplyr::filter(substr(date, 6,7) %in% c('06','07','08'))
d19_2 <- d19_1 %>% dplyr::filter(substr(date, 6,7) %in% c('06','07','08'))
d18_2 <- d18_1 %>% dplyr::filter(substr(date, 6,7) %in% c('06','07','08'))
d17_2 <- d17_1 %>% dplyr::filter(substr(date, 6,7) %in% c('06','07','08'))
summary(d20_2)
d20_2 <- d20_2 %>% filter(is.na(tem) == F)
d19_2 <- d19_2 %>% filter(is.na(tem) == F)
d18_2 <- d18_2 %>% filter(is.na(tem) == F)
d17_2 <- d17_2 %>% filter(is.na(tem) == F)

d20_3 <- d20_2 %>% group_by(name, date) %>% summarise(tem_max = max(tem))
d19_3 <- d19_2 %>% group_by(name, date) %>% summarise(tem_max = max(tem))
d18_3 <- d18_2 %>% group_by(name, date) %>% summarise(tem_max = max(tem))
d17_3 <- d17_2 %>% group_by(name, date) %>% summarise(tem_max = max(tem))

dd_choi <- d20_3 %>% bind_rows(d19_3, d18_3, d17_3)

write.csv(dd_choi, 'C:/Users/user/Desktop/클린로드/DATA/구별일자별최고기온.csv')

# 연도별 폭염 날짜 구하기
d20_1 <- d20 %>% filter(tem >= 33)
d19_1 <- d19 %>% filter(tem >= 33)
d18_1 <- d18 %>% filter(tem >= 33)
d17_1 <- d17 %>% filter(tem >= 33)

# 연도별 구별 최고기온
d20_2 <- d20_1 %>% group_by(name) %>% summarise(tem_max = max(tem, na.rm = T))
d19_2 <- d19_1 %>% group_by(name) %>% summarise(tem_max = max(tem, na.rm = T))
d18_2 <- d18_1 %>% group_by(name) %>% summarise(tem_max = max(tem, na.rm = T))
d17_2 <- d17_1 %>% group_by(name) %>% summarise(tem_max = max(tem, na.rm = T))

# 데이터 합치기
dd_mean <- d20_2 %>% bind_rows(d19_2, d18_2, d17_2)

# 구별 연 최고기온의 평균
dd_mean <- dd_mean %>% group_by(name) %>% summarise(tem_mean = mean(tem_max, na.rm = T))

# barplot
barplot(dd_mean$tem_mean, names.arg = dd_mean$name, ylim = c(34,38), xpd = F)

# csv 변환
write.csv(dd_mean, 'C:/Users/user/Desktop/DATA/구별최고기온평균.csv')

# 연도별 구별 일별 최고기온
d20_2 <- d20_1 %>% dplyr::mutate(date = substr(date, 1, 10))
d19_2 <- d19_1 %>% dplyr::mutate(date = substr(date, 1, 10))
d18_2 <- d18_1 %>% dplyr::mutate(date = substr(date, 1, 10))
d17_2 <- d17_1 %>% dplyr::mutate(date = substr(date, 1, 10))

d20_3 <- d20_2 %>% group_by(name, date) %>% summarise(tem_max = max(tem, na.rm = T))
d19_3 <- d19_2 %>% group_by(name, date) %>% summarise(tem_max = max(tem, na.rm = T))
d18_3 <- d18_2 %>% group_by(name, date) %>% summarise(tem_max = max(tem, na.rm = T))
d17_3 <- d17_2 %>% group_by(name, date) %>% summarise(tem_max = max(tem, na.rm = T))

summary(d20_3)

# csv
dd_max <- d20_3 %>% bind_rows(d19_3, d18_3, d17_3)
write.csv(dd_max, 'C:/Users/user/Desktop/클린로드/DATA/구별일자별최고.csv')

# 구별 폭염일수 합
dd_count <- d20_1 %>% bind_rows(d19_1, d18_1, d17_1)

# 구별 날짜별 중복 제거(일 마다 한 개의 행이 나오도록 처리)
dd_count$date <- as.character(dd_count$date)
dd_count$date <- substr(dd_count$date, 1, 10) # 일까지만 추출

dd_count <- dd_count %>% distinct(name, date, .keep_all = T)

# 구별 폭염일수(행 개수)
dd_count <- dd_count %>% group_by(name) %>% summarise(tem_count = n())

# barplot
barplot(dd_count$tem_count, names.arg = dd_count$name, ylim = c(25,82), xpd = F)

# csv 변환
write.csv(dd_count, 'C:/Users/user/Desktop/DATA/구별폭염일수.csv')