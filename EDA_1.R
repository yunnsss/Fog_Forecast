# import data
## road & setting
#install.packages("zoo")
library(dplyr)
library(zoo)
library(caret)
list = dbGetQuery(conn, "show tables")

# train, test data
fog = dbGetQuery(conn, "select * from fog_train")
test = dbGetQuery(conn, "select * from fog_test")

# 변수명에서 "" 제거
names(fog) <- gsub("fog_train\\.", "", names(fog))


## add datetime 
# 연도 데이터 변환
fog$year <- ifelse(fog$year == "I", 2020, ifelse(fog$year == "J", 2021, 2022))

# 연도, 월, 일, 시간, 분을 결합하여 datetime 형식으로 변환
fog$datetime <- as.POSIXct(paste(fog$year, fog$month, fog$day, fog$time, fog$minute,sep = "-"), format="%Y-%m-%d-%H-%M")


# data cleaning
## missing values
# -90 이하인 값은 결측치로 고려
cols <- c("ta", "hm", "sun10",  "ws10_deg", "ws10_ms", "ts", "re")
fog[cols] <- lapply(fog[cols], function(x) ifelse(x <= -90, NA, x))


### linear interpolate
library(dplyr)
library(tidyverse)
library(magrittr)
# 시간선형보간 함수
linear_interpolation <- function(df) {
  # 데이터프레임을 날짜별로 정렬
  df <- df[order(df$datetime), ]
  
  # 특정 열에서 선형 보간하여 결측치 채움
  columns_to_interpolate <- c("ta", "hm", "sun10", "ws10_deg", "ws10_ms", "ts")
  
  df[, columns_to_interpolate] <- lapply(df[, columns_to_interpolate], function(x) {
    # 기존 값 유지, 결측값만 보간
    na.approx(x, na.rm = FALSE, rule = 2)
  })
  
  return(df)
}

# 데이터프레임에 stn_id별로 그룹화하여 시간 선형 보간법 적용
fog_LI <- fog %>%
  group_by(stn_id) %>%
  do(linear_interpolation(.)) %>%
  ungroup()



## season values
# season 변수를 생성하는 함수
assign_season <- function(month) {
  if (month %in% c(2, 3, 4)) {
    return('Spring')
  } else if (month %in% c(5, 6, 7)) {
    return('Summer')
  } else if (month %in% c(8, 9, 10)) {
    return('Autumn')
  } else {
    return('Winter')
  }
}

# season 변수 추가
fog_LI$season <- sapply(fog_LI$month, assign_season)

head(fog_LI)


### re 
#### glm model
# re 결측치 처리
# 필요한 패키지 로드
library(tidyverse)
# 로지스틱 회귀 모델 적합
# 결측치 제거
fog_LI_complete <- na.omit(fog_LI)

# 여러 예측 변수를 모델에 포함시키기 위해 formula에 추가합니다.
model <- glm(re ~ ta + hm + sun10 + ws10_deg + ws10_ms + ts, data = fog_LI_complete, family = binomial(link = "logit"))

# 모델 요약
summary(model)


#### find opt_val and predict
# 패키지 로드
library(pROC)

# 예측된 확률값 계산
predicted_prob <- predict(model, type = "response")
# 실제 값과 예측된 확률값을 사용하여 ROC 커브 계산
roc_curve <- roc(fog_LI_complete$re, predicted_prob)

# ROC 커브 그리기
plot(roc_curve)

# AUC 값 출력
auc(roc_curve) #0.8 ≤ AUC < 0.9: 좋은 모델 :0.88

# Youden's J 통계량을 사용하여 최적의 기준값 찾기
opt_coords <- coords(roc_curve, "best", ret = "threshold", best.method = "youden")

# 최적의 기준값 출력
optimal_threshold <- opt_coords
print(optimal_threshold)

# 새로운 데이터에 대한 예측
# new_data는 예측을 수행하려는 새로운 데이터를 포함하는 데이터 프레임이며, WS10_deg, WS10_ms, TA, HM, sun10, TS 컬럼을 포함해야 합니다.
# 예시: new_data <- data.frame(WS10_deg = c(5, 10), WS10_ms = c(2, 3), TA = c(15, 20), HM = c(50, 60), sun10 = c(200, 300), TS = c(0, 1))
predicted_probs <- predict(model, newdata = fog_LI, type = "response")

# 최적의 기준값을 사용하여 분류
predicted_class_opt <- ifelse(predicted_probs >= 0.05714489, 1, 0)

# 예측된 클래스 확인
predicted_class_opt

# re 열에서 NA 값을 가진 행의 인덱스를 추출합니다.
na_indices <- which(is.na(fog_LI$re))

# re 열에서 NA 값을 가진 행에 대해서만 predicted_class_opt 값을 할당합니다.
fog_LI$re[na_indices] <- predicted_class_opt[na_indices]

##결측치 잘 채워졌는지 확인
missing_values <- colSums(is.na(fog_LI))
print(missing_values)



## outlier
# 이상치 탐지 함수 정의 (IQR 방법 사용)
cols <- c("ta", "hm", "sun10",  "ws10_deg", "ws10_ms", "ts")
detect_outliers <- function(data, cols) {
  outlier_indices_list <- lapply(cols, function(col) {
    Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    outlier_indices <- which(data[[col]] < lower_bound | data[[col]] > upper_bound)
    return(outlier_indices)
  })
  names(outlier_indices_list) <- cols
  return(outlier_indices_list)
}

# 이상치를 평균값으로 대체하는 함수 정의
replace_outliers_with_mean <- function(data, cols, outlier_indices_list) {
  for (col in cols) {
    outlier_indices <- outlier_indices_list[[col]]
    if (length(outlier_indices) > 0) {
      mean_value <- mean(data[[col]], na.rm = TRUE)
      data[[col]][outlier_indices] <- mean_value
    }
  }
  return(data)
}


# 이상치 탐지
outlier_indices_list <- detect_outliers(fog_LI, cols)

# 이상치를 평균값으로 대체
data_replaced <- replace_outliers_with_mean(fog_LI, cols, outlier_indices_list)

# 데이터 확인
head(fog_LI) ; head(data_replaced)
summary(fog_LI) ; summary(data_replaced)



## stn_id 
## stn_id 지점번호 변수 위치변수로 변환
library(dplyr)
data_replaced <- mutate(data_replaced, st = ta + sun10 + ts)
print(data_replaced)


## periodicity
#주기성 
# year 열의 값을 조건에 따라 변경
data_replaced$weekday <- as.POSIXlt(paste(data_replaced$year, data_replaced$month, data_replaced$day, sep="-"))$wday
data_replaced$time_in_rad_week <- (data_replaced$weekday / 7) * 2 * pi
data_replaced$time_in_rad_year <- (data_replaced$month / 12) * 2 * pi
data_replaced
# 사인과 코사인 변수를 추가 (주간 주기)
data_replaced$sin_week <- sin(data_replaced$time_in_rad_week)
data_replaced$cos_week <- cos(data_replaced$time_in_rad_week)
data_replaced
# 사인과 코사인 변수를 추가 (연간 주기)
data_replaced$sin_year <- sin(data_replaced$time_in_rad_year)
data_replaced$cos_year <- cos(data_replaced$time_in_rad_year)
data_replaced
# 시간 정보를 각도로 변환 (0에서 2π 사이)
data_replaced$time_in_hours <- data_replaced$time + data_replaced$minute / 60
data_replaced$time_in_rad_day <- (data_replaced$time_in_hours / 24) * 2 * pi
# 사인과 코사인 변수를 추가 (일간 주기)
data_replaced$sin_day <- sin(data_replaced$time_in_rad_day)
data_replaced$cos_day <- cos(data_replaced$time_in_rad_day)


# 특정 열 제거
data_replaced <- data_replaced %>%
  select(-time_in_rad_year, -time_in_rad_week,-time_in_rad_day, -time_in_hours, -weekday)

# 결과 확인
colnames(data_replaced)



## scaling
# 정규화
# 정규화할 열 목록
cols_to_normalize <- c("ws10_deg", "ws10_ms", "ta", "hm", "sun10")

# 정규화 함수 정의
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# 선택한 열만 정규화
data_replaced[cols_to_normalize] <- lapply(data_replaced[cols_to_normalize], normalize)

fog_final <- data_replaced



# test data cleaning
# 변수명에서 fog_ 제거
names(test) <- gsub("fog_test\\.", "", names(test))

#### data 변수 생성
# 연도 데이터 변환
test$year <- ifelse(test$year == "L", 2023)

# 연도, 월, 일, 시간, 분을 결합하여 datetime 형식으로 변환
test$datetime <- as.POSIXct(paste(test$year, test$month, test$day, test$time, test$minute,sep = "-"), format="%Y-%m-%d-%H-%M")

####결측치
# -90 이하인 값은 결측치로 고려
cols <- c("ta", "hm", "sun10",  "ws10_deg", "ws10_ms", "ts", "re")
test[cols] <- lapply(test[cols], function(x) ifelse(x <= -90, NA, x))


# 데이터프레임에 stn_id별로 그룹화하여 시간 선형 보간법 적용
test_LI <- test %>%
  group_by(stn_id) %>%
  do(linear_interpolation(.)) %>%
  ungroup()


head(test$ws10_ms) ; head(test_LI$ws10_ms)  


#### 계절변수 추가
# season 변수 추가
test_LI$season <- sapply(test_LI$month, assign_season)

#결측치 잘 채워졌는지 확인
colSums(is.na(test_LI))


#### re 결측치 처리
# 로지스틱 회귀 모델 적합
# 결측치 제거
test_LI_complete <- na.omit(test_LI)
# 여러 예측 변수를 모델에 포함시키기 위해 formula에 추가합니다.
model <- glm(re ~ ta + hm + sun10 + ws10_deg + ws10_ms + ts, data = test_LI_complete, family = binomial(link = "logit"))

# 모델 요약
summary(model)

# 예측된 확률값 계산
predicted_prob <- predict(model, type = "response")
# 실제 값과 예측된 확률값을 사용하여 ROC 커브 계산
roc_curve <- roc(test_LI_complete$re, predicted_prob)

# ROC 커브 그리기
plot(roc_curve)

# AUC 값 출력
auc(roc_curve) #0.8 ≤ AUC < 0.9: 좋은 모델 :0.88

# Youden's J 통계량을 사용하여 최적의 기준값 찾기
opt_coords <- coords(roc_curve, "best", ret = "threshold", best.method = "youden")

# 최적의 기준값 출력
optimal_threshold <- opt_coords
print(optimal_threshold)

# 새로운 데이터에 대한 예측
# new_data는 예측을 수행하려는 새로운 데이터를 포함하는 데이터 프레임이며, WS10_deg, WS10_ms, TA, HM, sun10, TS 컬럼을 포함해야 합니다.
# 예시: new_data <- data.frame(WS10_deg = c(5, 10), WS10_ms = c(2, 3), TA = c(15, 20), HM = c(50, 60), sun10 = c(200, 300), TS = c(0, 1))
predicted_probs <- predict(model, newdata = test_LI, type = "response")

# 최적의 기준값을 사용하여 분류
predicted_class_opt <- ifelse(predicted_probs >= 0.05714489, 1, 0)

# 예측된 클래스 확인
predicted_class_opt

# re 열에서 NA 값을 가진 행의 인덱스를 추출합니다.
na_indices <- which(is.na(test_LI$re))

# re 열에서 NA 값을 가진 행에 대해서만 predicted_class_opt 값을 할당합니다.
test_LI$re[na_indices] <- predicted_class_opt[na_indices]

##결측치 잘 채워졌는지 확인
missing_values <- colSums(is.na(test_LI))
# 결과 출력
print(missing_values)


####이상치 처리
cols <- c("ta", "hm", "sun10",  "ws10_deg", "ws10_ms", "ts")
# 이상치 탐지
outlier_indices_list <- detect_outliers(test_LI, cols)

# 이상치를 평균값으로 대체
data_replaced <- replace_outliers_with_mean(test_LI, cols, outlier_indices_list)

# 데이터 확인
summary(test_LI) ; summary(data_replaced)


## stn_id 지점번호 변수 위치변수로 변환
data_replaced <- mutate(data_replaced, st = ta + sun10 + ts)


##주기성 
# year 열의 값을 조건에 따라 변경
data_replaced$weekday <- as.POSIXlt(paste(data_replaced$year, data_replaced$month, data_replaced$day, sep="-"))$wday
data_replaced$time_in_rad_week <- (data_replaced$weekday / 7) * 2 * pi
data_replaced$time_in_rad_year <- (data_replaced$month / 12) * 2 * pi
data_replaced
# 사인과 코사인 변수를 추가 (주간 주기)
data_replaced$sin_week <- sin(data_replaced$time_in_rad_week)
data_replaced$cos_week <- cos(data_replaced$time_in_rad_week)
data_replaced
# 사인과 코사인 변수를 추가 (연간 주기)
data_replaced$sin_year <- sin(data_replaced$time_in_rad_year)
data_replaced$cos_year <- cos(data_replaced$time_in_rad_year)
data_replaced
# 시간 정보를 각도로 변환 (0에서 2π 사이)
data_replaced$time_in_hours <- data_replaced$time + data_replaced$minute / 60
data_replaced$time_in_rad_day <- (data_replaced$time_in_hours / 24) * 2 * pi
# 사인과 코사인 변수를 추가 (일간 주기)
data_replaced$sin_day <- sin(data_replaced$time_in_rad_day)
data_replaced$cos_day <- cos(data_replaced$time_in_rad_day)


# 특정 열 제거
data_replaced <- data_replaced %>%
  select(-time_in_rad_year, -time_in_rad_week,-time_in_rad_day, -time_in_hours, -weekday)

# 결과 확인
colnames(data_replaced)


#### 정규화
# 정규화할 열 목록
cols_to_normalize <- c("ws10_deg", "ws10_ms", "ta", "hm", "sun10")


# 선택한 열만 정규화
data_replaced[cols_to_normalize] <- lapply(data_replaced[cols_to_normalize], normalize)

test_final <- data_replaced



#데이터 내보내기기
write.csv(fog_final, file = 'fog_real_final.csv', row.names = FALSE)
write.csv(test_final, file = 'test_real_final.csv', row.names = FALSE)


