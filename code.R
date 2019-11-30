install.packages(c("tidyverse", "rvest", 
                   "lubridate", "glue", "N2H4"), 
                 dependencies = T)

library(tidyverse)
library(rvest)
library(lubridate)
library(N2H4)
library(glue)

#-----------------------------------------------------
# 간단한 크롤링부터 배워보자 
#-----------------------------------------------------

url <- "http://www.korea2me.com/country/KR"
h <- read_html(url)

href <- h %>% 
  html_nodes("div.col-md-6 > a") %>% 
  html_attr("href")

href <- str_remove_all(href, "/cc/")
base <- "http://www.korea2me.com/cc/"

base_url <- str_c(base, href)
base_url


test <- read_html(base_url[1])
value <- test %>% 
  html_nodes("h1.h3") %>% 
  html_text()

value2 <- test %>% 
  html_node("body > div:nth-child(2) > div > div.col-md-8 > table.table.table-condensed2.em12.resp-table > tbody > tr:nth-child(1) > td:nth-child(1)") %>% 
  html_text()

df_final <- NULL
for(i in base_url){
  print(i)
  base_h <- read_html(i)
  
  base_text <- base_h %>% 
    html_nodes("h1.h3") %>% 
    html_text()
  
  base_value <- base_h %>%
    html_node("body > div:nth-child(2) > div > div.col-md-8 > table.table.table-condensed2.em12.resp-table > tbody > tr:nth-child(1) > td:nth-child(1)") %>% 
    html_text()
  
  df_local <- data.frame(base_text, base_value)
  df_final <- rbind(df_final, df_local)
}

write_csv(df_final, "df_final.csv")

#-----------------------------------------------------
# 일베 사이트 크롤링
#-----------------------------------------------------
url.mlb <- "http://mlbpark.donga.com/mp/b.php?p=1&m=list&b=bullpen&query=&select=&user="
h.mlb <- read_html(url.mlb)


title <- h.mlb %>% 
  html_nodes("span.bullpen") %>% 
  html_text()

nick <- h.mlb %>% 
  html_nodes("td.t_left > span.nick") %>% 
  html_text()

nick <- str_remove_all(nick, "담당자")
nick <- str_remove_all(nick, "엠팍제휴팀")


dates <- h.mlb %>% 
  html_nodes("td > span.date")

view <- h.mlb %>% 
  html_nodes("span.viewV") %>% 
  html_text()

#container > div.contents > div.left_cont > div.tbl_box > table > tbody > tr:nth-child(5) > td:nth-child(4) > span
#-----------------------------------------------------
# 기상청 크롤링 
#-----------------------------------------------------

seq_years <- seq(2017, 2018, by = 1)
seq_month <- seq(7, 8, by = 1)
seq_section <- c(1, 11, 21)

df <- NULL
df_final <- NULL

for (i in seq_years) {
  print(i)
  
  for(k in seq_month) {
    print(k)
    
    for(s in seq_section) {
      print(s)
      # 코드 시작
      url <- paste0("http://www.weather.go.kr/weather/climate/past_tendays.jsp?stn=108&yy=", i, "&mm=", k, "&obs=", s)
      h <- read_html(url, encoding = "cp949")
      
      # 정제
      df <- h %>% html_table(fill = T)
      df <- df[[2]]
      colnames(df)[1:2] <- c("category", "category2")
      
      df <- df %>% rename(total1 = `순계`, total = `순평균`) %>% select(-total1, -total)
      df <- df %>% 
        gather("dates", "value", 3:ncol(df))
      
      df$year <- i
      
      # 쌓기
      df_final <- rbind(df_final, df)
      
    }
  }
}

#-----------------------------------------------------
# 데이터 정제 시작 ++++++++++
#-----------------------------------------------------
df_final$year <- replace_na(df_final$year, 2000)

df_final2 <- df_final %>% 
  filter(str_detect(category, "^최고기온") & str_detect(category2, "관측") |
           str_detect(category, "^최저기온") & str_detect(category2, "관측") |
           str_detect(category, "평균기온") & str_detect(category2, "관측") |
           str_detect(category, "상대습도") & str_detect(category2, "관측"))

df_final2$dates <- str_replace(df_final2$dates, "월", "-")
df_final2$dates <- str_remove(df_final2$dates, "일")
df_final2 <- df_final2 %>% drop_na(year)

#-----------------------------------------------------
# 날짜 형식 정확하게
#-----------------------------------------------------
library(lubridate)
df_final2$dates <- ymd(paste0(df_final2$year, "-", df_final2$dates))


df_final2 <- df_final2 %>% select(-year)                
df_final2 <- df_final2 %>% select(-category2)                      

df_final2$category <- str_remove(df_final2$category, "\\(.+\\)")
df_final2 <- df_final2 %>% drop_na(value)
#-----------------------------------------------------
# 불쾌지수 넣어주기
#-----------------------------------------------------
glimpse(df_final2)
df_final2$value <- as.numeric(df_final2$value)

df_final2 <- df_final2 %>% spread(category, value)

colnames(df_final2) <- c("dates", "humidity", "max_temp", "min_temp", "avg_temp")

df_final_wide <- df_final2 %>% 
  mutate(angry_index = 1.8 * avg_temp - 0.55 * (1 - (0.01 * humidity)) * (1.8 * avg_temp - 26) + 32)

#-----------------------------------------------------
# 연도, 월 변수 새롭게 생성
#-----------------------------------------------------
df_final2 <- df_final_wide %>% gather("category", "value", 2:6)
df_final2 <- df_final2 %>% mutate(year = year(dates),
                                  month = month(dates))

#-----------------------------------------------------
# 1994년 기점 나누기
#-----------------------------------------------------
df_final2 <- df_final2 %>% mutate(category2 = if_else(year >= 1994, "1994년_이후", "1994년_이전"))

#-----------------------------------------------------
# 폭염 여부
#-----------------------------------------------------
df_final2 <- df_final2 %>% 
  mutate(heat = if_else(category == "max_temp" & value >= 33, "폭염", "정상"))

#-----------------------------------------------------
# 서울 7월 날씨만 추출
#-----------------------------------------------------
seoul_july <- df_final2 %>% 
  filter(month == 7)
