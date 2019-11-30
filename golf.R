library(rvest)
years <- seq(1998, 2018, by = 1)

df_final <- NULL
for(i in years){
  url <- str_c("http://www.lpga.com/statistics/money/official-money?year=", i)
  
  h <- read_html(url)
  df <- h %>% 
    html_table() %>% 
    as.data.frame()
  
  country <- h %>% 
    html_nodes(".flag")
  
  country <- as.character(country)
  df$country <- country
  
  df$country <- str_extract(df$country, "\\/.+\\.png")
  df$country <- str_remove(df$country, "/media/images/global/countries/")
  df$country <- str_remove(df$country, "\\.png")
  
  df$year <- i
  
  df_final <- rbind(df_final, df)
  
}

write_csv(df_final, "golf_final.csv")


df_chart <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQl5PcnEILz1GGNMrQzAQNZjqmgZuPyjttU3qTvT0qM6V3FvDL4ualfBk8_t3wHs6ykwTnzab-7BfeU/pub?gid=0&single=true&output=csv")
glimpse(df_chart)

ggplot(df_chart, aes(x = as.character(연도), y = avg, group = 1)) +
  geom_line(stat = "identity") +
  geom_point() +
  theme_woons() +
  ylim(0, 0.5) +
  labs(title = "LPGA 총상금 대비 한국선수 총 상금 비율 변화", 
       caption = "단위 %")

ggplot(df_chart, aes(x = `한국 선수 총 상금`, y = `LPGA 총 상금`)) +
  geom_point() +
  geom_smooth(method = "loess")
