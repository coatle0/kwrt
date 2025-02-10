library(rvest)
library(dplyr)
library(readr)
#read jm from usob

gs4_auth(email = "coatle0@gmail.com")

ssid <- "1Wmf9Sh2hhQkiyf3GQX1fmpiRe1cIJF0BYmOEbH_5oiU"

usob_df <-read_sheet(ssid,sheet='usob_hub')

jm_list<-na.omit(usob_df$jm)

qtr_col <- data.frame()
qtr_row <- data.frame()
tgt_qtr <- '2025-Q1'

for(tgt_sym in jm_list){

# Yahoo Finance Earnings Calendar URL
#tgt_sym <- 'ASML'
print(tgt_sym)
base_url <- url <- "https://finance.yahoo.com/calendar/earnings?symbol="
url <- paste0(base_url,tgt_sym)

# User-Agent 설정 (웹 크롤링 방지를 피하기 위해 추가)
headers <- httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36")

# 웹 페이지 가져오기
page <- read_html(httr::GET(url, headers))

# 테이블 요소 선택
table_node <- page %>% html_node("table")

# 테이블이 존재하는지 확인
#if (!is.null(table_node)) {
  # 테이블 헤더 추출
  headers <- table_node %>% html_nodes("th") %>% html_text(trim = TRUE)
  
  # 테이블 데이터 추출
  rows <- table_node %>% html_nodes("tr")

  # 각 행에서 데이터 추출
  data_list <- rows %>%
    html_nodes("td") %>%
    html_text(trim = TRUE) %>%
    matrix(ncol = length(headers), byrow = TRUE) %>%
    as.data.frame(stringsAsFactors = FALSE)

  # 컬럼명 적용
  colnames(data_list) <- headers
  
  #date only
  date_only <- str_extract(data_list$`Earnings Date`, "[A-Za-z]+ \\d{1,2}, \\d{4}")
  date_reform <- strsplit(date_only,' ')
  date_dmy <- lapply(date_reform,function(x) dmy(paste(x[2],x[1],x[3])) )
  yy_q <- lapply(date_dmy, function(x)paste0(year(x),'-Q', quarter(x)))
  data_list2<-cbind(data_list$Symbol,data.frame(do.call("c",date_dmy)),as.data.frame(unlist(yy_q)))
  colnames(data_list2) <- c('Symbol','date','yy-Q')
  # 데이터 출력
  df_unique <- data_list2[!duplicated(data_list2$`yy-Q`),]
  if(any(df_unique$`yy-Q`==tgt_qtr)){
    tgt_qtr_idx <- which(df_unique$`yy-Q`==tgt_qtr)
  }else{
    tgt_qtr_idx<- 1
  }
  qtr_temp <- pivot_wider(df_unique[tgt_qtr_idx:dim(df_unique)[1],],names_from = 'yy-Q',values_from = date)
  qtr_col <- bind_rows(qtr_col,qtr_temp)

}
rownames(qtr_col)<-qtr_col$Symbol
qtr_col <- qtr_col[,-1]
qtr_row<-t(qtr_col)
range_write(ssid,data.frame(qtr_col$`2025-Q1`),range='F3',col_names = FALSE,sheet = 'usob_hub')
write.csv(qtr_row, "earning_25-Q1.csv", row.names = TRUE)

