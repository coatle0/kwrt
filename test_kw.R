library(googlesheets4)
library(lubridate)
library(quantmod)
library(PerformanceAnalytics)
library(telegram.bot)
library(tidyquant)

coatle_bot=Bot(token="5824250303:AAF30nE1zYlP28DzS-Gd69yAegN-LgHU_ag")
kw_bot=Bot(token="8109717276:AAFyiB__nN-6BLxhaGr6pTW2TB8o1xwKRKs")

#bot = coatle_bot
chat_id <- 1278251780

bot <- kw_bot

jm_nm <- '공구우먼'
code<-code_get()


setwd("c:/lab/")

gen_vsd_kw<- function(jm_code){
  tm_stamp <- read.csv("tm_stamp_fm.csv")
  tm_idx <- read.csv("tm_idx_fm.csv")
  
  lapply(jm_code,function(x){
    dut<-read.csv(paste0(x,'.csv'));
    dt<-ifelse(dut$time < 100000, paste(dut$date,'0',dut$time,sep=""),paste(dut$date,dut$time,sep=""));
    dt<-as.POSIXct(dt,format="%Y%m%d%H%M");
    print(x);
    dut_xts<-xts(dut[,3:7],dt);
    #cal mean and sd of vol (separated volume)
    dut_sd_lst<-lapply(t(tm_stamp),function(x){ dut_t<-dut_xts[x]$vol;
    dut_t_sd<-sd(dut_t);dut_t_mean <- mean(dut_t);
    c(mean(dut_t[!(dut_t > (dut_t_mean+3*dut_t_sd))]),sd(dut_t[!(dut_t$vol>(dut_t_mean+3*dut_t_sd))])*3)});
    #cal mean and sd of volume ( cummulative volume)
    
    dut_csd_lst<-lapply(t(tm_stamp),function(x){ dut_t<-dut_xts[x]$volume;
    dut_t_sd<-sd(dut_t);dut_t_mean <- mean(dut_t);
    c(mean(dut_t[!(dut_t > (dut_t_mean+3*dut_t_sd))]),sd(dut_t[!(dut_t$volume>(dut_t_mean+3*dut_t_sd))])*3)});
    
    #separated volume
    dut_sd_df <- as.data.frame(dut_sd_lst);
    dut_sd_dt <- t(dut_sd_df) %>% `colnames<-`(c('mean','sd3'))
    dut_sd_df_wt <- cbind(tm_idx,dut_sd_dt);
    
    #dut_sd_df <- t(dut_sd_df)*3
    
    write.csv(dut_sd_df_wt, file = paste0(x,"_vsdfm.csv"),row.names = FALSE,col.names = FALSE);
  })
}


tm_txt <- paste0('/testkw ', '10m ', "'",jm_nm,"'")
tm_fn <- paste0(tgt_dir,'A',code$scode[code$name==jm_nm],'.csv')
tm_stm_fn <- paste0(tgt_dir,"tm_stamp_fm.csv")
tm_idx_fn <- paste0(tgt_dir,"tm_idx_fm.csv")

tm_stamp <- read.csv(tm_stm_fn)
tm_idx <- read.csv(tm_idx_fn)
file.remove(tm_fn)

bot$sendMessage(chat_id = chat_id, text = tm_txt)

while (!file.exists(tm_fn)) {
  cat("Waiting for .csv...\n")
  Sys.sleep(1)  # 1초 대기 후 다시 확인
}
dut <- read.csv(tm_fn)
dt<-ifelse(dut$time < 100000, paste(dut$date,'0',dut$time,sep=""),paste(dut$date,dut$time,sep=""));
dt<-as.POSIXct(dt,format="%Y%m%d%H%M");
dut_xts<-xts(dut[,3:7],dt);

aimjm_lst <-  read_asgs_sheet('aimjm')

lapply(aimjm_lst$aimjm, function(x) bot$sendMessage(chat_id = chat_id, text = paste0('/testkw ', 'aim ',"'",x,"' ",0,' ',0)) )
jm_name <- "SK???̴н?"


jm_name <- "��?ֹݵ?ü"
bot$sendMessage(chat_id = chat_id, text = paste0('/testkw ', 'aim ',"'",jm_name,"' ",ord_qty,' ',sell_price))


bot$sendMessage(chat_id = chat_id, text = paste0('/testkw ', 'aim ',"'",code[match(jm_name,code$name),3]$code,"' ",ord_qty,' ',sell_price))

ord_qty <- 0
sell_price <- 0


jm_name<-"?Ｚ????"
ord_qty <- 1
bot$sendMessage(chat_id = chat_id, text = paste0('/testkw ', 'bm ',"'",code[match(jm_name,code$name),3]$code,"' ",ord_qty,' 0'))

jm_name<-"???????۷??̼?"
ord_qty <- 2
sell_price <-'3675'
bot$sendMessage(chat_id = chat_id, text = paste0('/testkw ', 'sm ',"'",code[match(jm_name,code$name),3]$code,"' ",ord_qty,' ',sell_price))

jm_name <- "SK???̴н?"
bot$sendMessage(chat_id = chat_id, text = paste0('/testkw ', 'aim ',"'",code[match(jm_name,code$name),3]$code,"' ",ord_qty,' ',sell_price))

ord_qty <- 0
sell_price <- 0


jm_name <- "��?콺"
bot$sendMessage(chat_id = chat_id, text = paste0('/testkw ', 'aim ',"'",jm_name,"' ",ord_qty,' ',sell_price))

jm_name <- "??ȭ?ַ???"
bot$sendMessage(chat_id = chat_id, text = paste0('/testkw ', 'aim ',"'",jm_name,"' ",ord_qty,' ',sell_price))

jm_name <- "HPSP"
bot$sendMessage(chat_id = chat_id, text = paste0('/testkw ', 'aim ',"'",jm_name,"' ",ord_qty,' ',sell_price))


jm_name <- "HD???뿡?????ַ???"
bot$sendMessage(chat_id = chat_id, text = paste0('/testkw ', 'aim ',"'",jm_name,"' ",ord_qty,' ',sell_price))


jm_name <- "��?ֹݵ?ü"
bot$sendMessage(chat_id = chat_id, text = paste0('/testkw ', 'aim ',"'",jm_name,"' ",ord_qty,' ',sell_price))


echo <- function(bot, update){
  #bot$sendMessage(chat_id, text = update$message$text)
  print(update$message$text)
}

updater <- updater + MessageHandler(echo, MessageFilters$text)

updater$start_polling()

sweep(prices_run,2,ref_prices,'/')
tmp<-lapply(df_tmp,dim)
max(unlist(tmp))



