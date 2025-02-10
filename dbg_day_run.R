library(googlesheets4)
library(tqk)
library(xts)
library(mypkg)
library(telegram.bot)
bot=Bot(token="5824250303:AAF30nE1zYlP28DzS-Gd69yAegN-LgHU_ag")
chat_id <- 1278251780

chk<-bot$send_message(chat_id,"starting jm code get")

setwd("~/tm_study")
#print(getwd())
dbgenv <- new.env()
tmenv <- new.env()
code<-code_get()
#shell('get_tm.xlsm',wait = FALSE)

args=commandArgs(trailingOnly=TRUE)

tgt_date <- args[1]
#tgt_date <-"20230831"

#tgt_date <- as.character(as.Date(tgt_date,format('%Y%m%d')))
tgt_date<-"2025-02-07"
print(tgt_date)

tm_stamp <- read.csv("tm_stamp_fm.csv")
tm_idx <- read.csv("tm_idx_fm.csv")

#get market index list
idx_fn <- 'dbg_day'
idx_gs_lst <- read_rtgs_idx(idx_fn)
dbg_wt_lst<-idx_gs_lst[[2]]

dbg_lst<-idx_gs_lst[[1]]

dbg_code_lst <- lapply(dbg_lst,function(x) paste0('A',code[match(x,code$name),3]$code))

dbg_code<-dbg_code_lst[[1]]
jm_lst <- dbg_lst[[1]]

bot$delete_message(chat_id,chk$message_id)
chk<-bot$send_message(chat_id,"get ohlc")
#load xts to envir
tgt_xts <-lapply(jm_lst,function(x) {
  comp_code<-paste0('A',code[match(x,code$name),3]$code);
  tgt_scr <-paste0('python c:/Users/coatle/get_ohlc.py ',comp_code);
  shell(tgt_scr);

  dut<-read.csv(paste0(comp_code,'.csv'));
  dt<-ifelse(dut$time < 1000, paste(dut$date,'0',dut$time,sep=""),paste(dut$date,dut$time,sep=""));
  dt<-as.POSIXct(dt,format="%Y%m%d%H%M");
  print(x);
  dut_xts<-xts(dut[,3:7],dt)[tgt_date];
  colnames(dut_xts)[5]<-'volume'
  assign(x,dut_xts,envir=dbgenv);
  })

#load vsdfm to envir
vsdfm_lst<-lapply(jm_lst,function(x){
  dut<-read.csv(paste0(paste0('A',code[match(x,code$name),]$scode),'_vsdfm.csv'));
  dt<-paste0(tgt_date,' ',dut$idx)
  dt<-as.POSIXct(dt,format="%Y-%m-%d %H:%M");
  #print(y)
  dut_x <- xts(dut[,c('mean','sd3')],dt);
  assign(paste0(x,"_vsdfm"),dut_x,envir=dbgenv)
}
)
#change ref_prices from Cl to Op
prices_run_ft<-lapply(jm_lst,function(x){print(x);ref_prices<-coredata(Op(get(x,envir=dbgenv))[1]);
     (Cl(get(x,envir=dbgenv))/ref_prices[1])*100} %>%`colnames<-`(paste0(x,'_idx')))


vol_run_nor<-lapply(jm_lst,function(x){dut<-Vo(get(x,envir=dbgenv));
          dut_vsdfm<-get(paste0(x,'_vsdfm'),envir=dbgenv);
          dut_nor<-(dut-dut_vsdfm$mean)/dut_vsdfm$sd3}%>%`colnames<-`(paste0(x,'_nvol')))

idx_all.xts<-do.call(merge,mapply(function(X,Y){merge(X,Y)},X=prices_run_ft,Y=vol_run_nor,SIMPLIFY = FALSE))
idx_all.df <- data.frame(time=as.character(index(idx_all.xts)),coredata(idx_all.xts))


bot$delete_message(chat_id,chk$message_id)
chk<-bot$send_message(chat_id,"ready to draw")

gs4_auth(email = "coatle0@gmail.com")
ssid<- "1GWW0Q1RgMNAvSG7S4OyrbXSpcSHDnMSmd2uTsmZyJJE"
range_clear(ssid,sheet='dbg_draw')
bot$delete_message(chat_id,chk$message_id)
range_write(ssid,idx_all.df,range="A1",col_names = TRUE,sheet = "dbg_draw")