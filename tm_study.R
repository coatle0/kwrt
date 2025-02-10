library(reshape2)
library(xts)

setwd("~/tm_study")

code<-code_get(fresh = TRUE)

tm_stamp <- read.csv("tm_stamp.csv")
tm_idx <- read.csv("tm_idx.csv")
sheet_num <- 14

tmenv <- new.env()
idx_fn <- 'kidx_ticker.csv'
tgt_date<-"2023-04-21"
gs_tmidx(tgt_date,sheet_num,idx_fn)
gs_tmsep(tgt_date,sheet_num,idx_fn)


gs_tmidx<-function(tgt_date,sheet_num,idx_fn){
  test_idx_wt<-read.csv(idx_fn)
  test_idx_wt_lst <- split.default(test_idx_wt,sub(".*_","",names(test_idx_wt)))
  test_wt <- test_idx_wt_lst[[2]]
  test_idx <- test_idx_wt_lst[[1]]
  kweight_lst<-lapply(test_wt,function(x) x[!is.na(x)])
  
  ksmb_lst<-lapply(test_idx, function(x) x[x!=""])
  #ksmb_lst<-ksmb_lst[-3]
  
  tgt_xts <-lapply(ksmb_lst,function(y){lapply(y,function(x) lapply(x,function (x){
    dut<-read.csv(paste0('A',code[match(x,code$name),3]$code,'.csv'));
    dt<-ifelse(dut$time < 1000, paste(dut$date,'0',dut$time,sep=""),paste(dut$date,dut$time,sep=""));
    dt<-as.POSIXct(dt,format="%Y%m%d%H%M");
    print(x);
    dut_xts<-xts(dut[,3:8],dt);
    dut_sd_lst<-lapply(t(tm_stamp),function(x){ dut_t<-dut_xts[x]$vol; 
    dut_t_sd<-sd(dut_t);dut_t_mean <- mean(dut_t); 
    c(mean(dut_t[!(dut_t>(dut_t_mean+3*dut_t_sd))]),sd(dut_t[!(dut_t>(dut_t_mean+3*dut_t_sd))])*3)});
    dut_sd_df <- as.data.frame(dut_sd_lst);
    #dut_sd_df <- t(dut_sd_df)*3
    dut_sd_dt <- t(dut_sd_df) %>% `colnames<-`(c('mean','sd3'))
    dut_sd_df_wt <- cbind(tm_idx,dut_sd_dt);
    write.csv(dut_sd_df_wt, file = paste0('A',code[match(x,code$name),3]$code,"_vsd.csv"),row.names = FALSE,col.names = FALSE);
    assign(x,dut_xts,envir=tmenv);
    assign(paste0(x,"_vsd"),dut_sd_df_wt,envir=tmenv)
  }))})
  
  
  
  
  #indexing prices
  print("cal ref_prices and ref_pf")
  ref_prices=lapply(ksmb_lst,function(x) do.call(cbind,lapply(
    x,function(x) coredata(Cl(get(x,envir=tmenv)[tgt_date])[1]))))
  ref_pf = mapply(function(X,Y){X/Y}, X=kweight_lst,Y=ref_prices,SIMPLIFY = FALSE)
  
  print("get index run")
  index_run=lapply(ksmb_lst,function(y) lapply(y,function(x){print(x);print(length(Cl(get(x,envir = tmenv)[tgt_date])));
    as.character(index(Cl(get(x,envir = tmenv)[tgt_date])))}))
  
  print("get pricees run")
  tgt_stamp<-lapply(index_run,function(x) Reduce(intersect,x))
  
  prices_run_ft <- mapply(function(X,Y){do.call(cbind,lapply(X,function(x)coredata(Cl(get(x,envir = tmenv)[tgt_date][Y]))))%>%
      `colnames<-`(X)},Y=tgt_stamp,X=ksmb_lst,SIMPLIFY = FALSE)
  
  prices_run.idx = mapply(function(X,Y){X %*% as.numeric(Y)},X=prices_run_ft,Y=ref_pf,SIMPLIFY = FALSE)
  prices_run.xts<- mapply(function(X,Y){ xts(X,as.POSIXct(Y,format="%Y-%m-%d %H:%M:%S"))},X=prices_run.idx,Y=tgt_stamp,SIMPLIFY = FALSE)
  
  #indexing vol
  print("vol list")
  
  vol_lst <- mapply(function(X,Y){xts(do.call(cbind,lapply(X,function(x)coredata(get(x,envir = tmenv)[tgt_date][Y]$vol)))%>%
                                        `colnames<-`(X),as.POSIXct(Y,format="%Y-%m-%d %H:%M:%S"))},Y=tgt_stamp,X=ksmb_lst,SIMPLIFY = FALSE)
  
  print("vol sd")
  
  vol_sd = mapply(function(X,Y){xts(do.call(cbind,lapply(X,function(x) get(paste0(x,'_vsd'),envir=tmenv)$sd3)) %>% 
                                      `colnames<-`(X),as.POSIXct(paste0(tgt_date," ",tm_idx$idx),format="%Y-%m-%d %H:%M"))[index(Y)]},X=ksmb_lst,Y=vol_lst,SIMPLIFY = FALSE)
  
  vol_mean = mapply(function(X,Y){xts(do.call(cbind,lapply(X,function(x) get(paste0(x,'_vsd'),envir=tmenv)$mean)) %>% 
                                        `colnames<-`(X),as.POSIXct(paste0(tgt_date," ",tm_idx$idx),format="%Y-%m-%d %H:%M"))[index(Y)]},X=ksmb_lst,Y=vol_lst,SIMPLIFY = FALSE)
  
  
  vol_nor =mapply(function(X,Y,Z){(X-Z)/Y},X=vol_lst,Y=vol_sd,Z=vol_mean,SIMPLIFY = FALSE)
  print("vol nor xts")
  
  vol_nor.xts<-lapply(vol_nor,function(x) xts(rowSums(x),index(x)))
  
  
  idx_all <-mapply(function(X,Y,Z){ merge(X,Y) %>% `colnames<-`(paste0(Z,c('idx','vol')))} ,X=prices_run.xts,Y=vol_nor.xts,Z=names(ksmb_lst),SIMPLIFY = FALSE)
  
  
  idx_all.xts <- na.locf(do.call(merge,idx_all))
  idx_all.df <- data.frame(time=as.character(index(idx_all.xts)),coredata(idx_all.xts))
  
  gs4_auth(email = "coatle0@gmail.com")
  ssid <- "1Edz1EPV6hqBM2tMKSkA3zNmysmugMrAg1u2H3fheXaM"
  range_clear(ssid,sheet=sheet_num)
  range_write(ssid,idx_all.df,range="A1",col_names = TRUE,sheet = sheet_num)
  
  idx_sep_df <-data.frame(time=as.character(index(idx_all.xts)),coredata(idx_all.xts))
  gs4_auth(email = "coatle0@gmail.com")
  ssid <- "1Edz1EPV6hqBM2tMKSkA3zNmysmugMrAg1u2H3fheXaM"
  range_clear(ssid,sheet=sheet_num+1)
  range_write(ssid,idx_sep_df,range="A1",col_names = TRUE,sheet = sheet_num+1)
  
}

gs_tmsep<-function(tgt_date,sheet_num,idx_fn){
  test_idx_wt<-read.csv(idx_fn)
  test_idx_wt_lst <- split.default(test_idx_wt,sub(".*_","",names(test_idx_wt)))
  test_wt <- test_idx_wt_lst[[2]]
  test_idx <- test_idx_wt_lst[[1]]
  kweight_lst<-lapply(test_wt,function(x) x[!is.na(x)])
  
  ksmb_lst<-lapply(test_idx, function(x) x[x!=""])
  #ksmb_lst<-ksmb_lst[-3]
  
  tgt_xts <-lapply(ksmb_lst,function(y){lapply(y,function(x) lapply(x,function (x){
    dut<-read.csv(paste0('A',code[match(x,code$name),3]$code,'.csv'));
    dt<-ifelse(dut$time < 1000, paste(dut$date,'0',dut$time,sep=""),paste(dut$date,dut$time,sep=""));
    dt<-as.POSIXct(dt,format="%Y%m%d%H%M");
    print(x);
    dut_xts<-xts(dut[,3:8],dt);
    dut_sd_lst<-lapply(t(tm_stamp),function(x){ dut_t<-dut_xts[x]$vol; 
    dut_t_sd<-sd(dut_t);dut_t_mean <- mean(dut_t); 
    c(mean(dut_t[!(dut_t > (dut_t_mean+3*dut_t_sd))]),sd(dut_t[!(dut_t$vol>(dut_t_mean+3*dut_t_sd))])*3)});
    dut_sd_df <- as.data.frame(dut_sd_lst);
    #dut_sd_df <- t(dut_sd_df)*3
    dut_sd_dt <- t(dut_sd_df) %>% `colnames<-`(c('mean','sd3'))
    dut_sd_df_wt <- cbind(tm_idx,dut_sd_dt);
    write.csv(dut_sd_df_wt, file = paste0('A',code[match(x,code$name),3]$code,"_vsd.csv"),row.names = FALSE,col.names = FALSE);
    assign(x,dut_xts,envir=tmenv);
    assign(paste0(x,"_vsd"),dut_sd_df_wt,envir=tmenv)
  }))})
  
  #indexing prices
  print("cal ref_prices and ref_pf")
  ref_prices=lapply(ksmb_lst,function(x) do.call(cbind,lapply(
    x,function(x) coredata(Cl(get(x,envir=tmenv)[tgt_date])[1]))))
  
  print("get index run")
  index_run=lapply(ksmb_lst,function(y) lapply(y,function(x){print(x);print(length(Cl(get(x,envir = tmenv)[tgt_date])));
    as.character(index(Cl(get(x,envir = tmenv)[tgt_date])))}))
  
  print("get pricees run")
  tgt_stamp<-lapply(index_run,function(x) Reduce(intersect,x))
  
  
  ksmb_lst_name<-lapply(ksmb_lst, function(x) ifelse(x=="JYP Ent.","JYP",x))
  prices_run_ft <- mapply(function(X,Y){do.call(cbind,lapply(X,function(x)coredata(Cl(get(x,envir = tmenv)[tgt_date][Y]))))%>%
      `colnames<-`(X)},Y=tgt_stamp,X=ksmb_lst,SIMPLIFY = FALSE)
  
  
  
  prices_run.nor = mapply(function(X,Y,Z){as.data.frame(sweep(X,2,Y,FUN='/')*100)%>% set_names(Z)},X=prices_run_ft,Y=ref_prices,Z=ksmb_lst_name)
  prices_run.xts<- mapply(function(X,Y){ xts(X,as.POSIXct(Y,format="%Y-%m-%d %H:%M:%S"))},X=prices_run.nor,Y=tgt_stamp,SIMPLIFY = FALSE)
  
  #indexing vol
  print("vol list")
  
  vol_lst <- mapply(function(X,Y,Z){xts(do.call(cbind,lapply(X,function(x)coredata(get(x,envir = tmenv)[tgt_date][Y]$vol)))%>%
                                          `colnames<-`(paste0(Z,'_vol')),as.POSIXct(Y,format="%Y-%m-%d %H:%M:%S"))},Y=tgt_stamp,X=ksmb_lst,Z=ksmb_lst_name,SIMPLIFY = FALSE)
  
  print("vol sd")
  vol_sd = mapply(function(X,Y,Z){xts(do.call(cbind,lapply(X,function(x) get(paste0(x,'_vsd'),envir=tmenv)$sd3)) %>% 
                                        `colnames<-`(paste0(Z,'_vol')),as.POSIXct(paste0(tgt_date," ",tm_idx$idx),format="%Y-%m-%d %H:%M"))[index(Y)]},
                  X=ksmb_lst,Y=vol_lst,Z=ksmb_lst_name,SIMPLIFY = FALSE)
  vol_mean = mapply(function(X,Y,Z){xts(do.call(cbind,lapply(X,function(x) get(paste0(x,'_vsd'),envir=tmenv)$mean)) %>% 
                                          `colnames<-`(paste0(Z,'_mean')),as.POSIXct(paste0(tgt_date," ",tm_idx$idx),format="%Y-%m-%d %H:%M"))[index(Y)]},
                    X=ksmb_lst,Y=vol_lst,Z=ksmb_lst_name,SIMPLIFY = FALSE)
  
  vol_nor =mapply(function(X,Y,Z){(X-Z)/Y},X=vol_lst,Z=vol_mean,Y=vol_sd,SIMPLIFY = FALSE)
  
  
  
  idx_vol_all <-mapply(function(X,Y,Z){ merge(X,Y)} ,X=prices_run.xts,Y=vol_nor,Z=names(ksmb_lst_name),SIMPLIFY = FALSE)
  
  
  idx_all.xts <- na.locf(do.call(merge,idx_vol_all))
  tgt_df <- coredata(idx_all.xts)
  xts_colnames <- lapply(ksmb_lst_name,function(x) lapply(x,function(x){print(x);tgt_df[,c(x,paste0(x,'_vol'))]}))
  names(xts_colnames) <-c()
  xts_all_df <- data.frame(xts_colnames)
  
  idx_all.df <- data.frame(time=as.character(index(idx_all.xts)),xts_all_df[,1:50])
  idx_all2.df <- data.frame(time=as.character(index(idx_all.xts)),xts_all_df[,51:dim(tgt_df)[2]])
  
  
  gs4_auth(email = "coatle0@gmail.com")
  ssid <- "1Edz1EPV6hqBM2tMKSkA3zNmysmugMrAg1u2H3fheXaM"
  range_clear(ssid,sheet=sheet_num+2)
  range_write(ssid,idx_all.df,range="A1",col_names = TRUE,sheet = sheet_num+2)
  
  
  
  gs4_auth(email = "coatle0@gmail.com")
  ssid <- "1Edz1EPV6hqBM2tMKSkA3zNmysmugMrAg1u2H3fheXaM"
  range_clear(ssid,sheet=sheet_num+3)
  range_write(ssid,idx_all2.df,range="A1",col_names = TRUE,sheet = sheet_num+3)
  
}




