#initialize set up
setwd("~/tm_study")
tmenv <- new.env()

code<-code_get(fresh = TRUE)


tm_stamp <- read.csv("tm_stamp_fm.csv")
tm_idx <- read.csv("tm_idx_fm.csv")

#get market index list
idx_fn <- 'kr_idx'
idx_gs_lst <- read_rtgs_idx(idx_fn)
kweight_lst<-idx_gs_lst[[2]]

ksmb_lst<-idx_gs_lst[[1]]

#get my pf list
pf_fn <- 'pf_idx'
pf_gs_lst <- read_rtgs_idx(pf_fn)
pfwt_lst<-pf_gs_lst[[2]]

pfsmb_lst<-pf_gs_lst[[1]]

#get every day focus jm list
focus_fn <- 'focus_idx'
fs_gs_lst <- read_rtgs_idx(focus_fn)
fswt_lst<-fs_gs_lst[[2]]

fssmb_lst<-fs_gs_lst[[1]]

smb_lst<-c(ksmb_lst,pfsmb_lst,fssmb_lst)

ksmb_code_lst <- lapply(smb_lst,function(x) lapply(x, function(x) paste0('A',code[match(x,code$name),3]$code)))
ksmb_code_vt<-t(as.data.frame(ksmb_code_lst))
ksmb_code_vt_uq<-unique(ksmb_code_vt)
print("start init volmon")
init_volmon(ksmb_code_vt_uq)
#recover from crush

 
i=0

while(TRUE) {


  Sys.sleep(60)
  
  cur_time <- format(Sys.time(),"%M")
  print(cur_time)
  if (endsWith(cur_time,'5')|| endsWith(cur_time,"7")){
    print(paste("its been", i, "seconds since this started, give or take a bit"))
    #updae file
    volmon <- read.csv("~/volmon.csv")
    update_vmon(today_str)
    vol_diff<-volmon_vol_diff()
    #calculate prices index 
    volmon_idx <- prices_idx_cal()
    #calculate normalized volume
    vol_nor_idx<-vol_nor_idx_cal()
    
    prices_vol_nor_idx_cal()

    #vol_idx_nor_df<- mapply(function(X,Y){merge(X,Y)},X=volmon_idx,Y=vol_nor_idx)
    volmon_idx_sep()
    
    #back up envir
    saveRDS(vmonenv,file="vmonenv.RData")
    saveRDS(diff_env,file="diff_env.RData")
    saveRDS(nor_vol_env,file="nor_vol_env.RData")
 
   }
}