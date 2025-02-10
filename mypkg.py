import OpenDartReader
from datetime import datetime
import time
import fnmatch

import telegram
import asyncio

import openpyxl
import pandas as pd
#telegram key
tele_api_key = "5824250303:AAF30nE1zYlP28DzS-Gd69yAegN-LgHU_ag"
chat_id = 1278251780
bot = telegram.Bot(token = tele_api_key)

#dart api key
api_key = '61b268a6147bf74f5b549a99bff5ea2974e011b2'
dart = OpenDartReader(api_key)

fn_prefix = 'perform3Q23_'

dart_url = 'http://dart.fss.or.kr/dsaf001/main.do?rcpNo='
naver_url = 'https://finance.naver.com/item/fchart.naver?code='

sfjm_fn = "stockfu.xls"
sfjm_list = pd.read_excel(sfjm_fn,dtype=str)

#start_date = input("input start date:")
today_date = datetime.today().strftime("%Y%m%d")
tgt_date = today_date

def issf(jm_name):
    return((sfjm_list['종목명']==jm_name).any())


def get_code(jm_name):
    df_code = dart.corp_codes 
    return(df_code[(df_code['corp_name']==jm_name) &(df_code['stock_code'] != ' ')]['stock_code'].iloc[0])

async def tele_send_msg(msg_txt):
    bot.send_message(chat_id,msg_txt)


def dart_mon_pf_td():
    tstamp = datetime.today().strftime("%Y-%m-%d %H:%M:%S")
    #print("check dart"+tstamp)
    list=dart.list(start=today_date,end=tgt_date,kind='I')
    #list1= dart.list(start=start_date,end=tgt_date,kind='A')
#list.query('report_nm.str.contains("30%")')
#list.query('report_nm.str.contains("잠정")')
    df_pef=list.query('report_nm.str.contains("잠정|실적|매출액")')
    df_pef=df_pef[~df_pef.report_nm.str.contains("기재정정|예고|전망")]
#pick only 'Y', 'K'
    #test = test[(test['corp_cls']=='Y') | (test['corp_cls']=='K')]
    df_pef['type'] = '잠정실적'
    #df_br=list1.query('report_nm.str.contains("분기보고서|사업보고서|반기보고서")')
    #df_br=df_br[~df_br.report_nm.str.contains("기재정정")]
    #test1 = test1[(test1['corp_cls']=='Y') | (test1['corp_cls']=='K')]
    #df_br['type'] = '사업보고서'
    tgt_url = dart_url+df_pef['rcept_no']
    df_pef['url'] = tgt_url
    return(df_pef)
