import sys
from PyQt5.QtWidgets import *
from PyQt5.QAxContainer import *
from PyQt5.QtGui import *
from PyQt5.QtCore import *
import datetime

from telethon import TelegramClient,events
from telethon import functions, types
from telethon.tl.types import MessageEntityTextUrl
import asyncio
import nest_asyncio
import openpyxl
import pandas as pd

from multiprocessing import Process, Queue

#from queue import Queue
from threading import Thread

from datetime import datetime
#import mypkg
import time

#telegram bot info
#token 8109717276:AAFyiB__nN-6BLxhaGr6pTW2TB8o1xwKRKs

#read kcode
kcod_df = pd.read_csv('c:\lab\kcode.csv',encoding='cp949')

#reference
#stackoverflow  https://stackoverflow.com/questions/71205781/combining-asyncio-library-telethon-with-pyqt
# jump2python  https://wikidocs.net/87141

global ref_pef, ref_br
ref_pef = pd.DataFrame()
ref_br = pd.DataFrame()

api_id = '26890695'
api_hash = '37fabab3680ade31f959c9c7f7241d71'
bot_id = 5824250303
awake_id = 1066938528
#my_bot_ch = 'https://t.me/coatle_bot'
my_bot_ch = 'https://t.me/coatle_kw_bot'

global aim_jm_lst
global chk_ready
chk_ready=True

global msg_dic

msg_dic = {}

global rq_prefix

global tslot
tslot = 0

global rt_jm_dic
rt_jm_dic = {}

global price_msg

price_msg = 0       

global perf_dic
perf_dic = {}

global hts_ready
hts_ready = False

global earing_season
earing_season = False




def run_bot(queue_in,queue_out):
    print("run_bot()")

  
    loop = asyncio.new_event_loop()
    asyncio.set_event_loop(loop)
    client = TelegramClient('kiwoom_session', api_id, api_hash)
    #client.send_message(my_bot_ch,'Kiwoom Bot is ready')
    print('bot is ready!')
    print('loop',loop)
    

    async def send_welcome_message(channel_username):
        # 클라이언트 연결
        #await client.start()
        print("Connected to Telegram")

        # 메시지 전송
        await client.send_message(channel_username, "welcome")
        print(f"Message sent to {channel_username}")

        # 연결 종료
        #await client.disconnect()

        
    @client.on(events.MessageEdited(chats=my_bot_ch))
    async def handler(event):
        global perf_dic
        global msg_dic
        global rt_jm_dic

        # Log the date of new edits
        if event.id in perf_dic:
            #print('Message', event.id, 'changed at', event.date)
            #await client.delete_messages(my_bot_ch,[event.id])
            #await client.send_message(my_bot_ch,"/delreal "+perf_dic[event.id])
            #print("del real data"+ perf_dic[event.id])
            del msg_dic[mypkg.get_code(perf_dic[event.id])]
            print(msg_dic)
            del rt_jm_dic[mypkg.get_code(perf_dic[event.id])]
            print(rt_jm_dic)

    @client.on(events.NewMessage(chats=my_bot_ch))
    async def handler(event):
        global msg_dic
        global rt_jm_dic
        if '/testkw' in event.message.message:
            print("[Bot] message:",event.message.message)
            queue_out.put(event.message.message)
        if '/qkw' in event.message.message:
            print("[Bot] message:",event.message.message)
            queue_out.put("quit")
            await event.message.delete()
            await client.disconnect()


            sys.exit()
        if '/tstsend' in event.message.message:
            print("send message")
            mes=await client.send_message(my_bot_ch,"test message")
            msg_dic["test message"] = mes.id
            print(mes.id)
        if '/tstdel' in event.message.message:
            print("delete message")
            print(msg_dic["test message"])
            await client.delete_messages(my_bot_ch,[msg_dic["test message"]])
        if '/delreal' in event.message.message:
            tgt_code = mypkg.get_code(event.message.message.split('"')[1])
            if tgt_code in msg_dic.keys():
                del msg_dic[tgt_code]
            print(msg_dic)
            if tgt_code in rt_jm_dic:
                del rt_jm_dic[tgt_code]
            print(rt_jm_dic)
            


    async def check_queue():
        global msg_dic
        global ref_pef
        global tslot
        global rt_jm_dic
        global price_msg
        global perf_dic
        global hts_ready
        global earing_season
        global chk_ready

        print('[BOT] check_queue(): start')
        while True:
            await asyncio.sleep(1)
            tslot = tslot + 1
            if chk_ready:
                await client.send_message(my_bot_ch,'ready')
                chk_ready = False

            if (tslot % 10 == 0) & hts_ready & earing_season:
                df_pef = mypkg.dart_mon_pf_td()
                df_pef_dt = df_pef.shape[0] - ref_pef.shape[0]

                if df_pef_dt != 0 :
                    df_pef_delta=df_pef[['stock_code','corp_name','url']][0:df_pef_dt]
                    for i in range(0,df_pef_dt):
                        aim_str = '/testkw aim '+"'"+df_pef['corp_name'].iloc[i]+"'"+' 0 0'
                        queue_out.put(aim_str)
                        #print(aim_str)
                        tstamp = datetime.today().strftime("%Y-%m-%d %H:%M:%S")
                        if mypkg.issf(df_pef['corp_name'].iloc[i]):
                            sf_str = "sf on"
                        else:
                            sf_str =""
                        
                        tgt_str = tstamp+" "+sf_str+" "+df_pef_delta['corp_name'].iloc[i]+' '+df_pef_delta['stock_code'].iloc[i]+' '+df_pef_delta['url'].iloc[i]
                        print("new announce "+tstamp)
                        print(tgt_str)
                        #asyncio.run(mypkg.tele_send_msg(tgt_str))
                        mes=await client.send_message(my_bot_ch,tgt_str)
                        tgt_str =""
                        perf_dic[mes.id] = df_pef['corp_name'].iloc[i]

                ref_pef=df_pef

            if len(rt_jm_dic) != 0:
                rt_jm_dic[list(rt_jm_dic.keys())[tslot%len(rt_jm_dic)]][1]=0
            
            if (tslot % 30 == 0) & (len(msg_dic) != 0):
                print(msg_dic)
                for k in msg_dic.keys():
                    tgt_str = tgt_str + msg_dic[k]+ '\n' 

                if price_msg != 0:
                    await client.delete_messages(my_bot_ch, [price_msg])
                    mes=await client.send_message(my_bot_ch,tgt_str)
                    price_msg = mes.id
                    tgt_str = ""
                else:
                    mes=await client.send_message(my_bot_ch,tgt_str)
                    price_msg = mes.id

                    

                
            if not queue_in.empty():

                cmd = queue_in.get()
                #print('[BOT] check_queue(): queue_in get:', cmd)
                if cmd.split(' ')[0] == 'stop':
                    await client.disconnect()
                    break
                elif cmd.split(' ')[0] == 'order':
                    #print('order'+cmd.split(' ')[1])
                    if cmd.split(' ')[1] in msg_dic:
                        await client.edit_message(entity=my_bot_ch, message=msg_dic[cmd.split(' ')[1]],text=cmd.split('_')[1])
                    else:
                        mes=await client.send_message(my_bot_ch,cmd.split('_')[1])
                        msg_dic[cmd.split(' ')[1]] = mes.id
                        print('init order msg dic')
                        print(msg_dic[cmd.split(' ')[1]])
                        print(mes.id)
                        
                elif cmd.split(' ')[0] == 'aim':
                    #print('code:'+cmd.split(' ')[1])
                    tgt_str = ''
                    if cmd.split(' ')[1] in msg_dic:
                        #print(msg_dic[cmd.split(' ')[1]])
                        #await client.edit_message(entity=my_bot_ch, message=msg_dic[cmd.split(' ')[1]],text=cmd.split('_')[1])
                        #await client.delete_messages(my_bot_ch, [msg_dic[cmd.split(' ')[1]]])
                        #mes=await client.send_message(my_bot_ch,cmd.split('_')[1])
                        msg_dic[cmd.split(' ')[1]] = cmd.split('_')[1]
                        print(msg_dic)        
                        #mes=await client.send_message(my_bot_ch,cmd.split('_')[1])
                        #price_msg = mes.id
                        #print("delete message:")
                        #print(msg_dic[cmd.split(' ')[1]])
                        #await client.invoke(DeleteMessagesRequest(my_bot_ch, [msg_dic[cmd.split(' ')[1]]]))
                        #await client.send_message(my_bot_ch,cmd.split('_')[1])

                    else:
                        
                        msg_dic[cmd.split(' ')[1]] = cmd.split('_')[1]
                        #print('init msg dic')
                        #print(msg_dic[cmd.split(' ')[1]])
                        #print(mes.id)


                    #print(mes)
                    #await client.delete_messages(entity=my_bot_ch, message_ids=[mes.id])


    loop.create_task(check_queue())
    
    with client:
        print('[BOT] start')
        client.run_until_disconnected()
        print('activate')
    
           

class bot_class(QThread):
    poped = pyqtSignal(str)
    def __init__(self,queue_in,queue_out):
        super().__init__()
        self.queue_in = queue_in
        self.queue_out = queue_out
    def run(self):
        while True:
            if not queue_out.empty():
                data = queue_out.get()
                self.poped.emit(data)

class MyWindow(QMainWindow):
    def __init__(self,queue_in,queue_out):

        super().__init__()
        global hts_ready

        self.codes = [
            "005930",
            "000660",
            "033180",
            "272210",
            "066570",
            "215600",
            "044380",
            "024110",
            "034220",
        ]

        self.df_10m = {'date':[],'time':[],'open':[],'high':[],'low':[],'close':[],'volume':[],'closedm1':[]}
        #self.ohlc10m_csvfn = 'test.csv'    




        self.bot_class = bot_class(queue_in,queue_out)
        self.bot_class.poped.connect(self.print_data)
        self.bot_class.start()

        self.queue_in = queue_in
        self.queue_out = queue_out

        self.target = None
        self.range = None
        self.account = "8068940011"
        self.accountf = "7018995931"
        self.amount = 10000
        self.hold = None


        self.initUI() # UI를 구성한다.



        self.ocx = QAxWidget("KHOPENAPI.KHOpenAPICtrl.1")
        self.ocx.OnEventConnect.connect(self._handler_login)
        self.ocx.OnReceiveTrData.connect(self._handler_tr_data)
        self.ocx.OnReceiveRealData.connect(self._handler_real_data)
        self.ocx.OnReceiveChejanData.connect(self._handler_chejan_data)

        self.login_event_loop = QEventLoop()
        self.CommConnect()          # 로그인이 될 때까지 대기
        hts_ready = True
        #self.rq_10m_data('005930')



                

        for code in self.codes:
            row   = self.codes.index(code)
            hname = self.ocx.dynamicCall("GetMasterCodeName(str)", code) # 한글 종목명
            self.table.setItem(row, 0, QTableWidgetItem(hname))
        #self.subs_jm_aim('403490',2)

        #self.run()
    
    def initUI(self):
        '''
        UI 생성 및 초기화
        '''
        width = 500
        
        # 메인 윈도우 설정 ---------------------------------------------
        self.setWindowTitle("Kiwoom Sample")
        
        self.setMinimumSize(width, 560) # 윈도우 크기를 고정한다.
        self.setMaximumSize(width, 560)
            
        # 상태표시줄 설정 ---------------------------------------------
        self.statusBar = QStatusBar()
        self.setStatusBar(self.statusBar)
        self.statusBar.showMessage("Ready...")
        
        # 테이블 위젯 설정 ---------------------------------------------
        self.table = QTableWidget(self)
        self.table.resize(width,1800)
        self.table.setEditTriggers(QAbstractItemView.NoEditTriggers)
        #text box wedget
        self.plain_text_edit = QPlainTextEdit(self)
        self.plain_text_edit.setReadOnly(True)
        self.plain_text_edit.move(50, 400)
        self.plain_text_edit.resize(380, 280)
        
        col_headers = ["종목명", "현재가", "전일대비", "거래량",] # 헤더 항목 설정
        
        self.table.setColumnCount(len(col_headers))
        self.table.setHorizontalHeaderLabels(col_headers)
        self.table.setRowCount(len(self.codes))
        self.table.setAlternatingRowColors(True)
        
        self.table.setColumnWidth(0, 95) # 헤더 필드 폭 설정
        self.table.setColumnWidth(1, 55)
        self.table.setColumnWidth(2, 55)
        self.table.horizontalHeader().setSectionResizeMode(3, QHeaderView.Stretch) # 마지막은 자동으로 조정
        self.table.resizeRowsToContents()
        
        # 현재가 요청 버튼 설정 ---------------------------------------------
        self.requestTrButton = QPushButton("Get Price", self)
        self.requestTrButton.setGeometry(width - 200, 200, 100, 25)
        self.requestTrButton.clicked.connect(self.requestTrButton_clicked)
        
        # 종료 버튼 설정 ---------------------------------------------
        self.exitButton = QPushButton("Exit", self)
        self.exitButton.setGeometry(width - 100, 200, 100, 25)
        self.exitButton.clicked.connect(self.exitButton_clicked)
    
    def stop_bot(self):
        self.queue_in.put('stop')
    
    @pyqtSlot()
    def exitButton_clicked(self):
        '''
        Exit 버튼을 클릭시 실행
        
        프로그램을 종료한다.
        '''
        QApplication.instance().quit()
 
    
    @pyqtSlot()
    def requestTrButton_clicked(self):
        '''
        TR 조회 버튼 클릭시 실행
        
        주식기본정보요청("opt10001") 대신 관심종목정보요청("OPTKWFID") TR을 사용한다.
        수신 데이터 항목은 차이가 없지만 주식 기본정보요청 TR은 다종목 수신시 빈 데이터를 받는 오류가 자주 발생한다.
        다종목 수신시 관심종목정보요청 TR을 사용할 것
        
        ※ 참고)
        주식기본정보요청은 100종목 조회시 TR 100회 요청이 필요하지만 관심종목정보요청은 TR 1회로 요청할 수 있다.
        대신 CommKwRqData 전용함수를 사용한다. 수신 데이터는 multi 데이터임(주식기본정보요청 TR은 single 데이터)
        '''
        # 관심종목정보요청 : "OPTKWFID"
        sCodes = ";".join(self.codes)
        self.ocx.dynamicCall("CommKwRqData(str, int, int, int, str, str)", sCodes, 0, len(self.codes), 0, "OPTKWFID_req", "RQTR")
    @pyqtSlot(str)       
    def print_data(self,data):
        global rt_jm_dic
        print(data)

        if data == 'quit':
            QApplication.instance().quit()

        buyorsell = data.split(' ')[1]
        
        jm_name = data.split("'")[1]

        if buyorsell != 'sf':
            #jm_code = mypkg.get_code(jm_name)
            jm_code = kcod_df.loc[kcod_df['name']==jm_name,'scode'].values[0]
            print(jm_name)
        
        if buyorsell != '10m':
            jm_qty = data.split(' ')[3]
            jm_tgt_price = data.split(' ')[4]

        if buyorsell == 'bl':
            self.plain_text_edit.appendPlainText("[BOT]"+'Buy Limit:'+jm_name+jm_code+ jm_qty+'@'+jm_tgt_price)
            #self.queue_in.put("order "+jm_code+" _"+'Buy Limit:'+jm_code+ jm_qty+'@'+jm_tgt_price)
            self.SendOrder("매수", "8001", self.account, 1, jm_code, int(jm_qty), int(jm_tgt_price), "00", "")
        elif buyorsell == 'bm':
            self.plain_text_edit.appendPlainText("[BOT]"+'Buy Market:'+jm_name+jm_code+ jm_qty)
            #self.queue_in.put("order "+jm_code+" _"+'Buy Market:'+jm_code+ jm_qty)
            self.SendOrder("매수", "8001", self.account, 1, jm_code, int(jm_qty), int(jm_tgt_price), "03", "")
            self.SendOrder("매도", "8001", self.account, 1, "005930", 10, 0, "03", "")
        

        elif buyorsell == 'sl':
            self.plain_text_edit.appendPlainText("[BOT]"+'Sell Limit:'+jm_name+jm_code+ jm_qty+'@'+jm_tgt_price)
            #self.queue_in.put("order "+jm_code+" _"+'Sell Limit:'+jm_code+ jm_qty+'@'+jm_tgt_price)
            self.SendOrder("매도", "8002", self.account, 2, jm_code, int(jm_qty), int(jm_tgt_price), "00", "")
        
        elif buyorsell == 'sm':
            self.plain_text_edit.appendPlainText("[BOT]"+'Sell Market:'+jm_name+jm_code+ jm_qty)
            #self.queue_in.put("order "+jm_code+" _"+'Sell Market:'+jm_code+ jm_qty)
            self.SendOrder("매도", "8002", self.account, 2, jm_code, int(jm_qty), int(jm_tgt_price), "03", "")
        elif buyorsell == 'aim':  #real jm setting
            #self.rq_opt10003(jm_code)
            self.plain_text_edit.appendPlainText("[BOT]"+'Aim jm:'+jm_name+jm_code+ jm_qty)
            #self.queue_in.put("aim "+jm_code+" _"+'Aim jm:'+jm_code+ jm_qty+'@'+jm_tgt_price)
            rt_jm_dic[jm_code]=[jm_name,0]
            print(jm_name + jm_code+"set real")
            self.SetRealReg(2, jm_code, "20",1)
            #self.subs_jm_aim(aim_jm_lst,2)
        elif buyorsell == 'sf':  #stock future
            #self.rq_opt10003(jm_code)
            self.plain_text_edit.appendPlainText("[BOT]"+'sf '+jm_name)
            #self.queue_in.put("aim "+jm_code+" _"+'Aim jm:'+jm_code+ jm_qty+'@'+jm_tgt_price)
            #rt_jm_dic[jm_code]=[jm_name,0]
            #print(jm_name + jm_code+"set real")
            self.SetRealReg(2, jm_code, "20",1)
            #self.subs_jm_aim(aim_jm_lst,2)
        elif buyorsell == '10m': #get 10m ohlc
            self.plain_text_edit.appendPlainText("[BOT]"+'sf '+jm_name)
            self.rq_10m_data(jm_code)
            self.df_10m = {'date':[],'time':[],'open':[],'high':[],'low':[],'close':[],'volume':[],'closedm1':[]}
            

        self.plain_text_edit.appendPlainText("[BOT]"+data)
    
    def rq_10m_data(self,jm_code):

        self.SetInputValue("종목코드",jm_code)
        self.SetInputValue("틱범위",5)
        self.SetInputValue("수정주가구분",0)

        self.ohlc10m_csvfn = 'A'+jm_code+'.csv'

        self.CommRqData("opt10080_req", "opt10080", 0, "1999")

        self.rq_10m_loop = QEventLoop()
        self.rq_10m_loop.exec()
        i=0
        print("5m req 0")
        while self.remained == True:
            time.sleep(0.2)
            i = i+1
            print("5m req ",i)
            self.SetInputValue("종목코드", jm_code)
            self.SetInputValue("틱범위", 5)
            self.SetInputValue("수정주가구분", 0)
            self.CommRqData("opt10080_req", "opt10080", 2, "1999")
            self.rq_10m_loop = QEventLoop()
            self.rq_10m_loop.exec()



    def rq_opt50001(self,jm_code):

        sf_code = '1'+jm_code+'000'
        self.SetInputValue("종목코드",jm_code)
        self.CommRqData("주식선물현재가", "opt50001", 0, "0003")
    
    def rq_opt10003(self,jm_code):
        global rq_prefix
        rq_prefix = jm_code
        self.SetInputValue("종목체결",jm_code)
        self.CommRqData("종목체결", "opt10003", 0, "0003")
    
    def CommConnect(self):
        self.ocx.dynamicCall("CommConnect()")
        self.login_event_loop.exec()

    def run(self):
        accounts = self.GetLoginInfo("ACCNO")
        #self.account = accounts.split(';')[0]
        print(accounts)

        # 주식체결 (실시간)
        self.subscribe_market_time('1')
        self.subscribe_stock_conclusion('2')

        self.SendOrder("매도", "8001", self.account, 1, "005930", 10, 0, "03", "")
        print("sell")

    def GetLoginInfo(self, tag):
        data = self.ocx.dynamicCall("GetLoginInfo(QString)", tag)
        return data

    def _handler_login(self, err_code):
        if err_code == 0:
            self.plain_text_edit.appendPlainText("로그인 완료") 
        self.login_event_loop.exit()
        self.queue_in.put("login kiwoom completed")

    def _handler_tr_data(self, screen_no, rqname, trcode, record, next):

        if trcode == "OPTKWFID": # 관심종목정보요청
            nRow = self.ocx.dynamicCall("GetRepeatCnt(str, str)", trcode, rqname)
            print("trReceive")
            
            for i in range(nRow):
                code   = self.ocx.dynamicCall("GetCommData(str, str, int, str)", trcode, "", i, "종목코드").strip()
                price  = self.ocx.dynamicCall("GetCommData(str, str, int, str)", trcode, "", i, "현재가"  ).strip()
                debi   = self.ocx.dynamicCall("GetCommData(str, str, int, str)", trcode, "", i, "전일대비").strip()
                volume = self.ocx.dynamicCall("GetCommData(str, str, int, str)", trcode, "", i, "거래량"  ).strip()
                
                #self.ocx.msg_data_received.emit(code, price, debi, volume) # 데이터 수신 완료 시그널 보내기
                row = self.codes.index(code)
                item = QTableWidgetItem(f"{int(price):,}")
                item.setTextAlignment(Qt.AlignVCenter | Qt.AlignRight)
                self.table.setItem(row, 1, item)
                item = QTableWidgetItem(f"{int(debi):,}")
                item.setTextAlignment(Qt.AlignVCenter | Qt.AlignRight)
                self.table.setItem(row, 2, item)
        
                item = QTableWidgetItem(f"{int(volume):,}")
                item.setTextAlignment(Qt.AlignVCenter | Qt.AlignRight)
                self.table.setItem(row, 3, item)
            
        self.statusBar.showMessage("TR 수신 완료")

        if rqname == '주식선물현재가':
            tgt_str= f"sf {rq_prefix} _{self.GetCommData(trcode, rqname, 0,'종가')}  체결시간: {self.GetCommData(trcode, rqname, 0,'시간')} 등락율:{self.GetCommData(trcode, rqname, 0, '대비율')} 누적거래량:{self.GetCommData(trcode, rqname, 0, '누적거래량')}"

        if   rqname == "종목체결":
            tgt_str= f"aim {rq_prefix} _{self.GetCommData(trcode, rqname, 0,'종가')}  체결시간: {self.GetCommData(trcode, rqname, 0,'시간')} 등락율:{self.GetCommData(trcode, rqname, 0, '대비율')} 누적거래량:{self.GetCommData(trcode, rqname, 0, '누적거래량')}"
            print(tgt_str)
            self.queue_in.put(tgt_str)
            

        elif rqname == "KODEX일봉데이터":
            now = datetime.datetime.now()
            today = now.strftime("%Y%m%d")
            일자 = self.GetCommData(trcode, rqname, 0, "일자")

            # 장시작 후 TR 요청하는 경우 0번째 row에 당일 일봉 데이터가 존재함
            if 일자 != today:
                고가 = self.GetCommData(trcode, rqname, 0, "고가")
                저가 = self.GetCommData(trcode, rqname, 0, "저가")
            else:
                일자 = self.GetCommData(trcode, rqname, 1, "일자")
                고가 = self.GetCommData(trcode, rqname, 1, "고가")
                저가 = self.GetCommData(trcode, rqname, 1, "저가")

            self.range = int(고가) - int(저가)
            info = f"일자: {일자} 고가: {고가} 저가: {저가} 전일변동: {self.range}"
            self.plain_text_edit.appendPlainText(info)

        elif rqname == "예수금조회":
            주문가능금액 = self.GetCommData(trcode, rqname, 0, "주문가능금액")
            주문가능금액 = int(주문가능금액)
            self.amount = int(주문가능금액 * 0.2)
            self.plain_text_edit.appendPlainText(f"주문가능금액: {주문가능금액} 투자금액: {self.amount}")

        elif rqname == "계좌평가현황":
            rows = self.GetRepeatCnt(trcode, rqname)
            for i in range(rows):
                종목코드 = self.GetCommData(trcode, rqname, i, "종목코드")
                보유수량 = self.GetCommData(trcode, rqname, i, "보유수량")

                if 종목코드[1:] == "229200":
                    self.previous_day_hold = True
                    self.previous_day_quantity = int(보유수량)
        elif rqname == "opt10080_req":
            print(next)
            if next == '2':
                self.remained = True
            else:
                self.remained = False
            
            data_cnt = self.GetRepeatCnt(trcode,rqname)
            print(data_cnt)
            for i in range(data_cnt):
                #print("분봉 데이터 저장중")
                date_temp = self.GetCommData(trcode,rqname, i, "체결시간").strip()
                date = date_temp[0:8]
                time = date_temp[8:]

                close = self.GetCommData(trcode, rqname, i, "현재가").strip().replace("-","")
                low = self.GetCommData(trcode, rqname, i, "저가").strip().replace("-","")
                high = self.GetCommData(trcode,rqname, i, "고가").strip().replace("-","")
                open = self.GetCommData(trcode, rqname, i, "시가").strip().replace("-","")
                volume = self.GetCommData(trcode, rqname, i, "거래량").strip().replace("-","")
                closedm1 = self.GetCommData(trcode, rqname, i, "전일종가").strip().replace("-","")

                self.df_10m['date'].append(date)
                self.df_10m['time'].append(time)
                self.df_10m['open'].append(open)
                self.df_10m['high'].append(high)
                self.df_10m['low'].append(low)
                self.df_10m['close'].append(close)
                self.df_10m['volume'].append(volume)
                self.df_10m['closedm1'].append(closedm1)

            
            self.rq_10m_loop.exit()
            
            if self.remained == False:  
                df_conv = pd.DataFrame(self.df_10m)
                print("now df conversion")
                df_conv.to_csv(self.ohlc10m_csvfn,index=False)
                



    def GetRepeatCnt(self, trcode, rqname):
        ret = self.ocx.dynamicCall("GetRepeatCnt(QString, QString)", trcode, rqname)
        return ret

    # 실시간 타입을 위한 메소드
    def SetRealReg(self, screen_no, code_list, fid_list, real_type):
        self.ocx.dynamicCall("SetRealReg(QString, QString, QString, QString)", 
                              screen_no, code_list, fid_list, real_type)

    def GetCommRealData(self, code, fid):
        data = self.ocx.dynamicCall("GetCommRealData(QString, int)", code, fid) 
        return data

    def DisConnectRealData(self, screen_no):
        self.ocx.dynamicCall("DisConnectRealData(QString)", screen_no)

    # 실시간 이벤트 처리 핸들러 
    def _handler_real_data(self, code, real_type, real_data):
        global rt_jm_dic
        if real_type == "주식체결":
            if code in rt_jm_dic:
                if rt_jm_dic[code][1] ==0:
                    pr_open = abs(int(self.GetCommRealData(code, 16)))
                    pr_low = abs(int(self.GetCommRealData(code, 18)))
                    lc_trig = str(int(pr_open*0.968))
                    get_trig = str(int(pr_open*0.982))
                    open_rev = str(round((1-pr_low/pr_open)*100,1)) 
                    tgt_str= f"aim {code} _{rt_jm_dic[code][0]}{mypkg.issf(rt_jm_dic[code][0])}{self.GetCommRealData(code, 10)}  체결시간: {self.GetCommRealData(code, 20)} 등락율:{self.GetCommRealData(code, 12)} rev:{open_rev} lc-trig:{lc_trig} get-trig:{get_trig} 시가:{self.GetCommRealData(code, 16)} 저가:{self.GetCommRealData(code, 18)} "
                    #print(tgt_str)
                    #self.plain_text_edit.appendPlainText(tgt_str)
                    self.queue_in.put(tgt_str)
                    rt_jm_dic[code][1] = 1

        if real_type == "주문체결":
            self.plain_text_edit.appendPlainText(f"real data 주문체결")


    def _handler_chejan_data(self, gubun, item_cnt, fid_list):
        print("chejan")
        print(gubun)
        #print(fid_list)
        if gubun == '0':      # 체결수량
            tgt_str= f"order {self.GetChejanData('9203')} _{self.GetChejanData('302').replace(' ','')} 주문가격: {self.GetChejanData('901')} 매도(1)/매수(2): {self.GetChejanData('907')} 주문:{self.GetChejanData('900')} 체결량:{self.GetChejanData('911')} 미체결:{self.GetChejanData('902')}"
            print(tgt_str)
            self.plain_text_edit.appendPlainText(tgt_str)
            self.queue_in.put(tgt_str)
    
    def subs_jm_aim(self,jm_code, screen_no):
        self.SetRealReg(screen_no, jm_code, "20", 0)
        self.plain_text_edit.appendPlainText("주식체결 구독신청")

    # TR 요청을 위한 메소드
    def SetInputValue(self, id, value):
        self.ocx.dynamicCall("SetInputValue(QString, QString)", id, value)

    def CommRqData(self, rqname, trcode, next, screen_no):
        self.ocx.dynamicCall("CommRqData(QString, QString, int, QString)", 
                              rqname, trcode, next, screen_no)

    def GetCommData(self, trcode, rqname, index, item):
        data = self.ocx.dynamicCall("GetCommData(QString, QString, int, QString)", 
                                     trcode, rqname, index, item)
        return data.strip()

    def SendOrder(self, rqname, screen, accno, order_type, code, quantity, price, hoga, order_no):
        self.ocx.dynamicCall("SendOrder(QString, QString, QString, int, QString, int, int, QString, QString)",
                             [rqname, screen, accno, order_type, code, quantity, price, hoga, order_no])

    def GetChejanData(self, fid):
        data = self.ocx.dynamicCall("GetChejanData(int)", fid)
        return data


if __name__ == "__main__":
    
    queue_in  = Queue()  # to send data to bot
    queue_out = Queue()  # to receive data from bot 
    process_bot = Thread(name="run_bot",target=run_bot, args=(queue_in, queue_out),daemon=True)
    process_bot.start()
                

    app = QApplication(sys.argv)

    window = MyWindow(queue_in, queue_out)
    window.show()

    app.exec_()