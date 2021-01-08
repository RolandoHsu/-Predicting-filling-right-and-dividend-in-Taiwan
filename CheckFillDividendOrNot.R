library(tidyverse)
library(data.table)
library(readxl)
library(stringr)
library(ggplot2)
library(DBI)
library(lubridate) # %m+% add one month 
library(corrplot)
library(RMySQL)

##### SQL 連接設定 #####
channel <- dbConnect(MySQL(),
                     user = "root", #使用者名稱
                     password = "***", #密碼
                     dbname = "***", #資料庫名稱
                     host = "localhost")


##### 導入資料 ##### 
DividendPolicy_Chi <- read_xlsx("Data/DividendPolicy_Chi.xlsx") %>% setDT()
DividendPolicy_Eng <- read_xlsx("Data/DividendPolicy_Eng.xlsx") %>% setDT()

# 建立中英文欄位名稱對照表
DividendPolicy_Colnames_EngChi <- data.frame(names(DividendPolicy_Chi), 
                              names(DividendPolicy_Eng)) %>% 
  setDT() %>% 
  setnames(c("names.DividendPolicy_Chi.", "names.DividendPolicy_Eng."),
           c("Chi", "Eng"))

rm(DividendPolicy_Chi)

# 導入股價資料
# load("Data/TaiwanPriceData.RData") # 載入後名稱為 Data_Final
# 
# TaiwanStocksPriceData <- Data_Final %>% 
#   setDT() 
# 
# rm(Data_Final)

# 公司名稱中英文對照
StockName_ChiEng <- read_xlsx("Data/StockName_ChiEng.xlsx") %>% 
  setDT() %>% 
  .[, `:=`(`股票代碼` = str_split(`公司簡稱`, " ", simplify = T)[, 1])] %>% 
  select("股票代碼", everything())

##### UnadjustData ##### 
# source("Script/AdjustTEJData.R", encoding = "utf-8")

# 公司基本資料
CompanyBasicData <- read_xlsx("Data/CompanyBasicData.xlsx") %>% 
  setDT() %>% 
  setnames(., 
           c("公司簡稱", "最近上市日", "會計月份", "TSE新產業名"), 
           c("Name", "ListedDate", "AccountingMonth", "Industry")) %>% 
  .[, ListedYear := year(ListedDate)]

load("Data/UnadjustOpenCloseData_Combine.RData")

UnadjustPriceData <- UnadjustOpenClose_Data %>% 
  setDT()

rm(UnadjustOpenClose_Data)

UnadjustPrice <- copy(UnadjustPriceData) %>% 
  .[, `:=`(StockCode = str_split(Name, " ", simplify = T)[, 1])] %>% 
  select("StockCode", everything()) %>% 
  .[, Year := year(Time) %>% as.character()] %>% 
  select("StockCode", "Year", everything()) %>% 
  setnames(., "Time", "Date") %>% 
  .[is.na(Close) == F]

## 將 UnadjustPrice 存入 SQL 中
# UnadjustPrice_ToSQL <- copy(UnadjustPrice) %>%
#   as.data.frame()
# 
# Encoding(UnadjustPrice_ToSQL$Name) <- "unknown"
# 
# dbWriteTable(channel, "UnadjustPrice", UnadjustPrice_ToSQL)
# rm(UnadjustPrice_ToSQL)

rm(UnadjustPriceData)

DividendPolicy_Data <- copy(DividendPolicy_Eng) %>% 
  .[!(is.na(`Last Ex_Date(Cash Div.)`) == T & is.na(`Ex_Date(Stock)`) == T)] %>% 
  .[, -c(31:90)] %>% 
  .[, .SD, .SDcols = c("Company Code", "Dividends-Cash-Distri.-Year", "Dividend_Pay_Times", "Payout Ratio", 
                       "Last Ex_Date(Cash Div.)", "Total Dividends-Cash -Earnings", "Total Dividends-Cash -Capital", "Total Dividends-Cash -Special", "Total Dividends-Cash",
                       "Ex_Date(Stock)", "Stock Div.-Earnings", "Stock Div.-Capital", "Subtotal Stock Div.")] %>%  # Subtotal Stock Div. 代表總股票股利
  .[order(`Company Code`, `Dividends-Cash-Distri.-Year`)] %>% 
  .[, `:=`(StockCode = str_split(`Company Code`, " ", simplify = T)[, 1])] %>% 
  select("StockCode", everything()) %>% 
  .[, `:=`(`Dividends-Cash-Distri.-Year` = year(`Dividends-Cash-Distri.-Year`) %>% as.character(),
           TotalDividend = `Total Dividends-Cash` + `Subtotal Stock Div.`)]

## 發現此三間公司在1992年股票股利有異 手動更改
DividendPolicy_Data[(`Company Code` == "1220 Tairoun Products" & as.character(`Ex_Date(Stock)`) == "1992-08-10")]$`Total Dividends-Cash` <- 0
DividendPolicy_Data[(`Company Code` == "1220 Tairoun Products" & as.character(`Ex_Date(Stock)`) == "1992-08-10")]$`Stock Div.-Earnings` <- 1.5
DividendPolicy_Data[(`Company Code` == "1220 Tairoun Products" & as.character(`Ex_Date(Stock)`) == "1992-08-10")]$`Stock Div.-Capital` <- 0
DividendPolicy_Data[(`Company Code` == "1220 Tairoun Products" & as.character(`Ex_Date(Stock)`) == "1992-08-10")]$`Stock Div.-Capital` <- 1.5

DividendPolicy_Data[(`Company Code` == "1446 Hong Ho Precision" & as.character(`Ex_Date(Stock)`) == "1992-08-26")]$`Total Dividends-Cash` <- 0
DividendPolicy_Data[(`Company Code` == "1446 Hong Ho Precision" & as.character(`Ex_Date(Stock)`) == "1992-08-26")]$`Stock Div.-Earnings` <- 1.5
DividendPolicy_Data[(`Company Code` == "1446 Hong Ho Precision" & as.character(`Ex_Date(Stock)`) == "1992-08-26")]$`Stock Div.-Capital` <- 1
DividendPolicy_Data[(`Company Code` == "1446 Hong Ho Precision" & as.character(`Ex_Date(Stock)`) == "1992-08-26")]$`Stock Div.-Capital` <- 2.5

DividendPolicy_Data[(`Company Code` == "2702 Hotel Holiday Garden" & as.character(`Ex_Date(Stock)`) == "1992-12-11")]$`Total Dividends-Cash` <- 0
DividendPolicy_Data[(`Company Code` == "2702 Hotel Holiday Garden" & as.character(`Ex_Date(Stock)`) == "1992-12-11")]$`Stock Div.-Earnings` <- 1
DividendPolicy_Data[(`Company Code` == "2702 Hotel Holiday Garden" & as.character(`Ex_Date(Stock)`) == "1992-12-11")]$`Stock Div.-Capital` <- 1
DividendPolicy_Data[(`Company Code` == "2702 Hotel Holiday Garden" & as.character(`Ex_Date(Stock)`) == "1992-12-11")]$`Stock Div.-Capital` <- 2

### 由於有些公司只有發現金股利，有些只有發股票股利，有些雖然兩者都有發但是發的日期不同，因此分別計算
## DividendPolicy_1 代表兩者皆有發，現金股利與股票股利發放日期相同，因此合併計算即可
## DividendPolicy_2 代表僅有發其中一個，因此抓現金股利或股票股利的日期與股利金額
## DividendPolicy_3 代表兩者皆有發但發放日期不同，因此先抓現金股利的金額以及日期
## DividendPolicy_4 代表兩者皆有發但發放日期不同，因此先抓股票股利的金額以及日期

DividendPolicy_1 <- copy(DividendPolicy_Data) %>% 
  .[`Last Ex_Date(Cash Div.)` == `Ex_Date(Stock)`] %>% 
  .[, .SD, .SDcols = c("StockCode", "Company Code", "Dividends-Cash-Distri.-Year", "Dividend_Pay_Times",
                       "Payout Ratio", "Last Ex_Date(Cash Div.)", "TotalDividend")] %>% 
  setnames("Last Ex_Date(Cash Div.)", "DividendDate")

DividendPolicy_2 <- copy(DividendPolicy_Data) %>% 
  .[(is.na(`Last Ex_Date(Cash Div.)`) == T | is.na(`Ex_Date(Stock)`) == T)] %>% 
  .[, DividendDate := ifelse(is.na(`Last Ex_Date(Cash Div.)`) == T, as.character(`Ex_Date(Stock)`), as.character(`Last Ex_Date(Cash Div.)`)) %>% as.POSIXct()] %>% 
  .[, .SD, .SDcols = c("StockCode", "Company Code", "Dividends-Cash-Distri.-Year", "Dividend_Pay_Times",
                       "Payout Ratio", "DividendDate", "TotalDividend")]

DividendPolicy_3 <- copy(DividendPolicy_Data) %>% 
  .[(is.na(`Ex_Date(Stock)`) == F & `Last Ex_Date(Cash Div.)` != `Ex_Date(Stock)`)] %>% 
  .[, .SD, .SDcols = c("StockCode", "Company Code", "Dividends-Cash-Distri.-Year", "Dividend_Pay_Times",
                       "Payout Ratio", "Last Ex_Date(Cash Div.)", "Total Dividends-Cash")] %>% 
  setnames(c("Last Ex_Date(Cash Div.)", "Total Dividends-Cash"), c("DividendDate", "TotalDividend"))

DividendPolicy_4 <- copy(DividendPolicy_Data) %>% 
  .[(is.na(`Ex_Date(Stock)`) == F & `Last Ex_Date(Cash Div.)` != `Ex_Date(Stock)`)] %>% 
  .[, .SD, .SDcols = c("StockCode", "Company Code", "Dividends-Cash-Distri.-Year", "Dividend_Pay_Times",
                       "Payout Ratio", "Ex_Date(Stock)", "Subtotal Stock Div.")] %>% 
  setnames(c("Ex_Date(Stock)", "Subtotal Stock Div."), c("DividendDate", "TotalDividend"))

DividendPolicy <- rbind(DividendPolicy_1, DividendPolicy_2, DividendPolicy_3, DividendPolicy_4) %>% 
  .[order(`Company Code`, `DividendDate`)] %>% 
  .[, DividendDate := as.Date(DividendDate)] %>% 
  .[, Year := year(DividendDate) %>% as.character()]

rm(DividendPolicy_Data, DividendPolicy_1, DividendPolicy_2, DividendPolicy_3, DividendPolicy_4)

# ## 將 DividendPolicy 存入 SQL 中
# DividendPolicy_ToSQL <- copy(DividendPolicy) %>%
#   as.data.frame()
# 
# Encoding(DividendPolicy_ToSQL$`Company Code`) <- "unknown"
# 
# dbWriteTable(channel, "DividendPolicy", DividendPolicy_ToSQL)
# rm(DividendPolicy_ToSQL)

# 找出有填息的公司以及其所屬的年份

FillDivStock_Data <- UnadjustPrice %>% 
  left_join(., DividendPolicy, by = c("StockCode", "Year")) %>% 
  setDT() %>% 
  .[order(StockCode, DividendDate)] %>% 
  .[as.Date(Date) >= (as.Date(DividendDate) - 1)] %>% 
  .[, head(.SD, 7), by = c("StockCode", "DividendDate")] %>% 
  .[, Threhold := head(Close, 1), by = c("StockCode", "DividendDate")] %>% 
  .[, .SD[-1], by = c("StockCode", "DividendDate")] %>% 
  .[, FillOrNot := ifelse(Close >= Threhold, 1, 0)] 

# rm(UnadjustPrice)

FillDivStock <- copy(FillDivStock_Data) %>% 
  .[, .SD, .SDcols = c("StockCode", "Name", "DividendDate", "FillOrNot")] %>% 
  .[, sum(FillOrNot), by = c("StockCode", "Name", "DividendDate")] %>% 
  .[, FillDiv_Sign := ifelse(V1 != 0, 1, 0)] %>% 
  .[, .SD, .SDcols = -c("V1")] %>% 
  .[, Year := year(DividendDate)] %>% 
  .[CompanyBasicData, on = "Name"] %>% 
  .[!(Industry %in% c("M2800 金融業"))] %>% 
  .[!(Name %in% c("2841 台開"))] # 台開資料缺失問題過於研究，直接刪除

###### 計算 3 7 30 天 三個月 半年 一年 三年後有多少比例的公司可以填權息 #####
# Days <- c(3, 7, 30, 90, 180, 360, 1080)
# 
# FillRatio <- data.table(Days, "0")
# 
# for (i in 1:7) {
#   FillDivStock_Data <- UnadjustPrice %>% 
#     left_join(., DividendPolicy, by = c("StockCode", "Year")) %>% 
#     setDT() %>% 
#     .[order(StockCode, DividendDate)] %>% 
#     .[as.Date(Date) >= (as.Date(DividendDate) - 1)] %>% 
#     .[, head(.SD, (i+1)), by = c("StockCode", "DividendDate")] %>% 
#     .[, Threhold := head(Close, 1), by = c("StockCode", "DividendDate")] %>% 
#     .[, .SD[-1], by = c("StockCode", "DividendDate")] %>% 
#     .[, FillOrNot := ifelse(Close >= Threhold, 1, 0)]
#   
#   
#   FillDivStock <- copy(FillDivStock_Data) %>% 
#     .[, .SD, .SDcols = c("StockCode", "Name", "DividendDate", "FillOrNot")] %>% 
#     .[, sum(FillOrNot), by = c("StockCode", "Name", "DividendDate")] %>% 
#     .[, FillDiv_Sign := ifelse(V1 != 0, 1, 0)] %>% 
#     .[, .SD, .SDcols = -c("V1")] %>% 
#     .[, Year := year(DividendDate)] %>% 
#     .[CompanyBasicData, on = "Name"] %>% 
#     .[!(Industry %in% c("M2800 金融業"))] %>% 
#     .[!(Name %in% c("2841 台開"))] # 台開資料缺失問題過於研究，直接刪除
# 
#   Ratio_Data <- copy(FillDivStock) %>% 
#     pull(FillDiv_Sign) %>% 
#     table() %>% 
#     as.data.table()
#   
#   Ratio <- round((Ratio_Data$N[2] / (Ratio_Data$N[1] + Ratio_Data$N[2]))*100, 2)
#   
#   FillRatio[i, 2] <- str_c(Ratio, " %")
#   
#   print(i)
# }
# 
# FillRatio <- copy(FillRatio) %>% 
#   setnames(., c("Days", "V2"), c("Days", "FillRatio"))

## 將結果存入MYSQL 

FillRatio_ToSQL <- copy(FillRatio) 

dbWriteTable(channel, "FillRatio_ToSQL", FillRatio_ToSQL)
rm(FillRatio_ToSQL)


##2020 還存在的公司名單
SurviveIn2020 <- copy(FillDivStock_Data) %>% 
  .[, tail(.SD, 1), by = Name] %>% 
  .[Year == 2020] %>% 
  pull(Name)

FillDivStock_SurviveIn2020 <- copy(FillDivStock_Data) %>% 
  .[Name %in% SurviveIn2020]

rm(FillDivStock_Data)

# 歷年填息次數排名
Rank_FillDiv <- copy(FillDivStock) %>%
  .[Year >= 2005] %>% 
  .[, .(Count = .N, Sum = sum(FillDiv_Sign)), by = Name] %>% 
  .[, FillProbability := round((Sum / Count)*100)] %>% 
  .[Count >= 5] %>% 
  .[order(FillProbability, decreasing = T)]

# 找尋填息頻率較高的個股
# 1. 歷年來填息次數高
# 2. 成立年數不能太低 (至少有10年的資料)
# 3. 近年來填息次數高的較佳

###### 個別股票過去填息表現 (以頻率表示) #####
# FillFrequency_Data <- copy(FillDivStock) %>% 
#   .[, .SD, .SDcols = c("StockCode", "Name", "DividendDate", "FillDiv_Sign")] %>% 
#   .[, DividendTimes := 1:.N, by = Name]  %>% 
#   .[, HistoryFillTimes := 0]
# 
# FillFrequency <- NULL
# 
# for (i in unique(FillFrequency_Data$Name)) {
#   
#   data <- copy(FillFrequency_Data) %>% 
#     .[Name == i] %>% 
#     .[order(DividendDate)]
#   
#   for (j in 1:nrow(data)) {
#     
#     data[j, 6] <- copy(data) %>% 
#       .[1 : j, ] %>% 
#       pull(FillDiv_Sign) %>% 
#       sum()
#   }
#   
#   FillFrequency <- rbind(FillFrequency, data)
#   
#   print(i)
#   
# }
# 
# FillFrequency <- copy(FillFrequency) %>% 
#   .[, FillFrequency := round((HistoryFillTimes / DividendTimes)*100, 2)]

# # 將運算結果儲存至MySql
# FillFrequency_Result_ToSQL <- copy(FillFrequency) %>%
#   as.data.frame()
# 
# channel <- dbConnect(MySQL(),
#                      user = "root", #使用者名稱
#                      password = "***", #密碼
#                      dbname = "***", #資料庫名稱
#                      host = "localhost")
# 
# Encoding(FillFrequency_Result_ToSQL$Name) <- "unknown"
# dbWriteTable(channel, "FillFrequency", FillFrequency_Result_ToSQL)
# rm(FillFrequency_Result_ToSQL)

# 將資料從 SQL 拉回 R
channel <- dbConnect(MySQL(),
                     user = "root", #使用者名稱
                     password = "***", #密碼
                     dbname = "***", #資料庫名稱
                     host = "localhost")

dbSendQuery(channel, "SET NAMES big5")

FillFrequency <- dbReadTable(channel, "fillfrequency") %>% 
  setDT() %>% 
  left_join(., CompanyBasicData[, .SD, .SDcols = c("Name", "Industry")], by = "Name") %>% 
  setDT() %>% 
  .[, .SD, .SDcols = c("StockCode", "Name", "Industry", "DividendDate", "FillDiv_Sign", "FillFrequency")] %>% 
  .[, DividendDate := as.Date(DividendDate)]

##### 過去 N 年是否填息 #####
FillPreviousNYearsFun <- function(data, NYears){ # data 內需包含公司某年度是否填息之變數 
  
  Result <- copy(data) %>% 
    .[, FillPreviousNYears := Reduce(`+`, shift(FillDiv_Sign, 1:NYears)), by = "Name"] %>% 
    # .[, .SD, .SDcols = c("StockCode", "Year", "Name", "FillPreviousNYears")] %>% 
    .[, FillPreviousNYears_1 := ifelse(FillPreviousNYears == NYears, 1, 0)] %>% 
    .[, .SD, .SDcols = c("StockCode", "Name", "Industry", "DividendDate", "FillDiv_Sign", "FillPreviousNYears_1")] %>% 
    setnames(.,  "FillPreviousNYears_1", str_c("FillPrevious", NYears, "Years"))
    
  return(Result)
}

FillPreviousNYears <- FillPreviousNYearsFun(FillDivStock, 2)

copy(FillPreviousNYears) %>% 
  .[FillPrevious2Years == 1] %>% 
  pull(FillDiv_Sign) %>% 
  table()

##### 財務比率資料 #####
# 以某公司某財務比率所有年度的平均做填補

FS_Combine <- NULL

for (i in 1:4) {
  data <- read_xlsx("Data/FS_Data_needcombine.xlsx", sheet = i)
  FS_Combine <- rbind(FS_Combine, data)
}

FS_OriginalData <- FS_Combine %>% 
  setDT()

rm(FS_Combine)

FS_EngChi <- data.table(names(FS_OriginalData),
                        c("Name", "Date", "Cash", "Inventory",
                          "CurrentAssets", "FixedAssets", "Goodwill_IntangibleAssets", "NonCurrentAssets", "TotalAssets",
                          "CurrentLiabilities", "NonCurrentLiabilities", "TotalLiabilities", "CommonStock", 
                          "UndistributedProfit", "RetainedEarning", "TotalEquity", "TotalLiabilitiesAndEquity", 
                          "NetOperatingProfit", "OperatingCost", "OperatingGrossProfit", "OperatingExpense",
                          "OperatingProfit", "EPS", "EBIT", "CF_Operating",
                          "CF_Invest", "CF_Fundraising", "ROA_Before", "ROE_After",
                          "OperatingGrossProfitRatio", "GrossProfitMargin", "BerryRatio", "RDExpenseRatio",
                          "CurrentRatio", "QuickRatio", "DebtRatio",
                          "InterestCoverageRatio", "AccountsReceviableTurnover", "TotalAssetsTurnover", "AverageCollectionDays",
                          "InventaryTurnover", "AverageSellingDays", "FixedAssetsTurnover", "FreeCashFlow", 
                          "TobinQ")) %>% 
  setnames(c("V1", "V2"), c("Chi", "Eng"))

colnames(FS_OriginalData) <- FS_EngChi$Eng

## 將資料由累計轉成單季資料後，利用各項財報資料計算財務比率
UseZeroToFillValueOne <- c("TotalAssets", "TotalEquity", "NetOperatingProfit", "OperatingExpense",
                           "CurrentLiabilities", "Inventory", "FixedAssets")

FS_DataToCbind <- copy(FS_OriginalData) %>% 
  .[, `:=`(Year = year(Date), Month = month(Date))] %>%
  .[order(Name, Year, Month)] %>% 
  .[CompanyBasicData, on = "Name"] %>%
  select("Name", "Date", "Year", "Month", "AccountingMonth", "Industry", everything()) %>%
  .[Year >= ListedYear] %>% 
  .[, .SD, .SDcols = !c("ListedDate", "ListedYear")] %>% 
  .[!(Industry %in% c("M2800 金融業"))] %>%  # 因為金融業產業過於特殊的關係 因此直接刪除
  .[(str_split(Name, "-", simplify = T)[, 2]) != "DR"] %>%  # 刪除DR 股，於股利資料中僅有92筆資料
  .[!(Name %in% c("2841 台開", "6541 泰福-KY", "4707 磐亞"))] %>% 
  .[, .SD, .SDcols = c("Name", "Date", "Year", "Month", "AccountingMonth", "Industry",
                       "RetainedEarning", "EPS", "FreeCashFlow", "EBIT", "TotalAssets", "TotalEquity",
                       "OperatingGrossProfit", "NetOperatingProfit", "OperatingExpense",
                       "CurrentAssets", "CurrentLiabilities", "TotalLiabilities", "Inventory",
                       "FixedAssets")] %>% 
  .[, .SD, .SDcols = !UseZeroToFillValueOne]

FS_Data <- copy(FS_OriginalData) %>% 
  .[, `:=`(Year = year(Date), Month = month(Date))] %>%
  .[order(Name, Year, Month)] %>% 
  .[CompanyBasicData, on = "Name"] %>%
  select("Name", "Date", "Year", "Month", "AccountingMonth", "Industry", everything()) %>%
  .[Year >= ListedYear] %>% 
  .[, .SD, .SDcols = !c("ListedDate", "ListedYear")] %>% 
  .[!(Industry %in% c("M2800 金融業"))] %>%  # 因為金融業產業過於特殊的關係 因此直接刪除
  .[(str_split(Name, "-", simplify = T)[, 2]) != "DR"] %>%  # 刪除DR 股，於股利資料中僅有92筆資料
  .[!(Name %in% c("2841 台開", "6541 泰福-KY", "4707 磐亞"))] %>% 
  .[, .SD, .SDcols = c("Name", "Date", "Year", "Month", "AccountingMonth", "Industry",
                       "RetainedEarning", "EPS", "FreeCashFlow", "EBIT", "TotalAssets", "TotalEquity",
                       "OperatingGrossProfit", "NetOperatingProfit", "OperatingExpense",
                       "CurrentAssets", "CurrentLiabilities", "TotalLiabilities", "Inventory",
                       "FixedAssets")] %>%
  .[, lapply(.SD, function(x) ifelse(x == 0, 1, x)),
  .SDcols = UseZeroToFillValueOne] %>% 
  cbind(., FS_DataToCbind) %>% 
  .[, `:=`(ROA = (EBIT/ TotalAssets)*100,
           ROE = (EBIT/ TotalEquity)*100,
           OperatingGrossProfitRatio = (OperatingGrossProfit/ NetOperatingProfit)*100,
           PreTaxIncomeMargin = (EBIT/ NetOperatingProfit)*100,
           BerryRatio = (OperatingGrossProfit/ OperatingExpense)*100,
           CurrentRatio = (CurrentAssets/ CurrentLiabilities)*100,
           DebtRatio = (TotalLiabilities/ TotalAssets)*100, 
           TotalAssetsTurnover = (NetOperatingProfit/ ((TotalAssets + lag(TotalAssets))/2))*100,
           InventaryTurnover = (OperatingExpense/ ((Inventory + lag(Inventory))/2))*100,
           FixedAssetsTurnover = (NetOperatingProfit/ ((FixedAssets + lag(FixedAssets))/2))*100),
    by = "Name"] %>% 
  .[, .SD, .SDcols = c("Name", "Date", "Year", "Month", "AccountingMonth", "Industry",
                       "RetainedEarning", "EPS", "FreeCashFlow", "ROA", "ROE", "OperatingGrossProfitRatio",
                       "PreTaxIncomeMargin", "BerryRatio", "CurrentRatio", "DebtRatio", 
                       "TotalAssetsTurnover", "InventaryTurnover", "FixedAssetsTurnover")] %>% 
  group_by(Name) %>% 
  slice(2:n()) %>% 
  ungroup() %>% 
  setDT()

rm(FS_DataToCbind, UseZeroToFillValueOne)

##### 確認 財務比率資料的NA #####
CheckNA_List <- copy(FS_Data) %>% 
  map(., function(x) .[is.na(x)])

## 無NA : TotalAssets, TotalLiabilities, CommonStock, TotalEquity, DebtRatio
# Inventory : NA 除台開 1999-2004 四間DR 公司 查無資料外，其餘皆為金融業
# CurrentAssets : NA 全為金融業
# FixedAssets : 	911868 同方友友-DR 2014 無資料
# Goodwill_IntangibleAssets : 若是沒有商譽或無形資產的資料 直接打成0即可
# NonCurrentAssets : 除了 2841 台開 1999-2005沒有資料外，其他都是金融業，且金融業中幾乎都是證券業才有非流動資產
# TotalAssets : 無NA
# CurrentLiabilities : NA 全為金融業
# NonCurrentLiabilities : 除了 2841 台開 1999-2005沒有資料外，其他都是金融業，且金融業中幾乎都是證券業才有非流動資產
# UndistributedProfit : 作法為 直接刪除此變數，因為可用其他變數代表公司盈餘。NA 全為存託憑證(DR)，公司分別為 "9103 美德醫療-DR", "911608 明輝-DR", "911613 特藝-DR", "911616 杜康-DR", "911622 泰聚亨-DR"
# RetainedEarning : NA 為"2816 旺旺保", "2823 中壽", "2832 台產", "2850 新產", "2851 中再保", "2852 第一保", "9103 美德醫療-DR", "9105 泰金寶-DR", "9110 越南控-DR" 等公司，
#                   且都僅有某一年而已，因此直接可考慮直接以前三年的平均做取代。
# 
# NetOperatingProfit : "2515 中工", "2801 彰銀", "4707 磐亞", "9103 美德醫療-DR", "910322 康師傅-DR", "911622 泰聚亨-DR", "911868 同方友友-DR", "9188 精熙-DR" 的某些年分為NA
#                      考慮以前三年平均填補，但要注意 4707 磐亞 是2014 2015 兩年皆為NA
# OperatingCost : 金融業幾乎全為NA，並非某幾年為NA而已，考慮直接不使用此變數，或直接去除金融業，另外亦有"M2500 建材營造" 的 2515 中工(1993) 2841 台開(1999-2004) 為NA，
#                 "M1721 化學工業" 4707 磐亞 (2014 2015)， "W91   存託憑證" 的公司僅有2011年有缺資料
# OperatingGrossProfit : 情況與 OperatingCost 差不多
# OperatingExpense : "M1721 化學工業" 4707 磐亞 (2014 2015) ,"M2500 建材營造" 2515 中工(1993), M2800 金融業 2801 彰銀 (1990), DR 股則是僅有2011年才會有資料缺漏的問題。
# OperatingProfit : 情況與 OperatingCost 差不多
# EPS : 產業頗多，基本上都僅有一年是遺失值，可考慮直接以前三年平均填補。
# EBIT : 共有23間公司存在NA值，除"2816 旺旺保", "2832 台產", "2841 台開", "2850 新產", "2851 中再保", "9105 泰金寶-DR", "911616 杜康-DR" 存在多年NA值外，
#        其餘可以考慮直接用 前三年平均補充。
# CF_Operating : 除 "9103 美德醫療-DR", "9105 泰金寶-DR" 外，其餘皆僅有幾年是NA值，且幾乎都是在199X年初，可以考慮不使用此資料。
# CF_Invest : 情況與 CF_Operating 差不多
# CF_Fundraising : 情況與 CF_Operating 差不多
# ROE_After : 情況與 CF_Operating 差不多
# OperatingGrossProfitRatio : 金融業缺失的資料為多數，其餘可考慮用前三年平均填補
# GrossProfitMargin : 可以考慮直接用 前三年平均做填補，產業並沒有較明顯趨勢
# BerryRatio (營業毛利 /營業費用 用以度量公司收支是否平衡): 金融股基本上都沒有資料，其餘可考慮用 前三年做填補
# RDExpenseRatio : 研究發展費用率在傳產可能重視程度不高，或幾乎沒有，但在高科技產業僅有幾間會有NA值，可以考慮先以0做填補後，跟產業做比較
# CurrentRatio : 金融業幾乎全為NA值，其餘則為	"2841 台開" 及 少數幾年的DR股
# QuickRatio : NA全為 金融股
# InterestCoverageRatio (倍數越高，代表公司長期的償債能力越強) : 資料缺失問題非常嚴重，可以先觀察以前三年平均填補後的情況後再做打算。
# AccountsReceviableTurnover : 缺失的幾乎為 金融股以及DR股，可以考慮刪除後以前三年資料填補。
# TotalAssetsTurnover : 情況與 AccountsReceviableTurnover 相當
# InventaryTurnover : 可以考慮刪除金融股、DR與台開後，直接以前三年資料做填補。
# FixedAssetsTurnover : 可以考慮刪除金融股、DR與台開後，直接以前三年資料做填補。
# FreeCashFlow : 刪除金融與DR股後，其餘公司幾乎都是前幾年的資料缺失，可以考慮直接刪掉前幾年的資料
# TobinQ : 刪除金融、DR股與台開後，其餘公司幾乎都是前幾年的資料缺失，可以考慮直接刪掉前幾年的資料，但是這個變數可以考慮直接丟掉，實用程度不高

 ##### 確認 財務比率資料的NA END #####

UseMeanToFillVar <- c("EPS", "FreeCashFlow", "ROA", "ROE", "OperatingGrossProfitRatio", "PreTaxIncomeMargin")

# 由於使用 lapply 來填補NA值，會導致沒有調整的變數被拿掉，因此製作 FS_DataToCbind 用以cbind回去原本的資料
FS_DataToCbind <- copy(FS_Data) %>% 
  .[, .SD, .SDcols = !UseMeanToFillVar]

FS_AdjustNA <- copy(FS_Data) %>% 
  .[, lapply(.SD, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)),
    by = "Name", 
    .SDcols = UseMeanToFillVar] %>%
  cbind(FS_DataToCbind, .) %>% 
  .[, -1] %>% # 要刪掉重複出現的欄位 : Name
  select(., names(FS_Data))

rm(FS_DataToCbind)

CheckNA_List_AfteraAdjustNA <- copy(FS_AdjustNA) %>% 
  map(., function(x) .[is.na(x)])

##### 相關係數 ##### 
# (僅有 速動比率QuickRatio 與 流動比率 CurrentRatio 相關係數為較高的 0.86)

GetHighCorr_Result <- function(Data, CorrLevel){
  ## 建立相關係數Data
  CorrData <- cor(Data) %>% 
    as.data.table() %>% 
    .[, Variable := names(Data)] %>% 
    dplyr::select(., "Variable", everything())
  
  ## Function 用於找出每一個變數的高相關係數關係變數
  GetHighCorr <- function(Variable_Name){
    
    Data <- CorrData %>% 
      .[, .SD, .SDcols = c("Variable", Variable_Name)] %>% 
      .[abs(get(Variable_Name))>= CorrLevel] %>% 
      .[, Variable2 := names(.)[2]] %>%
      setnames(., Variable_Name, "Corr") %>%
      .[, .SD, .SDcols = c("Variable", "Variable2", "Corr")]
    
    return(Data)
  }
  
  ## 利用迴圈針對每一個變數找到高相關係數關係並疊合
  HighCorr_UnClean <- NULL
  for (i in names(CorrData)[-1]) {
    
    Data <- GetHighCorr(i)
    if (nrow(Data) == 1) next
    
    HighCorr_UnClean <- rbind(HighCorr_UnClean, Data) %>% 
      setDT() %>% 
      .[Corr != 1]
  }
  
  if (is.null(HighCorr_UnClean) == T) {
    return(print("各變數的相關係數皆未達設定標準"))
    }else{
      
      ## 去掉重複值
      HighCorr <- HighCorr_UnClean %>% 
        .[order(-Corr)] %>% 
        .[, head(.SD, 1), by = "Corr"] %>% 
        .[, .SD, .SDcols = c("Variable", "Variable2", "Corr")]
      
      return(HighCorr)
    }
  } 

Corr_Result <- copy(FS_AdjustNA) %>%
  .[, -c(1:6)] %>% 
  GetHighCorr_Result(., 0.5)

##### 計算每一間公司每一次發股利的相對財務比率 #####
# 已發股利前一年到前一季的資料作平均計算 
FS_Result_Data <- copy(FillDivStock) %>% 
  .[, .SD, .SDcols = c(1:4)] %>% 
  full_join(., FS_AdjustNA, by = "Name") %>% 
  setDT() %>% 
  .[(Date < DividendDate & Date >= (DividendDate %m-% years(1)))] %>% 
  .[order(Name, DividendDate)] %>% 
  .[, lapply(.SD, function(x) mean(x, na.rm = T)), 
    by = c("StockCode", "Name", "DividendDate", "FillDiv_Sign", "Industry"),
    .SDcols = c(10:22)] %>%
  .[, YearQ := zoo::as.yearqtr(DividendDate, format = "%Y-%m-%d")] %>%
  select("StockCode", "Name", "DividendDate", "YearQ", everything())

##### 每一季的產業平均計算 ##### 
Industry_Mean_Data <- copy(FS_AdjustNA) %>%
  .[, .SD, .SDcols = !c("Name", "Date", "AccountingMonth")] %>%
  .[, Year_Month := str_c(Year, "-", Month, "-01") %>% as.POSIXct()] %>%
  .[, YearQ := zoo::as.yearqtr(Year_Month, format = "%Y-%m-%d") ] %>%
  select(YearQ, Year_Month, everything()) %>% 
  .[order(YearQ, Industry)]

YearQ_Chr <- seq(zoo::as.yearqtr("1991 Q1"), zoo::as.yearqtr("2020 Q4"), 0.25)

Industry_Mean <- NULL

for (i in YearQ_Chr) {
  
  data <- copy(Industry_Mean_Data) %>%
    .[(YearQ < i & YearQ >= (i - 1.25))]
  
  if (nrow(data) == 0) next
  
  data2 <- copy(data) %>% 
    .[, lapply(.SD, function(x) mean(x, na.rm = T)),
      by = c("Industry"),
      .SDcols = c(6:18)] %>% 
    .[, YearQ := i] %>% 
    select(YearQ, everything())
  
  Industry_Mean <- rbind(Industry_Mean, data2)
  
  print(i)
}

Industry_Mean <- copy(Industry_Mean) %>%
  .[, YearQ := zoo::as.yearqtr(YearQ)]

##### 財務比率資料與產業平均的比較 #####

FS_Result_ToCalculate <- copy(FS_Result_Data) %>%
  left_join(., Industry_Mean, by = c("YearQ", "Industry")) %>% 
  setDT()
# .[Industry_Mean, on = c("YearQ", "Industry")]

FS_Result <- copy(FS_Result_ToCalculate) %>% 
  .[, .SD, .SDcols = c("StockCode", "Name", "DividendDate", "YearQ", "FillDiv_Sign", "Industry")]

for (i in 7:19) {
  
  Colname <- str_split(names(FS_Result_ToCalculate)[i], "[.]", simplify = TRUE)[1, 1] %>% 
    str_c(., "_ToMarket")
  
  Result <- FS_Result_ToCalculate %>% 
    as.data.frame() %>%
    select(1:6, i, i+13) %>% 
    transmute(!!(Colname) := ifelse(.[, 7] >= .[, 8], 1, 0))
  
  FS_Result <- FS_Result %>%
    cbind(., Result)
  
  print(i)
}

##### 計算本益比，成長率等資料 ##### 
# 某一些產業 EPS 在每一季會有大幅跳動，導致PERatio 的漲跌加大，代表確實有某些產業確實有季節趨勢，不能單以某一季的情況去觀察
## EPS 資料 (PERatio 因為某些公司的EPS 有0 或者是 負的 的關係 導致沒有辦法使用)
EPS_Data <- copy(FS_AdjustNA) %>%
  .[, .SD, .SDcols = c("Name", "Date", "AccountingMonth", "Year", "Month",
                       "Industry", "EPS")] %>%
  .[, YearQ := zoo::as.yearqtr(Date)] %>%
  .[, lapply(.SD, function(x) mean(x, na.rm = T)),
    by = c("Name", "YearQ"),
    .SDcols = c("EPS")]

PERatio <- copy(UnadjustPrice) %>%
  .[, YearQ := zoo::as.yearqtr(Date)] %>% 
  left_join(., EPS_Data, by = c("Name", "YearQ")) %>% 
  setDT() %>%
  .[, PERatio := Close / EPS] %>% 
  .[, .SD, .SDcols = c("Name", "YearQ", "PERatio")] %>%
  .[, lapply(.SD, function(x) mean(x, na.rm = T)),
    by = c("Name", "YearQ"),
    .SDcols = "PERatio"] %>%
  .[is.na(PERatio) == F] %>%
  .[order(Name)] %>%
  left_join(.,
            copy(CompanyBasicData)[, .SD, .SDcols = c("Name", "Industry")],
            by = "Name") %>%
  setDT()

IndustryMean_PERatio_Data <- copy(PERatio) %>%
  .[, .SD, .SDcols = c("YearQ", "Industry", "PERatio")] %>%
  .[order(YearQ, Industry)]

IndustryMean_PERatio <- NULL

for (i in YearQ_Chr) {
  
  data <- copy(IndustryMean_PERatio_Data) %>% 
    .[(YearQ < i & YearQ >= (i - 1.25))]
  
  if (nrow(data) == 0) next
  
  data2 <- copy(data) %>%
    .[, lapply(.SD, function(x) mean(x, na.rm = T)),
      by = c("Industry"),
      .SDcols = "PERatio"] %>%
    .[, YearQ := i] %>%
    select(YearQ, everything())
  
  IndustryMean_PERatio <- rbind(IndustryMean_PERatio, data2)
  
  print(i)
}


IndustryMean_PERatio <- copy(IndustryMean_PERatio) %>%
  .[, YearQ := zoo::as.yearqtr(YearQ)]

##### 除息前六個月股價波動度 #####
# Dividend_Data <- copy(FillDivStock) %>% 
#   .[, .SD, .SDcols =c("StockCode", "Name", "Industry", "DividendDate", "FillDiv_Sign")] %>% 
#   .[, Volatility_PreSixMonth := 0]
# 
# AllCompanyName <- copy(Volatility_Data) %>% 
#   pull(Name) %>% 
#   unique()
# 
# Volatility_Result <- NULL
# 
# for (i in AllCompanyName) {
#   
#   Dividend_EachStock <- copy(Dividend_Data) %>% 
#     .[Name == i] %>% 
#     .[order(DividendDate)]
#   
#   PriceData <- copy(UnadjustPrice) %>% 
#     .[Name == i] %>% 
#     .[order(Date)]
#   
#   for (j in 1:nrow(Dividend_EachStock)) {
#     
#     Div_Date <- Dividend_EachStock$DividendDate[j]
#     
#     Div_Date_PreSixMonth <- Dividend_EachStock$DividendDate[j] %m-% months(6)
#     
#     Volatility <- copy(PriceData) %>% 
#       .[(Date < Div_Date & Date >= Div_Date_PreSixMonth)] %>% 
#       pull(Close) %>% 
#       sd(., na.rm = T)
#     
#     Dividend_EachStock[j, 6] <- Volatility
#     
#   }
#   
#   Volatility_Result <- rbind(Volatility_Result, Dividend_EachStock)
#   
#   print(i)
# }

# 將運算結果儲存至MySql
# Volatility_Result_ToSQL <- copy(Volatility_Result) %>% 
#   as.data.frame()
# channel <- dbConnect(MySQL(),
#                      user = "root", #使用者名稱
#                      password = "***", #密碼
#                      dbname = "***", #資料庫名稱
#                      host = "localhost")
# 
# Encoding(Volatility_Result_ToSQL$Name) <- "unknown"
# Encoding(Volatility_Result_ToSQL$Industry) <- "unknown"
# dbWriteTable(channel, "DividendData_Volatility", Volatility_Result_ToSQL)
# rm(Volatility_Result_ToSQL)

##### 公司基本資料 #####
# 若為四大會計師事務所，則定義 AccountancyFirms == 1
# 若為上市公司，則定義 Market == 1 
# 原始資料
BasicFirmInformation_Data <- read_xlsx("Data/BasicFirmData.xlsx") %>%
  setDT()

# 中英文名稱對照表
BasicFirmInformation_EngChi <- data.table(names(BasicFirmInformation_Data),
                                          c("Name", "Market", "EstablishmentDate", "Industry", 
                                            "CrisisDate", "CrisisEvent_BigCategory", "CrisisEvent_BigCategoryExplanation", "CrisisEvent",
                                            "AccountancyFirms")) %>%
  setnames(c("V1", "V2"), c("Chi", "Eng"))

# 重新命名
colnames(BasicFirmInformation_Data) <- BasicFirmInformation_EngChi$Eng

# 定義四大會計師事務所
Big4AccountancyFirms <- c("勤業", "安永", "資誠", "安侯", "畢馬")


# 定義電子產業
ElectiricIndustry <- c("M2324 半導體", "M2328 電子零組件", "M2326 光電業",
                       "M2325 電腦及週邊", "M2327 通信網路業", "M2329 電子通路業",
                       "M2330 資訊服務業", "M2331 其他電子業")

# 建構變數
BasicFirmInformation <- copy(BasicFirmInformation_Data) %>% 
  .[, `:=`(AccountancyFirms = ifelse(str_sub(AccountancyFirms, 1, 2) %in% Big4AccountancyFirms, 1, 0),
           Market = ifelse(Market == "TSE", 1, 0),
           EleIndustry = ifelse(Industry %in% ElectiricIndustry, 1, 0))] %>%
  .[, .SD, .SDcols = c("Name", "Market", "EleIndustry", "AccountancyFirms")]

BasicFirmInformation_ToJoin <- copy(FillDivStock) %>%
  .[, .SD, .SDcols = c("StockCode", "Name", "Industry", "DividendDate", "FillDiv_Sign")] %>%
  left_join(., BasicFirmInformation, by = c("Name")) %>%
  setDT()

##### 公司創立年數 #####
FirmAge_ToJoin <- copy(BasicFirmInformation_Data) %>% 
  .[, .SD, .SDcols = c("Name", "EstablishmentDate")]

FirmAge <- copy(FillDivStock) %>%
  .[FirmAge_ToJoin, on = "Name", EstablishmentDate := i.EstablishmentDate] %>%
  .[, FirmAge := Year - year(EstablishmentDate)] %>% 
  .[, .SD, .SDcols = c("StockCode", "Name", "Industry", "DividendDate", "FillDiv_Sign", "FirmAge")]

##### 重大危機事件 ##### 
# 若是前一年有發生重大危機事件，則定義 CrisisEvent_Dummy == 1
CrisisReason <- str_split(unique(BasicFirmInformation_Data$CrisisEvent), "[[(]]", simplify = T)[, 1] %>% 
  unique() %>% 
  .[-1]

CrisisData <- copy(BasicFirmInformation_Data) %>% 
  .[, .SD, .SDcols = c("Name", "CrisisDate", "CrisisEvent_BigCategory",
                       "CrisisEvent_BigCategoryExplanation", "CrisisEvent")] %>%
  .[, Year := year(CrisisDate)] %>% 
  .[is.na(CrisisEvent_BigCategory) == F]

CrisisData_ToJoin <- copy(CrisisData) %>%
  .[, Year := year(CrisisDate)] %>% 
  .[, .SD, .SDcols = c("Name", "Year", "CrisisDate")]

CrisisEvent <- copy(FillDivStock) %>%
  .[, .SD, .SDcols = c("StockCode", "Name", "Industry", "Year", "DividendDate", "FillDiv_Sign")] %>% 
  left_join(., CrisisData_ToJoin, by = c("Name", "Year")) %>% 
  setDT() %>% 
  .[, CrisisEvent_Dummy := ifelse((is.na(CrisisDate) == F & DividendDate > CrisisDate), 1, 0)] %>% 
  .[, .SD, .SDcols = c("StockCode", "Name", "Industry", "DividendDate", "FillDiv_Sign", "CrisisEvent_Dummy")]

# CrisisEvent <- copy(FillDivStock) %>% 
#   .[CrisisData_ToJoin, on = c("Name", "Year"), `:=`(
#     CrisisDate = i.CrisisDate,
#     CrisisEvent_BigCategory = i.CrisisEvent_BigCategory,
#     CrisisEvent_BigCategoryExplanation = i.CrisisEvent_BigCategoryExplanation,
#     CrisisEvent = i.CrisisEvent
#     )] %>% 
#   .[, CrisisEventOrNot := ifelse(is.na(CrisisDate) == F, 1, 0)] %>% 
#   .[, CrisisEvent_Dummy := Reduce(`+`, shift(CrisisEventOrNot, 1:3)), by = "Name"] %>% 
#   .[, .SD, .SDcols = c("StockCode", "Year", "Name", "FillOrNot", "CrisisEvent_Dummy")]


# 所有發生危機事件的公司僅有77筆，其中共有 30個樣本並沒有填息，比率約為 38%
CrisisEvent_UnFillDiv_Ratio <- copy(CrisisEvent) %>%
  .[CrisisEvent_Dummy == 1] %>%
  pull(FillDiv_Sign) %>% 
  table()

##### 營收資料 ##### 
channel <- dbConnect(MySQL(),
                       user = "root", #使用者名稱
                       password = "***", #密碼
                       dbname = "***", #資料庫名稱
                       host = "localhost")

dbSendQuery(channel, "SET NAMES big5")

Revenue_data <- dbReadTable(channel, "revenue_data") %>% setDT()

Colnames_Revenue_Data <- dbReadTable(channel, "colnames_revenue_data") %>% setDT()

Revenue_data_ToJoin <- copy(Revenue_data) %>%
  .[, DatePlusOneMonth := Date %>% as.Date %m+% months(1)] %>%
  .[, YearMonth := format(as.Date(DatePlusOneMonth), "%Y-%m")]

Revenue_Result <- copy(FillDivStock) %>% 
  .[, .SD, .SDcols = c("StockCode", "Name", "Industry", "DividendDate", "FillDiv_Sign")] %>% 
  .[, YearMonth := format(as.Date(DividendDate), "%Y-%m")] %>% 
  left_join(., Revenue_data_ToJoin, by = c("Name", "YearMonth")) %>% 
  setDT() %>% 
  .[, .SD, .SDcols = c("StockCode", "Name", "Industry", "DividendDate", "FillDiv_Sign",
                       "SingleRevenueToPreMonth_Percent", "SingleRevenueGrowthRate_Percent",
                       "RevenueGrowthRate_PreThreeMonth")] %>% 
  .[, `:=`(
    SingleRevenueToPreMonth_Percent = zoo::na.locf(SingleRevenueToPreMonth_Percent, na.rm = F,  fromLast = F),
    SingleRevenueGrowthRate_Percent = zoo::na.locf(SingleRevenueGrowthRate_Percent, na.rm = F,  fromLast = F),
    RevenueGrowthRate_PreThreeMonth = zoo::na.locf(RevenueGrowthRate_PreThreeMonth, na.rm = F,  fromLast = F)),
    by = Name]

CheckNA_Revenue_Result <- copy(Revenue_Result) %>%
  map(., function(x) .[is.na(x)])

###### 三大法人買賣超 #####
# channel <- dbConnect(MySQL(),
#                      user = "root", #使用者名稱
#                      password = "***", #密碼
#                      dbname = "***", #資料庫名稱
#                      host = "localhost")
# 
# dbSendQuery(channel, "SET NAMES big5")
# institutionalinvestors_netbuyandsell_data_Original <- dbReadTable(channel, "institutionalinvestors_netbuyandsell_data") %>% 
#   setDT()
# 
# colnames_institutionalinvestors_netbuyandsell <- dbReadTable(channel, "colnames_institutionalinvestors_netbuyandsell") %>% 
#   setDT()
# 
# 
# DividendPolicy_1101 <- copy(DividendPolicy) %>% 
#   .[StockCode == "1101"]
# 
# institutionalinvestors_netbuyandsell_1101 <- copy(institutionalinvestors_netbuyandsell_data_Original) %>% 
#   .[Name == "1101 台泥"] %>% 
#   .[, `:=`(StockCode = str_split(Name, " ", simplify = T)[, 1],
#            Year = year(Date) %>% as.character())] %>% 
#   .[DividendPolicy_1101, on = c("StockCode", "Year")] %>% 
#   select("StockCode", "Name", "Date", "Year", 
#          "Last Ex_Date(Cash Div.)", "Total Dividends-Cash -Earnings", "Stock Div.-Earnings", 
#          everything()) %>% 
#   .[, `:=`(Date = as.Date(Date),
#            `Last Ex_Date(Cash Div.)` = as.Date(`Last Ex_Date(Cash Div.)`))] %>% 
#   .[((`Last Ex_Date(Cash Div.)` %m-% months(1)) <= Date & Date <= `Last Ex_Date(Cash Div.)`), by = c("Name", "Year")]
# 
# institutionalinvestors_netbuyandsell_1101_Result <- copy(institutionalinvestors_netbuyandsell_1101) %>% 
#   .[, .SD, .SDcols = c("StockCode", "Name", "Date", "Year", "Last Ex_Date(Cash Div.)", 
#                        "Total Dividends-Cash -Earnings", "Stock Div.-Earnings", 
#                        "FI_NBS_Share", "IT_NBS_Share", "DL_NBS_Share", "InstitutionalInvestors_NBS")]
# 
# ggplot(data = institutionalinvestors_netbuyandsell_1101_Result)+
#   geom_point(mapping = aes(x = Date, y = FI_NBS_Share))+
#   geom_line(mapping = aes(x = Date, y = FI_NBS_Share))+
#   geom_hline(yintercept = 0)+
#   facet_wrap(vars(Year), scales = "free")

##### 台灣股價指數 ##### 
channel <- dbConnect(MySQL(),
                       user = "root", #使用者名稱
                       password = "***", #密碼
                       dbname = "***", #資料庫名稱
                       host = "localhost")

dbSendQuery(channel, "SET NAMES big5")

# 將Date 加上 一個月 並製作 YearMonth 用以 作為 join 到股利資料的 key 讓變數可以是當個日期的前一個月資料 
TWSEIndex_data <- dbReadTable(channel, "twseindex_data") %>%
  setDT() %>% 
  .[, YearMonth := as.Date(Date) %m+% months(1) %>% format(., "%Y-%m")] %>% 
  .[, .SD, .SDcols = c("YearMonth", "Volume", "Return_Percent", "PERatio", "PBRatio")] %>% 
  setnames(.,
           c("YearMonth", "Volume", "Return_Percent", "PERatio", "PBRatio"),
           c("YearMonth", "TWSE_Volume", "TWSE_Return_Percent", "TWSE_PERatio", "TWSE_PBRatio"))

# cor(TWSEIndex_data[c(5:nrow(TWSEIndex_data)), c(3:8)])

TWSEIndex <- copy(FillDivStock) %>%
  .[, .SD, .SDcols = c("StockCode", "Name", "Industry", "DividendDate", "FillDiv_Sign")] %>%
  .[, YearMonth := format(as.Date(DividendDate), "%Y-%m")] %>%
  left_join(., TWSEIndex_data, by = c("YearMonth")) %>%
  setDT()

CheckNA_TWSEIndex <- copy(TWSEIndex) %>%
  map(., function(x) .[is.na(x)])

## 想法 
# 1. 計算三大法人買賣超，應該要先去將是否填息改變為 30天內是否填息，然後去看一下三大法人的動作
# 2. 景氣指標 依樣先將是否填息改變為30天內是否填息，然後可以觀察一下景氣燈號，先去確定一下景氣燈號的計算長度是一季的燈號還是一個月的燈號
#   如果是一個月的燈號的話，可以考慮用景氣燈號就好，但如果是一季，就必須要考慮是否用當月大盤指數對比前一個月的大盤指數或者去年同期的大盤指數，去判斷是否景氣過熱，
#   也可以考慮之前碩論使用的方法，一個月的大盤持有報酬是否大於去年一整年的平均。
# 3. 經理人採購指數或許也是可以使用的指標，但是可能要考慮產業，如果某產業的cost of good sold(或原物料相關的成本指標) 超高，那可能代表那個產業是要一直購買原物料的，那去看經理人採購指數可能比較有意
#    因此，這個指標可能要考慮先分產業再來做計算
# 4. 一開始想法是 一年發一次股利的才列入樣本內，但是如果一年發多次股利的那種公司，可能也可以列入樣本，去看當次的股利發放是否有填息表現。

##### 彙整所有變數 ##### 

FillDivStock_ToJoin <- copy(FillDivStock) %>% 
  .[, .SD, .SDcols = c("StockCode", "Name", "Industry", "DividendDate", "FillDiv_Sign")]

JoinList <- list(FillDivStock_ToJoin, FillFrequency, FillPreviousNYears, FS_Result, Volatility_Result, 
                 BasicFirmInformation_ToJoin, FirmAge, CrisisEvent, Revenue_Result, TWSEIndex)

FinalData_SevenDays <- plyr::join_all(JoinList, by = c("StockCode", "Name", "Industry", "DividendDate", "FillDiv_Sign"),
                            type = 'left', match = "all") %>% 
  setDT() %>% 
  .[, .SD, .SDcols = !c("YearQ", "YearMonth")] %>% 
  .[is.na(SingleRevenueToPreMonth_Percent) == F] %>% 
  .[is.na(FillDiv_Sign) == F] %>% 
  .[is.na(ROA_ToMarket) == F] %>% 
  .[is.na(FillPrevious2Years) == F] %>% 
  .[is.na(Volatility_PreSixMonth) == F] %>% 
  .[is.na(RevenueGrowthRate_PreThreeMonth) == F]

CheckNA_FinalData <- copy(FinalData) %>% 
  map(., function(x) .[is.na(x)])

CheckNA_FinalData_SevenDays <- copy(FinalData_SevenDays) %>% 
  map(., function(x) .[is.na(x)])

CheckNA_FinalData_ThreeDays <- copy(FinalData_ThreeDays) %>% 
  map(., function(x) .[is.na(x)])

# 由於 FillFrequency 若是在公司的前幾年，數值不是一百就是0 為了避免掉造成模型影響，因此刪除每一家公司的前兩筆資料
FinalData <- copy(FinalData) %>% 
  group_by(Name) %>%
  slice(-c(1:2)) %>% 
  ungroup() %>% 
  setDT()

FinalData_SevenDays <- copy(FinalData_SevenDays) %>% 
  group_by(Name) %>%
  slice(-c(1:2)) %>% 
  ungroup() %>% 
  setDT()

FinalData_ThreeDays <- copy(FinalData_ThreeDays) %>% 
  group_by(Name) %>%
  slice(-c(1:2)) %>% 
  ungroup() %>% 
  setDT()

## 將最終資料存到 MYsql 中

FinalData_ToSQL <- copy(FinalData_SevenDays) %>%
  as.data.frame()

channel <- dbConnect(MySQL(),
                     user = "root", #使用者名稱
                     password = "***", #密碼
                     dbname = "***", #資料庫名稱
                     host = "localhost")

Encoding(FinalData_ToSQL$Name) <- "unknown"
Encoding(FinalData_ToSQL$Industry) <- "unknown"

dbWriteTable(channel, "FinalData_SevenDays", FinalData_ToSQL)
rm(FinalData_ToSQL)

##### 變數說明表 #####

VariableDescription <- data.table(names(FinalData_ThreeDays),
                                  c("股票代號", "股票名稱", "產業", "除權息日",
                                    "填權息與否", "過去填權息頻率", "過去兩年是否填權息", 
                                    "保留盈餘", "每股盈餘", "自由現金流量", "資產報酬率",
                                    "股東權益報酬率", "營業利益率", "稅前純益率", "貝理比率",
                                    "流動比率", "負債比率", "總資產周轉率", "存貨周轉率",
                                    "固定資產周轉率", "股價波動度", "交易市場", "電子產業",
                                    "會計公司", "公司年齡", "危機事件", "單月營收與上月比%", "單月營收成長率%",
                                    "近3月累計營收成長率", "大盤成交量", "大盤報酬率%", "大盤本益比", "大盤股價淨值比"),
                                  c("-", "-", "-", "-", "二元變數，1代表於除權息三日內填權息", "過去填權息次數除以1990年以來總除權息次數，以百分比表示",
                                    "二元變數，若過去兩年皆成功於三日內填息，則以1表示", 
                                    "二元變數，計算公司除權息前四季之 保留盈餘 平均，並與同產業同季別之所有公司平均做比較，若大於產業平均則為1",
                                    "二元變數，計算公司除權息前四季之 每股盈餘 平均，並與同產業同季別之所有公司平均做比較，若大於產業平均則為1",
                                    "二元變數，計算公司除權息前四季之 自由現金流量 平均，並與同產業同季別之所有公司平均做比較，若大於產業平均則為1",
                                    "二元變數，計算公司除權息前四季之 資產報酬率 平均，並與同產業同季別之所有公司平均做比較，若大於產業平均則為1",
                                    "二元變數，計算公司除權息前四季之 股東權益報酬率 平均，並與同產業同季別之所有公司平均做比較，若大於產業平均則為1",
                                    "二元變數，計算公司除權息前四季之 營業利益率 平均，並與同產業同季別之所有公司平均做比較，若大於產業平均則為1",
                                    "二元變數，計算公司除權息前四季之 稅前純益率 平均，並與同產業同季別之所有公司平均做比較，若大於產業平均則為1",
                                    "二元變數，計算公司除權息前四季之 貝理比率 平均，並與同產業同季別之所有公司平均做比較，若大於產業平均則為1",
                                    "二元變數，計算公司除權息前四季之 流動比率 平均，並與同產業同季別之所有公司平均做比較，若大於產業平均則為1",
                                    "二元變數，計算公司除權息前四季之 負債比率 平均，並與同產業同季別之所有公司平均做比較，若大於產業平均則為1",
                                    "二元變數，計算公司除權息前四季之 總資產周轉率 平均，並與同產業同季別之所有公司平均做比較，若大於產業平均則為1",
                                    "二元變數，計算公司除權息前四季之 存貨周轉率 平均，並與同產業同季別之所有公司平均做比較，若大於產業平均則為1",
                                    "二元變數，計算公司除權息前四季之 固定資產周轉率 平均，並與同產業同季別之所有公司平均做比較，若大於產業平均則為1",
                                    "計算除權息前六個月公司每日收盤價波動度",
                                    "二元變數，若公司為上市股票，則為1，若為上櫃股票，則為0",
                                    "二元變數，若公司屬於電子產業，則為1",
                                    "二元變數，若公司採用勤業、安永、資誠、安侯、畢馬等四大會計師事務所，則為1",
                                    "除權息年度減去公司成立年度",
                                    "二元變數，若除權息前一年曾發生危機事件，則為1",
                                    "( 本月單月營收 - 上月單月營收) / ( 上月單月營收 ) *100(%)",
                                    "( 單月營收 - 去年單月營收 ) / ( 去年單月營收 ) *100(%)",
                                    "( 近3月累計營收 - 去年近3月累計營收 ) / ( 去年近3月累計營收 ) *100(%)",
                                    "-", "-", "-", "-")) %>% 
  setnames(., c("V1", "V2", "V3"), c("Eng", "Chi", "Description"))

save(VariableDescription, file = "Data/VariableDescription.RData")












