## 專案說明 (可透過 DividendFillingReport.html 查看完整專案內容。)
此專案目的為 建構Extreme Gradient Boosting 模型預測公司是否能於除權息三日內，漲回除權息前一日之收盤價格(完成填權息)，於測試資料集之預測效果為 AUC 73%，透過變數重要性排序後發現過去填權息頻率、大盤股價淨值比與股價波動度對模型的預測貢獻程度最大。
在此專案中，主要使用R 完成資料清理與建構變數，並使用Python 建構預測模型。

## 程式碼說明

### BuildPredictionModel.py
  建構 Extreme Gradient Boosting 模型
### CheckFillDividendOrNot.R
  資料清理與變數建構
### DividendFillingReport.Rmd
  利用 Rmarkdown 編輯 研究報告
### DividendFillingReport.html
  研究報告

