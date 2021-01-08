# -*- coding: utf-8 -*-
"""
Created on Tue Dec 29 17:01:09 2020

@author: USER
"""

import mysql.connector
from pandas import DataFrame
from random import shuffle
from sklearn import preprocessing # 數據標準化
import datetime
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
from sklearn.model_selection import train_test_split # 拆分數據資料集
from sklearn.ensemble import RandomForestClassifier
from xgboost import XGBClassifier
import xgboost as xgb
from sklearn.preprocessing import LabelEncoder
from sklearn.model_selection import cross_validate #Additional scklearn functions
from sklearn import metrics
from sklearn.model_selection import GridSearchCV   #Perforing grid search
import matplotlib.pylab as plt
%matplotlib inline
from matplotlib.pylab import rcParams
rcParams['figure.figsize'] = 12, 4

mydb = mysql.connector.connect(
  host = "localhost",
  user = "root",
  password = "PKQ209pkq209",
  database = "excludedividendandright_data")

mycursor = mydb.cursor()

mycursor.execute("SHOW columns FROM finaldata_threedays")
columnsname = mycursor.fetchall()

mycursor.execute("SELECT * FROM excludedividendandright_data.finaldata_threedays")
finaldata = mycursor.fetchall()

# 隨機打亂資料 避免有序的排列造成模型預測失準 (需放入 list 資料)
shuffle(finaldata)

finaldata = DataFrame(finaldata)
columnsname = DataFrame(columnsname)

# 將 dataframe 重新命名 
finaldata.columns = columnsname.loc[:, 0]

finaldata.head()

# 調整變數型態
columnsNotDummy = ['row_names', 'StockCode', 'Name', 'DividendDate', "Industry", "FillFrequency", "Volatility_PreSixMonth", "FirmAge", 
                   "SingleRevenueToPreMonth_Percent", "SingleRevenueGrowthRate_Percent",
                   "RevenueGrowthRate_PreThreeMonth", "TWSE_Volume",
                   "TWSE_Return_Percent", "TWSE_PERatio", "TWSE_PBRatio"]

DummyTypeData = finaldata.drop(columnsNotDummy, inplace = False, axis = 1).astype(int)

# label encoding the industry variable 
labelencoder = LabelEncoder()
finaldata['Industry'] = labelencoder.fit_transform(finaldata['Industry'])

# 變數標準化 
zscore = preprocessing.StandardScaler()

DoNotNeedAdjustType = finaldata[columnsNotDummy]
DoNotNeedAdjustType["TWSE_Volume"] = zscore.fit_transform(DoNotNeedAdjustType[["TWSE_Volume"]])

# 組成最終資料 
FinalData_WithCompanyNameAndTime = pd.concat([DummyTypeData, DoNotNeedAdjustType], axis=1)

### 選擇要放入模型的資料
ColumnsToDrop = ['row_names', 'StockCode', 'Name', 'DividendDate']
FinalData = FinalData_WithCompanyNameAndTime.drop(ColumnsToDrop, axis = 1, inplace = False)

# 觀察資料內部的變數型態
FinalData.dtypes
FinalData.info
FinalData.shape

# 建立相關系數 矩陣與圖 
corrmat = FinalData.corr()
f, ax = plt.subplots(figsize=(12, 9))
sns.heatmap(corrmat, vmax=.8, square=True);

## 將資料拆分成 Train Test 資料集
X = FinalData[FinalData.columns[1:31]]
Y = FinalData["FillDiv_Sign"]

X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size = 0.25, random_state = 33)

Train, Test = train_test_split(FinalData, test_size = 0.25, random_state = 33)

## 隨機森林
rfc = RandomForestClassifier()
rfc.fit(X_train, Y_train)

print("The accuracy of Random Forest Classifier on testing set:", 
      rfc.score(X_test, Y_test))

## XGBoosting 
xgbc = XGBClassifier()
xgbc.fit(X_train, Y_train)
print('The accuracy of EXtreme Gradient Boosting Classifier on testing set:', 
      xgbc.score(X_test, Y_test))

modelfit(xgb1, Train, Test, predictors)

def modelfit(alg, dtrain, dtest, predictors, useTrainCV = True, cv_folds = 5, early_stopping_rounds = 50):
    if useTrainCV:
        xgb_param = alg.get_xgb_params() # 取得 預先設定的 XGB 參數
        xgtrain = xgb.DMatrix(dtrain[predictors].values, label = dtrain[target].values) # 將資料轉成XGB需要的資料格式
        cvresult = xgb.cv(xgb_param, xgtrain, num_boost_round=alg.get_params()['n_estimators'], nfold=cv_folds,
                          metrics='auc', early_stopping_rounds=early_stopping_rounds) # 透過CV 生成模型，每一個row 代表XGB中生成的每一棵樹，可以看到每一棵樹的AUC皆有所增加，可藉此得到最佳的決策樹數量
        alg.set_params(n_estimators = cvresult.shape[0]) # 重新設定參數 n_estimators
    
    #Fit the algorithm on the data
    alg.fit(dtrain[predictors], dtrain['FillDiv_Sign'], eval_metric='auc')
    
    #Predict training set:
    dtrain_predictions = alg.predict(dtrain[predictors]) # 預測結果 以 0 1 表示
    dtrain_predprob = alg.predict_proba(dtrain[predictors])[:,1] # 預測結果 以 機率 表示
    
    # Predict testing set:
    dtest_predictions = alg.predict(dtest[predictors]) # 預測結果 以 0 1 表示
    dtest_predprob = alg.predict_proba(dtest[predictors])[:,1] # 預測結果 以 機率 表示
    
    #Print model report:
    print("\nModel Report")
    print("Accuracy : %.4g" % metrics.accuracy_score(dtrain['FillDiv_Sign'].values, dtrain_predictions))
    print("AUC Score (Train): %f" % metrics.roc_auc_score(dtrain['FillDiv_Sign'], dtrain_predprob))
    print("AUC Score (Test): %f" % metrics.roc_auc_score(dtest['FillDiv_Sign'], dtest_predprob))

    feat_imp = pd.Series(alg.get_booster().get_fscore()).sort_values(ascending=False)
    feat_imp.plot(kind='bar', title='Feature Importances')
    plt.ylabel('Feature Importance Score')
    
    fpr_xgb, tpr_xgb, threshold  = metrics.roc_curve(dtest['FillDiv_Sign'], dtest_predprob)
    roc_auc = metrics.auc(fpr_xgb, tpr_xgb) ###計算auc的值
    plt.figure()
    plt.figure(figsize=(10,10))
    plt.plot(fpr_xgb, tpr_xgb, color = 'darkorange', label = 'ROC curve (area = %0.2f)' % roc_auc)
    plt.plot([0,0,1],[0,1,1], label = 'perfect', color = 'black', linestyle = ':')
    plt.plot([0, 1], [0, 1], label = 'auc = 50', color='navy', linestyle='--')
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.05])
    plt.xlabel('False Positive Rate')
    plt.ylabel('True Positive Rate')
    plt.title('Receiver operating characteristic')
    plt.legend(loc="lower right")
    plt.show()
    
    
    
#Choose all predictors except target & IDcols
target = 'FillDiv_Sign'
predictors = [x for x in FinalData.columns if x not in [target]]

xgb1 = XGBClassifier(
    learning_rate = 0.15,
    n_estimators = 1000,
    max_depth = 5,
    min_child_weight = 1,
    gamma = 0,
    subsample = 0.8,
    colsample_bytree = 0.8,
    objective = 'binary:logistic',
    nthread = 4,
    scale_pos_weight = 1,
    seed = 27)

modelfit(xgb1, Train, Test, predictors)

## 參數調整
# max_dept, min_child_weight = 3, 8 
param_test1 = {
 'max_depth':range(3, 10, 2),
 'min_child_weight':range(1, 6, 2)
}
gsearch1 = GridSearchCV(estimator = XGBClassifier(learning_rate =0.1, n_estimators=140, max_depth=5,
                                                  min_child_weight=1, gamma=0, subsample=0.8, colsample_bytree=0.8,
                                                  objective= 'binary:logistic', nthread=4, scale_pos_weight=1, seed=27), 
                        param_grid = param_test1,     
                        scoring = 'roc_auc',
                        n_jobs = 4,
                        iid = False, 
                        cv = 5)

gsearch1.fit(Train[predictors], Train[target])


gsearch1.cv_results_
gsearch1.best_score_
gsearch1.best_params_

param_test2 = {
 'max_depth':[2, 3, 4],
 'min_child_weight':[4,5,6]
}

gsearch2 = GridSearchCV(estimator = XGBClassifier(learning_rate = 0.1, n_estimators=140, max_depth=5,
                                                  min_child_weight=1, gamma=0, subsample=0.8, colsample_bytree=0.8,
                                                  objective= 'binary:logistic', nthread=4, scale_pos_weight=1, seed=27), 
                        param_grid = param_test2,     
                        scoring = 'roc_auc',
                        n_jobs = 4,
                        iid = False, 
                        cv = 5)

gsearch2.fit(Train[predictors], Train[target])

gsearch2.cv_results_
gsearch2.best_score_
gsearch2.best_params_

param_test2b = {
 'min_child_weight':[6,8,10,12]
 }
gsearch2b  = GridSearchCV(estimator = XGBClassifier(learning_rate = 0.1, n_estimators = 140, max_depth = 3,
                                                  min_child_weight = 6, gamma = 0, subsample = 0.8, colsample_bytree = 0.8,
                                                  objective = 'binary:logistic', nthread = 4, scale_pos_weight = 1, seed = 27), 
                        param_grid = param_test2b,     
                        scoring = 'roc_auc',
                        n_jobs = 4,
                        iid = False, 
                        cv = 5)

gsearch2b.fit(Train[predictors], Train[target])

gsearch2b.cv_results_
gsearch2b.best_score_
gsearch2b.best_params_

param_test2c = {
 'min_child_weight':[7, 8, 9]
 }
gsearch2c = GridSearchCV(estimator = XGBClassifier(learning_rate = 0.1, n_estimators = 140, max_depth = 3,
                                                  min_child_weight = 8, gamma = 0, subsample = 0.8, colsample_bytree = 0.8,
                                                  objective = 'binary:logistic', nthread = 4, scale_pos_weight = 1, seed = 27), 
                        param_grid = param_test2c,     
                        scoring = 'roc_auc',
                        n_jobs = 4,
                        iid = False, 
                        cv = 5)

gsearch2c.fit(Train[predictors], Train[target])

gsearch2c.cv_results_
gsearch2c.best_score_
gsearch2c.best_params_

# Gamma = 0
param_test3 = {
    'gamma':[i/10.0 for i in range(0,5)]
    }

gsearch3  = GridSearchCV(estimator = XGBClassifier(learning_rate = 0.1, n_estimators = 140, max_depth = 3,
                                                  min_child_weight = 8, gamma = 0, subsample = 0.8, colsample_bytree = 0.8,
                                                  objective = 'binary:logistic', nthread = 4, scale_pos_weight = 1, seed = 27), 
                        param_grid = param_test3,     
                        scoring = 'roc_auc',
                        n_jobs = 4,
                        iid = False, 
                        cv = 5)

gsearch3.fit(Train[predictors], Train[target])

gsearch3.cv_results_
gsearch3.best_score_
gsearch3.best_params_

## 調整完 max_dept, min_child_weight 與 gamma後 重新試跑
xgb2 = XGBClassifier(
    learning_rate = 0.1,
    n_estimators = 1000,
    max_depth = 3,
    min_child_weight = 8,
    gamma = 0,
    subsample = 0.8,
    colsample_bytree = 0.8,
    objective = 'binary:logistic',
    nthread = 4,
    scale_pos_weight = 1,
    seed = 27)

modelfit(xgb2, Train, Test, predictors)

# 調整 colsample_bytree, subsample = 0.5, 0.9
param_test4 = {
    'subsample':[i/10.0 for i in range(6,10)],
    'colsample_bytree':[i/10.0 for i in range(6,10)]
    }

gsearch4  = GridSearchCV(estimator = XGBClassifier(learning_rate = 0.1, n_estimators = 177, max_depth = 3,
                                                  min_child_weight = 8, gamma = 0, subsample = 0.8, colsample_bytree = 0.8,
                                                  objective = 'binary:logistic', nthread = 4, scale_pos_weight = 1, seed = 27), 
                        param_grid = param_test4,     
                        scoring = 'roc_auc',
                        n_jobs = 4,
                        iid = False, 
                        cv = 5)

gsearch4.fit(Train[predictors], Train[target])

gsearch4.cv_results_
gsearch4.best_score_
gsearch4.best_params_

param_test5 = {
    'colsample_bytree' : [i/100.0 for i in range(50, 70, 5)],
    'subsample' : [i/100.0 for i in range(80, 100, 5)]
    }

gsearch5 = GridSearchCV(estimator = XGBClassifier(learning_rate = 0.1, n_estimators = 177, max_depth = 2,
                                                  min_child_weight = 4, gamma = 0, subsample = 0.6, colsample_bytree = 0.6,
                                                  objective = 'binary:logistic', nthread = 4, scale_pos_weight = 1, seed = 27), 
                        param_grid = param_test5,     
                        scoring = 'roc_auc',
                        n_jobs = 4,
                        iid = False, 
                        cv = 5)

gsearch5.fit(Train[predictors], Train[target])

gsearch5.cv_results_
gsearch5.best_score_
gsearch5.best_params_

# 嘗試以新的參數再跑一遍模型效果後發現，用 XGB2 的參數 Test 的效果更好
xgb3 = XGBClassifier(
    learning_rate = 0.1,
    n_estimators = 1000,
    max_depth = 3,
    min_child_weight = 8,
    gamma = 0,
    colsample_bytree = 0.8,
    subsample = 0.8,
    objective = 'binary:logistic',
    nthread = 4,
    scale_pos_weight = 1,
    seed = 27)

modelfit(xgb3, Train, Test, predictors)

# reg_alpha 不做調整
param_test6 = {
    'reg_alpha':[1e-5, 1e-2, 0.1, 1, 100]
    }

gsearch6 = GridSearchCV(estimator = XGBClassifier(learning_rate = 0.1, n_estimators = 177, max_depth = 3,
                                                  min_child_weight = 8,  gamma = 0, subsample = 0.8, colsample_bytree = 0.8,
                                                  objective = 'binary:logistic', nthread = 4, scale_pos_weight = 1, seed = 27), 
                        param_grid = param_test6,     
                        scoring = 'roc_auc',
                        n_jobs = 4,
                        iid = False, 
                        cv = 5)

gsearch6.fit(Train[predictors], Train[target])

gsearch6.cv_results_
gsearch6.best_score_
gsearch6.best_params_

xgb4 = XGBClassifier(
    learning_rate = 0.01,
    n_estimators = 1000,
    max_depth = 3,
    min_child_weight = 8,
    gamma = 0,
    subsample = 0.8,
    colsample_bytree = 0.8,
    objective = 'binary:logistic',
    nthread = 4,
    scale_pos_weight = 1,
    seed = 27)

modelfit(xgb4, Train, Test, predictors)

##### 將資料以時間區段做分群

FinalData_WithCompanyNameAndTime["DividendDate"] = pd.to_datetime(FinalData_WithCompanyNameAndTime["DividendDate"])
FinalData_WithCompanyNameAndTime["DividendYear"] = pd.DatetimeIndex(FinalData_WithCompanyNameAndTime["DividendDate"]).year.astype(int)

Data_1990_2000 = FinalData_WithCompanyNameAndTime.loc[(FinalData_WithCompanyNameAndTime['DividendYear'] <= 2000)].drop(ColumnsToDrop, axis = 1, inplace = False)
Data_2001_2008 = FinalData_WithCompanyNameAndTime.loc[(FinalData_WithCompanyNameAndTime['DividendYear'] >= 2001) & (FinalData_WithCompanyNameAndTime['DividendYear'] <= 2008)].drop(ColumnsToDrop, axis = 1, inplace = False)
Data_2009_2020 = FinalData_WithCompanyNameAndTime.loc[(FinalData_WithCompanyNameAndTime['DividendYear'] >= 2009) & (FinalData_WithCompanyNameAndTime['DividendYear'] <= 2020)].drop(ColumnsToDrop, axis = 1, inplace = False)
Dara_Without000820 = FinalData_WithCompanyNameAndTime[~FinalData_WithCompanyNameAndTime['DividendYear'].isin([2000, 2008, 2020])].drop(ColumnsToDrop, axis = 1, inplace = False)


Train_1990_2000, Test_1990_2000 = train_test_split(Data_1990_2000, test_size = 0.25, random_state = 33)
Train_2001_2008, Test_2001_2008 = train_test_split(Data_2001_2008, test_size = 0.25, random_state = 33)
Train_2009_2020, Test_2009_2020 = train_test_split(Data_2009_2020, test_size = 0.25, random_state = 33)
Train_Without000820, Test_Without000820 = train_test_split(Dara_Without000820, test_size = 0.25, random_state = 33)


modelfit(xgb4, Train_1990_2000, Test_1990_2000, predictors) # AUC 73 %
modelfit(xgb4, Train_2001_2008, Test_2001_2008, predictors) # AUC 76 %
modelfit(xgb4, Train_2009_2020, Test_2009_2020, predictors) # AUC 71 %
modelfit(xgb4, Train_Without000820, Test_Without000820, predictors) # AUC 70 %


## 刪除原模型中重要性最差的十名後，再丟入模型嘗試

BadVariableToDrop = ['AccountancyFirms', 'RetainedEarning_ToMarket', 'CrisisEvent_Dummy',
                     'FreeCashFlow_ToMarket', 'ROE_ToMarket', 'OperatingGrossProfitRatio_ToMarket',
                     'BerryRatio_ToMarket', 'InventaryTurnover_ToMarket', 'TotalAssetsTurnover_ToMarket', 
                     'DebtRatio_ToMarket']

NewPredictors = [ele for ele in predictors if ele not in BadVariableToDrop]

RemoveBadVariables_Train = Train.loc[:, ~Train.columns.isin(BadVariableToDrop)]
RemoveBadVariables_Test = Test.loc[:, ~Test.columns.isin(BadVariableToDrop)]

xgb5 = XGBClassifier(
    learning_rate = 0.01,
    n_estimators = 1000,
    max_depth = 3,
    min_child_weight = 8,
    gamma = 0,
    subsample = 0.9,
    colsample_bytree = 0.5,
    objective = 'binary:logistic',
    nthread = 4,
    scale_pos_weight = 1,
    seed = 27)

modelfit(xgb5, RemoveBadVariables_Train, RemoveBadVariables_Test, NewPredictors)

