---
title: "Flight Delays January 2015"
author: "Jacob Duvall"
date: "7/10/2019"
output:
  html_document:
    toc: true
    keep_md: true
---

# Clean Data


```r
origData = read.csv2('C:\\Users\\jdduval1\\Desktop\\814981452_T_ONTIME_REPORTING.csv', sep=",", header=TRUE, stringsAsFactors=FALSE)
```


```r
nrow(origData)
```

```
## [1] 469968
```


```r
# reduce number of rows
airports = c('ATL', 'LAX', 'ORD', 'DFW', 'JFK', 'SFO', 'CLT', 'LAS', 'PHX')
origData = subset(origData, DEST %in% airports & ORIGIN %in% airports)
```


```r
nrow(origData)
```

```
## [1] 32716
```


```r
head(origData,2)
```

```
##    DAY_OF_MONTH DAY_OF_WEEK OP_UNIQUE_CARRIER OP_CARRIER_AIRLINE_ID
## 12            1           4                NK                 20416
## 17            1           4                NK                 20416
##    OP_CARRIER TAIL_NUM OP_CARRIER_FL_NUM ORIGIN_AIRPORT_ID
## 12         NK   N632NK               214             12889
## 17         NK   N504NK               219             13930
##    ORIGIN_AIRPORT_SEQ_ID ORIGIN DEST_AIRPORT_ID DEST_AIRPORT_SEQ_ID DEST
## 12               1288903    LAS           11298             1129803  DFW
## 17               1393003    ORD           10397             1039705  ATL
##    DEP_TIME DEP_DEL15 DEP_TIME_BLK ARR_TIME ARR_DEL15 CANCELLED DIVERTED
## 12      102      0.00    0001-0559      529      0.00      0.00     0.00
## 17     1103      0.00    1100-1159     1346      0.00      0.00     0.00
##    DISTANCE  X
## 12  1055.00 NA
## 17   606.00 NA
```

```r
# x column has no value
```


```r
# checking that x has no value here either
tail(origData, 2)
```

```
##        DAY_OF_MONTH DAY_OF_WEEK OP_UNIQUE_CARRIER OP_CARRIER_AIRLINE_ID
## 469666           31           6                WN                 19393
## 469667           31           6                WN                 19393
##        OP_CARRIER TAIL_NUM OP_CARRIER_FL_NUM ORIGIN_AIRPORT_ID
## 469666         WN   N659SW              3841             14771
## 469667         WN   N218WN              4481             14771
##        ORIGIN_AIRPORT_SEQ_ID ORIGIN DEST_AIRPORT_ID DEST_AIRPORT_SEQ_ID
## 469666               1477101    SFO           14107             1410702
## 469667               1477101    SFO           14107             1410702
##        DEST DEP_TIME DEP_DEL15 DEP_TIME_BLK ARR_TIME ARR_DEL15 CANCELLED
## 469666  PHX     1109      0.00    1100-1159     1417      0.00      0.00
## 469667  PHX     1426      0.00    1400-1459     1721      0.00      0.00
##        DIVERTED DISTANCE  X
## 469666     0.00   651.00 NA
## 469667     0.00   651.00 NA
```


```r
# remove x column
origData$X = NULL
head(origData, 2)
```

```
##    DAY_OF_MONTH DAY_OF_WEEK OP_UNIQUE_CARRIER OP_CARRIER_AIRLINE_ID
## 12            1           4                NK                 20416
## 17            1           4                NK                 20416
##    OP_CARRIER TAIL_NUM OP_CARRIER_FL_NUM ORIGIN_AIRPORT_ID
## 12         NK   N632NK               214             12889
## 17         NK   N504NK               219             13930
##    ORIGIN_AIRPORT_SEQ_ID ORIGIN DEST_AIRPORT_ID DEST_AIRPORT_SEQ_ID DEST
## 12               1288903    LAS           11298             1129803  DFW
## 17               1393003    ORD           10397             1039705  ATL
##    DEP_TIME DEP_DEL15 DEP_TIME_BLK ARR_TIME ARR_DEL15 CANCELLED DIVERTED
## 12      102      0.00    0001-0559      529      0.00      0.00     0.00
## 17     1103      0.00    1100-1159     1346      0.00      0.00     0.00
##    DISTANCE
## 12  1055.00
## 17   606.00
```


```r
# check for correlated columns that might skew the results - value close to 1 is a correlation
cor(origData[c("ORIGIN_AIRPORT_ID", "ORIGIN_AIRPORT_SEQ_ID")])
```

```
##                       ORIGIN_AIRPORT_ID ORIGIN_AIRPORT_SEQ_ID
## ORIGIN_AIRPORT_ID                     1                     1
## ORIGIN_AIRPORT_SEQ_ID                 1                     1
```

```r
cor(origData[c("DEST_AIRPORT_SEQ_ID", "DEST_AIRPORT_ID")])
```

```
##                     DEST_AIRPORT_SEQ_ID DEST_AIRPORT_ID
## DEST_AIRPORT_SEQ_ID                   1               1
## DEST_AIRPORT_ID                       1               1
```


```r
# drop correlated columns
origData$ORIGIN_AIRPORT_SEQ_ID = NULL
origData$DEST_AIRPORT_SEQ_ID = NULL
```


```r
# check if OP_CARRIER and OP_UNIQUE_CARRIER are different 
mismatched = origData[origData$OP_CARRIER != origData$OP_UNIQUE_CARRIER,]
nrow(mismatched)
```

```
## [1] 0
```

```r
# these are the same
```


```r
# drop correlated column
origData$OP_UNIQUE_CARRIER = NULL
head(origData,2)
```

```
##    DAY_OF_MONTH DAY_OF_WEEK OP_CARRIER_AIRLINE_ID OP_CARRIER TAIL_NUM
## 12            1           4                 20416         NK   N632NK
## 17            1           4                 20416         NK   N504NK
##    OP_CARRIER_FL_NUM ORIGIN_AIRPORT_ID ORIGIN DEST_AIRPORT_ID DEST
## 12               214             12889    LAS           11298  DFW
## 17               219             13930    ORD           10397  ATL
##    DEP_TIME DEP_DEL15 DEP_TIME_BLK ARR_TIME ARR_DEL15 CANCELLED DIVERTED
## 12      102      0.00    0001-0559      529      0.00      0.00     0.00
## 17     1103      0.00    1100-1159     1346      0.00      0.00     0.00
##    DISTANCE
## 12  1055.00
## 17   606.00
```

# Mold Data


```r
# create new dataframe where origData is modified to remove empty rows in ARR_DEL15 and DEP_DEL15
onTimeData = origData[!is.na(origData$ARR_DEL15) & origData$ARR_DEL15 != "" & !is.na(origData$DEP_DEL15) & origData$DEP_DEL15 != "",]
```


```r
# check the number of rows removed
nrow(origData)
```

```
## [1] 32716
```

```r
nrow(onTimeData)
```

```
## [1] 32124
```


```r
# change the data type of columns DISTANCE, CANCELLED, DIVERTED from chr to int
onTimeData$DISTANCE = as.integer(onTimeData$DISTANCE)
onTimeData$CANCELLED = as.integer(onTimeData$CANCELLED)
onTimeData$DIVERTED = as.integer(onTimeData$DIVERTED)
```


```r
# change the data type of columns DEP_DEL15, ARR_DEL15, DEST_AIRPORT_ID, ORIGIN_AIRPORT_ID
# DAY_OF_WEEK, DEST, ORIGIN, DEP_TIME_BLK, CARRIER to Factor
onTimeData$ARR_DEL15 = as.factor(onTimeData$ARR_DEL15)
onTimeData$DEP_DEL15 = as.factor(onTimeData$DEP_DEL15)
onTimeData$DEST_AIRPORT_ID = as.factor(onTimeData$DEST_AIRPORT_ID)
onTimeData$ORIGIN_AIRPORT_ID = as.factor(onTimeData$DAY_OF_WEEK)
onTimeData$DAY_OF_WEEK = as.factor(onTimeData$DEP_DEL15)
onTimeData$DEST = as.factor(onTimeData$DEST)
onTimeData$ORIGIN = as.factor(onTimeData$ORIGIN)
onTimeData$DEP_TIME_BLK = as.factor(onTimeData$DEP_TIME_BLK)
onTimeData$OP_CARRIER = as.factor(onTimeData$OP_CARRIER)
```


```r
# check number of delays vs number of non-delays
tapply(onTimeData$ARR_DEL15, onTimeData$ARR_DEL15, length)
```

```
##  0.00  1.00 
## 25664  6460
```

```r
(6460 / (25664 + 6460))
```

```
## [1] 0.2010958
```

```r
# can reasonably train model with 20% rate 
```

# Training the Model


```r
library(caret)
```

```
## Warning: package 'caret' was built under R version 3.6.1
```


```r
set.seed(122515)
```



```r
# The important columns to base training data 
featureCols = c("ARR_DEL15", "DAY_OF_WEEK", "OP_CARRIER", "DEST", "ORIGIN", "DEP_TIME_BLK")
onTimeDataFiltered = onTimeData[,featureCols]
```


```r
# split into training and testing data - 70% split among ARR_DEL15
inTrainRows = createDataPartition(onTimeDataFiltered$ARR_DEL15, p=0.70, list=FALSE)
```


```r
head(inTrainRows, 10)
```

```
##       Resample1
##  [1,]         1
##  [2,]         2
##  [3,]         4
##  [4,]         5
##  [5,]         6
##  [6,]         7
##  [7,]         9
##  [8,]        13
##  [9,]        14
## [10,]        15
```


```r
# set up training and test data based on the row split
trainDataFiltered = onTimeDataFiltered[inTrainRows,]
testDataFiltered = onTimeDataFiltered[-inTrainRows,]
```


```r
# check for 70% split
nrow(trainDataFiltered)/(nrow(testDataFiltered) + nrow(trainDataFiltered))
```

```
## [1] 0.7000062
```


```r
# check for 30% split
nrow(testDataFiltered)/(nrow(testDataFiltered) + nrow(trainDataFiltered))
```

```
## [1] 0.2999938
```


```r
# train with LOGISTIC REGRESSION using all columns other than ARR_DEL15
logisticRegModel = train(ARR_DEL15 ~ ., data=trainDataFiltered, method="glm", family = "binomial")
```


```r
logisticRegModel
```

```
## Generalized Linear Model 
## 
## 22487 samples
##     5 predictor
##     2 classes: '0.00', '1.00' 
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## Summary of sample sizes: 22487, 22487, 22487, 22487, 22487, 22487, ... 
## Resampling results:
## 
##   Accuracy   Kappa    
##   0.9032302  0.6928948
```

# Testing the Model Accuracy


```r
# predict using the model and the test data
logRegPrediction = predict(logisticRegModel, testDataFiltered)
```


```r
# create a confusion matrix
logRegConfMat = confusionMatrix(logRegPrediction, testDataFiltered[, "ARR_DEL15"])
```


```r
logRegConfMat
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction 0.00 1.00
##       0.00 7303  483
##       1.00  396 1455
##                                           
##                Accuracy : 0.9088          
##                  95% CI : (0.9029, 0.9145)
##     No Information Rate : 0.7989          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.7113          
##                                           
##  Mcnemar's Test P-Value : 0.003723        
##                                           
##             Sensitivity : 0.9486          
##             Specificity : 0.7508          
##          Pos Pred Value : 0.9380          
##          Neg Pred Value : 0.7861          
##              Prevalence : 0.7989          
##          Detection Rate : 0.7578          
##    Detection Prevalence : 0.8079          
##       Balanced Accuracy : 0.8497          
##                                           
##        'Positive' Class : 0.00            
## 
```

```r
# great results!
```
