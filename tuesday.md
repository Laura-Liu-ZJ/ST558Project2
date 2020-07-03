Project2
================
Zhijun Liu
2020-07-02

  - [Introduction](#introduction)
  - [Data](#data)
  - [Summarizations](#summarizations)
      - [Correlation Analysis](#correlation-analysis)
      - [Summary Statistics](#summary-statistics)
      - [Oringinal Accuracy](#oringinal-accuracy)
  - [Modeling](#modeling)
      - [Linear Model](#linear-model)
      - [Non-linear Model](#non-linear-model)
  - [Model Testing](#model-testing)
      - [Linear Model](#linear-model-1)
      - [Non-linear Model](#non-linear-model-1)
  - [Models Comparison](#models-comparison)
  - [Conclusion](#conclusion)

``` r
# prepare for the packages
library(readr)
library(tidyverse)
library(leaps)
library(caret)
library(knitr)
library(corrplot)
```

# Introduction

[This dataset summarizes a heterogeneous set of features about articles
published by Mashable in a period of two
years.](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity)
In this project, we try to use a logistic regression model (linear
model) and a bagged tree model (non-linear model) to predict the number
of shares in social networks.

# Data

The full dataset has 39644 observations and 61 variables. However, there
are only 1 response and 23 predictors we interested in. And following is
the description of variables we cared about.

  - Response:
      - shares: Number of shares
  - Predictors:
      - num\_hrefs: Number of links
      - num\_self\_hrefs: Number of links to other articles published by
        Mashable
      - num\_imgs: Number of images
      - num\_keywords: Number of keywords in the metadata
      - n\_tokens\_content: Number of words in the content
      - n\_unique\_tokens: Rate of unique words in the content
      - data\_channel\_is\_entertainment: Is data channel
        ‘Entertainment’?
      - data\_channel\_is\_bus: Is data channel ‘Business’?
      - data\_channel\_is\_socmed: Is data channel ‘Social Media’?
      - data\_channel\_is\_tech: Is data channel ‘Tech’?
      - kw\_min\_min: Worst keyword (min. shares)
      - kw\_max\_max: Best keyword (max. shares)
      - kw\_max\_avg: Avg. keyword (max. shares)
      - kw\_avg\_avg: Avg. keyword (avg. shares
      - LDA\_00: Closeness to LDA topic 0
      - LDA\_01: Closeness to LDA topic 1
      - LDA\_02: Closeness to LDA topic 2
      - LDA\_03: Closeness to LDA topic 3
      - LDA\_04: Closeness to LDA topic 4
      - title\_sentiment\_polarity: Title polarity
      - global\_subjectivity: Text subjectivity
      - self\_reference\_avg\_sharess: Avg. shares of referenced
        articles in Mashable
      - min\_positive\_polarity: Min. polarity of positive words

In addition, we divide the `shares` into two groups (\< 1400 and \>=
1400), and this variable is called `sharesInd`. And then, we split our
data sets into two data sets, one is the training data set (70% of the
data) and the other is the testing data set (30% of the data).

``` r
weekday_is <- paste0("weekday_is_", params$weekday)
# read the raw data
newsData <- read_csv("data/OnlineNewsPopularity.csv")%>%
  # for specific weekday
  filter(.data[[weekday_is]]==1)%>%
  # add new variable as an indicator of shares group
  mutate(sharesInd = ifelse(shares < 1400,0,1))%>%
  # select response and predictors we interested
  select(sharesInd,
         num_hrefs, num_self_hrefs, num_imgs, num_keywords,
         n_tokens_content, n_unique_tokens, 
         data_channel_is_entertainment, 
         data_channel_is_bus,
         data_channel_is_socmed,
         data_channel_is_tech,
         kw_min_min, kw_max_max, kw_max_avg, kw_avg_avg,
         contains("LDA"),
         title_sentiment_polarity,
         global_subjectivity,
         self_reference_avg_sharess,
         min_positive_polarity)

# change the indicator type into factor
newsData$sharesInd <- as.factor(newsData$sharesInd)

# set seed to generate reproducable results
set.seed(1) 
# split raw data set into training data set and test data set
train <- sample(1:nrow(newsData), size = nrow(newsData)*0.7) 
test <- dplyr::setdiff(1:nrow(newsData), train)
newsDataTrain <- newsData[train, ] 
newsDataTest <- newsData[test, ]

# checking the dimension of our training and testing data sets
dim(newsDataTrain)
```

    ## [1] 5173   24

``` r
dim(newsDataTest)
```

    ## [1] 2217   24

From the above 23 potential predictors, we use mallow’s cp and BIC to
choose the variables which can be used in the linear model. Therefore,
we use `newsDataFit` data set for fitting the linear model and use
`newsDataTrain` data set for fitting the non-linear model.

``` r
# variables selection process
all<-regsubsets(sharesInd ~., 
                data=newsDataTrain, 
                nbest=1, really.big=T)
info <- summary(all)
# combine the information of some marks
selectData<-cbind(info$which, round(cbind(rsq=info$rsq, 
                              adjr2=info$adjr2,
                              cp=info$cp,
                              bic=info$bic, 
                              rss=info$rss), 3)) %>% 
  tbl_df() %>% arrange(bic)

# get the variables' name from above result
varName <- colnames(selectData)[which(selectData[1,] == 1)][-1]
varName
```

    ## [1] "num_imgs"               "num_keywords"           "data_channel_is_socmed"
    ## [4] "data_channel_is_tech"   "kw_max_max"             "kw_max_avg"            
    ## [7] "kw_avg_avg"             "LDA_00"

``` r
# generate the data set only from above results
newsDataFit <- newsDataTrain %>%
  select(sharesInd,contains(varName))
```

# Summarizations

## Correlation Analysis

``` r
# reorganize the data to do the correlation analysis
shares <- as.numeric(newsDataFit$sharesInd)
cor.data <- cbind(shares, newsDataFit[,-1])
corrplot(cor(cor.data), 
         lower.col = "steelblue",
         type="upper",
         tl.cex = 0.7,
         title="Correlation Plot",
         mar=c(0,0,2,0))
```

<img src="tuesday_files/figure-gfm/corrplot-1.png" width="200%" />

``` r
# get the name of variables who have positive or negative relationship with our response
p_idx <- which(cor(newsDataFit[,-1],as.numeric(newsDataFit$sharesInd)) > 0)
n_idx <- which(cor(newsDataFit[,-1],as.numeric(newsDataFit$sharesInd)) < 0)
# Positive relationships
colnames(newsDataFit)[p_idx+1]
```

    ## [1] "num_imgs"               "num_keywords"           "data_channel_is_socmed"
    ## [4] "data_channel_is_tech"   "kw_max_avg"             "kw_avg_avg"            
    ## [7] "LDA_00"

``` r
# Negative relationships
colnames(newsDataFit)[n_idx+1]
```

    ## [1] "kw_max_max"

There are some variables who might have positive linear relationship
with our response: num\_hrefs, num\_self\_hrefs, num\_imgs,
num\_keywords, n\_unique\_tokens, data\_channel\_is\_entertainment,
data\_channel\_is\_bus.

And there are some variables who might have negative linear relationship
with our response: n\_tokens\_content.

## Summary Statistics

From the summary results of our training data set, we know that the
exact number of each shares group. Furthermore, we should standardize
our variables before fitting the model due to the extreme differences in
the maximum of each variable.

``` r
# get the summary statistics of our training data set
summary(newsDataTrain)
```

    ##  sharesInd   num_hrefs      num_self_hrefs      num_imgs      
    ##  0:2646    Min.   :  0.00   Min.   : 0.000   Min.   :  0.000  
    ##  1:2527    1st Qu.:  4.00   1st Qu.: 1.000   1st Qu.:  1.000  
    ##            Median :  7.00   Median : 3.000   Median :  1.000  
    ##            Mean   : 10.65   Mean   : 3.325   Mean   :  4.457  
    ##            3rd Qu.: 13.00   3rd Qu.: 4.000   3rd Qu.:  3.000  
    ##            Max.   :304.00   Max.   :62.000   Max.   :100.000  
    ##   num_keywords    n_tokens_content n_unique_tokens   
    ##  Min.   : 1.000   Min.   :   0.0   Min.   :  0.0000  
    ##  1st Qu.: 6.000   1st Qu.: 246.0   1st Qu.:  0.4723  
    ##  Median : 7.000   Median : 398.0   Median :  0.5412  
    ##  Mean   : 7.167   Mean   : 543.4   Mean   :  0.6662  
    ##  3rd Qu.: 9.000   3rd Qu.: 689.0   3rd Qu.:  0.6098  
    ##  Max.   :10.000   Max.   :7081.0   Max.   :701.0000  
    ##  data_channel_is_entertainment data_channel_is_bus data_channel_is_socmed
    ##  Min.   :0.0000                Min.   :0.0000      Min.   :0.00000       
    ##  1st Qu.:0.0000                1st Qu.:0.0000      1st Qu.:0.00000       
    ##  Median :0.0000                Median :0.0000      Median :0.00000       
    ##  Mean   :0.1746                Mean   :0.1531      Mean   :0.06418       
    ##  3rd Qu.:0.0000                3rd Qu.:0.0000      3rd Qu.:0.00000       
    ##  Max.   :1.0000                Max.   :1.0000      Max.   :1.00000       
    ##  data_channel_is_tech   kw_min_min       kw_max_max       kw_max_avg    
    ##  Min.   :0.0000       Min.   : -1.00   Min.   : 17100   Min.   :  2019  
    ##  1st Qu.:0.0000       1st Qu.: -1.00   1st Qu.:843300   1st Qu.:  3529  
    ##  Median :0.0000       Median : -1.00   Median :843300   Median :  4286  
    ##  Mean   :0.2014       Mean   : 25.02   Mean   :755769   Mean   :  5619  
    ##  3rd Qu.:0.0000       3rd Qu.:  4.00   3rd Qu.:843300   3rd Qu.:  6020  
    ##  Max.   :1.0000       Max.   :217.00   Max.   :843300   Max.   :178675  
    ##    kw_avg_avg          LDA_00            LDA_01            LDA_02       
    ##  Min.   :  804.4   Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
    ##  1st Qu.: 2368.3   1st Qu.:0.02506   1st Qu.:0.02501   1st Qu.:0.02857  
    ##  Median : 2842.3   Median :0.03337   Median :0.03334   Median :0.04004  
    ##  Mean   : 3129.8   Mean   :0.17780   Mean   :0.13432   Mean   :0.21945  
    ##  3rd Qu.: 3575.9   3rd Qu.:0.22811   3rd Qu.:0.13268   3rd Qu.:0.33900  
    ##  Max.   :29240.8   Max.   :0.91998   Max.   :0.91994   Max.   :0.92000  
    ##      LDA_03            LDA_04        title_sentiment_polarity
    ##  Min.   :0.00000   Min.   :0.00000   Min.   :-1.00000        
    ##  1st Qu.:0.02857   1st Qu.:0.02858   1st Qu.: 0.00000        
    ##  Median :0.04000   Median :0.05000   Median : 0.00000        
    ##  Mean   :0.22107   Mean   :0.24718   Mean   : 0.07309        
    ##  3rd Qu.:0.35652   3rd Qu.:0.44016   3rd Qu.: 0.13636        
    ##  Max.   :0.91997   Max.   :0.92719   Max.   : 1.00000        
    ##  global_subjectivity self_reference_avg_sharess min_positive_polarity
    ##  Min.   :0.0000      Min.   :     0             Min.   :0.00000      
    ##  1st Qu.:0.3952      1st Qu.:   992             1st Qu.:0.05000      
    ##  Median :0.4516      Median :  2250             Median :0.10000      
    ##  Mean   :0.4404      Mean   :  6450             Mean   :0.09483      
    ##  3rd Qu.:0.5052      3rd Qu.:  5200             3rd Qu.:0.10000      
    ##  Max.   :0.8420      Max.   :690400             Max.   :0.70000

## Oringinal Accuracy

According to the contingency table, we calculate the accuracy for each
data set by treating all the shares as in the “\>= 1400” group.

``` r
# accuracy for training data set if we treat all the shares as in the >= 1400 group
orgAccuracyTrain <- round(table(newsDataTrain$sharesInd)[2]/nrow(newsDataTrain),4)
orgAccuracyTrain
```

    ##      1 
    ## 0.4885

``` r
# accuracy for testing data set if we treat all the shares as in the >= 1400 group
orgAccuracyTest <- round(table(newsDataTest$sharesInd)[2]/nrow(newsDataTest),4)
orgAccuracyTest
```

    ##     1 
    ## 0.507

# Modeling

## Linear Model

We use the variables to choose from mallow’s cp and BIC as our
predictors in the linear model. Therefore, we use `newsDataFit` data set
for the linear model. But before fitting the model we should standardize
our data first. And then, we need to calculate odds for given predictors
and convert them into binary variables. After the prediction process, we
using the confusion matrix to get the accuracy of the linear model for
the training data set.

``` r
# standardize our data set
std.newsDataFit<- cbind(newsDataFit[,1],scale(newsDataFit[,-1]))

# fit the linear model
linearModel <- glm(sharesInd ~ .,std.newsDataFit,family="binomial")
# get the result of linear model
summary(linearModel)
```

    ## 
    ## Call:
    ## glm(formula = sharesInd ~ ., family = "binomial", data = std.newsDataFit)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.1611  -1.0607  -0.6989   1.0911   2.1416  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)            -0.04611    0.02927  -1.576    0.115    
    ## num_imgs                0.12909    0.03083   4.188 2.82e-05 ***
    ## num_keywords            0.17110    0.03063   5.585 2.33e-08 ***
    ## data_channel_is_socmed  0.24882    0.03166   7.859 3.87e-15 ***
    ## data_channel_is_tech    0.34623    0.03056  11.331  < 2e-16 ***
    ## kw_max_max             -0.22548    0.03030  -7.442 9.89e-14 ***
    ## kw_max_avg             -0.51665    0.05893  -8.766  < 2e-16 ***
    ## kw_avg_avg              0.85852    0.05716  15.019  < 2e-16 ***
    ## LDA_00                  0.22569    0.03091   7.301 2.85e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7168.6  on 5172  degrees of freedom
    ## Residual deviance: 6659.7  on 5164  degrees of freedom
    ## AIC: 6677.7
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# predict reponse of training data set by fitting model 
# get the log-odds and convert it into binomial variable
linearTrainPred <- ifelse(exp(predict(linearModel,
                                      newdata = std.newsDataFit,
                                      type="link"))<1,0,1)
# Corss validation for training data set
linearFit <- confusionMatrix(as.factor(linearTrainPred), newsDataFit$sharesInd)
linearFit
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 1723  961
    ##          1  923 1566
    ##                                           
    ##                Accuracy : 0.6358          
    ##                  95% CI : (0.6225, 0.6489)
    ##     No Information Rate : 0.5115          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.271           
    ##                                           
    ##  Mcnemar's Test P-Value : 0.394           
    ##                                           
    ##             Sensitivity : 0.6512          
    ##             Specificity : 0.6197          
    ##          Pos Pred Value : 0.6420          
    ##          Neg Pred Value : 0.6292          
    ##              Prevalence : 0.5115          
    ##          Detection Rate : 0.3331          
    ##    Detection Prevalence : 0.5188          
    ##       Balanced Accuracy : 0.6354          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

## Non-linear Model

We use `newsDataTrain` data set for fitting the bagged tree model. The
bagged tree model is a method combining the bootstrap and the tree
model. It creates a bootstrap sample first and then uses a train tree on
this sample. Also, we standardize the variables before fitting the
model.

``` r
# set seed to get a reproducable result
set.seed(1)
# fit the bagged tree model
treebagFit <- train(sharesInd ~ ., 
                    data = newsDataTrain, 
                    method = "treebag", 
                    tuneLength = 10,
                    # standardize the variables
                    preProcess = c("center", "scale"))
# get the fit result of training data set
treebagFit
```

    ## Bagged CART 
    ## 
    ## 5173 samples
    ##   23 predictor
    ##    2 classes: '0', '1' 
    ## 
    ## Pre-processing: centered (23), scaled (23) 
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 5173, 5173, 5173, 5173, 5173, 5173, ... 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa    
    ##   0.6089026  0.2176962

# Model Testing

## Linear Model

Before the predictions, we should standardize our test data first. And
then, we need to calculate odds for given predictors and convert them
into binary variables. After the prediction process, we use the
confusion matrix to get the accuracy of the linear model for the testing
data set.

``` r
# standardize our data set
std.newsDataTest<- cbind(newsDataTest[,1], scale(newsDataTest[,-1]))

# predict reponse of testing data set by fitting model
linearTestPred <- ifelse(exp(predict(linearModel,
                                     newdata = std.newsDataTest,
                                     type="link"))<1,0,1)
# Corss validation of testing data set
linearResult <- confusionMatrix(as.factor(linearTestPred), newsDataTest$sharesInd)
linearResult
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 728 437
    ##          1 365 687
    ##                                           
    ##                Accuracy : 0.6382          
    ##                  95% CI : (0.6179, 0.6583)
    ##     No Information Rate : 0.507           
    ##     P-Value [Acc > NIR] : < 2e-16         
    ##                                           
    ##                   Kappa : 0.277           
    ##                                           
    ##  Mcnemar's Test P-Value : 0.01217         
    ##                                           
    ##             Sensitivity : 0.6661          
    ##             Specificity : 0.6112          
    ##          Pos Pred Value : 0.6249          
    ##          Neg Pred Value : 0.6530          
    ##              Prevalence : 0.4930          
    ##          Detection Rate : 0.3284          
    ##    Detection Prevalence : 0.5255          
    ##       Balanced Accuracy : 0.6386          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

## Non-linear Model

We use the bagged tree model to predict the `sharesInd` in the testing
data set. And then, we calculate the accuracy of testing data set by the
confusion matrix.

``` r
# predict reponse of testing data set by fitting model
treebagTestPred <- predict(treebagFit, newdata = newsDataTest)
# Corss validation of testing data set
treebagResult <- confusionMatrix(treebagTestPred, newsDataTest$sharesInd)
treebagResult
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 694 419
    ##          1 399 705
    ##                                           
    ##                Accuracy : 0.631           
    ##                  95% CI : (0.6106, 0.6512)
    ##     No Information Rate : 0.507           
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.2621          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.5065          
    ##                                           
    ##             Sensitivity : 0.6349          
    ##             Specificity : 0.6272          
    ##          Pos Pred Value : 0.6235          
    ##          Neg Pred Value : 0.6386          
    ##              Prevalence : 0.4930          
    ##          Detection Rate : 0.3130          
    ##    Detection Prevalence : 0.5020          
    ##       Balanced Accuracy : 0.6311          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

# Models Comparison

``` r
# combine the accuracy of corss validation together
Accuracytable <- round(data.frame(Train=rbind(linearFit$overall[1],
                                             treebagFit$results$Accuracy,
                                             orgAccuracyTrain),
                                 Test = rbind(linearResult$overall[1],
                                              treebagResult$overall[1],
                                              orgAccuracyTest),
                                 row.names = c("Logistic Regression",
                                               "Bagged Tree",
                                               "Original Data")
                                 ),4)
# caculate the misclassification rate (Apparent Error)
APERtable <- 1-Accuracytable

# print the table
kable(Accuracytable,
      row.names=TRUE,
      col.names = c("Train","Test"),
      caption = "Accuracy Table")
```

|                     |  Train |   Test |
| ------------------- | -----: | -----: |
| Logistic Regression | 0.6358 | 0.6382 |
| Bagged Tree         | 0.6089 | 0.6310 |
| Original Data       | 0.4885 | 0.5070 |

Accuracy Table

``` r
kable(APERtable ,
      row.names=TRUE,
      col.names = c("Train","Test"),
      caption = "APER Table")
```

|                     |  Train |   Test |
| ------------------- | -----: | -----: |
| Logistic Regression | 0.3642 | 0.3618 |
| Bagged Tree         | 0.3911 | 0.3690 |
| Original Data       | 0.5115 | 0.4930 |

APER Table

``` r
# the best model for training data set
bestTrainModel <- APERtable %>% arrange(Accuracy)
rownames(bestTrainModel[1,])
```

    ## [1] "Logistic Regression"

``` r
# the best model for testing data set
bestTestModel <- APERtable %>% arrange(Accuracy.1)
rownames(bestTestModel[1,])
```

    ## [1] "Logistic Regression"

According to the results of the APER table, we would choose the smallest
value as the best model for each data set. In this case, our best model
for the training data set is Logistic Regression and the best model for
testing data set is Logistic Regression. However, if the best model
turns out to be the Original Data, neither the linear model nor the
non-linear model had done the job well.

# Conclusion

For TUESDAY data, I would choose the Logistic Regression because it fit
testing data sets better. And if I use this model, I would expect 36.18%
as its misclassification rate.
