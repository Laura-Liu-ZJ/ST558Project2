Project2
================
Zhijun Liu
2020-07-03

  - [Introduction](#introduction)
  - [Data](#data)
  - [Summarizations](#summarizations)
      - [Correlation Analysis](#correlation-analysis)
      - [Summary Statistics](#summary-statistics)
      - [Original Accuracy](#original-accuracy)
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

    ## [1] 3990   24

``` r
dim(newsDataTest)
```

    ## [1] 1711   24

From the above 23 potential predictors, we use mallow’s cp and BIC to
choose the variables which can be used in the linear model. Therefore,
we use `newsDataFit` data set for fitting the linear model and use
`newsDataTrain` data set for fitting the non-linear model.

``` r
# variables selection process
all<-regsubsets(sharesInd ~., 
                data=newsDataTrain, 
                nbest=1, really.big=T)
```

    ## Reordering variables and trying again:

``` r
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

    ## [1] "n_tokens_content"              "data_channel_is_entertainment"
    ## [3] "data_channel_is_socmed"        "data_channel_is_tech"         
    ## [5] "kw_min_min"                    "kw_max_avg"                   
    ## [7] "kw_avg_avg"                    "LDA_02"                       
    ## [9] "self_reference_avg_sharess"

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

<img src="friday_files/figure-gfm/corrplot-1.png" width="200%" />

``` r
# get the name of variables who have positive or negative relationship with our response
p_idx <- which(cor(newsDataFit[,-1],as.numeric(newsDataFit$sharesInd)) > 0)
n_idx <- which(cor(newsDataFit[,-1],as.numeric(newsDataFit$sharesInd)) < 0)
# Positive relationships
colnames(newsDataFit)[p_idx+1]
```

    ## [1] "n_tokens_content"           "data_channel_is_socmed"    
    ## [3] "data_channel_is_tech"       "kw_min_min"                
    ## [5] "kw_max_avg"                 "kw_avg_avg"                
    ## [7] "self_reference_avg_sharess"

``` r
# Negative relationships
colnames(newsDataFit)[n_idx+1]
```

    ## [1] "data_channel_is_entertainment" "LDA_02"

There are some variables who might have positive linear relationship
with our response: num\_hrefs, num\_imgs, num\_keywords,
n\_tokens\_content, n\_unique\_tokens, data\_channel\_is\_entertainment,
data\_channel\_is\_socmed.

And there are some variables who might have negative linear relationship
with our response: num\_self\_hrefs, data\_channel\_is\_bus.

## Summary Statistics

From the summary results of our training data set, we know that the
exact number of each shares group. Furthermore, we should standardize
our variables before fitting the model due to the extreme differences in
the maximum of each variable.

``` r
# get the summary statistics of our training data set
summary(newsDataTrain)
```

    ##  sharesInd   num_hrefs      num_self_hrefs      num_imgs        num_keywords   
    ##  0:1802    Min.   :  0.00   Min.   : 0.000   Min.   :  0.000   Min.   : 1.000  
    ##  1:2188    1st Qu.:  4.00   1st Qu.: 1.000   1st Qu.:  1.000   1st Qu.: 6.000  
    ##            Median :  7.00   Median : 2.000   Median :  1.000   Median : 7.000  
    ##            Mean   : 10.71   Mean   : 3.049   Mean   :  4.434   Mean   : 7.217  
    ##            3rd Qu.: 13.00   3rd Qu.: 4.000   3rd Qu.:  3.000   3rd Qu.: 9.000  
    ##            Max.   :186.00   Max.   :51.000   Max.   :108.000   Max.   :10.000  
    ##  n_tokens_content n_unique_tokens  data_channel_is_entertainment
    ##  Min.   :   0.0   Min.   :0.0000   Min.   :0.0000               
    ##  1st Qu.: 235.0   1st Qu.:0.4763   1st Qu.:0.0000               
    ##  Median : 408.0   Median :0.5456   Median :0.0000               
    ##  Mean   : 529.9   Mean   :0.5375   Mean   :0.1714               
    ##  3rd Qu.: 698.8   3rd Qu.:0.6168   3rd Qu.:0.0000               
    ##  Max.   :7413.0   Max.   :0.9474   Max.   :1.0000               
    ##  data_channel_is_bus data_channel_is_socmed data_channel_is_tech
    ##  Min.   :0.0000      Min.   :0.00000        Min.   :0.0000      
    ##  1st Qu.:0.0000      1st Qu.:0.00000        1st Qu.:0.0000      
    ##  Median :0.0000      Median :0.00000        Median :0.0000      
    ##  Mean   :0.1534      Mean   :0.05539        Mean   :0.1687      
    ##  3rd Qu.:0.0000      3rd Qu.:0.00000        3rd Qu.:0.0000      
    ##  Max.   :1.0000      Max.   :1.00000        Max.   :1.0000      
    ##    kw_min_min       kw_max_max       kw_max_avg       kw_avg_avg     
    ##  Min.   : -1.00   Min.   : 28000   Min.   :  2195   Min.   :  776.1  
    ##  1st Qu.: -1.00   1st Qu.:843300   1st Qu.:  3570   1st Qu.: 2381.3  
    ##  Median : -1.00   Median :843300   Median :  4398   Median : 2858.1  
    ##  Mean   : 26.99   Mean   :753092   Mean   :  5662   Mean   : 3152.7  
    ##  3rd Qu.:  4.00   3rd Qu.:843300   3rd Qu.:  6079   3rd Qu.: 3617.0  
    ##  Max.   :217.00   Max.   :843300   Max.   :171030   Max.   :37607.5  
    ##      LDA_00            LDA_01            LDA_02            LDA_03       
    ##  Min.   :0.01818   Min.   :0.01818   Min.   :0.01818   Min.   :0.01818  
    ##  1st Qu.:0.02503   1st Qu.:0.02502   1st Qu.:0.02857   1st Qu.:0.02655  
    ##  Median :0.03335   Median :0.03334   Median :0.04003   Median :0.04000  
    ##  Mean   :0.17576   Mean   :0.13937   Mean   :0.22754   Mean   :0.23209  
    ##  3rd Qu.:0.23156   3rd Qu.:0.15038   3rd Qu.:0.36692   3rd Qu.:0.39842  
    ##  Max.   :0.92000   Max.   :0.91998   Max.   :0.92000   Max.   :0.92554  
    ##      LDA_04        title_sentiment_polarity global_subjectivity
    ##  Min.   :0.01819   Min.   :-1.00000         Min.   :0.0000     
    ##  1st Qu.:0.02857   1st Qu.: 0.00000         1st Qu.:0.3987     
    ##  Median :0.04003   Median : 0.00000         Median :0.4560     
    ##  Mean   :0.22524   Mean   : 0.06749         Mean   :0.4467     
    ##  3rd Qu.:0.37212   3rd Qu.: 0.13636         3rd Qu.:0.5123     
    ##  Max.   :0.92653   Max.   : 1.00000         Max.   :0.9500     
    ##  self_reference_avg_sharess min_positive_polarity
    ##  Min.   :     0.0           Min.   :0.00000      
    ##  1st Qu.:   944.5           1st Qu.:0.05000      
    ##  Median :  2200.0           Median :0.10000      
    ##  Mean   :  6501.1           Mean   :0.09757      
    ##  3rd Qu.:  5100.0           3rd Qu.:0.10000      
    ##  Max.   :663600.0           Max.   :1.00000

## Original Accuracy

According to the contingency table, we calculate the accuracy for each
data set by treating all the shares as in the “\>= 1400” group.

``` r
# accuracy for training data set if we treat all the shares as in the >= 1400 group
orgAccuracyTrain <- round(table(newsDataTrain$sharesInd)[2]/nrow(newsDataTrain),4)
orgAccuracyTrain
```

    ##      1 
    ## 0.5484

``` r
# accuracy for testing data set if we treat all the shares as in the >= 1400 group
orgAccuracyTest <- round(table(newsDataTest$sharesInd)[2]/nrow(newsDataTest),4)
orgAccuracyTest
```

    ##      1 
    ## 0.5406

# Modeling

## Linear Model

We use the variables to choose from mallow’s cp and BIC as our
predictors in the linear model. Therefore, we use `newsDataFit` data set
for the logistic regression model. But before fitting the model we
should standardize our data first. And then, we need to calculate odds
for given predictors and convert them into binary variables. After the
prediction process, we using the confusion matrix to get the accuracy of
the linear model for the training data set.

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
    ## -3.3823  -1.0871   0.6964   1.0210   1.9841  
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                    0.23047    0.03402   6.774 1.25e-11 ***
    ## n_tokens_content               0.15627    0.03649   4.283 1.84e-05 ***
    ## data_channel_is_entertainment -0.25877    0.03617  -7.153 8.47e-13 ***
    ## data_channel_is_socmed         0.22193    0.03863   5.745 9.18e-09 ***
    ## data_channel_is_tech           0.23856    0.03759   6.346 2.22e-10 ***
    ## kw_min_min                     0.19371    0.03610   5.365 8.08e-08 ***
    ## kw_max_avg                    -0.38923    0.08232  -4.728 2.26e-06 ***
    ## kw_avg_avg                     0.63932    0.08047   7.944 1.95e-15 ***
    ## LDA_02                        -0.24220    0.03956  -6.122 9.25e-10 ***
    ## self_reference_avg_sharess     0.29928    0.07453   4.016 5.93e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 5493.9  on 3989  degrees of freedom
    ## Residual deviance: 5072.0  on 3980  degrees of freedom
    ## AIC: 5092
    ## 
    ## Number of Fisher Scoring iterations: 5

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
    ##          0 1009  596
    ##          1  793 1592
    ##                                           
    ##                Accuracy : 0.6519          
    ##                  95% CI : (0.6369, 0.6667)
    ##     No Information Rate : 0.5484          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.2903          
    ##                                           
    ##  Mcnemar's Test P-Value : 1.448e-07       
    ##                                           
    ##             Sensitivity : 0.5599          
    ##             Specificity : 0.7276          
    ##          Pos Pred Value : 0.6287          
    ##          Neg Pred Value : 0.6675          
    ##              Prevalence : 0.4516          
    ##          Detection Rate : 0.2529          
    ##    Detection Prevalence : 0.4023          
    ##       Balanced Accuracy : 0.6438          
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
    ## 3990 samples
    ##   23 predictor
    ##    2 classes: '0', '1' 
    ## 
    ## Pre-processing: centered (23), scaled (23) 
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 3990, 3990, 3990, 3990, 3990, 3990, ... 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa    
    ##   0.6243985  0.2353663

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
    ##          0 439 273
    ##          1 347 652
    ##                                           
    ##                Accuracy : 0.6376          
    ##                  95% CI : (0.6143, 0.6605)
    ##     No Information Rate : 0.5406          
    ##     P-Value [Acc > NIR] : 2.819e-16       
    ##                                           
    ##                   Kappa : 0.2653          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.003371        
    ##                                           
    ##             Sensitivity : 0.5585          
    ##             Specificity : 0.7049          
    ##          Pos Pred Value : 0.6166          
    ##          Neg Pred Value : 0.6527          
    ##              Prevalence : 0.4594          
    ##          Detection Rate : 0.2566          
    ##    Detection Prevalence : 0.4161          
    ##       Balanced Accuracy : 0.6317          
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
    ##          0 426 287
    ##          1 360 638
    ##                                           
    ##                Accuracy : 0.6219          
    ##                  95% CI : (0.5984, 0.6449)
    ##     No Information Rate : 0.5406          
    ##     P-Value [Acc > NIR] : 6.752e-12       
    ##                                           
    ##                   Kappa : 0.2333          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.004646        
    ##                                           
    ##             Sensitivity : 0.5420          
    ##             Specificity : 0.6897          
    ##          Pos Pred Value : 0.5975          
    ##          Neg Pred Value : 0.6393          
    ##              Prevalence : 0.4594          
    ##          Detection Rate : 0.2490          
    ##    Detection Prevalence : 0.4167          
    ##       Balanced Accuracy : 0.6159          
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
| Logistic Regression | 0.6519 | 0.6376 |
| Bagged Tree         | 0.6244 | 0.6219 |
| Original Data       | 0.5484 | 0.5406 |

Accuracy Table

``` r
kable(APERtable ,
      row.names=TRUE,
      col.names = c("Train","Test"),
      caption = "APER Table")
```

|                     |  Train |   Test |
| ------------------- | -----: | -----: |
| Logistic Regression | 0.3481 | 0.3624 |
| Bagged Tree         | 0.3756 | 0.3781 |
| Original Data       | 0.4516 | 0.4594 |

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

For FRIDAY data, I would choose the Logistic Regression because it fit
testing data sets better. And if I use this model, I would expect 36.24%
as its misclassification rate.
