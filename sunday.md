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

    ## [1] 1915   24

``` r
dim(newsDataTest)
```

    ## [1] 822  24

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

    ## [1] "num_hrefs"              "num_self_hrefs"         "num_keywords"          
    ## [4] "data_channel_is_bus"    "data_channel_is_socmed" "data_channel_is_tech"  
    ## [7] "kw_avg_avg"             "LDA_02"                 "min_positive_polarity"

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

<img src="sunday_files/figure-gfm/corrplot-1.png" width="200%" />

``` r
# get the name of variables who have positive or negative relationship with our response
p_idx <- which(cor(newsDataFit[,-1],as.numeric(newsDataFit$sharesInd)) > 0)
n_idx <- which(cor(newsDataFit[,-1],as.numeric(newsDataFit$sharesInd)) < 0)
# Positive relationships
colnames(newsDataFit)[p_idx+1]
```

    ## [1] "num_hrefs"              "num_self_hrefs"         "num_keywords"          
    ## [4] "data_channel_is_bus"    "data_channel_is_socmed" "data_channel_is_tech"  
    ## [7] "kw_avg_avg"

``` r
# Negative relationships
colnames(newsDataFit)[n_idx+1]
```

    ## [1] "LDA_02"                "min_positive_polarity"

There are some variables who might have positive linear relationship
with our response: num\_hrefs, num\_self\_hrefs, num\_imgs,
num\_keywords, n\_tokens\_content, n\_unique\_tokens,
data\_channel\_is\_entertainment.

And there are some variables who might have negative linear relationship
with our response: data\_channel\_is\_bus, data\_channel\_is\_socmed.

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
    ##  0: 582    Min.   :  0.00   Min.   : 0.000   Min.   :  0.000  
    ##  1:1333    1st Qu.:  5.00   1st Qu.: 1.000   1st Qu.:  1.000  
    ##            Median :  9.00   Median : 2.000   Median :  1.000  
    ##            Mean   : 12.52   Mean   : 3.556   Mean   :  5.897  
    ##            3rd Qu.: 16.00   3rd Qu.: 4.000   3rd Qu.:  9.000  
    ##            Max.   :153.00   Max.   :39.000   Max.   :111.000  
    ##   num_keywords    n_tokens_content n_unique_tokens 
    ##  Min.   : 1.000   Min.   :   0.0   Min.   :0.0000  
    ##  1st Qu.: 6.000   1st Qu.: 245.0   1st Qu.:0.4618  
    ##  Median : 8.000   Median : 454.0   Median :0.5275  
    ##  Mean   : 7.669   Mean   : 593.5   Mean   :0.5268  
    ##  3rd Qu.: 9.000   3rd Qu.: 794.5   3rd Qu.:0.6073  
    ##  Max.   :10.000   Max.   :8474.0   Max.   :0.9796  
    ##  data_channel_is_entertainment data_channel_is_bus data_channel_is_socmed
    ##  Min.   :0.0000                Min.   :0.0000      Min.   :0.00000       
    ##  1st Qu.:0.0000                1st Qu.:0.0000      1st Qu.:0.00000       
    ##  Median :0.0000                Median :0.0000      Median :0.00000       
    ##  Mean   :0.1906                Mean   :0.1243      Mean   :0.04648       
    ##  3rd Qu.:0.0000                3rd Qu.:0.0000      3rd Qu.:0.00000       
    ##  Max.   :1.0000                Max.   :1.0000      Max.   :1.00000       
    ##  data_channel_is_tech   kw_min_min       kw_max_max       kw_max_avg    
    ##  Min.   :0.0000       Min.   : -1.00   Min.   : 37400   Min.   :  2536  
    ##  1st Qu.:0.0000       1st Qu.: -1.00   1st Qu.:843300   1st Qu.:  3623  
    ##  Median :0.0000       Median : -1.00   Median :843300   Median :  4816  
    ##  Mean   :0.1457       Mean   : 29.59   Mean   :748983   Mean   :  6060  
    ##  3rd Qu.:0.0000       3rd Qu.:  4.00   3rd Qu.:843300   3rd Qu.:  6864  
    ##  Max.   :1.0000       Max.   :217.00   Max.   :843300   Max.   :120100  
    ##    kw_avg_avg          LDA_00            LDA_01            LDA_02       
    ##  Min.   :  743.5   Min.   :0.01824   Min.   :0.01820   Min.   :0.01818  
    ##  1st Qu.: 2497.0   1st Qu.:0.02500   1st Qu.:0.02500   1st Qu.:0.02500  
    ##  Median : 3040.8   Median :0.03333   Median :0.03334   Median :0.03334  
    ##  Mean   : 3306.3   Mean   :0.16610   Mean   :0.15677   Mean   :0.19628  
    ##  3rd Qu.: 3855.7   3rd Qu.:0.19850   3rd Qu.:0.17435   3rd Qu.:0.27968  
    ##  Max.   :15336.1   Max.   :0.92000   Max.   :0.92595   Max.   :0.92000  
    ##      LDA_03            LDA_04        title_sentiment_polarity
    ##  Min.   :0.01818   Min.   :0.01945   Min.   :-1.0000         
    ##  1st Qu.:0.02523   1st Qu.:0.02530   1st Qu.: 0.0000         
    ##  Median :0.05000   Median :0.04000   Median : 0.0000         
    ##  Mean   :0.26628   Mean   :0.21456   Mean   : 0.0888         
    ##  3rd Qu.:0.50891   3rd Qu.:0.32739   3rd Qu.: 0.2500         
    ##  Max.   :0.91997   Max.   :0.92644   Max.   : 1.0000         
    ##  global_subjectivity self_reference_avg_sharess min_positive_polarity
    ##  Min.   :0.0000      Min.   :     0.0           Min.   :0.0000       
    ##  1st Qu.:0.3974      1st Qu.:   984.5           1st Qu.:0.0500       
    ##  Median :0.4604      Median :  2150.8           Median :0.1000       
    ##  Mean   :0.4490      Mean   :  6125.8           Mean   :0.0988       
    ##  3rd Qu.:0.5148      3rd Qu.:  5000.0           3rd Qu.:0.1000       
    ##  Max.   :0.9125      Max.   :843300.0           Max.   :1.0000

## Oringinal Accuracy

According to the contingency table, we calculate the accuracy for each
data set by treating all the shares as in the “\>= 1400” group.

``` r
# accuracy for training data set if we treat all the shares as in the >= 1400 group
orgAccuracyTrain <- round(table(newsDataTrain$sharesInd)[2]/nrow(newsDataTrain),4)
orgAccuracyTrain
```

    ##      1 
    ## 0.6961

``` r
# accuracy for testing data set if we treat all the shares as in the >= 1400 group
orgAccuracyTest <- round(table(newsDataTest$sharesInd)[2]/nrow(newsDataTest),4)
orgAccuracyTest
```

    ##      1 
    ## 0.6667

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
    ## -2.9183  -1.1341   0.6281   0.8646   1.5672  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             0.94491    0.05528  17.095  < 2e-16 ***
    ## num_hrefs               0.33998    0.07643   4.448 8.66e-06 ***
    ## num_self_hrefs         -0.24911    0.05929  -4.201 2.65e-05 ***
    ## num_keywords            0.17570    0.05467   3.214  0.00131 ** 
    ## data_channel_is_bus     0.18502    0.05998   3.085  0.00204 ** 
    ## data_channel_is_socmed  0.32932    0.07309   4.506 6.61e-06 ***
    ## data_channel_is_tech    0.41724    0.06415   6.504 7.82e-11 ***
    ## kw_avg_avg              0.37473    0.07099   5.279 1.30e-07 ***
    ## LDA_02                 -0.16572    0.05680  -2.918  0.00352 ** 
    ## min_positive_polarity  -0.16418    0.05229  -3.140  0.00169 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2352.2  on 1914  degrees of freedom
    ## Residual deviance: 2156.0  on 1905  degrees of freedom
    ## AIC: 2176
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
    ##          0  131   96
    ##          1  451 1237
    ##                                           
    ##                Accuracy : 0.7144          
    ##                  95% CI : (0.6935, 0.7345)
    ##     No Information Rate : 0.6961          
    ##     P-Value [Acc > NIR] : 0.04266         
    ##                                           
    ##                   Kappa : 0.1848          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2e-16         
    ##                                           
    ##             Sensitivity : 0.22509         
    ##             Specificity : 0.92798         
    ##          Pos Pred Value : 0.57709         
    ##          Neg Pred Value : 0.73282         
    ##              Prevalence : 0.30392         
    ##          Detection Rate : 0.06841         
    ##    Detection Prevalence : 0.11854         
    ##       Balanced Accuracy : 0.57653         
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
    ## 1915 samples
    ##   23 predictor
    ##    2 classes: '0', '1' 
    ## 
    ## Pre-processing: centered (23), scaled (23) 
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 1915, 1915, 1915, 1915, 1915, 1915, ... 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa    
    ##   0.6858888  0.1551348

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
    ##          0  63  44
    ##          1 211 504
    ##                                           
    ##                Accuracy : 0.6898          
    ##                  95% CI : (0.6569, 0.7213)
    ##     No Information Rate : 0.6667          
    ##     P-Value [Acc > NIR] : 0.08494         
    ##                                           
    ##                   Kappa : 0.1765          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2e-16         
    ##                                           
    ##             Sensitivity : 0.22993         
    ##             Specificity : 0.91971         
    ##          Pos Pred Value : 0.58879         
    ##          Neg Pred Value : 0.70490         
    ##              Prevalence : 0.33333         
    ##          Detection Rate : 0.07664         
    ##    Detection Prevalence : 0.13017         
    ##       Balanced Accuracy : 0.57482         
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
    ##          0  72  54
    ##          1 202 494
    ##                                           
    ##                Accuracy : 0.6886          
    ##                  95% CI : (0.6557, 0.7201)
    ##     No Information Rate : 0.6667          
    ##     P-Value [Acc > NIR] : 0.09718         
    ##                                           
    ##                   Kappa : 0.1899          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2e-16         
    ##                                           
    ##             Sensitivity : 0.26277         
    ##             Specificity : 0.90146         
    ##          Pos Pred Value : 0.57143         
    ##          Neg Pred Value : 0.70977         
    ##              Prevalence : 0.33333         
    ##          Detection Rate : 0.08759         
    ##    Detection Prevalence : 0.15328         
    ##       Balanced Accuracy : 0.58212         
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
| Logistic Regression | 0.7144 | 0.6898 |
| Bagged Tree         | 0.6859 | 0.6886 |
| Original Data       | 0.6961 | 0.6667 |

Accuracy Table

``` r
kable(APERtable ,
      row.names=TRUE,
      col.names = c("Train","Test"),
      caption = "APER Table")
```

|                     |  Train |   Test |
| ------------------- | -----: | -----: |
| Logistic Regression | 0.2856 | 0.3102 |
| Bagged Tree         | 0.3141 | 0.3114 |
| Original Data       | 0.3039 | 0.3333 |

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

For SUNDAY data, I would choose the Logistic Regression because it fit
testing data sets better. And if I use this model, I would expect 31.02%
as its misclassification rate.
