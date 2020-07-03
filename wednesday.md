Project2
================
Zhijun Liu
2020-07-03

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

    ## [1] 5204   24

``` r
dim(newsDataTest)
```

    ## [1] 2231   24

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

    ## [1] "num_keywords"                  "n_tokens_content"             
    ## [3] "data_channel_is_entertainment" "data_channel_is_socmed"       
    ## [5] "data_channel_is_tech"          "kw_min_min"                   
    ## [7] "kw_max_avg"                    "kw_avg_avg"                   
    ## [9] "LDA_00"

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

<img src="wednesday_files/figure-gfm/corrplot-1.png" width="200%" />

``` r
# get the name of variables who have positive or negative relationship with our response
p_idx <- which(cor(newsDataFit[,-1],as.numeric(newsDataFit$sharesInd)) > 0)
n_idx <- which(cor(newsDataFit[,-1],as.numeric(newsDataFit$sharesInd)) < 0)
# Positive relationships
colnames(newsDataFit)[p_idx+1]
```

    ## [1] "num_keywords"           "n_tokens_content"       "data_channel_is_socmed"
    ## [4] "data_channel_is_tech"   "kw_min_min"             "kw_max_avg"            
    ## [7] "kw_avg_avg"             "LDA_00"

``` r
# Negative relationships
colnames(newsDataFit)[n_idx+1]
```

    ## [1] "data_channel_is_entertainment"

There are some variables who might have positive linear relationship
with our response: num\_hrefs, num\_self\_hrefs, num\_keywords,
n\_tokens\_content, n\_unique\_tokens, data\_channel\_is\_entertainment,
data\_channel\_is\_bus, data\_channel\_is\_socmed.

And there are some variables who might have negative linear relationship
with our response: num\_imgs.

## Summary Statistics

From the summary results of our training data set, we know that the
exact number of each shares group. Furthermore, we should standardize
our variables before fitting the model due to the extreme differences in
the maximum of each variable.

``` r
# get the summary statistics of our training data set
summary(newsDataTrain)
```

    ##  sharesInd   num_hrefs      num_self_hrefs      num_imgs       num_keywords   
    ##  0:2692    Min.   :  0.00   Min.   : 0.000   Min.   : 0.000   Min.   : 1.000  
    ##  1:2512    1st Qu.:  4.00   1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 6.000  
    ##            Median :  7.00   Median : 2.000   Median : 1.000   Median : 7.000  
    ##            Mean   : 10.18   Mean   : 3.113   Mean   : 4.074   Mean   : 7.131  
    ##            3rd Qu.: 13.00   3rd Qu.: 4.000   3rd Qu.: 3.000   3rd Qu.: 9.000  
    ##            Max.   :150.00   Max.   :41.000   Max.   :92.000   Max.   :10.000  
    ##  n_tokens_content n_unique_tokens  data_channel_is_entertainment
    ##  Min.   :   0.0   Min.   :0.0000   Min.   :0.0000               
    ##  1st Qu.: 243.0   1st Qu.:0.4707   1st Qu.:0.0000               
    ##  Median : 400.0   Median :0.5415   Median :0.0000               
    ##  Mean   : 528.1   Mean   :0.5322   Mean   :0.1733               
    ##  3rd Qu.: 696.0   3rd Qu.:0.6126   3rd Qu.:0.0000               
    ##  Max.   :4747.0   Max.   :0.9375   Max.   :1.0000               
    ##  data_channel_is_bus data_channel_is_socmed data_channel_is_tech
    ##  Min.   :0.0000      Min.   :0.00000        Min.   :0.0000      
    ##  1st Qu.:0.0000      1st Qu.:0.00000        1st Qu.:0.0000      
    ##  Median :0.0000      Median :0.00000        Median :0.0000      
    ##  Mean   :0.1693      Mean   :0.05496        Mean   :0.1845      
    ##  3rd Qu.:0.0000      3rd Qu.:0.00000        3rd Qu.:0.0000      
    ##  Max.   :1.0000      Max.   :1.00000        Max.   :1.0000      
    ##    kw_min_min       kw_max_max       kw_max_avg       kw_avg_avg     
    ##  Min.   : -1.00   Min.   : 17100   Min.   :  1953   Min.   :  424.3  
    ##  1st Qu.: -1.00   1st Qu.:690400   1st Qu.:  3521   1st Qu.: 2356.2  
    ##  Median : -1.00   Median :843300   Median :  4257   Median : 2832.3  
    ##  Mean   : 27.51   Mean   :746551   Mean   :  5613   Mean   : 3109.5  
    ##  3rd Qu.:  4.00   3rd Qu.:843300   3rd Qu.:  5937   3rd Qu.: 3534.7  
    ##  Max.   :294.00   Max.   :843300   Max.   :112787   Max.   :21000.7  
    ##      LDA_00            LDA_01            LDA_02            LDA_03       
    ##  Min.   :0.01828   Min.   :0.01819   Min.   :0.01819   Min.   :0.01820  
    ##  1st Qu.:0.02512   1st Qu.:0.02504   1st Qu.:0.02857   1st Qu.:0.02857  
    ##  Median :0.03348   Median :0.03335   Median :0.04003   Median :0.04000  
    ##  Mean   :0.18844   Mean   :0.13753   Mean   :0.22033   Mean   :0.21979  
    ##  3rd Qu.:0.25223   3rd Qu.:0.14958   3rd Qu.:0.34746   3rd Qu.:0.36517  
    ##  Max.   :0.92000   Max.   :0.91998   Max.   :0.92000   Max.   :0.91998  
    ##      LDA_04        title_sentiment_polarity global_subjectivity
    ##  Min.   :0.01818   Min.   :-1.0000          Min.   :0.0000     
    ##  1st Qu.:0.02858   1st Qu.: 0.0000          1st Qu.:0.3945     
    ##  Median :0.05000   Median : 0.0000          Median :0.4528     
    ##  Mean   :0.23392   Mean   : 0.0656          Mean   :0.4413     
    ##  3rd Qu.:0.39822   3rd Qu.: 0.1364          3rd Qu.:0.5047     
    ##  Max.   :0.92712   Max.   : 1.0000          Max.   :1.0000     
    ##  self_reference_avg_sharess min_positive_polarity
    ##  Min.   :     0.0           Min.   :0.00000      
    ##  1st Qu.:   922.8           1st Qu.:0.05000      
    ##  Median :  2150.0           Median :0.10000      
    ##  Mean   :  6407.1           Mean   :0.09571      
    ##  3rd Qu.:  5100.0           3rd Qu.:0.10000      
    ##  Max.   :690400.0           Max.   :1.00000

## Oringinal Accuracy

According to the contingency table, we calculate the accuracy for each
data set by treating all the shares as in the “\>= 1400” group.

``` r
# accuracy for training data set if we treat all the shares as in the >= 1400 group
orgAccuracyTrain <- round(table(newsDataTrain$sharesInd)[2]/nrow(newsDataTrain),4)
orgAccuracyTrain
```

    ##      1 
    ## 0.4827

``` r
# accuracy for testing data set if we treat all the shares as in the >= 1400 group
orgAccuracyTest <- round(table(newsDataTest$sharesInd)[2]/nrow(newsDataTest),4)
orgAccuracyTest
```

    ##      1 
    ## 0.4971

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
    ## -2.4064  -1.0314  -0.6836   1.0923   1.8880  
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                   -0.06638    0.02944  -2.255   0.0242 *  
    ## num_keywords                   0.18520    0.03088   5.997 2.01e-09 ***
    ## n_tokens_content               0.21272    0.03090   6.885 5.79e-12 ***
    ## data_channel_is_entertainment -0.15168    0.03218  -4.713 2.44e-06 ***
    ## data_channel_is_socmed         0.25448    0.03317   7.671 1.71e-14 ***
    ## data_channel_is_tech           0.26839    0.03155   8.506  < 2e-16 ***
    ## kw_min_min                     0.30525    0.03100   9.847  < 2e-16 ***
    ## kw_max_avg                    -0.53982    0.05567  -9.697  < 2e-16 ***
    ## kw_avg_avg                     0.92000    0.05654  16.272  < 2e-16 ***
    ## LDA_00                         0.15487    0.03206   4.831 1.36e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 7208.0  on 5203  degrees of freedom
    ## Residual deviance: 6616.6  on 5194  degrees of freedom
    ## AIC: 6636.6
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
    ##          0 1864 1014
    ##          1  828 1498
    ##                                          
    ##                Accuracy : 0.646          
    ##                  95% CI : (0.6329, 0.659)
    ##     No Information Rate : 0.5173         
    ##     P-Value [Acc > NIR] : < 2.2e-16      
    ##                                          
    ##                   Kappa : 0.2895         
    ##                                          
    ##  Mcnemar's Test P-Value : 1.629e-05      
    ##                                          
    ##             Sensitivity : 0.6924         
    ##             Specificity : 0.5963         
    ##          Pos Pred Value : 0.6477         
    ##          Neg Pred Value : 0.6440         
    ##              Prevalence : 0.5173         
    ##          Detection Rate : 0.3582         
    ##    Detection Prevalence : 0.5530         
    ##       Balanced Accuracy : 0.6444         
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
    ## 5204 samples
    ##   23 predictor
    ##    2 classes: '0', '1' 
    ## 
    ## Pre-processing: centered (23), scaled (23) 
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 5204, 5204, 5204, 5204, 5204, 5204, ... 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa    
    ##   0.6165061  0.2319115

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
    ##          0 769 463
    ##          1 353 646
    ##                                           
    ##                Accuracy : 0.6342          
    ##                  95% CI : (0.6139, 0.6543)
    ##     No Information Rate : 0.5029          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.268           
    ##                                           
    ##  Mcnemar's Test P-Value : 0.0001358       
    ##                                           
    ##             Sensitivity : 0.6854          
    ##             Specificity : 0.5825          
    ##          Pos Pred Value : 0.6242          
    ##          Neg Pred Value : 0.6466          
    ##              Prevalence : 0.5029          
    ##          Detection Rate : 0.3447          
    ##    Detection Prevalence : 0.5522          
    ##       Balanced Accuracy : 0.6339          
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
    ##          0 714 428
    ##          1 408 681
    ##                                           
    ##                Accuracy : 0.6253          
    ##                  95% CI : (0.6048, 0.6454)
    ##     No Information Rate : 0.5029          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.2505          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.5111          
    ##                                           
    ##             Sensitivity : 0.6364          
    ##             Specificity : 0.6141          
    ##          Pos Pred Value : 0.6252          
    ##          Neg Pred Value : 0.6253          
    ##              Prevalence : 0.5029          
    ##          Detection Rate : 0.3200          
    ##    Detection Prevalence : 0.5119          
    ##       Balanced Accuracy : 0.6252          
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
| Logistic Regression | 0.6460 | 0.6342 |
| Bagged Tree         | 0.6165 | 0.6253 |
| Original Data       | 0.4827 | 0.4971 |

Accuracy Table

``` r
kable(APERtable ,
      row.names=TRUE,
      col.names = c("Train","Test"),
      caption = "APER Table")
```

|                     |  Train |   Test |
| ------------------- | -----: | -----: |
| Logistic Regression | 0.3540 | 0.3658 |
| Bagged Tree         | 0.3835 | 0.3747 |
| Original Data       | 0.5173 | 0.5029 |

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

For WEDNESDAY data, I would choose the Logistic Regression because it
fit testing data sets better. And if I use this model, I would expect
36.58% as its misclassification rate.
