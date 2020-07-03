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
# read the raw data
newsData <- read_csv("data/OnlineNewsPopularity.csv")%>%
  # for specific weekday
  filter(weekday_is_monday==1)%>%
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

    ## [1] 4662   24

``` r
dim(newsDataTest)
```

    ## [1] 1999   24

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

    ## [1] "num_keywords"                  "data_channel_is_entertainment"
    ## [3] "data_channel_is_bus"           "data_channel_is_socmed"       
    ## [5] "kw_max_max"                    "kw_max_avg"                   
    ## [7] "kw_avg_avg"                    "LDA_00"                       
    ## [9] "LDA_04"

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
corrplot(cor(cor.data), lower.col = "steelblue",type="upper",tl.cex = 0.7,title="correlation plot")
```

<img src="README_files/figure-gfm/corrplot-1.png" width="200%" />

``` r
# get the name of variables who have positive or negative relationship with our response
p_idx <- which(cor(newsDataFit[,-1],as.numeric(newsDataFit$sharesInd)) > 0)
n_idx <- which(cor(newsDataFit[,-1],as.numeric(newsDataFit$sharesInd)) < 0)
# Positive relationships
colnames(newsDataFit)[p_idx+1]
```

    ## [1] "num_keywords"           "data_channel_is_bus"    "data_channel_is_socmed"
    ## [4] "kw_max_avg"             "kw_avg_avg"             "LDA_00"                
    ## [7] "LDA_04"

``` r
# Negative relationships
colnames(newsDataFit)[n_idx+1]
```

    ## [1] "data_channel_is_entertainment" "kw_max_max"

There are some variables who might have positive linear relationship
with our response: num\_hrefs, num\_imgs, num\_keywords,
n\_unique\_tokens, data\_channel\_is\_entertainment,
data\_channel\_is\_bus, data\_channel\_is\_socmed.

And there are some variables who might have negative linear relationship
with our response: num\_self\_hrefs, n\_tokens\_content.

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
    ##  0:2297    Min.   :  0.00   Min.   : 0.000   Min.   : 0.000   Min.   : 1.000  
    ##  1:2365    1st Qu.:  4.00   1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 6.000  
    ##            Median :  7.00   Median : 3.000   Median : 1.000   Median : 7.000  
    ##            Mean   : 10.62   Mean   : 3.367   Mean   : 4.382   Mean   : 7.153  
    ##            3rd Qu.: 13.00   3rd Qu.: 4.000   3rd Qu.: 3.000   3rd Qu.: 9.000  
    ##            Max.   :162.00   Max.   :51.000   Max.   :93.000   Max.   :10.000  
    ##  n_tokens_content n_unique_tokens  data_channel_is_entertainment
    ##  Min.   :   0.0   Min.   :0.0000   Min.   :0.0000               
    ##  1st Qu.: 248.0   1st Qu.:0.4738   1st Qu.:0.0000               
    ##  Median : 397.5   Median :0.5427   Median :0.0000               
    ##  Mean   : 538.2   Mean   :0.5308   Mean   :0.2059               
    ##  3rd Qu.: 711.0   3rd Qu.:0.6088   3rd Qu.:0.0000               
    ##  Max.   :7764.0   Max.   :1.0000   Max.   :1.0000               
    ##  data_channel_is_bus data_channel_is_socmed data_channel_is_tech
    ##  Min.   :0.0000      Min.   :0.00000        Min.   :0.0000      
    ##  1st Qu.:0.0000      1st Qu.:0.00000        1st Qu.:0.0000      
    ##  Median :0.0000      Median :0.00000        Median :0.0000      
    ##  Mean   :0.1695      Mean   :0.05277        Mean   :0.1836      
    ##  3rd Qu.:0.0000      3rd Qu.:0.00000        3rd Qu.:0.0000      
    ##  Max.   :1.0000      Max.   :1.00000        Max.   :1.0000      
    ##    kw_min_min       kw_max_max       kw_max_avg       kw_avg_avg   
    ##  Min.   : -1.00   Min.   :     0   Min.   :     0   Min.   :    0  
    ##  1st Qu.: -1.00   1st Qu.:843300   1st Qu.:  3531   1st Qu.: 2355  
    ##  Median : -1.00   Median :843300   Median :  4255   Median : 2832  
    ##  Mean   : 26.82   Mean   :748229   Mean   :  5582   Mean   : 3074  
    ##  3rd Qu.:  4.00   3rd Qu.:843300   3rd Qu.:  5938   3rd Qu.: 3535  
    ##  Max.   :318.00   Max.   :843300   Max.   :298400   Max.   :33536  
    ##      LDA_00            LDA_01            LDA_02            LDA_03       
    ##  Min.   :0.01818   Min.   :0.01819   Min.   :0.01819   Min.   :0.01819  
    ##  1st Qu.:0.02517   1st Qu.:0.02504   1st Qu.:0.02857   1st Qu.:0.02857  
    ##  Median :0.03341   Median :0.03337   Median :0.04000   Median :0.04000  
    ##  Mean   :0.18670   Mean   :0.15456   Mean   :0.21064   Mean   :0.21781  
    ##  3rd Qu.:0.24603   3rd Qu.:0.17145   3rd Qu.:0.32402   3rd Qu.:0.35340  
    ##  Max.   :0.91999   Max.   :0.91997   Max.   :0.92000   Max.   :0.91998  
    ##      LDA_04        title_sentiment_polarity global_subjectivity
    ##  Min.   :0.01818   Min.   :-1.00000         Min.   :0.0000     
    ##  1st Qu.:0.02857   1st Qu.: 0.00000         1st Qu.:0.3951     
    ##  Median :0.04001   Median : 0.00000         Median :0.4512     
    ##  Mean   :0.23029   Mean   : 0.06694         Mean   :0.4402     
    ##  3rd Qu.:0.39356   3rd Qu.: 0.13636         3rd Qu.:0.5047     
    ##  Max.   :0.92708   Max.   : 1.00000         Max.   :1.0000     
    ##  self_reference_avg_sharess min_positive_polarity
    ##  Min.   :     0             Min.   :0.00000      
    ##  1st Qu.:  1000             1st Qu.:0.05000      
    ##  Median :  2168             Median :0.10000      
    ##  Mean   :  6321             Mean   :0.09543      
    ##  3rd Qu.:  5200             3rd Qu.:0.10000      
    ##  Max.   :690400             Max.   :1.00000

## Oringinal Accuracy

According to the contingency table, we calculate the accuracy for each
data set by treating all the shares as in the “\>= 1400” group.

``` r
# accuracy for training data set if we treat all the shares as in the >= 1400 group
orgAccuracyTrain <- round(table(newsDataTrain$sharesInd)[2]/nrow(newsDataTrain),4)
orgAccuracyTrain
```

    ##      1 
    ## 0.5073

``` r
# accuracy for testing data set if we treat all the shares as in the >= 1400 group
orgAccuracyTest <- round(table(newsDataTest$sharesInd)[2]/nrow(newsDataTest),4)
orgAccuracyTest
```

    ##      1 
    ## 0.5133

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
    ## -3.0508  -1.0631   0.4736   1.0756   1.9327  
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                    0.04486    0.03105   1.445 0.148509    
    ## num_keywords                   0.15731    0.03264   4.820 1.43e-06 ***
    ## data_channel_is_entertainment -0.14625    0.03502  -4.176 2.97e-05 ***
    ## data_channel_is_bus           -0.21051    0.05745  -3.664 0.000248 ***
    ## data_channel_is_socmed         0.20494    0.03893   5.265 1.40e-07 ***
    ## kw_max_max                    -0.18254    0.03294  -5.542 2.99e-08 ***
    ## kw_max_avg                    -0.52231    0.05855  -8.921  < 2e-16 ***
    ## kw_avg_avg                     0.87737    0.05928  14.800  < 2e-16 ***
    ## LDA_00                         0.39865    0.05908   6.748 1.50e-11 ***
    ## LDA_04                         0.27954    0.03545   7.886 3.13e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 6461.9  on 4661  degrees of freedom
    ## Residual deviance: 5963.8  on 4652  degrees of freedom
    ## AIC: 5983.8
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
    ##          0 1476  887
    ##          1  821 1478
    ##                                           
    ##                Accuracy : 0.6336          
    ##                  95% CI : (0.6196, 0.6475)
    ##     No Information Rate : 0.5073          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.2674          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.1158          
    ##                                           
    ##             Sensitivity : 0.6426          
    ##             Specificity : 0.6249          
    ##          Pos Pred Value : 0.6246          
    ##          Neg Pred Value : 0.6429          
    ##              Prevalence : 0.4927          
    ##          Detection Rate : 0.3166          
    ##    Detection Prevalence : 0.5069          
    ##       Balanced Accuracy : 0.6338          
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
    ## 4662 samples
    ##   23 predictor
    ##    2 classes: '0', '1' 
    ## 
    ## Pre-processing: centered (23), scaled (23) 
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 4662, 4662, 4662, 4662, 4662, 4662, ... 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa    
    ##   0.6137421  0.2277363

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
    ##          0 598 368
    ##          1 375 658
    ##                                           
    ##                Accuracy : 0.6283          
    ##                  95% CI : (0.6067, 0.6495)
    ##     No Information Rate : 0.5133          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.256           
    ##                                           
    ##  Mcnemar's Test P-Value : 0.8258          
    ##                                           
    ##             Sensitivity : 0.6146          
    ##             Specificity : 0.6413          
    ##          Pos Pred Value : 0.6190          
    ##          Neg Pred Value : 0.6370          
    ##              Prevalence : 0.4867          
    ##          Detection Rate : 0.2991          
    ##    Detection Prevalence : 0.4832          
    ##       Balanced Accuracy : 0.6280          
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
    ##          0 589 344
    ##          1 384 682
    ##                                          
    ##                Accuracy : 0.6358         
    ##                  95% CI : (0.6143, 0.657)
    ##     No Information Rate : 0.5133         
    ##     P-Value [Acc > NIR] : <2e-16         
    ##                                          
    ##                   Kappa : 0.2703         
    ##                                          
    ##  Mcnemar's Test P-Value : 0.1483         
    ##                                          
    ##             Sensitivity : 0.6053         
    ##             Specificity : 0.6647         
    ##          Pos Pred Value : 0.6313         
    ##          Neg Pred Value : 0.6398         
    ##              Prevalence : 0.4867         
    ##          Detection Rate : 0.2946         
    ##    Detection Prevalence : 0.4667         
    ##       Balanced Accuracy : 0.6350         
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
| Logistic Regression | 0.6336 | 0.6283 |
| Bagged Tree         | 0.6137 | 0.6358 |
| Original Data       | 0.5073 | 0.5133 |

Accuracy Table

``` r
kable(APERtable ,
      row.names=TRUE,
      col.names = c("Train","Test"),
      caption = "APER Table")
```

|                     |  Train |   Test |
| ------------------- | -----: | -----: |
| Logistic Regression | 0.3664 | 0.3717 |
| Bagged Tree         | 0.3863 | 0.3642 |
| Original Data       | 0.4927 | 0.4867 |

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

    ## [1] "Bagged Tree"

According to the results of the APER table, we would choose the smallest
value as the best model for each data set. In this case, our best model
for the training data set is Logistic Regression and the best model for
testing data set is Bagged Tree. However, if the best model turns out to
be the Original Data, neither the linear model nor the non-linear model
had done the job well.

# Conclusion

For *ne* data, I would choose the Bagged Tree because it fit testing
data sets better. And if I use this model, I would expect 36.42% as its
misclassification rate.
