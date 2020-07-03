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

    ## [1] 1717   24

``` r
dim(newsDataTest)
```

    ## [1] 736  24

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

    ## [1] "num_keywords"         "data_channel_is_tech" "kw_max_max"          
    ## [4] "kw_max_avg"           "kw_avg_avg"           "LDA_00"

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

<img src="saturday_files/figure-gfm/corrplot-1.png" width="200%" />

``` r
# get the name of variables who have positive or negative relationship with our response
p_idx <- which(cor(newsDataFit[,-1],as.numeric(newsDataFit$sharesInd)) > 0)
n_idx <- which(cor(newsDataFit[,-1],as.numeric(newsDataFit$sharesInd)) < 0)
# Positive relationships
colnames(newsDataFit)[p_idx+1]
```

    ## [1] "num_keywords"         "data_channel_is_tech" "kw_max_avg"          
    ## [4] "kw_avg_avg"           "LDA_00"

``` r
# Negative relationships
colnames(newsDataFit)[n_idx+1]
```

    ## [1] "kw_max_max"

There are some variables who might have positive linear relationship
with our response: num\_hrefs, num\_self\_hrefs, num\_keywords,
n\_tokens\_content, n\_unique\_tokens.

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

    ##  sharesInd   num_hrefs      num_self_hrefs      num_imgs     
    ##  0: 428    Min.   :  0.00   Min.   : 0.000   Min.   : 0.000  
    ##  1:1289    1st Qu.:  5.00   1st Qu.: 1.000   1st Qu.: 1.000  
    ##            Median : 10.00   Median : 3.000   Median : 1.000  
    ##            Mean   : 13.03   Mean   : 3.917   Mean   : 5.411  
    ##            3rd Qu.: 17.00   3rd Qu.: 4.000   3rd Qu.: 8.000  
    ##            Max.   :105.00   Max.   :74.000   Max.   :99.000  
    ##   num_keywords    n_tokens_content n_unique_tokens 
    ##  Min.   : 1.000   Min.   :   0.0   Min.   :0.0000  
    ##  1st Qu.: 6.000   1st Qu.: 277.0   1st Qu.:0.4595  
    ##  Median : 8.000   Median : 496.0   Median :0.5199  
    ##  Mean   : 7.547   Mean   : 596.8   Mean   :0.5112  
    ##  3rd Qu.: 9.000   3rd Qu.: 781.0   3rd Qu.:0.5927  
    ##  Max.   :10.000   Max.   :4046.0   Max.   :0.9574  
    ##  data_channel_is_entertainment data_channel_is_bus data_channel_is_socmed
    ##  Min.   :0.0000                Min.   :0.0000      Min.   :0.00000       
    ##  1st Qu.:0.0000                1st Qu.:0.0000      1st Qu.:0.00000       
    ##  Median :0.0000                Median :0.0000      Median :0.00000       
    ##  Mean   :0.1555                Mean   :0.1043      Mean   :0.06639       
    ##  3rd Qu.:0.0000                3rd Qu.:0.0000      3rd Qu.:0.00000       
    ##  Max.   :1.0000                Max.   :1.0000      Max.   :1.00000       
    ##  data_channel_is_tech   kw_min_min       kw_max_max       kw_max_avg    
    ##  Min.   :0.0000       Min.   : -1.00   Min.   : 37400   Min.   :  2414  
    ##  1st Qu.:0.0000       1st Qu.: -1.00   1st Qu.:843300   1st Qu.:  3578  
    ##  Median :0.0000       Median : -1.00   Median :843300   Median :  4662  
    ##  Mean   :0.2196       Mean   : 23.59   Mean   :761791   Mean   :  5982  
    ##  3rd Qu.:0.0000       3rd Qu.:  4.00   3rd Qu.:843300   3rd Qu.:  6620  
    ##  Max.   :1.0000       Max.   :217.00   Max.   :843300   Max.   :237967  
    ##    kw_avg_avg        LDA_00            LDA_01            LDA_02       
    ##  Min.   : 1115   Min.   :0.02000   Min.   :0.01819   Min.   :0.01832  
    ##  1st Qu.: 2505   1st Qu.:0.02500   1st Qu.:0.02352   1st Qu.:0.02500  
    ##  Median : 3045   Median :0.03333   Median :0.03333   Median :0.04000  
    ##  Mean   : 3300   Mean   :0.16589   Mean   :0.13438   Mean   :0.21386  
    ##  3rd Qu.: 3839   3rd Qu.:0.19810   3rd Qu.:0.13412   3rd Qu.:0.33416  
    ##  Max.   :36717   Max.   :0.91998   Max.   :0.91996   Max.   :0.92000  
    ##      LDA_03            LDA_04        title_sentiment_polarity
    ##  Min.   :0.01821   Min.   :0.02000   Min.   :-1.00000        
    ##  1st Qu.:0.02502   1st Qu.:0.02857   1st Qu.: 0.00000        
    ##  Median :0.04000   Median :0.05000   Median : 0.00000        
    ##  Mean   :0.22585   Mean   :0.26003   Mean   : 0.09693        
    ##  3rd Qu.:0.38332   3rd Qu.:0.45662   3rd Qu.: 0.22500        
    ##  Max.   :0.91997   Max.   :0.91999   Max.   : 1.00000        
    ##  global_subjectivity self_reference_avg_sharess min_positive_polarity
    ##  Min.   :0.0000      Min.   :     0             Min.   :0.00000      
    ##  1st Qu.:0.4063      1st Qu.:  1000             1st Qu.:0.05000      
    ##  Median :0.4630      Median :  2350             Median :0.10000      
    ##  Mean   :0.4491      Mean   :  6087             Mean   :0.08906      
    ##  3rd Qu.:0.5179      3rd Qu.:  5200             3rd Qu.:0.10000      
    ##  Max.   :0.8125      Max.   :663600             Max.   :1.00000

## Oringinal Accuracy

According to the contingency table, we calculate the accuracy for each
data set by treating all the shares as in the “\>= 1400” group.

``` r
# accuracy for training data set if we treat all the shares as in the >= 1400 group
orgAccuracyTrain <- round(table(newsDataTrain$sharesInd)[2]/nrow(newsDataTrain),4)
orgAccuracyTrain
```

    ##      1 
    ## 0.7507

``` r
# accuracy for testing data set if we treat all the shares as in the >= 1400 group
orgAccuracyTest <- round(table(newsDataTest$sharesInd)[2]/nrow(newsDataTest),4)
orgAccuracyTest
```

    ##      1 
    ## 0.7323

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
    ## -3.7945   0.1623   0.5775   0.7810   1.3247  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.27267    0.06505  19.563  < 2e-16 ***
    ## num_keywords          0.24282    0.05932   4.093 4.25e-05 ***
    ## data_channel_is_tech  0.33556    0.06597   5.087 3.65e-07 ***
    ## kw_max_max           -0.42311    0.08366  -5.058 4.24e-07 ***
    ## kw_max_avg           -0.53779    0.12252  -4.389 1.14e-05 ***
    ## kw_avg_avg            0.75334    0.12955   5.815 6.05e-09 ***
    ## LDA_00                0.58862    0.08271   7.117 1.10e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1928.3  on 1716  degrees of freedom
    ## Residual deviance: 1757.8  on 1710  degrees of freedom
    ## AIC: 1771.8
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
    ##          0   35   25
    ##          1  393 1264
    ##                                           
    ##                Accuracy : 0.7566          
    ##                  95% CI : (0.7355, 0.7767)
    ##     No Information Rate : 0.7507          
    ##     P-Value [Acc > NIR] : 0.2992          
    ##                                           
    ##                   Kappa : 0.0875          
    ##                                           
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 0.08178         
    ##             Specificity : 0.98061         
    ##          Pos Pred Value : 0.58333         
    ##          Neg Pred Value : 0.76282         
    ##              Prevalence : 0.24927         
    ##          Detection Rate : 0.02038         
    ##    Detection Prevalence : 0.03494         
    ##       Balanced Accuracy : 0.53119         
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
    ## 1717 samples
    ##   23 predictor
    ##    2 classes: '0', '1' 
    ## 
    ## Pre-processing: centered (23), scaled (23) 
    ## Resampling: Bootstrapped (25 reps) 
    ## Summary of sample sizes: 1717, 1717, 1717, 1717, 1717, 1717, ... 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa    
    ##   0.7481498  0.1905186

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
    ##          0  20  17
    ##          1 177 522
    ##                                          
    ##                Accuracy : 0.7364         
    ##                  95% CI : (0.703, 0.7679)
    ##     No Information Rate : 0.7323         
    ##     P-Value [Acc > NIR] : 0.42           
    ##                                          
    ##                   Kappa : 0.0943         
    ##                                          
    ##  Mcnemar's Test P-Value : <2e-16         
    ##                                          
    ##             Sensitivity : 0.10152        
    ##             Specificity : 0.96846        
    ##          Pos Pred Value : 0.54054        
    ##          Neg Pred Value : 0.74678        
    ##              Prevalence : 0.26766        
    ##          Detection Rate : 0.02717        
    ##    Detection Prevalence : 0.05027        
    ##       Balanced Accuracy : 0.53499        
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
    ##          0  61  42
    ##          1 136 497
    ##                                           
    ##                Accuracy : 0.7582          
    ##                  95% CI : (0.7255, 0.7887)
    ##     No Information Rate : 0.7323          
    ##     P-Value [Acc > NIR] : 0.06061         
    ##                                           
    ##                   Kappa : 0.2731          
    ##                                           
    ##  Mcnemar's Test P-Value : 3.155e-12       
    ##                                           
    ##             Sensitivity : 0.30964         
    ##             Specificity : 0.92208         
    ##          Pos Pred Value : 0.59223         
    ##          Neg Pred Value : 0.78515         
    ##              Prevalence : 0.26766         
    ##          Detection Rate : 0.08288         
    ##    Detection Prevalence : 0.13995         
    ##       Balanced Accuracy : 0.61586         
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
| Logistic Regression | 0.7566 | 0.7364 |
| Bagged Tree         | 0.7481 | 0.7582 |
| Original Data       | 0.7507 | 0.7323 |

Accuracy Table

``` r
kable(APERtable ,
      row.names=TRUE,
      col.names = c("Train","Test"),
      caption = "APER Table")
```

|                     |  Train |   Test |
| ------------------- | -----: | -----: |
| Logistic Regression | 0.2434 | 0.2636 |
| Bagged Tree         | 0.2519 | 0.2418 |
| Original Data       | 0.2493 | 0.2677 |

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

For SATURDAY data, I would choose the Bagged Tree because it fit testing
data sets better. And if I use this model, I would expect 24.18% as its
misclassification rate.
