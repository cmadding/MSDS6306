---
title: "CMadding_Livesession10assignment"
author: "Chad Madding"
date: "November 11, 2018"
output:
  html_document:
    keep_md: yes
  pdf_document: default
  word_document: default
---


#### MSDS 6306: Doing Data Science

#### Live session Unit 10/11 assignment

#### Monday November 12th at 11:59pm

## Submission

ALL MATERIAL MUST BE KNITTED INTO A SINGLE, LEGIBLE, AND DOCUMENTED HTML DOCUMENT. Use RMarkdown to create this file. Formatting can be basic, but it should be easily human-readable. Unless otherwise stated, please enable {r, echo=TRUE} so your code is visible. 

## Questions

**Background:** Brewmeisters in Colorado and Texas have teamed up to analyze the relationship between ABV and IBU in each of their states.  Use the data sets from the project to help them in their analysis.  There three main questions of interest are 1) Is there a significant linear relationship between ABV (response) and IBU (explanatory), 2) Is this relationship different between beers in Colorado and Texas and 3) Is there a significant quadratic component in this relationship for either Colorado or Texas or both?


```r
#R libraries used in the report
library(plyr)
library(dplyr)
library(caret)
library(ggplot2)
library(pastecs)
library(reshape2)
library(ggpubr)
library(kableExtra)
library(MLmetrics)
library(mnormt)
library(mlr)
library(FNN)
library(magrittr)
library(Metrics)
library(tidyr)
library(RTextTools)
library(jsonlite)
library(caTools)
require(quanteda)
require(RColorBrewer)
```

### I. KNN Regression versus Linear Regression
## A. Clean an prepare the data:

    1. Create column for brewery ID that is common to both datasets similar to what you did in the project. So we can merge!


```r
#function to trim white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
```


```r
#read in the two datasets
breweries <- read.csv("Data/Breweries.csv")
#States has a leading space, removing it
breweries$State <- trim(breweries$State)
beers <- read.csv("Data/Beers.csv")
```

    2. Merge the beer and brewery data into a single dataframe.


```r
#rename Brew_ID to Brewery_id to merge data
colnames(breweries)[1] <- "Brewery_id"
#both have a "Name" two for different groups
#rename Name to Brewery_Name
colnames(breweries)[2] <- "Brewery_Name"
#rename Name to Beer_Name
colnames(beers)[1] <- "Beer_Name"
#merge both data sets
brew_beer <- merge.data.frame(beers, breweries, by = "Brewery_id")
```

    3. Clean the State Column … get rid of extraneous white space.
    
**This is handled with the trim fubction above.**

    4. Create One Dataset that has only Colorado and Texas beers and no IBU NAs … name it “beerCOTX”


```r
#create beerCOTX with only TX and CO, removing NAs from IBU
beerCOTX <- filter(brew_beer,!is.na(IBU), State == "CO" | State == "TX")
```

    5. Order beerCOTX by IBU (ascending) ... this will be important later in graphing
    

```r
beerCOTX <- beerCOTX[order(beerCOTX$IBU),]
```

## C. Compare two competing models: External Cross Validation

    8. For this assignment we will concentrate only on the Texas data! Create a training and test set from the data (60%/40% split respectively). Print a summary of each new data frame… there should be two: TrainingTX, TestTX.
    

```r
#create the beerTX data set
beerTX <- filter(beerCOTX, State == "TX")
# Splitting the beerTX dataset into the Training set and Test set
# install.packages('caTools')
set.seed(7) # Set Seed so that same sample can be reproduced in the future
split = sample.split(beerTX$ABV, SplitRatio = .6)
TrainingTX = subset(beerTX, split == TRUE)
TestTX = subset(beerTX, split == FALSE)
#A summary of the TrainingTX data
summary(TrainingTX)
```

```
##    Brewery_id                    Beer_Name     Beer_ID    
##  Min.   : 30.0   1836                 : 1   Min.   :  44  
##  1st Qu.:126.0   18th Anniversary Gose: 1   1st Qu.:1300  
##  Median :129.0   Barn Burner Saison   : 1   Median :2040  
##  Mean   :162.3   Bat Outta Helles     : 1   Mean   :1762  
##  3rd Qu.:215.2   Battle LIne          : 1   3rd Qu.:2216  
##  Max.   :427.0   BLAKKR               : 1   Max.   :2588  
##                  (Other)              :50                 
##       ABV               IBU                                    Style   
##  Min.   :0.04000   Min.   :  5.00   American Blonde Ale           : 7  
##  1st Qu.:0.05075   1st Qu.: 21.00   American IPA                  : 6  
##  Median :0.05550   Median : 32.50   American Double / Imperial IPA: 5  
##  Mean   :0.06075   Mean   : 41.25   American Pale Ale (APA)       : 4  
##  3rd Qu.:0.06925   3rd Qu.: 50.50   Saison / Farmhouse Ale        : 4  
##  Max.   :0.09900   Max.   :118.00   American Amber / Red Ale      : 2  
##                                     (Other)                       :28  
##      Ounces                             Brewery_Name       City   
##  Min.   :12.00   Real Ale Brewing Company     : 7    Blanco  : 7  
##  1st Qu.:12.00   Southern Star Brewing Company: 7    Conroe  : 7  
##  Median :12.00   Karbach Brewing Company      : 5    Dallas  : 7  
##  Mean   :12.86   Deep Ellum Brewing Company   : 4    Houston : 7  
##  3rd Qu.:12.00   Texian Brewing Co.           : 4    Austin  : 6  
##  Max.   :16.00   Cedar Creek Brewery          : 3    Richmond: 4  
##                  (Other)                      :26    (Other) :18  
##     State          
##  Length:56         
##  Class :character  
##  Mode  :character  
##                    
##                    
##                    
## 
```

```r
#A summary of the TestTX data
summary(TestTX)
```

```
##    Brewery_id               Beer_Name     Beer_ID          ABV         
##  Min.   : 30.0   Alteration      : 1   Min.   :  45   Min.   :0.04200  
##  1st Qu.:119.0   Bombshell Blonde: 1   1st Qu.:1011   1st Qu.:0.05000  
##  Median :126.0   Chupahopra      : 1   Median :2042   Median :0.05500  
##  Mean   :155.2   Deep Ellum IPA  : 1   Mean   :1685   Mean   :0.05961  
##  3rd Qu.:185.0   Evil Owl        : 1   3rd Qu.:2212   3rd Qu.:0.06500  
##  Max.   :427.0   Gone A-Rye      : 1   Max.   :2600   Max.   :0.09900  
##                  (Other)         :27                                   
##       IBU                                    Style        Ounces     
##  Min.   :  5.00   American Pale Ale (APA)       : 5   Min.   :12.00  
##  1st Qu.: 20.00   American Double / Imperial IPA: 3   1st Qu.:12.00  
##  Median : 35.00   American IPA                  : 3   Median :12.00  
##  Mean   : 38.94   Witbier                       : 3   Mean   :12.73  
##  3rd Qu.: 45.00   American Amber / Red Ale      : 2   3rd Qu.:12.00  
##  Max.   :110.00   American Blonde Ale           : 2   Max.   :16.00  
##                   (Other)                       :15                  
##                         Brewery_Name               City  
##  Karbach Brewing Company      : 5    Austin          :7  
##  Southern Star Brewing Company: 5    Houston         :6  
##  Freetail Brewing Company     : 4    Conroe          :5  
##  Oasis Texas Brewing Company  : 3    San Antonio     :5  
##  Cedar Creek Brewery          : 2    Dripping Springs:2  
##  Hops & Grain Brewery         : 2    Fort Worth      :2  
##  (Other)                      :12    (Other)         :6  
##     State          
##  Length:33         
##  Class :character  
##  Mode  :character  
##                    
##                    
##                    
## 
```

    9. Using the training data, fit a KNN regression model to predict ABV from IBU. You should use the knnreg function in the caret package. Fit two separate models: one with k = 3 and one with k = 5. (This is 2 models total.)
    

```r
#fit a KNN regression model using knn.reg to predict ABV from IBU, k = 3
Train3TX.knn<- FNN::knn.reg(TrainingTX$IBU, y=TrainingTX$ABV, k=3)
plot(TrainingTX$IBU, Train3TX.knn$pred, xlab="IBU", ylab="Predicted ABV", main = "KNN K=3")
```

![](CMadding_Livesession10assignment_files/figure-html/fit a KNN k3-1.png)<!-- -->

```r
#print fit a KNN regression model to predict ABV from IBU, k = 3
Train3TX.knn
```

```
## PRESS =  0.006254556 
## R2-Predict =  0.4543463
```


```r
#fit a KNN regression model to predict ABV from IBU, k = 5
Train5TX.knn<- knn.reg(TrainingTX$IBU, y=TrainingTX$ABV, k=5)
plot(TrainingTX$IBU, Train5TX.knn$pred, xlab="IBU", ylab="Predicted ABV", main = "KNN K=5")
```

![](CMadding_Livesession10assignment_files/figure-html/fit a KNN k5-1.png)<!-- -->

```r
#Print fit a KNN regression model to predict ABV from IBU, k = 5
Train5TX.knn
```

```
## PRESS =  0.00576672 
## R2-Predict =  0.4969056
```

    10. Use the ASE loss function and external cross validation to provide evidence as to which model (k = 3 or k = 5) is more appropriate. Remember your answer should be supported with why you feel a certain model is appropriate. Your analysis should include the average squared error (ASE) for each model from the test set. Your analysis should also include a clear discussion, using the ASEs, as to which model you feel is more appropriate.
    

```r
# Fit the model on the training set and test out k at 3, 5 and 7
set.seed(123)
TXmodel <- caret::train(
  ABV ~ IBU, data = TrainingTX, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneGrid = expand.grid(k = c(3, 5, 7, 9)
  ))
# Plot model error RMSE vs different values of k
plot(TXmodel)
```

![](CMadding_Livesession10assignment_files/figure-html/ASE loss function and external cross validation-1.png)<!-- -->

```r
# Best tuning parameter k that minimize the RMSE
TXmodel$bestTune
```

```
##   k
## 3 7
```

```r
# Make predictions on the test data
predictions <- TXmodel %>% predict(TestTX)
head(predictions)
```

```
## [1] 0.05171429 0.05171429 0.05291667 0.05462500 0.05110000 0.05110000
```

```r
# Compute the prediction error RMSE
RMSE(predictions, TestTX$ABV)
```

```
## [1] 0.008617931
```

```r
#Lets add the predictions to the test data
TestTX$ABVPredistions <- predictions
head(TestTX)
```

```
##    Brewery_id                          Beer_Name Beer_ID   ABV IBU
## 1          67                 Yo Soy Un Berliner    2520 0.044   5
## 3          67                            Rye Wit    2522 0.042  10
## 7         126                Weisse Versa (2012)    2374 0.052  16
## 11         39                          Twisted X    2212 0.051  19
## 13        119                   Bombshell Blonde     856 0.050  20
## 15        126 Love Street Summer Seasonal (2014)    1235 0.047  20
##                     Style Ounces                  Brewery_Name
## 1      Berliner Weissbier     12      Freetail Brewing Company
## 3                 Witbier     12      Freetail Brewing Company
## 7              Hefeweizen     12       Karbach Brewing Company
## 11 American Adjunct Lager     12     Twisted X Brewing Company
## 13    American Blonde Ale     16 Southern Star Brewing Company
## 15                KÃ¶lsch     12       Karbach Brewing Company
##                City State ABVPredistions
## 1       San Antonio    TX     0.05171429
## 3       San Antonio    TX     0.05171429
## 7           Houston    TX     0.05291667
## 11 Dripping Springs    TX     0.05462500
## 13           Conroe    TX     0.05110000
## 15          Houston    TX     0.05110000
```

```r
TXmodel
```

```
## k-Nearest Neighbors 
## 
## 56 samples
##  1 predictor
## 
## Pre-processing: centered (1), scaled (1) 
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 50, 50, 49, 51, 51, 50, ... 
## Resampling results across tuning parameters:
## 
##   k  RMSE        Rsquared   MAE        
##   3  0.01064881  0.5316810  0.008590341
##   5  0.01057158  0.5494185  0.008281026
##   7  0.01028120  0.5530533  0.008005807
##   9  0.01031716  0.5539453  0.008036008
## 
## RMSE was used to select the optimal model using the smallest value.
## The final value used for the model was k = 7.
```
**I ran the test on a K of 3 and a K of 5. Out of those numbers the 5 scored slightly better. When I trained the TXmodel I wanted to see if there were any better values for K so I added 7 and 9 and plotted the results. Looking at the graph and the bestTune data we can now see that a K of 7 is the best number.
    
ASE=  (∑▒(y ̃_i-y_i )^2 )/n  

Here y ̃_i is the predicted ABV for the ith beer, y_iis the actual ABV of the ith beer and n is the sample size.

    11. Now use the ASE loss function and external cross validation to provide evidence as to which model (the linear regression model from last week or the “best” KNN regression model from this week (from question 10)) is more appropriate.
    
**Looking at the numbers from last week, the best I could do was an RMSE of 0.009727404 with just the IBU data and 0.009718517 with the IBU squared.**
   
    12. Use your “best” KNN regression model to predict the ABV for an IBU of 150, 170 and 190.  What issue do you see with using KNN to extrapolate?
**This is an observation so you should always be careful when you extrapolate.**


```r
TXmodel
```

```
## k-Nearest Neighbors 
## 
## 56 samples
##  1 predictor
## 
## Pre-processing: centered (1), scaled (1) 
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 50, 50, 49, 51, 51, 50, ... 
## Resampling results across tuning parameters:
## 
##   k  RMSE        Rsquared   MAE        
##   3  0.01064881  0.5316810  0.008590341
##   5  0.01057158  0.5494185  0.008281026
##   7  0.01028120  0.5530533  0.008005807
##   9  0.01031716  0.5539453  0.008036008
## 
## RMSE was used to select the optimal model using the smallest value.
## The final value used for the model was k = 7.
```

### II. KNN Classification

We would like to be able to use ABV and IBU to classify beers between 2 styles: American IPA and American Pale Ale.
   
    13. Filter the beerCOTX dataframe for only beers that are from Texas and are American IPA and American Pale Ale.
    

```r
#create beerTX_AIPA_APA with only TX American IPA and American Pale Ale (APA), removing NAs from IBU
beerTX_AIPA_APA <- filter(beerTX, Style == "American IPA" | Style == "American Pale Ale (APA)")
head(beerTX_AIPA_APA)
```

```
##   Brewery_id                      Beer_Name Beer_ID   ABV IBU
## 1        185                      Slow Ride    2069 0.048  35
## 2        214          Lakefire Rye Pale Ale    2126 0.055  35
## 3         30 Elliott's Phoned Home Pale Ale    1182 0.051  36
## 4        126       Weekend Warrior Pale Ale    1557 0.055  40
## 5        258           Texas Pale Ale (TPA)    1971 0.055  40
## 6        141                  Power & Light    2301 0.055  42
##                     Style Ounces                 Brewery_Name
## 1 American Pale Ale (APA)     12  Oasis Texas Brewing Company
## 2 American Pale Ale (APA)     12      Grapevine Craft Brewery
## 3 American Pale Ale (APA)     16          Cedar Creek Brewery
## 4 American Pale Ale (APA)     12      Karbach Brewing Company
## 5            American IPA     16         South Austin Brewery
## 6 American Pale Ale (APA)     12 Independence Brewing Company
##             City State
## 1         Austin    TX
## 2 Farmers Branch    TX
## 3   Seven Points    TX
## 4        Houston    TX
## 5   South Austin    TX
## 6         Austin    TX
```

```r
#convert to a character
beerTX_AIPA_APA$Style <- factor (as.character(beerTX_AIPA_APA$Style))

#then back to a factor to get rid of the extra styles 
beerTX_AIPA_APA$Style <- factor (beerTX_AIPA_APA$Style)

str(beerTX_AIPA_APA$Style)
```

```
##  Factor w/ 2 levels "American IPA",..: 2 2 2 2 1 2 2 2 2 2 ...
```
    
    14. Divide this filtered data set into a training and test set (60/40, training / test split).
    

```r
set.seed(7) # Set Seed so that same sample can be reproduced in future also
split = sample.split(beerTX_AIPA_APA$ABV, SplitRatio = .6)
TrainingTXIPA = subset(beerTX_AIPA_APA, split == TRUE)
TestTXIPA = subset(beerTX_AIPA_APA, split == FALSE)
#A summary of the TrainingTX data
summary(TrainingTXIPA)
```

```
##    Brewery_id                             Beer_Name    Beer_ID    
##  Min.   : 30.0   Chupahopra                    :1   Min.   : 463  
##  1st Qu.: 59.0   Dankosaurus                   :1   1st Qu.:1276  
##  Median :126.0   El Chingon IPA                :1   Median :1922  
##  Mean   :165.5   Elliott's Phoned Home Pale Ale:1   Mean   :1648  
##  3rd Qu.:254.2   Hopadillo India Pale Ale      :1   3rd Qu.:2048  
##  Max.   :396.0   Infamous IPA                  :1   Max.   :2458  
##                  (Other)                       :4                 
##       ABV               IBU                            Style  
##  Min.   :0.05100   Min.   :36.00   American IPA           :6  
##  1st Qu.:0.05625   1st Qu.:41.25   American Pale Ale (APA):4  
##  Median :0.06550   Median :56.50                              
##  Mean   :0.06410   Mean   :56.20                              
##  3rd Qu.:0.06950   3rd Qu.:70.00                              
##  Max.   :0.07600   Max.   :75.00                              
##                                                               
##      Ounces                           Brewery_Name               City  
##  Min.   :12.0   Cedar Creek Brewery         :2     Austin          :2  
##  1st Qu.:12.0   Karbach Brewing Company     :2     Houston         :2  
##  Median :12.0   Four Corners Brewing Company:1     Seven Points    :2  
##  Mean   :13.2   Hops & Grain Brewery        :1     Conroe          :1  
##  3rd Qu.:15.0   Infamous Brewing Company    :1     Dallas          :1  
##  Max.   :16.0   South Austin Brewery        :1     Dripping Springs:1  
##                 (Other)                     :2     (Other)         :1  
##     State          
##  Length:10         
##  Class :character  
##  Mode  :character  
##                    
##                    
##                    
## 
```

```r
#A summary of the TestTX data
summary(TestTXIPA)
```

```
##    Brewery_id                      Beer_Name    Beer_ID    
##  Min.   : 67.0   Deep Ellum IPA         :1   Min.   :  45  
##  1st Qu.:125.0   Lakefire Rye Pale Ale  :1   1st Qu.:1788  
##  Median :134.5   Pine Belt Pale Ale     :1   Median :2178  
##  Mean   :144.8   Power & Light          :1   Mean   :1826  
##  3rd Qu.:179.0   Pride of Texas Pale Ale:1   3rd Qu.:2319  
##  Max.   :214.0   Slow Ride              :1   Max.   :2521  
##                  (Other)                :2                 
##       ABV               IBU                            Style  
##  Min.   :0.04800   Min.   :35.00   American IPA           :3  
##  1st Qu.:0.05500   1st Qu.:40.25   American Pale Ale (APA):5  
##  Median :0.05850   Median :52.50                              
##  Mean   :0.05875   Mean   :54.00                              
##  3rd Qu.:0.06125   3rd Qu.:70.00                              
##  Max.   :0.07000   Max.   :75.00                              
##                                                               
##      Ounces                         Brewery_Name             City  
##  Min.   :12   Deep Ellum Brewing Company  :1     Austin        :3  
##  1st Qu.:12   Freetail Brewing Company    :1     Conroe        :1  
##  Median :12   Grapevine Craft Brewery     :1     Dallas        :1  
##  Mean   :13   Independence Brewing Company:1     Farmers Branch:1  
##  3rd Qu.:13   Oasis Texas Brewing Company :1     Fort Worth    :1  
##  Max.   :16   Rahr & Sons Brewing Company :1     San Antonio   :1  
##               (Other)                     :2     (Other)       :0  
##     State          
##  Length:8          
##  Class :character  
##  Mode  :character  
##                    
##                    
##                    
## 
```

    15. Use the class packages knn function to build an KNN classifier with k = 3 that will use ABV and IBU as features (explanatory variables) to classify Texas beers as American IPA or American Pale Ale using the Training data.  Use your test set to create a confusion table to estimate the accuracy, sensitivity and specificity of the model.
    

```r
# Classification using Kmeans clustering and KNN
resultsTXIPA3 = class::knn(TrainingTXIPA[,c(4:5)],TestTXIPA[,c(4:5)],TrainingTXIPA[,6], k = 3)
TestTXIPA$StylePred = resultsTXIPA3
TestTXIPA
```

```
##    Brewery_id               Beer_Name Beer_ID   ABV IBU
## 1         185               Slow Ride    2069 0.048  35
## 2         214   Lakefire Rye Pale Ale    2126 0.055  35
## 6         141           Power & Light    2301 0.055  42
## 8         119      Pine Belt Pale Ale      45 0.065  45
## 10        177 Pride of Texas Pale Ale    2229 0.058  60
## 13         67              Soul Doubt    2521 0.059  70
## 15        128          Deep Ellum IPA     943 0.070  70
## 17        127          The Green Room    2372 0.060  75
##                      Style Ounces                        Brewery_Name
## 1  American Pale Ale (APA)     12         Oasis Texas Brewing Company
## 2  American Pale Ale (APA)     12             Grapevine Craft Brewery
## 6  American Pale Ale (APA)     12        Independence Brewing Company
## 8  American Pale Ale (APA)     16       Southern Star Brewing Company
## 10 American Pale Ale (APA)     12         Rahr & Sons Brewing Company
## 13            American IPA     12            Freetail Brewing Company
## 15            American IPA     12          Deep Ellum Brewing Company
## 17            American IPA     16 Uncle Billy's Brewery and Smokeh...
##              City State               StylePred
## 1          Austin    TX American Pale Ale (APA)
## 2  Farmers Branch    TX American Pale Ale (APA)
## 6          Austin    TX American Pale Ale (APA)
## 8          Conroe    TX American Pale Ale (APA)
## 10     Fort Worth    TX            American IPA
## 13    San Antonio    TX            American IPA
## 15         Dallas    TX            American IPA
## 17         Austin    TX            American IPA
```

```r
#Confusion Matrix on ConMtableTestTXIPA
ConMtableTestTXIPA <- table(TestTXIPA$Style,TestTXIPA$StylePred)
confusionMatrix(ConMtableTestTXIPA)
```

```
## Confusion Matrix and Statistics
## 
##                          
##                           American IPA American Pale Ale (APA)
##   American IPA                       3                       0
##   American Pale Ale (APA)            1                       4
##                                           
##                Accuracy : 0.875           
##                  95% CI : (0.4735, 0.9968)
##     No Information Rate : 0.5             
##     P-Value [Acc > NIR] : 0.03516         
##                                           
##                   Kappa : 0.75            
##  Mcnemar's Test P-Value : 1.00000         
##                                           
##             Sensitivity : 0.750           
##             Specificity : 1.000           
##          Pos Pred Value : 1.000           
##          Neg Pred Value : 0.800           
##              Prevalence : 0.500           
##          Detection Rate : 0.375           
##    Detection Prevalence : 0.375           
##       Balanced Accuracy : 0.875           
##                                           
##        'Positive' Class : American IPA    
## 
```
    
   
    16. Using the same process as in the last question, find the accuracy, sensitivity and specificity of a KNN model with k = 5.  Which is better?  Why?
    
**The model with k = 5 out performs the other. The ability to have a larger number to group with caused this model to do better.** 
    

```r
resultsTXIPA5 = class::knn(TrainingTXIPA[,c(4:5)],TestTXIPA[,c(4:5)],TrainingTXIPA[,6], k = 5)
TestTXIPA$StylePred5 = resultsTXIPA5
TestTXIPA
```

```
##    Brewery_id               Beer_Name Beer_ID   ABV IBU
## 1         185               Slow Ride    2069 0.048  35
## 2         214   Lakefire Rye Pale Ale    2126 0.055  35
## 6         141           Power & Light    2301 0.055  42
## 8         119      Pine Belt Pale Ale      45 0.065  45
## 10        177 Pride of Texas Pale Ale    2229 0.058  60
## 13         67              Soul Doubt    2521 0.059  70
## 15        128          Deep Ellum IPA     943 0.070  70
## 17        127          The Green Room    2372 0.060  75
##                      Style Ounces                        Brewery_Name
## 1  American Pale Ale (APA)     12         Oasis Texas Brewing Company
## 2  American Pale Ale (APA)     12             Grapevine Craft Brewery
## 6  American Pale Ale (APA)     12        Independence Brewing Company
## 8  American Pale Ale (APA)     16       Southern Star Brewing Company
## 10 American Pale Ale (APA)     12         Rahr & Sons Brewing Company
## 13            American IPA     12            Freetail Brewing Company
## 15            American IPA     12          Deep Ellum Brewing Company
## 17            American IPA     16 Uncle Billy's Brewery and Smokeh...
##              City State               StylePred              StylePred5
## 1          Austin    TX American Pale Ale (APA) American Pale Ale (APA)
## 2  Farmers Branch    TX American Pale Ale (APA) American Pale Ale (APA)
## 6          Austin    TX American Pale Ale (APA) American Pale Ale (APA)
## 8          Conroe    TX American Pale Ale (APA) American Pale Ale (APA)
## 10     Fort Worth    TX            American IPA            American IPA
## 13    San Antonio    TX            American IPA            American IPA
## 15         Dallas    TX            American IPA            American IPA
## 17         Austin    TX            American IPA            American IPA
```

```r
#Confusion Matrix on ConMtableTestTXIPA
ConMtableTestTXIPA5 <- table(TestTXIPA$Style,TestTXIPA$StylePred5)
confusionMatrix(ConMtableTestTXIPA5)
```

```
## Confusion Matrix and Statistics
## 
##                          
##                           American IPA American Pale Ale (APA)
##   American IPA                       3                       0
##   American Pale Ale (APA)            1                       4
##                                           
##                Accuracy : 0.875           
##                  95% CI : (0.4735, 0.9968)
##     No Information Rate : 0.5             
##     P-Value [Acc > NIR] : 0.03516         
##                                           
##                   Kappa : 0.75            
##  Mcnemar's Test P-Value : 1.00000         
##                                           
##             Sensitivity : 0.750           
##             Specificity : 1.000           
##          Pos Pred Value : 1.000           
##          Neg Pred Value : 0.800           
##              Prevalence : 0.500           
##          Detection Rate : 0.375           
##    Detection Prevalence : 0.375           
##       Balanced Accuracy : 0.875           
##                                           
##        'Positive' Class : American IPA    
## 
```

```r
TestTXIPA
```

```
##    Brewery_id               Beer_Name Beer_ID   ABV IBU
## 1         185               Slow Ride    2069 0.048  35
## 2         214   Lakefire Rye Pale Ale    2126 0.055  35
## 6         141           Power & Light    2301 0.055  42
## 8         119      Pine Belt Pale Ale      45 0.065  45
## 10        177 Pride of Texas Pale Ale    2229 0.058  60
## 13         67              Soul Doubt    2521 0.059  70
## 15        128          Deep Ellum IPA     943 0.070  70
## 17        127          The Green Room    2372 0.060  75
##                      Style Ounces                        Brewery_Name
## 1  American Pale Ale (APA)     12         Oasis Texas Brewing Company
## 2  American Pale Ale (APA)     12             Grapevine Craft Brewery
## 6  American Pale Ale (APA)     12        Independence Brewing Company
## 8  American Pale Ale (APA)     16       Southern Star Brewing Company
## 10 American Pale Ale (APA)     12         Rahr & Sons Brewing Company
## 13            American IPA     12            Freetail Brewing Company
## 15            American IPA     12          Deep Ellum Brewing Company
## 17            American IPA     16 Uncle Billy's Brewery and Smokeh...
##              City State               StylePred              StylePred5
## 1          Austin    TX American Pale Ale (APA) American Pale Ale (APA)
## 2  Farmers Branch    TX American Pale Ale (APA) American Pale Ale (APA)
## 6          Austin    TX American Pale Ale (APA) American Pale Ale (APA)
## 8          Conroe    TX American Pale Ale (APA) American Pale Ale (APA)
## 10     Fort Worth    TX            American IPA            American IPA
## 13    San Antonio    TX            American IPA            American IPA
## 15         Dallas    TX            American IPA            American IPA
## 17         Austin    TX            American IPA            American IPA
```

    BONUS (5 pts total): We did not have a lot data to build and test this classifier.
  
    Check out the class package’s knn.cv function that will perform leave-one-out cross validation.
  
    What is leave-one-out CV (2pts)?
  
  **"Leave-one-out cross validation is K-fold cross validation taken to its logical extreme, with K equal to N, the number of data points in the set." https://www.cs.cmu.edu/~schneide/tut5/node42.html**
  
    Get the accuracy metric for from this function for both the k = 3 and k = 5 KNN classifiers (2pts).
    

```r
resultsTXIPA3knn.cv <- knn.cv(TrainingTXIPA[,c(4:5)], TrainingTXIPA[,6], k = 3, prob = TRUE)
resultsTXIPA3knn.cv
```

```
##  [1] American Pale Ale (APA) American Pale Ale (APA)
##  [3] American Pale Ale (APA) American Pale Ale (APA)
##  [5] American Pale Ale (APA) American IPA           
##  [7] American IPA            American IPA           
##  [9] American IPA            American IPA           
## attr(,"prob")
##  [1] 0.6666667 1.0000000 1.0000000 0.6666667 0.6666667 1.0000000 1.0000000
##  [8] 1.0000000 1.0000000 1.0000000
## attr(,"nn.index")
##       [,1] [,2] [,3]
##  [1,]    2    3    4
##  [2,]    2    1    4
##  [3,]    2    1    4
##  [4,]    5    3    2
##  [5,]    4    3    2
##  [6,]    7    8    9
##  [7,]    8    9   10
##  [8,]    7    9   10
##  [9,]   10    7    8
## [10,]    9    7    8
## attr(,"nn.dist")
##           [,1]      [,2]      [,3]
##  [1,] 4.000002  4.000002  9.000011
##  [2,] 0.000000  4.000002  5.000010
##  [3,] 0.000000  4.000002  5.000010
##  [4,] 5.000002  5.000010  5.000010
##  [5,] 5.000002 10.000001 10.000001
##  [6,] 7.000003  7.000006 10.000000
##  [7,] 0.002000  3.000011  5.000000
##  [8,] 0.002000  3.000017  5.000002
##  [9,] 2.000009  3.000011  3.000017
## [10,] 2.000009  5.000000  5.000002
## Levels: American IPA American Pale Ale (APA)
```


```r
resultsTXIPA5knn.cv <- knn.cv(TrainingTXIPA[,c(4:5)], TrainingTXIPA[,6], k = 5, prob = TRUE)
resultsTXIPA5knn.cv
```

```
##  [1] American Pale Ale (APA) American Pale Ale (APA)
##  [3] American Pale Ale (APA) American Pale Ale (APA)
##  [5] American Pale Ale (APA) American IPA           
##  [7] American IPA            American IPA           
##  [9] American IPA            American IPA           
## attr(,"prob")
##  [1] 0.6 0.8 0.8 0.6 0.6 0.8 0.8 0.8 0.8 0.8
## attr(,"nn.index")
##       [,1] [,2] [,3] [,4] [,5]
##  [1,]    2    3    4    5    6
##  [2,]    2    1    4    5    6
##  [3,]    2    1    4    5    6
##  [4,]    5    3    2    1    6
##  [5,]    4    3    2    6    1
##  [6,]    7    8    9   10    5
##  [7,]    8    9   10    6    5
##  [8,]    7    9   10    6    5
##  [9,]   10    7    8    6    5
## [10,]    9    7    8    6    5
## attr(,"nn.dist")
##           [,1]      [,2]      [,3]      [,4]     [,5]
##  [1,] 4.000002  4.000002  9.000011 14.000003 27.00001
##  [2,] 0.000000  4.000002  5.000010 10.000001 23.00001
##  [3,] 0.000000  4.000002  5.000010 10.000001 23.00001
##  [4,] 5.000002  5.000010  5.000010  9.000011 18.00000
##  [5,] 5.000002 10.000001 10.000001 13.000009 14.00000
##  [6,] 7.000003  7.000006 10.000000 12.000001 13.00001
##  [7,] 0.002000  3.000011  5.000000  7.000003 20.00000
##  [8,] 0.002000  3.000017  5.000002  7.000006 20.00000
##  [9,] 2.000009  3.000011  3.000017 10.000000 23.00001
## [10,] 2.000009  5.000000  5.000002 12.000001 25.00000
## Levels: American IPA American Pale Ale (APA)
```
    
    Which model is suggested by the leave-one-out CV method (1pt)?
    
  **The k=5 model.**

Reminder 
To complete this assignment, please submit one RMarkdown and matching HTML file by the deadline. Please submit all files at the same time; only one submission is granted. 
Good luck!
