##Coursera:Practical Machine Learning:final project: 

*This assignment is submited by Shushanik Gasparyan*

First lets load necessary libraries and set our working directory
```{r,echo=TRUE}
library(ggplot2)
library(caret)
library(randomForest)
setwd("/Users/shushanikgasparyan/Desktop/Coursera/machinelearning")
```
Now let`s  download both the training and testing data files and take a look at the data provided to build our model. The goal of the model is to use any variables provided to predict the manner in which a person did the exercise (classe).
```{r,echo=TRUE}
#download files from the urls provided
#train_url <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
#download.file(url=train_url, destfile="training.csv")

#test_url <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
#download.file(url=test_url, destfile="testing.csv")

#read in training and testing data
train <- read.csv("training.csv", na.strings=c("NA","#DIV/0!",""))
test <- read.csv("testing.csv", na.strings=c("NA","#DIV/0!",""))

names(train)
str(train)
summary(train)
summary(train$classe)#this is the outcome we want to predict
```

<h3>Split training/testing data</h3>
```{r,echo=TRUE}
#we want to predict the 'classe' variable using any other variable to predict with

inTrain <- createDataPartition(y=train$classe, p=0.6, list=FALSE)
myTrain <- train[inTrain, ]
myTest <- train[-inTrain, ]
dim(myTrain)

dim(myTest)
```
<h3>Feature selection</h3>
Now we can tranform the data to only include the variables we will need to build our model. We will remove variables with near zero variance, variables with mostly missing data, and variables that are obviously not useful as predictors.

```{r,echo=TRUE}
#first we will remove variables with mostly NAs (use threshold of >75%)
mytrain_SUB <- myTrain
for (i in 1:length(myTrain)) {
  if (sum(is.na(myTrain[ , i])) / nrow(myTrain) >= .75) {
    for (j in 1:length(mytrain_SUB)) {
      if (length(grep(names(myTrain[i]), names(mytrain_SUB)[j]))==1) {
        mytrain_SUB <- mytrain_SUB[ , -j]
      }
    }
  }
}

dim(mytrain_SUB)


#remove columns that are obviously not predictors
mytrain_SUB2 <- mytrain_SUB[,8:length(mytrain_SUB)]

#remove variables with near zero variance
NZV <- nearZeroVar(mytrain_SUB2, saveMetrics = TRUE)
NZV #all false, none to remove

keep <- names(mytrain_SUB2)

```
<h3>Random Forest Model</h3>

I decided to use the random forest model to build my machine learning algorithm as it is appropriate for a classification problem as we have and based on information provided in class lectures this model tends to be more accurate than some other classification models.

Below I fit my model on my training data and then use my model to predict classe on my subset of data used for cross validation.
```{r,echo=TRUE}
set.seed(2000)
modFit <- randomForest(classe~., data = mytrain_SUB2)
print(modFit)

#cross validation on my testing data
#out of sample error
predict1 <- predict(modFit, myTest, type = "class")
confusionMatrix(myTest$classe, predict1)

#in sample error
predict_train <- predict(modFit, myTrain, type = "class")
confusionMatrix(myTrain$classe, predict_train)

modFit <- train(classe ~., method = "rf", trControl=trainControl(method = "cv", number = 4), data = mytrain_SUB)
```



<h3>Error</h3>

As we can see from the model summaries above, when we run the model on our test data for cross validation we get an accuracy of 99.4% that we can estimate to be our out of sample error. When the model is fitted to the training data used to build the model it shows 100% accuracy, which we can assume as our in sample error.

