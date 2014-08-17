---
title: "Machine Learning - Course project"
output: html_document
---

*Introduction

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, our goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. ***The goal of our project is to predict the manner in which they did the exercise.***

**Methods**
**Getting and splitting the data**

```{r}
library (caret)
set.seed(1988)

download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
              "pml-training.csv",
              method = "wget")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
              "pml-testing.csv",
              method = "wget")


training <- read.table("pml-training.csv",
                       header = TRUE,
                       sep = ",",
                       na.strings = c(".", "NA", "", "?"))
test <- read.table("pml-testing.csv", 
                   header = TRUE, 
                   sep = ",", 
                   na.strings = c(".", "NA", "", "?"))



trainIndex <- createDataPartition(training$classe,
                                  p = .9,
                                  list = FALSE,
                                  times = 1)
innerTrain <- training[trainIndex, ]
innerTest <- training[-trainIndex, ]
```

**Preprocessing**

Let's have a look at what we have

```{r}
names(innerTrain)
```

Looks like we may delete something here. 
But first let's save our outcome somewhere, so it doesn't get into troubles.

```{r}
classeITrain <- innerTrain[, 160]
classeITest <- innerTest[, 160]
classeTest <- test[, 160]

innerTrain <- innerTrain[, -160]
innerTest <- innerTest[, -160]
test <- test[, -160]

innerTrain <- innerTrain[, 8 : 159]
innerTest <- innerTest[, 8 : 159]
test <- test[, 8 : 159]
dim(innerTrain)
```

Now, let's get rid of variables full of NAs.
Please note that here, like anywhere in this work, I use only innerTrain to generate the rules i than apply to innerTest and test. 

```{r}
missing_prop <- sapply(innerTrain,function(x) { !mean(is.na(x)) })
innerTrain <- innerTrain[missing_prop>0.5] 
innerTest <- innerTest[missing_prop>0.5] 
test <- test[missing_prop>0.5] 
dim(innerTrain)
```

Even if we are going to use a tree-based model, let's also get rid of near-zero-variance predictors.


```{r}
nearZeroVariance <- nearZeroVar(training)
innerTrain <- innerTrain[, -nearZeroVariance]
innerTest <- innerTest[, -nearZeroVariance]
test <- test[, -nearZeroVariance]
dim(innerTrain)
```

Now, let's face the issue of collinearity.

```{r}
descrCor <- cor(innerTrain)
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.75)
innerTrain <- innerTrain[, -highlyCorDescr]
innerTest <- innerTest[, -highlyCorDescr]
test <- test[, -highlyCorDescr]
dim(innerTrain)
```

And now, we can also have our outcome variable back.

```{r}
innerTrain[, "classe"] <- classeITrain
innerTest[, "classe"] <- classeITest
test[, 30] <- classeTest 
```

And now, the most wanted (and long) moment.
We build our predictor!

```{r}
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5)

rfFit <- train(classe ~ .,
               data = innerTrain,
               method = "rf",
               trControl = fitControl,
               verbose = TRUE)
```

**Results**

```{r}
rfFit
```

Looks like our laptop did a great job here!
Anyway, even if the caret function does bootstrapping, we may still want to double-check how the predictor performs with different data.

```{r}
testPred <- predict(rfFit, innerTest)
postResample(testPred, innerTest$classe)
```

Now we are happy and safe!
Let's generate our TXTs

```{r}

predictions <- predict(rfFit, test)
predictions


pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictions)
```

That's all!
See you in the next class!