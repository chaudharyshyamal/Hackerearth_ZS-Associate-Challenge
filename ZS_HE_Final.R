library(caret)
library(plyr)
library(forecast)
library(nnet)
library(xgboost)
library(stats)
library(Matrix)
library(e1071)


setwd("C:/Users/mahe/Desktop/dataset")

#train, test and promotional expense dataset
train <- as.data.frame(read.table("yds_train2018.csv",header=TRUE,sep=","))
test <- as.data.frame(read.table("yds_test2018.csv",header=TRUE,sep=","))
promo <- as.data.frame(read.table("promotional_expense.csv",header=TRUE,sep=","))

#Copy of test data
ts <- as.data.frame(read.table("yds_test2018.csv",header=TRUE,sep=","))

train$S_No <- NULL
train$Week <- NULL
train$Merchant_ID <- NULL

test$S_No <- NULL
test$Sales <- NULL

#Copy of test data to take unique elements
t<-train
t$Sales<-NULL

#Calculating total sales of month as training data contains sales based on week
for(i in 1:nrow(tr) )
{
  tr$Sales[i] <- 0
  for (j in 1:nrow(train))
  {
    if(tr$Year[i]==train$Year[j] && tr$Month[i]==train$Month[j] && 
       tr$Product_ID[i]==train$Product_ID[j] && tr$Country[i]==train$Country[j])
      
      tr$Sales[i] <- tr$Sales[i] + train$Sales[j]
  }
}

#Taking unique training data from training dataset base on Year, Month, Product_ID, Country
tr<-unique(t)

#Combining promotional expense with train data
for(i in 1:nrow(tr))
{
  for(j in 1:nrow(promo))
  {
    if(tr$Year[i]==promo$Year[j] && tr$Month[i]==promo$Month[j] &&
       tr$Product_ID[i]==promo$Product_ID[j] && tr$Country[i]==promo$Country[j])
      
      tr$Expense[i] <- promo$Expense_Price[j]
  }
}

#Combining promotional expense with test data
for(i in 1:nrow(test))
{
  for(j in 1:nrow(promo))
  {
    if(test$Year[i]==promo$Year[j] && test$Month[i]==promo$Month[j] &&
       test$Product_ID[i]==promo$Product_ID[j] && test$Country[i]==promo$Country[j])
      
      test$Expense[i] <- promo$Expense_Price[j]
  }
}

#SVM with cross validation
fitControl <- trainControl(method = "repeatedcv", number = 30, repeats = 10)

gbmFit1 <- train(Sales ~., data = tr, method = "gbm",
                 trControl = fitControl,verbose = FALSE)

predict.gbmCV <- predict(gbmFit1, test)

y<-data.frame(test,predict.gbmCV)
y$Expense<-NULL
y$Sales<-NULL
z<-data.frame(ts$S_No,y)

colnames(z) <- c("S_No","Year","Month","Product_ID","Country","Sales")

write.csv(z, file = "resultsvm.csv",row.names=FALSE)
