library(readr)
library(xgboost)
library(randomForest)
library(parallel)
library(doSNOW) #parallel adapter for 'foreach'
library(foreach) #for parallelization
library(missForest) #For missing value imputation if needed
library(doParallel)
library(itertools)
library(BLR)

set.seed(666)
setwd("/Users/deepesh.sharma/projects/Kaggle/springLeaf/data")
cat("reading the train and test data\n")
train.raw <- read_csv("train.csv")
test.raw  <- read_csv("test.csv")

train<-train.raw
test<-test.raw

feature.names <- names(train)[2:ncol(train)-1]

train_numr = train[, sapply(train, is.numeric)]
test_numr=test[, sapply(test, is.numeric)]
train_char = train[, sapply(train, is.character)]
test_char=test[, sapply(test, is.character)]
cat("Numerical column count for train : ", dim(train_numr)[2], 
    "; Character column count for train : ", dim(train_char)[2])

cat("Numerical column count for test: ", dim(test_numr)[2], 
    "; Character column count for test : ", dim(test_char)[2])


train_char[train_char==-1] = NA
train_char[train_char==""] = NA
train_char[train_char=="[]"] = NA

test_char[test_char==-1] = NA
test_char[test_char==""] = NA
test_char[test_char=="[]"] = NA

train<-cbind(train_char,train_numr)
test<-cbind(test_char,test_numr)


#Removing columns with only 1 value
col_ct = sapply(train, function(x) length(unique(x)))
cat("Constant feature count:", length(col_ct[col_ct==1]))
train = train[, !names(train) %in% names(col_ct[col_ct==1])]

#<Deepesh Edit>
sapply(train, function(x) sum(is.na(x))/nrow(train)) 
train <- train[,colSums(is.na(train))<(.90*nrow(train))]

feature.names <- names(train)[2:ncol(train)-1]
#</Deepesh Edit>

