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
ncores <- 6
setwd("/Users/deepesh.sharma/projects/Kaggle/springLeaf/data")
cat("reading the train and test data\n")
train <- read_csv("train.csv")
test  <- read_csv("test.csv")