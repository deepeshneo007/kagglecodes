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
library(mi)

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

train_date = train_char[,grep("JAN1|FEB1|MAR1", train_char),]
train_char = train_char[, !colnames(train_char) %in% colnames(train_date)]
train_date = sapply(train_date, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
train_date = do.call(cbind.data.frame, train_date)
for (f in colnames(train_date)){
  train_date[[f]]<-as.numeric(train_date[[f]])
}

train_char=cbind(train_char,train_date)

test_date = test_char[,grep("JAN1|FEB1|MAR1", test_char),]
test_char = test_char[, !colnames(test_char) %in% colnames(test_date)]
test_date = sapply(test_date, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
test_date = do.call(cbind.data.frame, test_date)
for (f in colnames(test_date)){
  test_date[[f]]<-as.numeric(test_date[[f]])
}

unique(lapply(test_date, class))

test_char=cbind(test_char,test_date)
unique(lapply(test_char, class))
train<-cbind(train_char,train_numr)
test<-cbind(test_char,test_numr)
unique(lapply(train, class))
unique(lapply(test, class))


#Removing columns with only 1 value
col_ct = sapply(train, function(x) length(unique(x)))
cat("Constant feature count:", length(col_ct[col_ct==1]))
train = train[, !names(train) %in% names(col_ct[col_ct==1])]

#<Deepesh Edit>
train <- train[,colSums(is.na(train))<(.80*nrow(train))]

feature.names <- names(train)[2:ncol(train)-1]

for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
    cat(f, "unique: ", length(unique(levels)), "\n")
  }
}

unique(lapply(test, class))



#Clearing RAM
rm(train.raw)
rm(test.raw)
rm(train_char)
rm(test_char)
rm(train_date)
rm(test_date)
rm(train_numr)
rm(test_numr)

#</Deepesh Edit>

numcores_free=2 #Number of cores set free. Warning: Setting low value will slow down the system
registerDoSNOW(makeCluster(detectCores()-numcores_free, type="SOCK"))

imputed <- missForest(train,maxiter = 10,parallelize = 'forests', verbose = TRUE,variablewise=TRUE)