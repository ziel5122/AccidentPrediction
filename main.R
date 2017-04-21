library(e1071)
library(randomForest)
library(rpart)
library(rpart.plot) 

if (dirname(rstudioapi::getActiveDocumentContext()$path) != "") {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

