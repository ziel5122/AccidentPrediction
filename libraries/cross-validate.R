#
# Cross-Validation Functions 
# Supports e1071, rpart, and RandomForest
#

crossValidate.rpart = function(data, features, target, folds=5) {
    n = nrow(data)
    formula = reformulate(features, target)
    
    data.sets = splitData(accident.GES, 5)
    average = 0
    
    for (i in i:folds) {
        data.te = data.sets[[i]]
        data.tr = data[-data.te]
        
        fit = rpart(formula, data=data.tr, method="class")
        
    }
}

# data - a data fram
# features - string vector of features to use
# target -  
crossValidate.logreg = function(data, features, target, folds=5) {
    n = nrow(data)
    formula = reformulate(features, target)
    
    data.sets = splitData(accident.GES, 5)
    
    
    for (i in 1:5) {
        vector = 1:folds
        vector = vector[-i]
        data.tr = do.call("rbind", data.sets[[vector]])
        data.te = data.sets[[i]]
        
        fit = lm(formula, data=data.tr)
        
        predict(fit, data=data.te)
    }
}