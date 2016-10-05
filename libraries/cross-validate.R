#
# Cross-Validation Functions 
# Supports e1071, rpart, and RandomForest
#

crossValidate.rpart = function(data, formula) {
    
}

# data - a data fram
# features - string vector of features to use
# target -  
crossValidate.linreg = function(data, features, target, folds=5) {
    n = nrow(data)
    formula = reformulate(features, target)
    
    data.sets = splitData(accident.GES, 5)
    
    
    for (i in 1:5) {
        data.tr = do.call("rbind", data.sets[[i]])
        data.te = data.sets[[i]]
        
        fit = lm(formula, data=data.tr)
        
        predict(fit, data=data.te)
    }
}