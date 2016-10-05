#
# Utility functions for data preparation/manipulation
# splitData, selectFeatures
#

# Splits data into parts as defined by "split"
# 75/25 split by default
splitData = function(data, split=c(0.75)) {
    # make sure split is not empty
    len = length(split)
    stopifnot(len > 0)
  
    # if second argument not given make sure first is less than one
    if (len == 1) {
        if (split %% 1 == 0) {
            split = rep(1/split, each=split-1)
        } else
            stopifnot(split[1] < 1)
    }
        
    n = nrow(data)
    splits = cumsum(split)
    splits = splits * n
    starts = c(1, splits)
    ends = c(splits+1, n)
    data.sets = list()
    samp = sample(n)
    
    for (i in 1:length(starts)) {
        data.sets[[i]] = data[samp[starts[i]:ends[i]],]
        print(ends[i] - starts[i])
    }
    
    return(data.sets)
}