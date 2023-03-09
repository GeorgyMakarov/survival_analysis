library(caret)
source('data_generator.R')
rm(raw_dt)

set.seed(123)
train_idx <- createDataPartition(dt[['def_event']],p = 0.8, list = F)
train_dt  <- dt[train_idx, , drop = FALSE]
other_dt  <- dt[-train_idx, , drop = FALSE]

valid_idx <- createDataPartition(other_dt[['def_event']], p = 0.5, list = F)
valid_dt  <- other_dt[valid_idx, , drop = FALSE]
test_dt   <- other_dt[-valid_idx, , drop = FALSE]

rm(list = setdiff(ls(), c('train_dt', 'valid_dt', 'test_dt')))
