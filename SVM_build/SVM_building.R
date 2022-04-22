## ---- SVM BUILD ---- 
library(caret)
library(e1071)
library(tidyr)
library(ggplot2)
library(lattice)

# Cleaned up dataframe is depicted as total_df

#split frame depending on quantile percentage up to 80th
lower_df <- total_df[total_df$glob_15_m <= quantile(total_df$glob_15_m, p=0.8),]
upper_df <- total_df[total_df$glob_15_m > quantile(total_df$glob_15_m, p=0.8),]

#Assign new label depending on split
lower_df$label <- "stable"
upper_df$label <- "unstable"

#create random wanted partition to be used in train/test-partition
row_id_stable <- createDataPartition(1:nrow(lower_df), p=0.15, list=FALSE)

# create var to use in training, without label var.
train_var <- lower_df[row_id_stable, -which(colnames(lower_df)=="label")]
# create vector with just the label
train_label <- lower_df[row_id_stable, which(colnames(lower_df)=="label")]

#create test-partition of x-% of the 'unstable' data
row_id_unstable <- createDataPartition(1:nrow(upper_df), p=0.10, list=FALSE)
#create new test-partition of stable data to add to unstable for testing purposes
test_id_stable <- createDataPartition(1:nrow(lower_df), p=0.10, list=FALSE)
# bind both partitions together
test_df <- rbind(upper_df[row_id_unstable,], lower_df[-test_id_stable,])
#create dataframe for testing and vector for label
test_var <- test_df[,-which(colnames(test_df)=="label")]
test_label <- test_df[,which(colnames(test_df)=="label")]

# train the model, scaling is false because values are already normalized.
svm_high_group <- svm(x=train_var, 
                      type="one-classification",
                      y=NULL,
                      nu=0.05,
                      scale=FALSE,
                      kernel="radial")

# Predict both train- and test sets
svm_train_pred <- predict(svm_high_group, train_var)
svm_test_pred <- predict(svm_high_group, test_var)

# Output as confidence-table for accuracy.
print(table(Predicted=svm_train_pred, Reference=unlist(train_label)))
print(table(Predicted=svm_test_pred, Reference=unlist(test_label)))

