library(dplyr)
library(ggplot2)
library(rpart)
library(randomForest)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(caret)
library(Information)
library(ggplot2)
library(ggthemes) 

# reading the data from 7 different files each corresponding to a group
masterfile <- read.csv("group_data\\masterfile_clean.csv")

# CMS ratings data 
ratings <- read.csv("Hospital General Information.csv")
ratings <- ratings[, c(1, 13)]


ratings[which(ratings$Hospital.overall.rating == "Not Available"), 2] <- NA
sum(is.na(ratings))

# removing row numbers
masterfile <- masterfile[, -1]
masterfile <- merge(masterfile, ratings, by="Provider.ID")

# removing provider ID
masterfile <- masterfile[, -1]

#removing the entries where rating is NA
masterfile <- masterfile[-which(is.na(masterfile$Hospital.overall.rating)), ]

# removing the factor level 'Not Available'
masterfile$Hospital.overall.rating <- as.factor(as.integer(masterfile$Hospital.overall.rating))
summary(masterfile$Hospital.overall.rating)
nrow(masterfile)

# splitting the data
str(masterfile)

# removing provider ID
masterfile <- masterfile[ , -1]

na_cols <- sapply(masterfile, function(x) sum(is.na(x)))
na_cols

# removing columns having more than 1800 NA values
na_cols <- names(na_cols)[which(na_cols > 1800)]
na_cols

masterfile <- masterfile[, -which(names(masterfile) %in% na_cols)]
str(masterfile)

# checking out the number of missing values after removing a few columns
sapply(masterfile, function(x) sum(is.na(x)))

### replacing na with median#####
f_replace_by_median <- function(some_vector){
  some_vector[which(is.na(some_vector))] <- median(some_vector, na.rm=T)
  return(some_vector)
}

masterfile[, -ncol(masterfile)] <- sapply(masterfile[, -ncol(masterfile)], f_replace_by_median)
###

n <- nrow(masterfile)
s <- sample(1:n, size=0.7*n)
train <- masterfile[s, ]
test <- masterfile[-s, ] 
summary(train$Hospital.overall.rating)
summary(test$Hospital.overall.rating)


# random forest
rf <- randomForest(Hospital.overall.rating ~., data=train, mtry=20, na.action=na.omit, ntree=800)
rf
d = data.frame(rf$importance)
d
View(d)

#predict using rf
rf_pred <- predict(rf, newdata=test[, -52])
table(rf_pred, test[, 52])
confusionMatrix(rf_pred, test[, 52])

# random forest 2: Increase the number of trees to 1500 
rf <- randomForest(Hospital.overall.rating ~., data=train, mtry=20, na.action=na.omit, ntree=1500)
rf
d = data.frame(rf$importance)
d
View(d)
# write.csv(d, "group_data\\rf_important_measures.csv")

#predict using rf
rf_pred <- predict(rf, newdata=test[, -52])
table(rf_pred, test[, 52])
confusionMatrix(rf_pred, test[, 52])
# this one does slightly better

# merging the predicted values with the actual values to compare
test$predicted <- rf_pred

## A better way to present the model accuracy
# If the actual value is 3 and predicted is 4, then the error is +1
# If actual is 2 and predicted is 1, error is -1
# This will allow us to see how many 0s, +1s and -1s are there in predictions 
# (1s and -1s are better than 2s and -2s)

test$error <- as.numeric(test$predicted) - as.numeric(test$Hospital.overall.rating)
test$error <- factor(test$error, levels = c(-2, -1, 0, 1, 2))
error_df <- data.frame(summary(test$error))
error_df$rating <- c(-2, -1, 0, 1, 2)

colnames(error_df) <- c("count", "error")
str(error_df)
ggplot(error_df, aes(x=error, y=count)) + geom_bar(stat="identity")+geom_text(aes(label=count))+
  xlab("Prediction Error (Star Rating)")+ylab("Count")+
  ggtitle("Random Forest Prediction Accuracy (Test Data - 1095 Providers)")+theme_minimal()
  




## Optional: RF with only three classes ##### 
# We can achieve much better accuracy by collapsing the ratings into low (1, 2), 
# medium (3) and high (4, 5)

# Trying collapsing ratings into (1,2=low), (3=avg), (4,5) = good
summary(masterfile$Hospital.overall.rating)

for (row in 1:nrow(masterfile)){
  if (masterfile$Hospital.overall.rating[row] == 2){
    masterfile$Hospital.overall.rating[row] = 1
  }
  
   if (masterfile$Hospital.overall.rating[row] == 4){
    masterfile$Hospital.overall.rating[row] = 5
  }
}
# now 1 implies low, 3 implies medium and 5 implies good rating

masterfile$Hospital.overall.rating <- as.integer(masterfile$Hospital.overall.rating)
masterfile$Hospital.overall.rating <- as.factor(masterfile$Hospital.overall.rating)
summary(masterfile$Hospital.overall.rating)

# Building a decision tree to observe the variable importance visually
n <- nrow(masterfile)
s <- sample(1:n, size=0.8*n)
train <- masterfile[s, ]
test <- masterfile[-s, ] 
summary(train$Hospital.overall.rating)
summary(test$Hospital.overall.rating)

tree <- rpart(Hospital.overall.rating ~., data=train, na.action=na.omit, 
              control = rpart.control(minsplit=50, cp=0.01))

fancyRpartPlot(tree)
tree$cptable
summary(tree)

#Predictions
tree_pred <-  predict(tree, test[, -52], type = "class")
table(tree_pred, test[, 52])
confusionMatrix(tree_pred, test[, 52])

#tree with reduced minsplit of 10 and cp=0.01
tree <- rpart(Hospital.overall.rating ~., data=train, na.action=na.omit, 
              control = rpart.control(minsplit=10, cp=0.01))

fancyRpartPlot(tree)

tree_pred <-  predict(tree, test[, -52], type = "class")
table(tree_pred, test[, 52])
confusionMatrix(tree_pred, test[, 52])



# random forest
summary(train$Hospital.overall.rating)
summary(test$Hospital.overall.rating)
rf <- randomForest(Hospital.overall.rating ~., data=train, mtry=20, na.action=na.omit, ntree=800)
rf

#predict using rf
rf_pred <- predict(rf, newdata=test[, -52])
table(rf_pred, test[, 52])
confusionMatrix(rf_pred, test[, 52])

# random forest performs much better than the tree

# trying to increase the number of trees to 1200
rf <- randomForest(Hospital.overall.rating ~., data=train, mtry=20, na.action=na.omit, ntree=1200)
rf
rf_pred <- predict(rf, newdata=test[, -52])
table(rf_pred, test[, 52])
confusionMatrix(rf_pred, test[, 52])
imp <- rf$importance
View(imp)

