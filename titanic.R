# Titanic Kaggle

# Importing the datasets
setwd("~/Kaggle/Titanic")
train = read.csv('train.csv')
test = read.csv('test.csv')

# Organising the data
head(train)
summary(train)
str(train)
train[,c(1,2,3,7,8)] = lapply(train[,c(1,2,3,7,8)], as.factor)
summary(train) # columns not imoprtant: ID,Name,Ticket,Cabin
str(test)
test[,c("Pclass","SibSp","Parch")] = lapply(test[,c("Pclass","SibSp","Parch")], as.factor)
summary(test)

# Fixing data
test[is.na(test$Fare),]
summary(test[which(test$Pclass==3),"Fare"])
test[is.na(test$Fare),"Fare"] = median(test[which(test$Pclass==3),"Fare"], na.rm=TRUE)
test[which(test$PassengerId == 1044),]

summary(train$Age)
summary(test$Age)

summary(train)
summary(test)

test[is.na(test$Age),]
test[is.na(test$Age), "Age"] = median(test[test$Age,"Age"],na.rm=TRUE)

train[is.na(train$Age),]
train[is.na(train$Age), "Age"] = median(train[train$Age,"Age"],na.rm=TRUE)

summary(train)
train[train$Embarked=="","Embarked"] = "S"
train[train$Ticket == 113572,]
test[test$Ticket == 113572,]

# Analyzing the data:
train[which(train$SibSp==8),] # Sage family of 8 all died
train[train$SibSp==8,]
train[which(train$SibSp==5),] # Goodwin family of 5 all died
train[train$Ticket =="CA 2144",]
train[train$Parch == 6,]
test[test$Parch == 6, ] # let's see if the Goodwin father lived
d <- train[which(train$SibSp==4),]
d[order(d$Ticket),]
head(train)
train[train$Ticket==1601,]
train[train$Ticket==3101281,]
train[which(train$Age < 2),]
test[test$Parch==9,]
train[which(train$SibSp==8),]
test[test$Ticket=="CA. 2343",] # Parch with 9 are the Parents of the Sage Family

# Fixing Parch's 9 value

summary(train$Parch)
summary(test$Parch)

train$Parch = as.numeric(as.character(train$Parch))
test$Parch = as.numeric(as.character(test$Parch))

# Creating training and test sets
training_set = train[,c(3,5,6,7,8,10,12,2)]
head(training_set)

test_set = test[,c(2,4,5,6,7,9,11)]
head(test_set)

# Encoding categorical data

training_set$Sex = as.numeric(factor(training_set$Sex,
                          levels = c('female', 'male'),
                          labels = c(1,2)))
training_set$Embarked = as.numeric(factor(training_set$Embarked,
                               levels = c('C',"Q","S"),
                               labels = c(1,2,3)))
training_set$SibSp = as.numeric(factor(training_set$SibSp,
                                  levels = c("0","1","2","3","4","5",'8'),
                                  labels = c(1,2,3,4,5,6,7)))
training_set$Pclass = as.numeric(training_set$Pclass)

test_set$Sex = as.numeric(factor(test_set$Sex,
                          levels = c('female', 'male'),
                          labels = c(1,2)))
test_set$Embarked = as.numeric(factor(test_set$Embarked,
                               levels = c('C',"Q","S"),
                               labels = c(1,2,3)))
test_set$SibSp = as.numeric(factor(test_set$SibSp,
                                       levels = c("0","1","2","3","4","5",'8'),
                                       labels = c(1,2,3,4,5,6,7)))
test_set$Pclass = as.numeric(test_set$Pclass)

# Feature Scaling
# training_set[,c(3,5,6)] = scale(training_set[,c(3,5,6)])
# test_set[,c(3,5,6)] = scale(test_set[,c(3,5,6)])
# training_set[,c(1,2,4,7,8)] = as.numeric(as.character(training_set[,c(1,2,4,7,8)]))
# training_set2 = as.numeric(training_set)
# test_set$Sex = as.numeric(as.character(test_set$Sex))
# training_set[,c(1,2,4,7)] = scale(training_set[,c(1,2,4,7)])
# test_set = scale(test_set)
training_set[-8] = scale(training_set[-8])
test_set = scale(test_set)

### Fitting Lositic Regression to the Training Set
test_set = data.frame(test_set)
classifier = glm(formula = Survived ~ . ,
                 family = binomial,
                 data=training_set)

#Predicting the Test set results
prob_pred = predict(classifier, type= 'response', newdata = test_set)
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#### Trying Random Forest
#install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-8],
                          y = training_set$Survived,
                          ntree = 500)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set)

### Trying ANN with h2o
# Fitting ANN to the Training set
library(h2o)
h2o.init(nthreads = -1)
training_set$Survived = as.numeric(as.character(training_set$Survived))
classifier = h2o.deeplearning(y = 'Survived',
                              training_frame = as.h2o(training_set),
                              activation = 'RectifierWithDropout',
                              hidden = c(100,100,100),
                              epochs = 8000,
                              train_samples_per_iteration = -2)

# Predicting the Test set results
prob_pred = h2o.predict(classifier,newdata = as.h2o(test_set))
y_pred = (prob_pred > 0.5)
y_pred = as.vector(y_pred)

h2o.shutdown()

# Fitting XGBoost to the Training set and Test set
library(xgboost)
classifier = xgboost(data = as.matrix(training_set[-8]), label = training_set$Survived, nrounds = 100)

# Predicting the Test set results
y_pred = predict(classifier, newdata = as.matrix(test_set))
y_pred = (prob_pred > 0.5)
y_pred = as.vector(y_pred)

#Preparing submission
prediction = data.frame(PassengerId = test$PassengerId,
                        Survived = y_pred)
write.csv(prediction, "predicted", row.names = FALSE)

# Analysing predictions
test[test$Parch == 6, ]
prediction[prediction[,1]== 1031,] # Goodwin father died too
test[test$Ticket=="CA. 2343",] 
prediction[prediction[,1]== 1080 | prediction[,1]==  1234 | prediction[,1]== 1252 | prediction[,1]==1257,] 
                        # Sage family all died...
