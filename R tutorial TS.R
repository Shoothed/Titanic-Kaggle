# Titanic R tutorial from Trevor Stephens
# http://trevorstephens.com/kaggle-titanic-tutorial/getting-started-with-r/

# Importing the datasets
setwd("~/Kaggle/Titanic")
train = read.csv("train.csv")
test = read.csv("test.csv")

# Analysing data
table(train$Survived)
prop.table(table(train$Survived))
test$Survived <- rep(0,418) # Assume everyone dies

summary(train$Sex)
prop.table(table(train$Sex, train$Survived), 1)

# Feature Engineering with Age
summary(train$Age) # Average age is that of an adult
train$Child <- 0
train$Child[train$Age < 18] <- 1 # This also changes NA to the average(adult)
head(train,10)
train[is.na(train$Age),]

aggregate(Survived ~ Child + Sex, data = train, FUN = sum)
aggregate(Survived ~ Child + Sex, data = train, FUN = length)
aggregate(Survived ~ Child + Sex, data = train,
          FUN = function(x) {sum(x)/ length(x)})
aggregate(Survived ~ Child + Sex, data = train,
          FUN = function(x) {round((sum(x)/ length(x)*100),2)})

# Feature Engineering with Fare
summary(train$Fare)
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data = train,
          FUN = function(x) {sum(x)/ length(x)})

# Decision Tree

library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = train, method = 'class')
plot(fit)
text(fit)

#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

?rpart.control
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train, method = 'class',
             control = rpart.control(minsplit = 2, cp = 0))
fancyRpartPlot(fit) # Hello overfitting

#new.fit <- prp(fit, snip=TRUE)$obj
#fancyRpartPlot(new.fit)

rm(fit)
#rm(new.fit)

# Feature engineering names
train$Name[1]
test$Survived <- NA
train = read.csv("train.csv")
combi <- rbind(train, test)

combi$Name <- as.character(combi$Name)
combi$Name[1]

strsplit(combi$Name[1], split = '[,.]')
strsplit(combi$Name[1], split = '[,.]')[[1]]
strsplit(combi$Name[1], split = '[,.]')[[1]][2]

combi$Title <- sapply(combi$Name, FUN = function(x) 
  {strsplit(x, split= '[,.]')[[1]][2]})
table(combi$Title)
combi$Title <- sub(' ','', combi$Title)

combi$Title[combi$Title %in% c('Mlle')] <- 'Miss'
combi$Title[combi$Title %in% c('Mme')] <- 'Mrs'
combi$Title[combi$Title %in% c('Capt','Col','Don','Major','Jonkheer')] <- 'Sir' 
combi$Title[combi$Title %in% c('Dona','Lady','the Countess')] <- 'Lady'

combi$Title <- factor(combi$Title)

# Feature Engineering Family sizes

combi$FamilySize <- combi$SibSp + combi$Parch + 1
table(combi$FamilySize)

combi$Surname <- sapply(combi$Name, FUN = function(x) 
  {strsplit(x, split='[,.]')[[1]][1]})

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep='')
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))

famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize,
             data = train, method = 'class')
fancyRpartPlot(fit)          

# Fixing Age's missing values using a decision tree

summary(combi$Age)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title,
                data = combi[!is.na(combi$Age),], method = 'anova')
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

summary(combi) # missing values found in Embarked and Fare 

# Fixing Embarked missing values
summary(combi$Embarked)
which(combi$Embarked =='')
combi$Embarked[c(62,830)] = 'S'
combi$Embarked <- factor(combi$Embarked) # to remove that 4th level

# Fixing Fare missing values

summary(combi$Fare)
which(is.na(combi$Fare))
combi[1044,]
combi$Fare[1044] = median(combi$Fare, na.rm=TRUE)

# Preparing Random Forests

str(combi) # Family ID has too many factors for random forest (max is 32)
combi$FamilyID2 <-  combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize == 3] <- 'Small'             
combi$FamilyID2 <- factor(combi$FamilyID2)   
str(combi$FamilyID2) # 22 levels! Much better

train = combi[1:891,]
test = combi[892:1309,]

# Performing Random Forest

#install.packages('randomForest')
library(randomForest)
set.seed(415)

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                    data = train, importance = TRUE, ntree = 2000)

varImpPlot(fit)

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "TSForest.csv", row.names = FALSE)

# Trying a forest of conditional inference trees that can use FamilyID (not 2)

#install.packages('party')
library(party)
set.seed(415)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls = cforest_unbiased(ntree = 2000, mtry= 3))

Prediction <- predict(fit, test, OOB=TRUE, type = 'response')
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "TSpartyForest.csv", row.names = FALSE)








             