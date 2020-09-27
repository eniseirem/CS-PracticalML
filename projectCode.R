library(caret)

train_in <- read.csv('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv', header=T)
valid_in <- read.csv('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv', header=T)
dim(train_in)

set.seed(10)
inTrain <- createDataPartition(y=train_in$classe,p=0.7, list=FALSE) 
train_in.train <- train_in[inTrain,]
train_in.test <- train_in[-inTrain,]

# remove variables with nearly zero variance
nzv <- nearZeroVar(train_in.train)
train_in.train <- train_in.train[, -nzv]
train_in.test <- train_in.test[, -nzv]

# remove variables that are almost always NA
mostlyNA <- sapply(train_in.test, function(x) mean(is.na(x))) > 0.95
train_in.train <- train_in.train[, mostlyNA==F]
train_in.test <- train_in.test[, mostlyNA==F]

# remove variables that don't make intuitive sense for prediction (X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp), which happen to be the first five variables
trainData1 <- train_in.train[, -(1:5)]
trainData2 <- train_in.test[, -(1:5)]

#MODEL BUILDING
#Random_Forest Model
library(randomForest)
library(e1071)
controlRF <- trainControl(method="cv", number=3, verboseIter=FALSE)
modRF1 <- train(classe ~ ., data=trainData1, method="rf", trControl=controlRF)
modRF1$finalModel

predictRF1 <- predict(modRF1, newdata=trainData2)
cmrf <- confusionMatrix(predictRF1, trainData2$classe)
cmrf

sum(cmrf$predRight)/nrow(cmrf)

