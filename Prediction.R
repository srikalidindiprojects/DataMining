#Loading the data into DataFrame
Data <- read.csv2('C:/Users/Sri Kalidindi/Documents/DataMiningProject/DataMiningProject/Data/data_flights.csv', sep=",", header=TRUE, stringsAsFactors = FALSE)
print("Number of Rows",nrow(Data))

airports <-c('ATL','LAX',  'JFK', 'SFO',  'LAS', 'PHX')
Data <- subset(Data, DEST %in% airports & ORIGIN %in% airports)

print("Number of Rows after selecting Airports",nrow(Data))

#	----- 1. Data Preparation ------
head(Data,5)
tail(Data,5) # Removing useless X column
origData$X <- NULL # remove X column

#Dropping similar columns
Data$ORIGIN_AIRPORT_SEQ_ID <- NULL # dropping ORIGIN_AIRPORT_SEQ_ID
Data$DEST_AIRPORT_SEQ_ID <- NULL # dropping DEST_AIRPORT_SEQ_ID
origData$UNIQUE_CARRIER <- NULL #No mismatches Removing UNIQUE_CARRIER

#Removing null values
Data_OnTime <- Data[!is.na(Data$ARR_DEL15) & Data$ARR_DEL15!="" & !is.na(Data$DEP_DEL15) & Data$DEP_DEL15!="",]

#Data type convertions
Data_OnTime$DISTANCE <- as.integer(Data_OnTime$DISTANCE)
Data_OnTime$CANCELLED <- as.integer(Data_OnTime$CANCELLED)
Data_OnTime$DIVERTED <- as.integer(Data_OnTime$DIVERTED)

Data_OnTime$ARR_DEL15 <- as.factor(Data_OnTime$ARR_DEL15)
Data_OnTime$DEP_DEL15 <-as.factor(Data_OnTime$DEP_DEL15)
Data_OnTime$DEST_AIRPORT_ID <- as.factor(Data_OnTime$DEST_AIRPORT_ID)
Data_OnTime$ORIGIN_AIRPORT_ID <- as.factor(Data_OnTime$ORIGIN_AIRPORT_ID)
Data_OnTime$DAY_OF_WEEK <- as.factor(Data_OnTime$DAY_OF_WEEK)
Data_OnTime$DEST <- as.factor(Data_OnTime$DEST)
Data_OnTime$ORIGIN <- as.factor(Data_OnTime$ORIGIN)
Data_OnTime$DEP_TIME_BLK <- as.factor(Data_OnTime$DEP_TIME_BLK)
Data_OnTime$CARRIER <- as.factor(Data_OnTime$CARRIER)

# Using tapply to verify arrival delayed-non-delayed flights and departure delayed-non-delayed flights
tapply(Data_OnTime$ARR_DEL15, Data_OnTime$ARR_DEL15, length)
tapply(Data_OnTime$DEP_DEL15, Data_OnTime$DEP_DEL15, length)

#	----- 2. Training algorithms -----
library(caret) # load caret

set.seed(122515) # set random # seed

# selecting columns
featurecolumns <- c("ARR_DEL15", "DAY_OF_WEEK", "CARRIER", "DEST","ORIGIN","DEP_TIME_BLK")

Data_onTime_Filtered <- Data_OnTime[,featurecolumns] #Filter version of data
TrainingRows <- createDataPartition(Data_onTime_Filtered$ARR_DEL15, p=0.70, list=FALSE)


FilteredTrainData <- Data_onTime_Filtered[TrainingRows,] # Training DataFrame
FilteredTestData <- Data_onTime_Filtered[-TrainingRows,] # Testing DataFrame 

#Logistic Regression

LogisticRegressionModel <- train(ARR_DEL15 ~ ., data=FilteredTrainData, method="glm", family="binomial",
                                 trControl=trainControl(method="cv", number=10, repeats=10))
print(LogisticRegressionModel$modelInfo)

# Prediction
logisticRegressionPrediction <- predict(LogisticRegressionModel, FilteredTestData)

# Confusion matrix to get stats of prediction vs. actual
logRegConfMat <- confusionMatrix(logisticRegressionPrediction, FilteredTestData[,"ARR_DEL15"])
logRegConfMat


LogisticRegressionModel$resample

#Random Forest
library(randomForest) # load library into current session
rfModel <- randomForest(FilteredTrainData[-1], FilteredTrainData$ARR_DEL15, proximity = TRUE, importance = TRUE)
rfModel
rfValidation <- predict(rfModel, FilteredTestData)

# Confusion matrix to get stats of prediction vs. actual
rfConfMat <- confusionMatrix(rfValidation, FilteredTestData[,"ARR_DEL15"])
rfConfMat

