library(rpart)
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
org_testing = read.csv("./pml-testing.csv",na.strings=c("NA","#DIV/0!",""))
org_training = read.csv("./pml-training.csv",na.strings=c("NA","#DIV/0!",""))
inTrain = createDataPartition(org_training$classe, p = 3/4, list=FALSE)
training = org_training[ inTrain,]
testing = org_training[ -inTrain,]
#zvars <- nearZeroVar(training, saveMetrics=TRUE)#remove meaningless 
zvars <- nearZeroVar(training)#find  meaningless 
training <- training[,-zvars] #remove meaningless
training <- training[c(-1)] #remove id col
trainingTemp <- training
for(i in 1:length(training)) { #for every column in the training dataset
  if( sum( is.na( training[, i] ) ) /nrow(training) >= .6 ) { 
    for(j in 1:length(trainingTemp)) {
      if( length( grep(names(training[i]), names(trainingTemp)[j]) ) ==1)  { #if the columns are the same:
        trainingTemp <- trainingTemp[ , -j] #Remove that column
      }   
    } 
  }
}
training<-trainingTemp
clean_cols <- colnames(training)
clean_cols_no_classe<-clean_cols[-which(clean_cols %in% c("classe"))]
org_testing<-org_testing[,clean_cols_no_classe]
testing <- testing[,clean_cols]



for (i in 1:length(org_testing) ) {
  for(j in 1:length(training)) {
    if (names(org_testing[i])==names(training[j]))
    {
      class(org_testing[i]) <- class(training[j])
    }
    #if( length( grep(names(training[i]), names(names(training[2]))[j]) ) ==1)  {
    #  class(org_testing[j]) <- class(training[i])
    #}      
  }      
}
common <- intersect(names(training), names(org_testing))
for (p in common)
{ 
  if (class(training[[p]]) == "factor") 
  { 
    levels(org_testing[[p]]) <- levels(training[[p]]) 
  } 
} 

regressionTreeModel <- rpart(classe ~ ., data=training, method="class")
regressionTreePrediction <- predict(regressionTreeModel, testing, type = "class")
confusionMatrix(regressionTreePrediction, testing$classe)

randomForestModel <- randomForest(classe ~. , data=training,na.action=na.exclude)
randomForestPrediction <- predict(randomForestModel, testing, type = "class")
confusionMatrix(randomForestPrediction, testing$classe)
predictionsB2 <- predict(modFitB1, org_testing, type = "class")




