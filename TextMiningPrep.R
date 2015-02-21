## TextMiningPrep.R file for Coursera Developing Data Products project
## Author : Jamie Harwood
## Date : 21/02/2015

library(stringr)
library(tm)
library(caret)
library(randomForest)
library(doMC)
registerDoMC(cores=4)
set.seed(4321)

## Get all pdfs from downloaded course materials
rmds<- dir(path=".", pattern="*\\.md", recursive=T)
## Get a list of course names from file paths
courseNames <- str_match(rmds, "/\\d\\d_(.+?)/")[,2]

## Create a temporary corpus from this data
corpus <- VCorpus(DirSource(
  directory="/home/jamie/DataProductsProject/courses", pattern = ".*\\.md", recursive = T ))
## turn into data frame that contains the content from each lecture as text
## with the course name the lecture belongs to (will become training outcome)
dataDf <- data.frame(t(sapply(corpus, FUN=function(x){
  
 c(x$meta$id, paste(x$content, collapse = " "))
  
})))
dataDf$X1 <- courseNames
dataDf$X2 <- as.character(dataDf$X2)

## Recreate corpus using this data frame
corpus <- VCorpus(DataframeSource(dataDf))
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, content_transformer(tolower))
## Lets not stem
#corpus <- tm_map(corpus, stemDocument, language="english")
dtm <- DocumentTermMatrix(corpus)
m <- as.data.frame(as.matrix(dtm))

## We now have a matrix with frequencies for every term against each of the lectures


## user near zero values to reduce size of data set
inTrain <- createDataPartition(courseNames, p = 0.75, list=F)
training <- m[inTrain,]
trainingOutcomes <- courseNames[inTrain]
testingOutcomes <- courseNames[-inTrain]
trainingNzv <- m[inTrain,]
nzv <- nearZeroVar(training)
trainingNzv <- trainingNzv[,-nzv]
trainingNzv <- as.data.frame(trainingNzv)
trainingTerms <- names(trainingNzv)
names(trainingNzv) <- paste0("V_",names(trainingNzv[seq(1,length(trainingNzv))]))
trainingNzv$Y_outcome <- as.factor(trainingOutcomes)

testingNzv <- m[-inTrain,]
testingNzv <- testingNzv[,-nzv]
testingNzv <- as.data.frame(testingNzv)
names(testingNzv) <- paste0("V_",names(testingNzv[seq(1,length(testingNzv))]))
testingNzv$Y_outcome <- as.factor(testingOutcomes)

fitNzv <- randomForest(Y_outcome ~ ., data = trainingNzv)
#fitPca <- train(Y_outcome ~ ., method = "rf", data =  trainingNzv)
predNzv <- predict(fitPca, newdata = testingNzv, method = "prob")

confusionMatrix(predNzv, testingNzv$Y_outcome)

##Good accuracy lets use the same method to train on all the data

trainingFull <- as.data.frame(training[, -nzv])
trainingTerms <- names(trainingFull)
names(trainingFull) <- paste0("V_",names(trainingFull[seq(1,length(trainingFull))]))
trainingFull$Y_outcome <- as.factor(trainingOutcomes)

fit<- randomForest(Y_outcome ~ ., data = trainingFull)
fit

save(fit, file = "shinyApp/fit.RData")
save(trainingTerms, file="shinyApp/trainingTerms.RData")


## calculate trainingFrequencies

## aggregate by course
byCourse <- aggregate(X2 ~ X1, data=dataDf, FUN=paste, collapse = " ")
## remove some common words prevalent in course material
customStopWords <- read.csv(file="stop.csv", header = F, stringsAsFactors = F)
## Generate a corpus for each course and return the term/frequencies for each course 
courseCorpora<- lapply(byCourse$X1, FUN=function(x){

lcorpus <- VCorpus(VectorSource(byCourse[byCourse$X1 == x,2]))

  lcorpus <- tm_map(lcorpus, removePunctuation)
  lcorpus <- tm_map(lcorpus, removeNumbers)
  lcorpus <- tm_map(lcorpus, content_transformer(tolower))
  lcorpus <- tm_map(lcorpus, removeWords, stopwords("en"))
  lcorpus <- tm_map(lcorpus, removeWords, customStopWords$V1)
  tdm <- TermDocumentMatrix(lcorpus)
  m <- as.matrix(tdm)
  m
  })
## Generate top 200 terms by course
courseTermFreqs <- lapply(courseCorpora, FUN = function(x){
  
  termFreqs = head(sort(rowMeans(x), decreasing = TRUE), 200)

  
  
})

names(courseTermFreqs) <- byCourse$X1
names(courseCorpora) <- byCourse$X1
##write out termFreqs to file for shiny app to use
save(courseTermFreqs, file="shinyApp/courseTermFreqs.RData")

## Now work out freqs for terms in model, not just top 200

courseModelFreqs <- lapply(names(courseCorpora), FUN = function(x){
  
  df <- as.data.frame(courseCorpora[x])
  df$trainingTerms <- row.names(df)
  tt <- as.data.frame(trainingTerms)
  out <- as.data.frame( merge(x = tt, y = df, by.x = "trainingTerms", by.y = "trainingTerms", all.x = T))
  out[is.na(out[2]),2] <- 0
  out$source <- rep(x, nrow(out))
  names(out) <- c("terms", "frequency", "source")
  out
})

## save it
names(courseModelFreqs) <- byCourse$X1
save(courseModelFreqs, file = "shinyApp/courseModelFreqs.RData")





# 
# 
# 
# 
# 
# 
# 
# 
# inTrain <- createDataPartition(courseNames, p = 0.75, list=F)
# training <- m[inTrain,]
# 
# ##TODO - better in dplyr?
# zeroCounts <- aggregate(values~ind, stack(training), FUN=function(x) {sum(x==0)})
# 
# #training <- training[,zeroCounts$values <= 160]
# 
# ##We can't scale as we will have variables in testing with zero for all observations :()
# # training <- scale(training)
# ## remove nearZeroVar cols
# #training <- training[,-nearZeroVar(training)]
# ## create training outcomes
# trainingOutcomes <- courseNames[inTrain]
# training <- as.data.frame(cbind(outcome=trainingOutcomes, training), stringsAsFactors = T)
# trainingTerms <- names(training)
# ## save this terms to file
# save(trainingTerms, file="shinyApp/trainingTerms.RData")
# names(training) <- c("outcome", paste0("V_",names(training[seq(2,length(training))])))
# 
# 
# library(caret)
# fit <- train(outcome ~ ., method = "rf", data =  training)
# ## save the model
# #fit <- train(outcome ~ ., method="rf", data=training)
# 
# ## Now prep test data
# 
# testing <- m[-inTrain,]
# 
# 
# ## create testing outcomes
# testingOutcomes <- courseNames[-inTrain]
# testing <- as.data.frame(cbind(outcome=testingOutcomes, testing), stringsAsFactors = T)
# names(testing) <- c("outcome", paste0("V_",names(testing[seq(2,length(testing))])))
# #remove columns not in training
# testing <- testing[,names(training)]
# ## predict
# 
# pred <- predict(fit, newdata = testing)
# 
# confusionMatrix(pred, testing$outcome)
# 
# 
# ## Great accuracy but too many vars making prediction to slow for a web app rethink 1:
# 
# ## Reduce by looking at importance of vars
# imp <- importance(fit$finalModel)
# ord <- order(imp, decreasing = T)
# head(imp[ord,], n=500)
# ## lots of zeros there, cut them all out
# 
# zeros <- as.matrix(imp[imp != 0,])
# 
# trainingSmall <- training[, c("outcome", row.names(zeros))]
# testingSmall <- testing[, c("outcome", row.names(zeros))]
# 
# fitSmall <- train(outcome ~ ., method = "rf", data =  trainingSmall)
# 
# predSmall <- predict(fit, newdata = testing)
# 
# confusionMatrix(predSmall, testing$outcome)
# 
# ## We just want the final model to keep file size down
# fitSmall <- fit$finalModel
# save(fitSmall, file="shinyApp/fitSmall.RData")
# ## looks the same but performs terribly on real data in app - rethink 2:


