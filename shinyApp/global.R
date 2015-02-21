## Shiny global.R file for Coursera Developing Data Products project
## Author : Jamie Harwood
## Date : 21/02/2015

library(tm)  
library(XML)
library(wordcloud)
library(randomForest)
library(RCurl)
library(rCharts)
## Load pre-prepared training related data and model - generation code available on github
load(file = "trainingTerms.RData")
load(file = "fit.RData")
load(file = "courseTermFreqs.RData")
load(file = "longWords.RData")
load(file = "courseModelFreqs.RData")
doco <- scan(file="doco.txt", what="character")
## Take a url and generate a corpus from the terms in the destination web page
getUserCorpus <- function(url){


  ## note this line is so much neater but loading the tm.plugin.webmining library seems to crash Shiny
  ##text <- extractHTMLStrip(url, asText = F, encoding = "UTF-8")
  ## So we use XML library and xpath
  htmlRaw = htmlTreeParse(url,
           useInternal = TRUE)
  text= unlist(xpathApply(htmlRaw, '//*/text()', xmlValue))
  text = gsub("\\n", " ", text)
  text = paste(text, collapse = " ")
  
  userCorpus <- VCorpus(VectorSource(text))
  userCorpus <- tm_map(userCorpus, removePunctuation)
  userCorpus <- tm_map(userCorpus, removeNumbers)
  userCorpus <- tm_map(userCorpus, content_transformer(tolower))
  userCorpus <- tm_map(userCorpus, removeWords, stopwords("en"))
  userCorpus <- tm_map(userCorpus, removeWords, c())
 
  userCorpus
}

## Create a dtm from corpus generated from the web page we downloaded and find those terms
## that match terms we have in our model.  Predict this data using our pre-prepared model
## which we loaded from file above
prepareData <- function(userCorpus){
  
  userDtm <- DocumentTermMatrix(userCorpus)
  userTerms <- as.data.frame(as.matrix(userDtm))
  userObs <- unlist(lapply(trainingTerms, FUN = function(term){
    if(term %in% names(userTerms)){
         userTerms[1,term]
    }
    else{
         NA
    }
  }))
  userObs <- as.data.frame(rbind(userObs))
  userObs[is.na(userObs)] <- 0
  names(userObs) <- paste0("V_",trainingTerms)
  userObs
  
}

## Predict using user observations and return the prediction
getPrediction <- function(userObs){
  
  pred <- predict(fit, newdata= userObs)
  as.character(pred)
  
  
}

## Get the probability for the prediction
## note we also do a sanity check on the number of terms
## to spot non data science content
getProbability <- function(userObs){
  
  magicNumber <-30
  if(sum(userObs[1,] >= 3) < magicNumber){
    0
  }
  else{
    prob <- predict(fit, newdata = userObs, type= "prob")
    max(prob)
  }
  
}


## Generate term frequency data from the  web page we downloaded
getUserTermFreqs <- function(userCorpus){
  
  tdm <- TermDocumentMatrix(userCorpus)
  m <- as.matrix(tdm)
  ## remove some stubborn html tags that xpath didn't get rid of and long words that are probably corruptions
  longWords <- unlist(lapply(rownames(m), FUN = function(x){
    
    if(nchar(x, type = "bytes") >= 12)
      x
  }))
  cutWords <- c(longWords, c("span", "pre","div","classbr", "classrbr", "alignright", "hreftoptopp", "classrxspan", "classrspan"))
  m <- as.matrix(m[!rownames(m) %in% cutWords,])
 
  termFreqs = sort(rowSums(m), decreasing = TRUE)
  termFreqs
  
}

## Get the frequencies for terms in the user supplied content that are 
## terms used in the model
getUserModelFreqs <- function(userCorpus){
  
  ## x is full a corpus
  tdm <- TermDocumentMatrix(userCorpus)
  m <- as.matrix(tdm)
  df <- as.data.frame(m)
  df$trainingTerms <- row.names(m)
  tt <- as.data.frame(trainingTerms)
  out <- as.data.frame( merge(x = tt, y = df, by.x = "trainingTerms", by.y = "trainingTerms", all.x = T))
  out[is.na(out[2]),2] <- 0
  out
  
}

