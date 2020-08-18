######################
# Author Attribution #
######################
rm(list=ls())

library(tm) 
library(tidyverse)
library(slam)
library(proxy)

readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

## globbing
file_list = Sys.glob('~/MSBAsummer20/STA380-master/data/ReutersC50/*/*/*.txt')
full_set = lapply(file_list, readerPlain) 

# The file names
# commenting it out because it takes so long to run file_list

## get just the author names
mynames = file_list %>%
  { strsplit(., '/', fixed=TRUE) } %>%
  { lapply(., function(x) x[length(x) -1])} %>%
  unlist

# Rename the articles
mynames
names(full_set) = mynames

## text mining corpus
documents_raw = Corpus(VectorSource(full_set))

##pre-processing/tokenization 
my_documents = documents_raw %>%
  tm_map(content_transformer(tolower))  %>%             # make everything lowercase
  tm_map(content_transformer(removeNumbers)) %>%        # remove numbers
  tm_map(content_transformer(removePunctuation)) %>%    # remove punctuation
  tm_map(content_transformer(stripWhitespace))          # remove excess white-space


# remove the "SMART" stop words
my_documents = tm_map(my_documents, content_transformer(removeWords), stopwords("SMART"))


## create a doc-term-matrix from the corpus
DTM_full_set = DocumentTermMatrix(my_documents)
DTM_full_set

## You can inspect its entries...
inspect(DTM_full_set[1:10,1:20])

## ...find words with greater than a min count...
findFreqTerms(DTM_full_set, 50)


## drop those terms that only occur in one or two documents
DTM_full_set = removeSparseTerms(DTM_full_set, 0.95)
DTM_full_set

library(plyr)
DTM_test_clean = rbind.fill(common, all.train)

# construct TF IDF weights
tfidf_full_set = weightTfIdf(DTM_full_set)


####
# Dimensionality reduction
####

#  PCA on term frequencies
X = as.matrix(tfidf_full_set)
summary(colSums(X))
scrub_cols = which(colSums(X) == 0)
X = X[,-scrub_cols]

pca_full_set = prcomp(X, scale=TRUE, tol = .3)

summary(pca_full_set) 

# Look at the loadings
pca_full_set$rotation[order(abs(pca_full_set$rotation[,1]),decreasing=TRUE),1][1:25]
pca_full_set$rotation[order(abs(pca_full_set$rotation[,2]),decreasing=TRUE),2][1:25]


########
## random forest classification
##########


library(randomForest)

X_train = pca_full_set$x[2501:5000,]
y_train = {mynames[2501:5000]}

hundredths <- seq(from=min(X_train), to=max(X_train), by=.01)
WNF = sample(hundredths, size=2500, replace=TRUE)

train_data = data.frame(y_train,X_train,WNF)
train_data$y_train = factor(train_data$y_train)

X_test = pca_full_set$x[0:2500,]
y_test = {mynames[0:2500]}

test_data = data.frame(y_test,X_test,WNF)

test_data$y_test = factor(test_data$y_test)

# random forest
rffit = randomForest(x=DTM_train_df, 
                     y=as.factor(labels), 
                     importnace=TRUE, 
                     ntree=2000,
                     replace = TRUE)

print(rffit)

################ test data ##############

CM = table(predict(rffit,newdata = test_data),test_data$y_test)

accuracy = (sum(diag(CM)))/sum(CM)

accuracy

########################## KNN #######################

library(class)

knn.pred=knn(X_train,X_test, y_train,k=1)
table(knn.pred ,y_test)


mean(knn.pred==y_test)


############# naive-bayes ################
library(e1071)

nbmodel = naiveBayes(y_train~., train_data)
predict_nbmodel = table(predict(nbmodel,newdata = test_data),test_data$y_test)
accuracy = (sum(diag(predict_nbmodel)))/sum(predict_nbmodel)

accuracy

