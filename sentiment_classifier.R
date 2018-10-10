dataset_original = read.csv('/Users/sgangireddy/Downloads/Reviews.csv', stringsAsFactors = FALSE)
str(dataset_original)
table(dataset_original$Score)

#required columns
data_text<-dataset_original[,c('Summary','Score')]
str(data_text)

#score(encoding)
data_text$Score[data_text$Score==3]<-0
data_text$Score[data_text$Score==1 | data_text$Score==2]<--1
data_text$Score[data_text$Score==4 | data_text$Score==5]<-1
table(data_text$Score)
head(data_text)

#seperating reviews
neutral<-(data_text[which(data_text$Score==0), ])
Positive<-(data_text[which(data_text$Score==1), ])
negative<-(data_text[which(data_text$Score==-1), ])
str(Positive)
#downsampling positive and negative for class balance 
set.seed(123456)
index1 <- sample(1:nrow(Positive), 44377, replace=FALSE)
index2 <- sample(1:nrow(negative), 41019, replace=FALSE)
positive_reviews <- Positive[index1, ]
negative_reviews <- negative[index2, ]
str(negative_reviews)

data_bal<-rbind(positive_reviews,negative_reviews,neutral)
data_bal$Score<-as.factor(data_bal$Score)
table(data_bal$Score)

#pre processing
library(tm)
library(SnowballC)
corpus = VCorpus(VectorSource(data_bal$Summary))
#inspect(corpus[1:3])
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)
removeHashTags = function(x) gsub("#\\S+", "", x)
corpus = tm_map(corpus, content_transformer(removeHashTags))
removeHandles = function(x) gsub("@\\S+", "", x)
corpus = tm_map(corpus, content_transformer(removeHandles))
removeURL = function(x) gsub("http[[:alnum:]]*", "", x)
corpus = tm_map(corpus, content_transformer(removeURL))

#Tf-IDF
tfidf = DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
tfidf <- removeSparseTerms(tfidf, 0.999)
data_tf <- as.data.frame(as.matrix(tfidf))
data_tf$Score = as.factor(data_bal$Score)
table(data_tf$Score)

#document term matrix
#dt_m = DocumentTermMatrix(corpus)
#dt_m = removeSparseTerms(dt_m, 0.999)
#matrix_data = as.data.frame(as.matrix(dt_m))
#matrix_data$Score = as.factor(data_bal$Score)
#View(matrix_data[1:20,])
#table(matrix_data$Score)

#visualization plots
par(mfrow=c(1,2))
barplot(pl, main="Reviews", 
        xlab="Score",ylab="Number of Reviews",col="red",names.arg=c("Negative", "Neutral", "Postive"))
barplot(plb, main="Reviews", 
        xlab="Score",ylab="Number of Reviews",col="green",names.arg=c("Negative", "Neutral", "Postive"))
#wordcloud for reviews
freq <- colSums(as.matrix(tfidf))
freq1 <- colSums(as.matrix(neu_tfidf))
library(wordcloud)
par(mfrow=c(1,2))
wordcloud(names(freq),freq,min.freq = 1500,colors = brewer.pal(6, 'Dark2')) 
wordcloud(names(freq1),freq1,min.freq = 1500,colors = brewer.pal(6, 'Dark2')) 

#data sample 1(positive,negative and neutral reviews)
set.seed(123456)
index3 <- sample(1:nrow(data_tf),60000, replace=FALSE)
data_tfidf_matrix<-data_tf[index3, ]
table(data_tfidf_matrix$Score)
library(caret)
set.seed(123456)
index4 <- createDataPartition(data_tfidf_matrix$Score,p=0.80,list=FALSE)
tr1_tfidf<- data_tfidf_matrix[index4,]
tes1_tfidf<- data_tfidf_matrix[-index4,]
table(tr1_tfidf$Score)
table(tes1_tfidf$Score)
dim(tes1_tfidf)

#k-fold cross validation for choosing best model)
ctrl <- trainControl(method="repeatedcv",number = 10, repeats = 1)
cl <- trainControl(method="repeatedcv", number=10, repeats=1, search="grid")

#SVM
set.seed(123456)
svm.radial  <- train(Score ~ . , data=tr1_tfidf, trControl = ctrl, method = "svmRadial")
svm.radial.predict <- predict(svm.radial,newdata = tes1_tfidf)

##naive bayes
matrix_data_nb<-data_tf
matrix_data_nb[,1:476]<-lapply(data_tf[,1:476],factor)
str(matrix_data_nb)
set.seed(123456)
library(caret)
index_nb <- createDataPartition(matrix_data_nb$Score,p=0.75,list=FALSE)
training_nb<- matrix_data_nb[index_nb,]
testing_nb<- matrix_data_nb[-index_nb,]
library(e1071)
nb <- naiveBayes(training_nb, training_nb$Score)
nbPredict <- predict(nb, newdata = testing_nb[, -476])
caret::confusionMatrix(testing_nb$Score, nbPredict)

#Random forest
# Algorithm Tune (tuneRF)
set.seed(123456)
bestmtry <- tuneRF(tr1_tfidf[,-476], tr1_tfidf[,476], stepFactor=1.5, improve=1e-5, ntree=100)
print(bestmtry)

#grid search-random forest(parameter tuning)
set.seed(seed)
tunegrid <- expand.grid(.mtry=22)
rf_gridsearch <- train(Score~., data=tr1_tfidf, method="rf", metric="metric", tuneGrid=tunegrid, trControl=cl)
rf_grid_pred<-predict(rf_gridsearch,tes1_tfidf[,-476])
(ran_acc_grid <- 1-mean(rf_grid_pred != tes1_tfidf$Score))
print(rf_gridsearch)
plot(rf_gridsearch)

#Data sample 2(Positive and negative reviews)
#filtering neutral reviews
fil_neu<-subset(data_bal,(Score==1 | Score==-1))
str(fil_neu)
fil_neu$Score<-factor(fil_neu$Score)
table(fil_neu$Score)

library(tm)
library(SnowballC)
corpus1 = VCorpus(VectorSource(fil_neu$Summary))
#inspect(corpus[1:3])
corpus1 = tm_map(corpus1, content_transformer(tolower))
corpus1 = tm_map(corpus1, removeNumbers)
corpus1 = tm_map(corpus1, removePunctuation)
corpus1 = tm_map(corpus1, removeWords, stopwords())
corpus1 = tm_map(corpus1, stemDocument)
corpus1 = tm_map(corpus1, stripWhitespace)
removeHashTags1 = function(x) gsub("#\\S+", "", x)
corpus1 = tm_map(corpus1, content_transformer(removeHashTags1))
removeHandles1 = function(x) gsub("@\\S+", "", x)
corpus1 = tm_map(corpus1, content_transformer(removeHandles1))
removeURL1 = function(x) gsub("http[[:alnum:]]*", "", x)
corpus1 = tm_map(corpus1, content_transformer(removeURL1))

#Tf-IDF
neu_tfidf = DocumentTermMatrix(corpus1, control = list(weighting = weightTfIdf))
neu_tfidf <- removeSparseTerms(neu_tfidf, 0.999)
neu_tf <- as.data.frame(as.matrix(neu_tfidf))
neu_tf$Score = as.factor(fil_neu$Score)
table(neu_tf$Score)
dim(neu_tf)

#training and testing data
library(caret)
index <- createDataPartition(neu_tf$Score,p=0.75,list=FALSE)
tr_neu<- neu_tf[index,]
tes_neu<- neu_tf[-index,]
table(tr_neu$Score)

#grid search
library(caret)
cl <- trainControl(method="repeatedcv", number=10, repeats=1, search="grid")
set.seed(123456)
tunegrid <- expand.grid(.mtry=22)
rf_gridsearch <- train(Score~., data=tr_neu, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=cl)
rf_grid_pred<-predict(rf_gridsearch,tes_neu[,-471])
(ran_acc_neu <- 1-mean(rf_grid_pred != tes_neu$Score))
print(rf_gridsearch)
plot(rf_gridsearch)

#loaded model and saved for reference
#model(ntree=50)
model <- readRDS("/Users/sgangireddy/anusha_adm/mt14/tfidf_model_tr50_mt14_n10_r1.rds")
print(model)
pred_tfidf <- predict(model, tes1_tfidf)
confusionMatrix(pred_tfidf, tes1_tfidf$Score)
#print(predictions)

#mtry=22
model_22 <- readRDS("/Users/sgangireddy/anusha_adm/mt22/tfidf_model_tr50_mt22_n10_r1.rds")
print(model_22)
pred_tfidf_22 <- predict(model_22, tes1_tfidf)
confusionMatrix(pred_tfidf_22, tes1_tfidf$Score)
#print(predictions)

#neutral
model_neutral <- readRDS("/Users/sgangireddy/anusha_adm/tfidf_model_without_neutral_tr100_mt22_n10_r1.rds")
print(model_neutral)
pred_tfidf_neu <- predict(model_neutral, tes_neu)
confusionMatrix(pred_tfidf_neu, tes_neu$Score)

#AUC_ROCR
library(ModelMetrics)
auc(tes_neu$Score, pred_tfidf_neu)
library(ROCR)
pred_neu_num <- prediction(as.numeric(pred_tfidf_neu), as.numeric(tes_neu$Score))
perfom_roc <- performance(pred_neu_num, measure = "tpr", x.measure = "fpr")
plot(perfom_roc,main="ROC Curve")

#Results Interpretation
#confusion matrix of positive,negative reviews
library(ggplot2)
ggplot(data = as.data.frame(c$table), mapping = aes(x = Prediction, y = Reference))+
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f",Freq)), vjust = 1) +
  scale_fill_gradient(low = "White", high = "Blue",trans = "log") +
  ggtitle("Confusion Matrix of Postive and Negative Reviews") +
  labs(y = 'Observed Class', x='Predicted Class')

#confusion matrix of positive,negative reviews and neutral reviews
library(ggplot2)
ggplot(data = as.data.frame(cm$table), mapping = aes(x = Prediction, y = Reference))+
  geom_tile(aes(fill = Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f",Freq)), vjust = 1) +
  scale_fill_gradient(low = "White", high = "Blue",trans = "log") +
  ggtitle("Confusion Matrix of Postive, Negative and Neutral Reviews") +
  labs(y = 'Observed Class', x='Predicted Class')

#F-score(all reviews)
cm
sensitivity_1<-0.7666
sensitivity0<-0.5637
sensitivity1<-0.7562
specificity_1<-0.7866
specificity0<-0.8847
specificity1<-0.8735
F_score_negative<-(2*sensitivity_1*specificity_1)/(sensitivity_1+specificity_1)
F_score_neutral<-(2*sensitivity0*specificity0)/(sensitivity0+specificity0)
F_score_positive<-(2*sensitivity1*specificity1)/(sensitivity1+specificity1)
F_score_negative
F_score_neutral
F_score_positive

#F-score(positive and negative reviews)
c
sensitivity_data1<-0.8374
specificity_data1<-0.8735
F_score_positive_negative<-(2*sensitivity_data1*specificity_data1)/(sensitivity_data1+specificity_data1)
F_score_positive_negative
