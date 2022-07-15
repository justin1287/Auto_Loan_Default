
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(caret)
library(ROCR)
library(glmnet)
library(randomForest)
library(MLmetrics)
library(DT)

## Read in the data:
dictionary<-read.csv('Auto_Data_Dictionary.csv')
final_test<-read.csv('Auto_Test_Data.csv')
auto_train<-read.csv('Auto_Train_Data.csv')

lapply(auto_train,class)

## Change NAs to 0s and Impute Blanks With Mean of Column:
auto_train[is.na(auto_train)]<-0
auto_train[auto_train=='']<-NA
auto_train[,c(2,8,9,17:21,30,36)]<-sapply(auto_train[,c(2,8,9,17:21,30,36)],as.numeric)
auto_train<-auto_train %>% mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))
auto_train<-auto_train %>% mutate_if(is.character, ~replace_na(.,'Unreported'))
auto_train$Population_Region_Relative<-ifelse(auto_train$Population_Region_Relative==100,median(auto_train$Population_Region_Relative),auto_train$Population_Region_Relative)
auto_train$Employed_Days<-ifelse(auto_train$Employed_Days>18000,median(auto_train$Employed_Days),auto_train$Employed_Days)

## Change Character Columns into Factors:
auto_train$Accompany_Client<-ifelse(auto_train$Accompany_Client=='Unreported'|auto_train$Accompany_Client=='##','Others',auto_train$Accompany_Client)
auto_train$Client_Gender<-ifelse(auto_train$Client_Gender=='XNA','Unreported',auto_train$Client_Gender)
auto_train[,c(3:6,10:16,23:26,28:33,40)]<-lapply(auto_train[,c(3:6,10:16,23:26,28:33,40)],as.factor)

## EDA:
(sum(auto_train$Default==1)/nrow(auto_train))*100
# Roughly 8% of the data points defaulted on their car loan
# Default vs no default:
ggplot(data=auto_train,aes(x=Default,fill=Default))+geom_bar(alpha=0.7)+
  labs(x='Loan Default',y='Count',title='Frequency Of Auto Loan Default')+scale_x_discrete(labels=c('No','Yes'))+
  theme(legend.position = 'none')+stat_count(aes(label = ..count..), geom = "label",show.legend = F)
# Male default vs female default:
ggplot(data = auto_train,aes(x=Client_Gender,fill=Default))+geom_bar(alpha=0.7)+stat_count(aes(label = ..count..), geom = "label",show.legend = F)+
  scale_x_discrete(name='Sex',labels=c('Female','Male','Unreported'))+labs(y='Count',title='Frequency of Default by Sex')+
  scale_fill_discrete(name='Loan Default',labels=c('No','Yes'))
# Roughly 10% of females, 7% of males, and 8% of unreported individuals defaulted
# Default by loan type:
ggplot(data = auto_train,aes(x=Loan_Contract_Type,fill=Default))+geom_bar(alpha=0.7)+stat_count(aes(label = ..count..), geom = "label",show.legend = F)+
  scale_x_discrete(name='Loan Contract Type',labels=c('Cash Loan','Revolving Loan','Unreported'))+labs(y='Count',title='Frequency of Default by Loan Type')+
  scale_fill_discrete(name='Loan Default',labels=c('No','Yes'))
# Roughly 8% of cash loans, 6% of revolving loans, and 8% of unreported loan type defaulted
# Default by City Rating:
ggplot(data = auto_train,aes(x=Cleint_City_Rating,fill=Default))+geom_bar(alpha=0.7)+stat_count(aes(label = ..count..), geom = "label",show.legend = F)+
  scale_x_discrete(name='City Rating',labels=c('Unreported','Average','Good','Best'))+labs(y='Count',title='Frequency of Default by City Rating')+
  scale_fill_discrete(name='Loan Default',labels=c('No','Yes'))
# Roughly 8% of unreported, 5% of average, 8% of good, and 11% of best city ratings defaulted
# Default by Permanent Address Match:
ggplot(data = auto_train,aes(x=Client_Permanent_Match_Tag,fill=Default))+geom_bar(alpha=0.7)+stat_count(aes(label = ..count..), geom = "label",show.legend = F)+
  scale_x_discrete(name='Contact Address Matches Permanent Address',labels=c('No','Yes'))+labs(y='Count',title='Frequency of Default by Address Matching')+
  scale_fill_discrete(name='Loan Default',labels=c('No','Yes'))
# Roughly 12% with mismatched addresses and 8% with matching addresses defaulted
# Default by previous car ownership status:
ggplot(data = auto_train,aes(x=Car_Owned,fill=Default))+geom_bar(alpha=0.7)+stat_count(aes(label = ..count..), geom = "label",show.legend = F)+
  scale_x_discrete(name='Previous/Current Car Owner',labels=c('No','Yes'))+labs(y='Count',title='Frequency of Default by Car Ownership Status')+
  scale_fill_discrete(name='Loan Default',labels=c('No','Yes'))
# Roughly 8.5% with no ownership and 7% with ownership defaulted
# Density plot of loan amount:
ggplot(data=auto_train,aes(x=Credit_Amount,fill=Default))+geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=mean(Credit_Amount[Default==1])),col='#00BFC4',linetype='dashed')+
  geom_vline(aes(xintercept=mean(Credit_Amount[Default==0])),col='#F8766D',linetype='dashed')+
  labs(x='Loan Amount',y='Density',title='Distribution of Loan Amount by Default Status')+
  scale_fill_discrete(name='Loan Default',labels=c('No','Yes'))
# Density plot of client age:
ggplot(data=auto_train,aes(x=Age_Days,fill=Default))+geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=mean(Age_Days[Default==1])),col='#00BFC4',linetype='dashed')+
  geom_vline(aes(xintercept=mean(Age_Days[Default==0])),col='#F8766D',linetype='dashed')+
  labs(x='Client Age in Days',y='Density',title='Distribution of Client Age by Default Status')+
  scale_fill_discrete(name='Loan Default',labels=c('No','Yes'))
# Density plot of days since ID change:
ggplot(data=auto_train,aes(x=ID_Days,fill=Default))+geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=mean(ID_Days[Default==1])),col='#00BFC4',linetype='dashed')+
  geom_vline(aes(xintercept=mean(ID_Days[Default==0])),col='#F8766D',linetype='dashed')+
  labs(x='Days Before Application in Which ID Was Altered',y='Density',title='Distribution of Days Since ID Change by Default Status')+
  scale_fill_discrete(name='Loan Default',labels=c('No','Yes'))
# Density plot of number of employed days:
ggplot(data=auto_train,aes(x=Employed_Days,fill=Default))+geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=mean(Employed_Days[Default==1])),col='#00BFC4',linetype='dashed')+
  geom_vline(aes(xintercept=mean(Employed_Days[Default==0])),col='#F8766D',linetype='dashed')+
  labs(x='Number of Employed Days Prior To Application',y='Density',title='Distribution of Employment Time by Default Status')+
  scale_fill_discrete(name='Loan Default',labels=c('No','Yes'))

# Correlation plot of numeric variables:
corrplot(cor(auto_train[c(2,8,9,17:22,27,34:39)]))


####### Begin Modeling ##########

## Randomly divide data into 75/25 train/test split:
smp_size <- floor(0.75 * nrow(auto_train))
set.seed(123)
train_index <- sample(seq_len(nrow(auto_train)), size = smp_size)
training <- auto_train[train_index, ]
testing <- auto_train[-train_index, ]

## Logistic Regression Model:
log.mod <- glm(Default ~.-ID-Type_Organization,family=binomial(link='logit'),data=training)
summary(log.mod)
log.predict <- predict(log.mod,newdata=testing,type='response')
log.predict <- ifelse(log.predict > 0.5,1,0)
log.error <- mean(log.predict != testing$Default)
logistic.accuracy<- 1 - log.error
log.conufsion<-confusionMatrix(as.factor(log.predict),testing$Default,mode = 'everything',positive = '1')
log.conufsion
# Logistic regression predicts with roughly 92.15% accuracy on testing subset

## Logistic Regression With Lasso Penalty:
x.train<-data.matrix(training[,c(2:32,34:39)])
y.train<-as.numeric(training$Default)
cv_model<-cv.glmnet(x.train, y.train, alpha = 1)
best_lambda<-cv_model$lambda.1se
plot(cv_model)
lasso.mod<-glmnet(x.train, y.train, alpha = 1, lambda = best_lambda)
coef(lasso.mod)
x.test<-data.matrix(testing[,c(2:32,34:39)])
lasso.predict<-predict(lasso.mod,newx=x.test)
lasso.predict<-ifelse(lasso.predict>1.02,0,1)
lasso.error<-mean(lasso.predict!=testing$Default)
lasso.accuracy<-1-lasso.error
lasso.confusion<-confusionMatrix(as.factor(lasso.predict),testing$Default,mode='everything',positive = '1')
lasso.confusion
# Lasso regression predicts with roughly 88.01% accuracy on testing subset (Depends on threshold used for prediction)

## Logistic Regression With Ridge Penalty:
ridge.mod<-glmnet(x.train, y.train, alpha = 0, lambda = best_lambda)
coef(ridge.mod)
ridge.predict<-predict(ridge.mod,newx=x.test,type='response')
ridge.predict<-ifelse(ridge.predict>1.02,0,1)
ridge.error<-mean(ridge.predict!=testing$Default)
ridge.accuracy<-1-ridge.error
ridge.confusion<-confusionMatrix(as.factor(ridge.predict),testing$Default,mode = 'everything',positive = '1')
ridge.confusion
# Ridge regression predicts with roughly 79.35% accuracy on testing subset (Depends on threshold used for prediction)

## Random Forest Model:
forest.mod<-randomForest(Default~.-ID-Type_Organization,data=training,ntree=500,keep.forest=T)
varImpPlot(forest.mod,main='Variable Importance According to Random Forest')
forest.predict<-predict(forest.mod,newdata = testing,type='class')
forest.error <- mean(forest.predict!= testing$Default)
forest.accuracy<-1-forest.error
forest.confusion<-confusionMatrix(as.factor(forest.predict),testing$Default,mode = 'everything',positive = '1')
forest.confusion
# Random forest predicts with roughly 93.04% accuracy on testing subset

## Report ROC Curves
log.prediction<-prediction(predict(log.mod,newdata=testing,type='response'),testing$Default)
log.performance<-performance(log.prediction,'tpr','fpr')
log.auc<-as.numeric(performance(log.prediction,'auc')@y.values)
# AUC for logistic model is roughly 73.08%
lasso.prediction<-prediction(predict(lasso.mod,newx=x.test,type='response'),testing$Default)
lasso.performance<-performance(lasso.prediction,'tpr','fpr')
lasso.auc<-as.numeric(performance(lasso.prediction,'auc')@y.values)
# AUC for lasso model is roughly 69.78%
ridge.prediction<-prediction(predict(ridge.mod,newx=x.test,type='response'),testing$Default)
ridge.performance<-performance(ridge.prediction,'tpr','fpr')
ridge.auc<-as.numeric(performance(ridge.prediction,'auc')@y.values)
# AUC for ridge model is roughly 71.73%
forest.prediction<-prediction(predict(forest.mod,newdata=testing,type='prob')[,2],testing$Default)
forest.performance<-performance(forest.prediction,'tpr','fpr')
forest.auc<-as.numeric(performance(forest.prediction,'auc')@y.values)
# AUC for random forest model is roughly 77.94%
plot(log.performance,col='orange',lwd=1.75,xlab='False Positive Rate (1 - Specificity)',
     ylab='True Positive Rate (Sensitivity)',main='Receiver Operating Characteristic (ROC) Curves')
abline(0,1,lty=2)
plot(lasso.performance,add=T,col='red',lwd=1.75)
plot(ridge.performance,add=T,col='blue',lwd=1.75)
plot(forest.performance,add=T,col='dark green',lwd=1.75)
legend('bottomright',legend=c('Logistic Regression','Lasso Regression','Ridge Regression','Random Forest',
      'Reference Random Classifier'),col=c('orange','red','blue','dark green','black'),lty=c(1,1,1,1,2))

## Computing summary statistics for each of the models:
# MAE:
testing.default<-ifelse(as.numeric(testing$Default)==1,0,1)
log.mae<-MAE(as.numeric(log.predict),testing.default)
lasso.mae<-MAE(as.numeric(lasso.predict),testing.default)
ridge.mae<-MAE(as.numeric(ridge.predict),testing.default)
forest.mae<-MAE(as.numeric(forest.predict),as.numeric(testing$Default))
# RMSE:
log.rmse<-RMSE(as.numeric(log.predict),testing.default)
lasso.rmse<-RMSE(as.numeric(lasso.predict),testing.default)
ridge.rmse<-RMSE(as.numeric(ridge.predict),testing.default)
forest.rmse<-RMSE(as.numeric(forest.predict),as.numeric(testing$Default))
# Summary Table:
summary<-t(rbind(as.table(c(log.mae,lasso.mae,ridge.mae,forest.mae)),
as.table(c(log.rmse,lasso.rmse,ridge.rmse,forest.rmse)),
as.table(c(log.auc,lasso.auc,ridge.auc,forest.auc))))
colnames(summary)<-c('MAE','RMSE','AUC')
rownames(summary)<-c('Logistic Regression','Lasso Regression','Ridge Regression','Random Forest')
datatable(summary)

## According to all recorded metrics, random forest is the best classification method for this dataset

######## Applying best model to final test dataset: #######

## Cleaning data:
final_test[is.na(final_test)]<-0
final_test[final_test=='']<-NA
final_test[,c(2,8,9,17:21,30,36)]<-sapply(final_test[,c(2,8,9,17:21,30,36)],as.numeric)
final_test<-final_test %>% mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))
final_test<-final_test %>% mutate_if(is.character, ~replace_na(.,'Unreported'))
final_test$Population_Region_Relative<-ifelse(final_test$Population_Region_Relative==100,median(final_test$Population_Region_Relative),final_test$Population_Region_Relative)
final_test$Employed_Days<-ifelse(final_test$Employed_Days>18000,median(final_test$Employed_Days),final_test$Employed_Days)
final_test$Accompany_Client<-ifelse(final_test$Accompany_Client=='Unreported'|final_test$Accompany_Client=='##','Others',final_test$Accompany_Client)
final_test$Client_Gender<-ifelse(final_test$Client_Gender=='XNA','Unreported',final_test$Client_Gender)
final_test$Client_Education<-ifelse(as.character(final_test$Client_Education)=='junior secondary','Junior secondary',final_test$Client_Education)
final_test$Client_Marital_Status<-ifelse(as.character(final_test$Client_Marital_Status)=='Unknown','Unreported',final_test$Client_Marital_Status)
final_test$Application_Process_Hour<-ifelse(final_test$Application_Process_Hour>12&final_test$Application_Process_Hour<13,12.063100183598,final_test$Application_Process_Hour)
final_test[,c(3:6,10:16,23:26,28:33)]<-lapply(final_test[,c(3:6,10:16,23:26,28:33)],as.factor)
final_test$Score_Source_2<-as.numeric(ifelse(final_test$Score_Source_2=='Unreported',0,final_test$Score_Source_2))
final_test$Score_Source_2<-as.numeric(ifelse(is.na(final_test$Score_Source_2)==T,0,final_test$Score_Source_2))
final_test$Score_Source_2<-as.numeric(ifelse(final_test$Score_Source_2==0,median(final_test$Score_Source_2),final_test$Score_Source_2))
final_test$Score_Source_1<-as.numeric(ifelse(final_test$Score_Source_1==0,mean(final_test$Score_Source_1),final_test$Score_Source_1))
final_test <- rbind(training[1,-40 ] , final_test)
final_test <- final_test[-1,]

## Apply random forest model for prediction:
final.predict<-predict(forest.mod,newdata = final_test,type='class')
final.predictions<-cbind(final_test$ID,final.predict)
final.predictions[,2]<-ifelse(final.predictions[,2]==2,'DEFAULT','No')
colnames(final.predictions)<-c('ID','Default Prediction')
final.predictions<-as.data.frame(final.predictions)
table(final.predictions$`Default Prediction`)
# 712 individuals are predicted to default and should not be given an auto loan


