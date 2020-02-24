library(readxl)
f<-read_excel(choose.files())
attach(f)
library(mlr)
f_dummy<-createDummyFeatures(f)
#unique(marital)
#class(marital)
#unique(education)
#unique(job)
#unique(default)
#unique(housing)
#unique(loan)
#unique(contact)
#unique(poutcome)
#unique(y)
#f$loan<-ifelse(f$loan=='yes',1,0)
#f$housing<-ifelse(f$housing=='yes',1,0)
f$y<-ifelse(f$y=='yes',1,0)
#f$default<-ifelse(f$default=='yes',1,0)
#i<-0
#for(i in 1:45211){
#  if(f$marital[i]=='married'){
#}
#  else-if(f$marital[i]=='single'){
#      f$marital[i]<-1
#  }
#  else{
#      f$marital[i]<-2
#  }
#}
#for(i in 1:45211){
#  if(f$contact[i]=='unknown'){
#    f$contact[i]<-0
#  }
#  else-if(f$contact[i]=='cellular'){
#    f$contact[i]<-1
#  }
#  else{
#    f$contact[i]<-2
#  }
#}
model_1<-glm(y~.,family = "binomial",data=f)
summary(model_1)
y_predicted<-predict(model_1,f,type='response')
View(y_predicted)
conf_matrix<-table(y_predicted>0.5,f$y)
accuracy<-sum(diag(conf_matrix)/sum(conf_matrix))
accuracy    #model is 90% accurate

library(ROCR)
rocrpred<-prediction(y_predicted,f$y)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
plot(rocrperf,colorize=T)
