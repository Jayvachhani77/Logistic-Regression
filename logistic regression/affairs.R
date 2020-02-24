f<-read.csv(choose.files())
attach(f)
class(f$gender)
class(f$children)
class(f$affairs)
class(f$occupation)
class(f$education)
class(f$rating)
class(f$religiousness)
class(f$yearsmarried)
class(f$age)
sum(is.na(f))
f$gender<-as.character(f$gender,inplace=TRUE)
f$children<-as.character(f$children,inplace=TRUE)

i<-0
for(i in 1:601){
  if(f$gender[i]=="male")
    f$gender[i]<-1
  else
    f$gender[i]<-0
}
for(i in 1:601){
  if(f$children[i]=='yes')
    f$children[i]<-1
  else
    f$children[i]<-0
}
for(i in 1:601){
  if(f$affairs[i]>0)
    f$affairs[i]<-1
}
f$affairs<-as.integer(f$affairs)
f$gender<-as.integer(f$gender)
f$age<-as.integer(f$age)
f$yearsmarried<-as.integer(f$yearsmarried)
f$children<-as.integer(f$children)
f$religiousness<-as.integer(f$religiousness)
f$education<-as.integer(f$education)
f$occupation<-as.integer(f$occupation)
f$rating<-as.integer(f$rating)
f<-na.omit(f)
model_0<-glm(affairs~.,family ="binomial",data =f)
model_0
exp(coef(model))
summary(model_0)
predict_0<-predict(model_0,f,type ="response")
predict_0
conf_matrix<-table(predict_0>0.5,f$affairs)
conf_matrix
Accuracy<-sum(diag(conf_matrix)/sum(conf_matrix))
Accuracy  #76.54% accurate model
pred_values <- NULL
yes_no <- NULL
pred_values <- ifelse(predict_0>=0.5,1,0)
yes_no <- ifelse(predict_0>=0.5,"yes","no")
f[,"predict_0"] <- predict_0
f[,"pred_values"] <- pred_values
f[,"yes_no"] <- yes_no
View(f[,c(1,10:12)])
table(f$affairs,f$pred_values)
install.packages("ROCR")
library(ROCR)
rocrpred<-prediction(predict_0,f$affairs)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

install.packages("dplyr")
library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)

