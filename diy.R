# diy auc
diy_auc<-function(y,prob){
  data=data.frame(y,prob)
  posi_prob=data[data$y==1,2]
  nega_prob=data[data$y==0,2]
  total=length(posi_prob)*length(nega_prob)
  cnt=0
  for(i in 1:length(posi_prob)){
    for(j in 1:length(nega_prob)){
      if(posi_prob[i]>nega_prob[j]) {cnt = cnt+1}
      else if(posi_prob[i]==nega_prob[j]) {cnt=cnt+0.5}
    }
  }
  return(round(cnt/total,7))
}



# trainx is a dataframe, trainy is a vector, k1 denotes k folds, k2 denote the k in knn.
# a list of mean accuracy and mean auc will be return.
knn_cv<-function(trainx,trainy,k1,k2){
  #set.seed(10)
  KFolds=createFolds(trainy,k=k1)
  accuracy=c()
  auc=c()
  for(i in 1:length(KFolds)){
    
    tr=trainx[-KFolds[[i]],]
    te=trainx[KFolds[[i]],]
    n=trainy[KFolds[[i]]]
    model=knn(train = tr,test = te,cl = trainy[-KFolds[[i]]],k=k2,prob = T)
    accuracy[i]=sum(model==trainy[KFolds[[i]]])/length(n)
    da=data.frame(trainy[KFolds[[i]]],model,attributes(model)$prob)
    names(da)=c('y','pred_y','prob')
    da$prob1<-ifelse(da$pred_y==1,da$prob,1-da$prob)
    auc[i]=roc(da$y,da$prob1)$auc[1]
    
  }
  result=list(k2,accuracy,mean(accuracy),auc,mean(auc))
  names(result)=c('k-neighbor','Accuracy Score','Avearge Accuracy Score','AUC','Avearge AUC')
  return(result)
}
find_knn_bestk<-function(start,end,trainx,trainy,k1){
  all_li=list()
  for(i in start:end){
    all_li=c(all_li,knn_cv(trainx,trainy,k1,i))
  }
  j=1
  acc<-c()
  for(i in seq(3,length(all_li),5)){
    acc[j]=all_li[[i]]
    j=j+1
  }
  m=1
  auc<-c()
  for(i in seq(5,length(all_li),5)){
    auc[m]=all_li[[i]]
  }
  for(i in seq(3,length(all_li),5)){
    if(all_li[[i]]==max(acc)){
      k_acc=all_li[[i-2]]
      break
    }
  }
  for(i in seq(5,length(all_li),5)){
    if(all_li[[i]]==max(auc)){
      k_auc=all_li[[i-4]]
      break
    }
  }
  li=list(paste(k_acc,':',max(acc)),paste(k_auc,':',max(auc)),'All Results')
  names(li)=c('Best k suggested by accuracy score','Best k suggested by auc score','--------------------------------')
  return(c(li,all_li))
}




