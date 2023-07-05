
#计算一个向量到矩阵中每一个向量的距离
distance<-function(x1,x2){
  res=c()
  j=1
  for(i in 1:length(x2[,1])){
    res[j]=sqrt(sum((x2[i,]-x1)^2))
    j=j+1
  }
  return(res)
}

#计算概率
proba<-function(dis){
  res=c()
  for(i in 1:length(dis)){
    res[i]=dis[i]^2/sum(dis^2)
  }
  return(res)
}

#计算向量x到各个中心的距离的最小值
updatedis<-function(data,center){
  mindis=c()
  for(i in 1:length(data[,1])){
    tem=distance(data[i,],center)
    mindis[i]=tem[tem==min(tem)]
  }
  return(mindis)
}

#随机抽取k个中心点
k_center<-function(data,k){
  index=sample(1:length(data[,1]),1)
  center=data[index,]
  while(length(center[,1])<k){
    dis1=updatedis(data,center)
    prob=proba(dis1)
    new_index=sample(1:length(data[,1]),1,prob=prob)
    center=rbind(center,data[new_index,])
  }
  print(center)
}
kmeans_pp<-function(data,k){
  model=kmeans(data,k_center(data,k))
  return(model)
}


