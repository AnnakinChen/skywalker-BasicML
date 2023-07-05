library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(dplyr)
library(ggplot2)
library(vcd)
library(cluster)
library(pROC)
library(caret)
library(tidyr)
library(class)
source('kmeans++.R')
source('diy.R')

server<-function(input,output,session){
  #可视化部分
  data1<-reactive({
    if(is.null(input$file1)){
      return(NULL)
    }
    read_csv(input$file1$datapath)
  })
  data2<-reactive({
    if(is.null(data1())){
      return(iris)
    }
    res2=data1()%>%
      select_if(is.numeric)%>%
      names()
    a=unlist(strsplit(input$text1,','))
    b<-c()
    j=1
    for(i in 1:length(a)){
      tryCatch(
        expr = {
          if(a[i] %in% res2){
            b[j]=a[i]
            j=j+1
          }
        },
        error=function(e){
          return(data1()[res2])
        }
      )
    }
    if(length(b)==0){
      return(data1()[res2])
    }
    else{
      return(data1()[b])
    }
  })
  
  data3<-reactive({
    if(is.null(data1())){
      return(iris)
    }
    res2=data1()%>%
      select_if(~!is.numeric(.))%>%
      names()
    a=unlist(strsplit(input$text1,','))
    b<-c()
    j=1
    for(i in 1:length(a)){
      tryCatch(
        expr = {
          if(a[i] %in% res2){
            b[j]=a[i]
            j=j+1
          }
        },
        error=function(e){
          return(data1()[res2])
        }
      )
    }
    if(length(b)==0){
      return(data1()[res2])
    }
    else{
      return(data1()[b])
    }
  })
  
  data4<-reactive({
    req(data1())
    res2=data1()%>%
      select_if(is.numeric)%>%
      names()
    return(data1()[res2])
    
  })
  data5<-reactive({
    req(data1())
    res2=data1()%>%
      select_if(~!is.numeric(.))%>%
      names()
    return(data1()[res2])
    
  })
  observe({
    req(data5())
    updateSelectInput(session,'choice2',choices = names(data5()))
  })
  
  data6<-reactive({
    req(data5())
    res2=data5()%>%
      lapply(table)
    res3=as.data.frame(res2[[input$choice2]])
    res4=res3[order(-res3[,2]),]
    names(res4)=c('Values','Frequency')
    return(res4)
    
  })
  
  observeEvent(
    input$action1,
    {
      output$table2=renderDT(
        datatable(
          data2()%>%
            summary()%>%
            as.data.frame.matrix(),
          rownames = F,
          options = list(
            scrollX=T
          )
        )
      )
    }
  )
  output$table1=renderDT(
    datatable(
      data1(),
      options = list(
        columnDefs=list(list(className='dt-center',targets=1:length(colnames(data1())))),
        scrollX=T,
        lengthMenu=c(5,10,20)
      )
    )
  )
  observeEvent(
    input$action1,
    {
      output$table3=renderDT(
        datatable(
          data3()%>%
            summary()%>%
            as.data.frame.matrix(),
          rownames = F,
          options = list(
            scrollX=T
          )
        )
      )
    }
  )
  output$table4=renderDT(
    datatable(
      data6()
    )
  )
  observe({
    req(data4())
    updateSelectInput(session,'choice1',choices = names(data4()))
  })
  
  observe({
    req(input$choice1)
    updateSliderInput(session,'slider1',
                      min = min(data4()[input$choice1],na.rm = T),
                      max = max(data4()[input$choice1],na.rm = T),
                      value = c(min(data4()[input$choice1],na.rm = T),max(data4()[input$choice1],na.rm = T)))
  })
  
  output$plot1=renderPlot({
    req(input$choice1)
    ggplot(
      data = data4(),aes_string(x=input$choice1))+
      geom_histogram(aes(y=..density..),color='black',fill='#98F5FF',alpha=0.5)+
      geom_density(alpha=0.2,fill='#FF6666')+
      scale_x_continuous(limits = input$slider1)
    
  })
  output$plot2=renderPlot({
    req(input$choice1)
    ggplot(data=data4(),aes_string(y=input$choice1))+
      stat_boxplot(geom='errorbar',width=0.5,color='#778899')+
      geom_boxplot(fill='#98F5FF')
  })
  
  #线性回归部分
  datab1<-reactive({
    req(input$file2)
    read_csv(input$file2$datapath)
  })
  observe({
    req(input$file2)
    updateSelectInput(session,'choice3',choices = names(datab1()))
  })
  observe({
    req(input$file2)
    updateSelectInput(session,'choice4',choices = names(datab1()))
  })
  #训练模型
  model<-eventReactive(
    input$action2,
    {
      req(input$choice3,input$choice4)
      lm(as.formula(paste(input$choice4,"~",paste(input$choice3,collapse = "+"))),datab1())
    }
  )
  output$print1<-renderPrint({
    summary(model())
  })
  datab2<-reactive({
    req(input$file3)
    read_csv(input$file3$datapath)
  })
  observe({
    req(datab2())
    updateSelectInput(session,'choice5',choices = names(datab2()))
  })
  observeEvent(
    input$action3,
    output$print2<-renderPrint({
      req(input$choice5)
      predict(model(),datab2()[input$choice5])
    })
  )
  output$download1<-downloadHandler(
    'LinearRresult.csv',
    content = function(file){
      req(datab2(),input$choice5)
      df=data.frame(datab2()[input$choice5],predict(model(),datab2()[input$choice5]))
      names(df)=c(input$choice5,'prediction')
      write.csv(df,file=file)
    }
  )
  output$print3<-renderPrint({
    req(model())
    model()[input$choice6]
  })
  observeEvent(
    input$action3,
    output$print4<-renderPrint({
      req(model(),input$choice5,input$choice7,input$choice8,datab2())
      predict(model(),datab2()[input$choice5],level=as.numeric(input$choice7),interval=input$choice8)
    })
  )
  
  #glm begin
  #Logistic Regression
  datag1<-reactive({
    req(input$glm_f1)
    read_csv(input$glm_f1$datapath)
  })
  observe({
    req(input$glm_f1)
    updateSelectInput(session,'glm_c1',choices = names(datag1()))
  })
  observe({
    req(input$glm_f1)
    updateSelectInput(session,'glm_c2',choices = names(datag1()))
  })
  glm_model<-eventReactive(
    input$glm_a1,
    {
      req(input$glm_c1,input$glm_c2)
      glm(as.formula(paste(input$glm_c2,"~",paste(input$glm_c1,collapse = "+"))),family = 'binomial',data=datag1())
    }
  )
  observeEvent(
    input$glm_a1,
    output$glm_p1<-renderPrint({
      req(glm_model())
      summary(glm_model())
    })
  )
  datag2<-reactive({
    req(input$glm_f2)
    read_csv(input$glm_f2$datapath)
  })
  observeEvent(
    input$glm_a2,
    {
      req(glm_model(),datag2())
      fit=predict.glm(glm_model(),datag2(),se.fit = T)$fit
      prob=exp(fit)/(1+exp(fit))
      y=ifelse(prob>=0.5,1,0)
      res=list(prob,y)
      names(res)=c('Probability','y')
      output$glm_p2<-renderPrint({
        print(res)
      })


      
    }
    
  )
  dataglm3<-reactive({
    req(glm_model(),datag2())
    fit=predict.glm(glm_model(),datag2(),se.fit = T)$fit
    prob=exp(fit)/(1+exp(fit))
    y=ifelse(prob>=0.5,1,0)
    data=as.data.frame(datag2()[input$glm_c1])
    data$prob=prob
    data$predY=y
    return(data)
  })
  output$glm_d1<-downloadHandler(
    'LogisticResult.csv',
    content<-function(file){
      data=as.data.frame(dataglm3())
      write.csv(data,file,row.names = F)
    }
  )
  
  observe({
    req(input$glm_f2)
    updateSelectInput(session,'glm_c6',choices = names(datag2()))
  })
  
  observeEvent(
    input$glm_a5,
    {
      req(glm_model(),datag2(),input$glm_c6)
      da=data.frame(datag2()[input$glm_c6],predict(glm_model(),datag2(),type = 'response'))
      names(da)=c('y','prob')
      roc_obj=roc(da$y,da$prob,smooth=T)
      TPR=roc_obj$sensitivities
      FPR=1-roc_obj$specificities
      data=data.frame(TPR,FPR)
      output$glm_plot1<-renderPlot({
        ggplot(data,aes(FPR,TPR))+
          geom_line(color='skyblue',linewidth=0.8)+
          geom_segment(x=0,y=0,xend=1,yend=1,linetype=2)+
          ggtitle('ROC Curve')+
          theme(
            plot.title = element_text(hjust=0.5)
          )
      })
    }
  )
  
  observeEvent(
    input$glm_a5,
    {
      req(glm_model(),datag2(),input$glm_c6)
      da=data.frame(datag2()[input$glm_c6],predict(glm_model(),datag2(),type = 'response'))
      names(da)=c('y','prob')
      roc_obj=roc(da$y,da$prob)
      TPR=roc_obj$sensitivities
      FPR=1-roc_obj$specificities
      threshold=roc_obj$thresholds
      data1=data.frame(TPR,FPR,threshold)
      max_thre=data1[which(data1$TPR-data1$FPR==max(data1$TPR-data1$FPR)),][3][1,1]
      tpr=data1[which(data1$TPR-data1$FPR==max(data1$TPR-data1$FPR)),][1][1,1]
      fpr=data1[which(data1$TPR-data1$FPR==max(data1$TPR-data1$FPR)),][2][1,1]
      output$glm_plot2<-renderPlot({
        ggplot(data1,aes(x=threshold))+
          geom_line(aes(y=TPR,color='TPR'),linewidth=0.8)+
          geom_line(aes(y=FPR,color='FPR'),linewidth=0.8)+
          geom_line(aes(y=TPR-FPR,color='KS'),linewidth=0.8)+
          geom_segment(
            aes(x=max_thre,xend=max_thre,
                y=fpr,yend=tpr),linetype=2
          )+
          xlim(c(1,0))+
          scale_color_manual(values = c('TPR'='orange','FPR'='skyblue',
                                        'KS'='lightgreen'))+
          ggtitle('KS Curve')+
          theme(
            legend.title = element_blank()
          )
      })
    }
  )
  
  observeEvent(
    input$glm_a5,
    {
      req(glm_model(),datag2(),input$glm_c6)
      da=data.frame(datag2()[input$glm_c6],predict(glm_model(),datag2(),type = 'response'))
      prob=predict(glm_model(),datag2(),type = 'response')
      py=ifelse(prob>=0.5,1,0)
      names(da)=c('y','prob')
      da$pred_y=py
      acc_score=sum(da$pred_y==da$y)/length(da[,1])
      roc_obj=roc(da$y,da$prob)
      output$glm_p6=renderPrint({
        cat('Accuracy score is:','\n',acc_score,'\n',
          'DIY AUC by probability approach is:','\n',diy_auc(da$y,da$prob),'\n',
            'AUC computed by "pROC":','\n',roc_obj$auc[1],
            sep = '')
      })
    }
  )

  observeEvent(
    input$glm_a2,
    output$glm_p3<-renderPrint({
      req(glm_model(),datag2())
      se=predict.glm(glm_model(),datag2(),se.fit = T)$se
      fit=predict.glm(glm_model(),datag2(),se.fit = T)$fit
      prob=exp(fit)/(1+exp(fit))
      alpha=as.numeric(input$glm_c3)
      v=qnorm(alpha/2,lower.tail = F)
      lower=fit-v*se
      upper=fit+v*se
      lower=exp(lower)/(1+exp(lower))
      upper=exp(upper)/(1+exp(upper))
      data.frame(prob,lower,upper)
    })
  )
  
  #poisson regression
  datag3<-reactive({
    req(input$glm_f3)
    read_csv(input$glm_f3$datapath)
  })
  observe({
    req(input$glm_f3)
    updateSelectInput(session,'glm_c4',choices = names(datag3()))
  })
  observe({
    req(input$glm_f3)
    updateSelectInput(session,'glm_c5',choices = names(datag3()))
  })
  pois_model1<-eventReactive(
    input$glm_a3,
    {
      req(datag3(),input$glm_c4,input$glm_c5,input$glm_t1)
      glm(as.formula(paste(input$glm_c5,"~",paste(input$glm_c4,collapse = "+"))),
          family = 'poisson',offset = log(offset),data=datag3())
      
    }
  )
  pois_model2<-eventReactive(
    input$glm_a3,
    {
      req(datag3(),input$glm_c4,input$glm_c5)
      glm(as.formula(paste(input$glm_c5,"~",paste(input$glm_c4,collapse = "+"))),
          family = 'poisson',data=datag3())
      
    }
  )
  observeEvent(
    input$glm_a3,
    {
      if(input$glm_t1=='No'){
        output$glm_p4<-renderPrint({
          summary(pois_model2())
        })
      }
      else{
        output$glm_p4<-renderPrint({
          summary(pois_model1())
        })
      }
    }
  )
  datag4<-reactive({
    req(input$glm_f4)
    read_csv(input$glm_f4$datapath)
  })
  observeEvent(
    input$glm_a4,
    if(input$glm_t1=='No'){
      output$glm_p5<-renderPrint({
        predict.glm(pois_model2(),datag4(),type = 'response')
      })
    }
    else{
      output$glm_p5<-renderPrint({
        predict.glm(pois_model1(),datag4(),type = 'response')
      })
    }

  )
  
  #kmeans
  datak2<-reactive({
    req(input$kmeansf1)
    read_csv(input$kmeansf1$datapath)
  })
  datak1<-reactive({
    req(datak2())
    scale(datak2())
  })
  observeEvent(
    input$kmeansa1,
    {
      req(datak1(),input$kmeanst1,input$kmeanst2)
      k=c()
      silhouette=c()
      j=1
      for(i in as.numeric(input$kmeanst1):as.numeric(input$kmeanst2)){
        model=kmeans(datak1(),centers=i,nstart=10)
        k[j]=i
        silhouette[j]=mean(silhouette(model$cluster,dist(datak1()))[,3])
        j=j+1
      }
      res=data.frame(k,silhouette)
      best=res[res$silhouette==max(res$silhouette),]
      result=list(res,best)
      names(result)=c('All Silhouette Coeficients','Best k')
      output$kmeansp1<-renderPrint({result})
    }
  )
  observeEvent(
    input$kmeansa1,
    {
      req(datak1(),input$kmeanst1,input$kmeanst2)
      withinss<-c()
      k<-c()
      j=1
      for(i in as.numeric(input$kmeanst1):as.numeric(input$kmeanst2)){
        model=kmeans(datak1(),centers=i,nstart=10)
        withinss[j]=model$tot.withinss
        k[j]=i
        j=j+1
        
      }
      res=data.frame(k,withinss)
      output$kmeansplot1<-renderPlot({
        ggplot(res,aes(x=k,y=withinss))+
          geom_line(color='skyblue')+
          geom_point(color='orange',size=1.8)+
          scale_x_continuous(breaks = seq(as.numeric(input$kmeanst1),as.numeric(input$kmeanst2),1))+
          ggtitle('Elbow Method')+
          theme(
            panel.grid.minor.x  = element_blank(),
            plot.title = element_text(hjust = 0.5)
          )
      })
    }
  )
  kmodel=eventReactive(
    input$kmeansa2,
    {
      req(datak1(),input$kmeanst3,input$kmeanst4)
      kmeans(datak1(),centers = as.numeric(input$kmeanst3),nstart = as.numeric(input$kmeanst4))
    }
  )
  observeEvent(
    input$kmeansa2,
    {
      req(datak1(),input$kmeanst3,input$kmeanst4)
      output$kmeansp2<-renderPrint({
        print(kmodel())
      })
    }
  )
  observeEvent(
    input$kmeansa3,
    {
      da=datak2()
      da$cluster=factor(kmodel()$cluster)
      output$kmeansplot2<-renderPlot({
        req(kmodel(),input$kt1,input$kt2)
        ggplot(da,aes_string(x=input$kt1,y=input$kt2,color='cluster',shape='cluster'))+
          geom_point()
      })
    }
  )
  output$kmeansdown1<-downloadHandler(
    'kmeans.csv',
    content = function(file){
      data=as.data.frame(datak2())
      data$cluster=kmodel()$cluster
      write.csv(data,file,row.names = F)
    }
  )
  
  #kmeans++
  datap1<-reactive({
    req(input$kppf1)
    read_csv(input$kppf1$datapath)
  })
  datap2<-reactive({
    req(datap1())
    scale(datap1())
  })
  observeEvent(
    input$kppa1,
    {
      req(datap2(),input$kppt1,input$kppt2)
      k=c()
      silhouette=c()
      j=1
      for(i in as.numeric(input$kppt1):as.numeric(input$kppt2)){
        model=kmeans_pp(as.data.frame(datap2()),i)
        k[j]=i
        silhouette[j]=mean(silhouette(model$cluster,dist(datap2()))[,3])
        j=j+1
      }
      res=data.frame(k,silhouette)
      best=res[res$silhouette==max(res$silhouette),]
      result=list(res,best)
      names(result)=c('All Silhouette Coeficients','Best k')
      output$kppp1<-renderPrint({result})
    }
  )
  observeEvent(
    input$kppa1,
    {
      req(datap2(),input$kppt1,input$kppt2)
      withinss<-c()
      k<-c()
      j=1
      for(i in as.numeric(input$kppt1):as.numeric(input$kppt2)){
        model=kmeans_pp(as.data.frame(datap2()),i)
        withinss[j]=model$tot.withinss
        k[j]=i
        j=j+1
        
      }
      res=data.frame(k,withinss)
      output$kppplot1<-renderPlot({
        ggplot(res,aes(x=k,y=withinss))+
          geom_line(color='skyblue')+
          geom_point(color='orange',size=1.8)+
          scale_x_continuous(breaks = seq(as.numeric(input$kppt1),as.numeric(input$kppt2),1))+
          ggtitle('Elbow Method')+
          theme(
            panel.grid.minor.x  = element_blank(),
            plot.title = element_text(hjust = 0.5)
          )
      })
    }
  )
  kppmodel=eventReactive(
    input$kppa2,
    {
      req(datap2(),input$kppt3)
      kmeans_pp(as.data.frame(datap2()),as.numeric(input$kppt3))
    }
  )
  observeEvent(
    input$kppa2,
    {
      req(datap2(),kppmodel())
      output$kppp2<-renderPrint({
        print(kppmodel())
      })
    }
  )
  observeEvent(
    input$kppa3,
    {
      da=datap1()
      da$cluster=factor(kppmodel()$cluster)
      output$kppplot2<-renderPlot({
        req(kppmodel(),input$kp1,input$kp2)
        ggplot(da,aes_string(x=input$kp1,y=input$kp2,color='cluster',shape='cluster'))+
          geom_point()
      })
    }
  )
  output$kppdown1<-downloadHandler(
    'kmeans++.csv',
    content = function(file){
      data=as.data.frame(datap1())
      data$cluster=kppmodel()$cluster
      write.csv(data,file,row.names = F)
    }
  )
  
  #knn begin
  dataknn1<-reactive({
    req(input$knn_f1)
    read_csv(input$knn_f1$datapath)
  })
  dataknn2<-reactive({
    req(input$knn_f2)
    read_csv(input$knn_f2$datapath)
  })
  observe({
    req(dataknn1())
    updateSelectInput(session,'knn_c1',choices = names(dataknn1()),selected = names(dataknn1())[-length(names(dataknn1()))])
  })
  observe({
    req(dataknn1())
    updateSelectInput(session,'knn_c2',choices = names(dataknn1()),selected = names(dataknn1())[length(names(dataknn1()))])
  })
  observe({
    req(dataknn2())
    updateSelectInput(session,'knn_c3',choices = names(dataknn2()),selected = names(dataknn2())[-length(names(dataknn2()))])
  })
  observe({
    req(dataknn2())
    updateSelectInput(session,'knn_c6',choices = names(dataknn2()),selected = names(dataknn2())[length(names(dataknn2()))])
  })
  
  knnmodel=eventReactive(
    input$knn_a1,
    {
      req(dataknn1(),dataknn2(),input$knn_c1,input$knn_c2,input$knn_c3)
      da=as.data.frame(dataknn1()[input$knn_c2])
      knn(train=dataknn1()[input$knn_c1],test=dataknn2()[input$knn_c3],cl=factor(da[,1]),
          prob = T,k=as.numeric(input$knn_t1))
    }
  )
  observeEvent(
    input$knn_a1,
    {
      output$knn_p1<-renderPrint({
        knnmodel()
      })
    }
  )
  
  observeEvent(
    input$knn_a1,
    {
      data=as.data.frame(dataknn2()[input$knn_c3])
      data$predY=knnmodel()
      output$knn_d1=downloadHandler(
        'knn.csv',
        content = function(file){
          write.csv(data,file,row.names = F)
        }
      )
      
    }
  )
  
  observeEvent(
    input$knn_a3,
    {
      prob=attributes(knnmodel())$prob
      da=data.frame(dataknn2()[input$knn_c6],knnmodel(),prob)
      names(da)<-c('y','pred_y','prob')
      acc_score=sum(da$y==da$pred_y)/length(da$y)
      da$prob1=ifelse(da$pred_y==1,da$prob,1-da$prob)
      roc_obj=roc(da$y,da$prob1,smooth=T)
      TPR=roc_obj$sensitivities
      FPR=1-roc_obj$specificities
      data=data.frame(TPR,FPR)
      output$knn_plot1<-renderPlot({
        ggplot(data,aes(FPR,TPR))+
          geom_line(color='skyblue',linewidth=0.8)+
          geom_segment(x=0,y=0,xend=1,yend=1,linetype=2)+
          ggtitle('ROC Curve')+
          theme(
            plot.title = element_text(hjust=0.5)
          )
      })
      roc_obj1=roc(da$y,da$prob1)
      output$knn_p3=renderPrint({
        cat('Accuracy score is:','\n',acc_score,'\n',
            'DIY AUC by probability approach is:','\n',diy_auc(da$y,da$prob1),'\n',
            'AUC computed by "pROC":','\n',roc_obj1$auc[1],
            sep = '')
      })
      
    }
  )
  
  observeEvent(
    input$knn_a3,
    {
      prob=attributes(knnmodel())$prob
      da=data.frame(dataknn2()[input$knn_c6],knnmodel(),prob)
      names(da)<-c('y','pred_y','prob')
      da$prob1=ifelse(da$pred_y==1,da$prob,1-da$prob)
      roc_obj=roc(da$y,da$prob1)
      TPR=roc_obj$sensitivities
      FPR=1-roc_obj$specificities
      threshold=roc_obj$thresholds
      data1=data.frame(TPR,FPR,threshold)
      max_thre=data1[which(data1$TPR-data1$FPR==max(data1$TPR-data1$FPR)),][3][1,1]
      tpr=data1[which(data1$TPR-data1$FPR==max(data1$TPR-data1$FPR)),][1][1,1]
      fpr=data1[which(data1$TPR-data1$FPR==max(data1$TPR-data1$FPR)),][2][1,1]
      output$knn_plot2<-renderPlot({
        ggplot(data1,aes(x=threshold))+
          geom_line(aes(y=TPR,color='TPR'),linewidth=0.8)+
          geom_line(aes(y=FPR,color='FPR'),linewidth=0.8)+
          geom_line(aes(y=TPR-FPR,color='KS'),linewidth=0.8)+
          geom_segment(
            aes(x=max_thre,xend=max_thre,
                y=fpr,yend=tpr),linetype=2
          )+
          xlim(c(1,0))+
          scale_color_manual(values = c('TPR'='orange','FPR'='skyblue',
                                        'KS'='lightgreen'))+
          ggtitle('KS Curve')+
          theme(
            legend.title = element_blank()
          )
      })
    }
  )
  
  
  
  dataknn3<-reactive({
    req(input$knn_f3)
    read_csv(input$knn_f3$datapath)
  })
  observe({
    req(dataknn3())
    updateSelectInput(session,'knn_c4',choices = names(dataknn3()),selected = names(dataknn3())[-length(names(dataknn3()))])
  })
  observe({
    req(dataknn3())
    updateSelectInput(session,'knn_c5',choices = names(dataknn3()),selected = names(dataknn3())[length(names(dataknn3()))])
  })
  observeEvent(
    input$knn_a2,
    {
      req(dataknn3(),input$knn_c4,input$knn_c5,input$knn_t2,input$knn_t3,input$knn_t4)
      trainx=dataknn3()[input$knn_c4]
      trainy=as.data.frame(dataknn3()[input$knn_c5])
      trainy1=trainy[,1]
      start=as.numeric(input$knn_t3)
      end=as.numeric(input$knn_t4)
      k1=as.numeric(input$knn_t2)
      output$knn_p2<-renderPrint({
        print(find_knn_bestk(start,end,trainx,trainy1,k1))
      })
      
    }
  )
  


  
  
  
  
  
  
  
  
}