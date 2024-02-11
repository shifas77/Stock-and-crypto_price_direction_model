
beg_time=Sys.time() 
print(beg_time)



library(e1071)
library(SmartEDA)
library(GGally)
library(DataExplorer)
library(alphavantager)
library(BatchGetSymbols)
library(gtrendsR)
library(onehot)
library(dplyr)
library(mltools)
library(data.table)
library(magrittr)
library(lightgbm)
library(methods)
library(naniar)
library(missRanger)
library(doParallel)
library(ggplot2)
library(tidyverse)
library(onehot)
library(data.table)
library(mltools)
library(caret)
library(car)
library(dplyr)
library(ggplot2)
require(xgboost)
library(DiagrammeR)
library(rpart)
library(rpart.plot)
library(corrplot)
library(VIM)
library(randomForest)
library(ggplot2)
library(graphics)
library(caTools)
library(GLDEX)
library(modeest)
library(rlist)
library(tidyr)
library(quantmod)
library(coinmarketcapr)
library(quantmod)


library(plyr)
library(readr)
library(neuralnet)
library(nnet)
library(lubridate)
library(BatchGetSymbols)
library(caTools)




memory.limit(810241024*1024)

memory.limit()
all_cores <- parallel::detectCores(logical = TRUE)
registerDoParallel(cores = all_cores)



tickers=scan("Yahoo_Ticker_2022.txt", character(), quote = '')

holder=tickers[1]
tickers=tickers[-1]
ko=sample(tickers)
ko[length(ko)+1]=ko[1]
ko[1]=holder
tickers=ko


past=1400
WRONG=round(past*4.5)/100
first.date <- Sys.Date()-past
last.date <- Sys.Date()-2
freq.data <- 'daily'
# set tickers
Lrow=0
core=0
Logo=0
axis=1
GRO=1
danumere=0
actual=0
actuall=0
Balance=0
coin=0
ORG=1
new_row=0
app=1
TTHRES=0.50
THETHE=0
THELEN=0
names=c("CRYPTO","ACCURACY","ADJUSTED_ACCURACY","CROSS-ENTROPHY","CB-ONE","CB-TWO","LIVE PRICE","PERCENTAGE GAIN","DIRECTION ABOVE MEAN")
for(i in 1:length(tickers)){
  out <- BatchGetSymbols(tickers = tickers[i], 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) 
  
  
  a=out$df.tickers
  danumere[i]=nrow(a)
  All_count=danumere
  a=a %>% distinct(ref.date, .keep_all= TRUE)
  actual[i]=nrow(a)
  
}
danumere=fun.zero.omit(danumere)
actual=fun.zero.omit(actual)
danumere=mfv(danumere)
dafinal=mfv(actual)


for(i in 1:length(tickers)){
  out <- BatchGetSymbols(tickers = tickers[i], 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) 
  
  
  a=out$df.tickers
  
  if(i==which(All_count==dafinal)){
    kera=25
    a=a%>% select(-ticker,-price.adjusted,-ret.adjusted.prices,-ret.closing.prices)
    a<- missRanger(a, pmm.k = 3, splitrule = "extratrees", num.trees = 100)
    a=a %>% distinct(ref.date, .keep_all= TRUE)
    timestamp=a$ref.date
    actuall[ORG]=nrow(a)
    ORG=ORG+1
    
  }
  else if(nrow(a)>0) {
    a=a %>% distinct(ref.date, .keep_all= TRUE)
    actuall[ORG]=nrow(a)
    a=a%>% select(-ticker,-price.adjusted,-ret.adjusted.prices,-ret.closing.prices)
    a<- missRanger(a, pmm.k = 3, splitrule = "extratrees", num.trees = 100)
    ORG=ORG+1
  }
  
  if(nrow(a)==dafinal){
    Latex=a$ref.date
    print(a[duplicated(a)])
    print(a$ref.date[duplicated(a$ref.date)])
    Lrow[GRO]=as.data.frame(a$ref.date[nrow(a)])
    if(i==which(All_count==dafinal)[1]){a=a%>% select(-ref.date)}
    print(nrow(a)) 
    Logo[axis]=tickers[i]
    core[axis]=list(a)
    axis=axis+1
    GRO=GRO+1
  }
}

sdate=list.rbind(Lrow)

remov=which(sdate!=mfv(sdate))

Bminus=0
for(bullet in remov){
  print(bullet)
  bullet=bullet-Bminus
  core=core[-bullet]
  Logo=Logo[-bullet]
  Bminus=Bminus+1
}


crust=core
for(moon in seq(from=6, to=78, by=8)){
  core=crust
  for(gon in 1:length(core)){
    
    coin=coin+1
    
    YORK=list.cbind(core)
    
    colnames(YORK)=make.unique(colnames(YORK))
    
    
    
    YORK=YORK%>% select(-ref.date)
    
    Time=as.data.frame(unclass(as.POSIXlt(Latex))) %>% select(-isdst,-hour,-min,-sec)
    t=cbind(Time,YORK)
    
    comp=getQuote(Logo[gon],src = "yahoo")
    comp=comp$Last
    
    
    PERCENTAGE_PROFIT=moon
    Train_size=90
    
    Cmean=((comp/100)*PERCENTAGE_PROFIT)+comp
    
    
    
    t$BUY <- ifelse(t$price.high > Cmean, 1, 0)
    
    ClassB=as.data.frame(table(t$BUY[1:Train_size]))
    Balance=0
    Balance[1]=round(ClassB$Freq[1]/sum(ClassB$Freq)*100)
    Balance[2]=round(ClassB$Freq[2]/sum(ClassB$Freq)*100)
    Balance[is.na(Balance)] = 0
    
    TBal=as.data.frame(table(tail(t$BUY,100)))
    TBalance=0
    TBalance[1]=round(TBal$Freq[1]/sum(TBal$Freq)*100)
    TBalance[2]=round(TBal$Freq[2]/sum(TBal$Freq)*100)
    TBalance[is.na(TBalance)] = 0
    
    
    
    
    if((Balance[1]>30&Balance[2]>30)&(TBalance[1]>30&TBalance[2]>30)){
      
      t$BUY=factor(t$BUY)
      Cmean=((comp/100)*PERCENTAGE_PROFIT)+comp
      
      
    
      
      #Finding correlation between numeric variables in the dataset:
      #Filtering in numerical variables in the dataset:
      numeric_cor=t[,which(sapply(t, class) == "numeric")]
      
      int_cor=t[,which(sapply(t, class) == "integer")]
      
      t_numint=cbind(numeric_cor,int_cor)
      
      t_numint<- missRanger(t_numint, pmm.k = 3, splitrule = "extratrees", num.trees = 100,verbose = 0)
      
      t_cor=cor(t_numint)
      
      
      
      UnCorrelated = findCorrelation(t_cor, cutoff=0.1)
      
      
      fin_uncornames=names(t_numint[,-c(UnCorrelated)])
      
      
      t=cbind(t[,fin_uncornames],t$BUY)  
      
      colnames(t)[ncol(t)]="t$BUY"
      
      
      t %<>% mutate_if(is.factor,as.numeric)
      
      t$`t$BUY` <-ifelse(t$`t$BUY`==1, 0, 1)
      
      
    
      fork_repli=fork
      Target=fork[ncol(fork)]
      fork=fork[-ncol(fork)]
      
      t=t[-nrow(t),]
      t=t[-nrow(t),]
      
      Train_size=Train_size
      Tratio=nrow(t)-Train_size
      T_V_Split=0.5
      
      
      training_set = t[1:(nrow(t)-Tratio),]
      test_set=tail(t,Tratio)
      
      pca = preProcess(training_set[-ncol(training_set)], method = 'pca', pcaComp = 10)
      
      training_set.pca = predict(pca,training_set[-ncol(training_set)])
      test_set.pca = predict(pca, test_set[-ncol(test_set)])
      fork=predict(pca, fork)
      
      training_set=cbind(training_set.pca,training_set$`t$BUY`)
      test_set=cbind(test_set.pca,test_set$`t$BUY`)
      
      colnames(training_set)[ncol(training_set)]="t$BUY"
      colnames(test_set)[ncol(test_set)]="t$BUY"
      
      split = sample.split(test_set$`t$BUY`, SplitRatio = T_V_Split)
      valid_set = subset(test_set, split == FALSE)
      test_set = subset(test_set, split == TRUE)
      
      
      Target=test_set[ncol(test_set)]
      TA <-ifelse(Target==1, 1, 0)
      Target=as.numeric(TA)
      
      forklayer=test_set[-ncol(test_set)]
      
      fork=rbind(forklayer,fork) 
      
      dsfork=fork
      
      
      
      start_time <- Sys.time()
      
      print(Logo[gon])
      
      for(spinit in 1:1){
        
        
        cross_label=test_set$`t$BUY`
        dstrain=training_set
        
        dstest=test_set
        
        dsvalid=valid_set
        
        
        
        dtrain <- xgb.DMatrix(data = as.matrix(dstrain[,1:ncol(dstrain)-1]),label = dstrain$`t$BUY`)
        
        dstest1=dstest[,1:ncol(dstest)-1]
        dtest <- xgb.DMatrix(data =as.matrix(dstest1))
        
        dvalid <- xgb.DMatrix(data =as.matrix(dsvalid[,1:ncol(dsvalid)-1]),label=dsvalid$`t$BUY`)
        
        #xgboost- Tuning Parameters 
        
        params <- list(booster = "gbtree", objective = "binary:logistic",
                       eta=0.5, gamma=0.2, max_depth=6, min_child_weight=1,
                       subsample=0.4, colsample_bytree=1,scale_pos_weight = 4,reg_alpha=2)
        
        
        #Find optimum number of iterations for Xgboost model
        
        #first default - model training
        
        xgb1 <- xgb.train (params = params, data = dtrain, nrounds =1000,
                           watchlist = list(val=dvalid,train=dtrain), print_every_n = 1500
                           , maximize = F , eval_metric = "error",verbose = 0)
        
        
        xgb1 <- xgb.train (params = params, data = dtrain, nrounds =which.min(xgb1$evaluation_log$val_error),
                           watchlist = list(val=dvalid,train=dtrain),print_every_n = 1500
                           , maximize = F , eval_metric = "error",verbose = 0)
        
        
        # Same thing with co-occurence computation tthi       #model prediction
        
        xgbpred <- predict (xgb1,dtest)
        cross_prob=xgbpred
        xgbpred <- ifelse (xgbpred > 0.5,1,0)
        Record=xgbpred
        
        if(length(unique(xgbpred))==1){break}
        # Making confusion matrix
        cross_table=cbind(cross_label,cross_prob)
        cross_entrophy=ifelse(cross_label==1,-log(cross_prob) ,-log(1-cross_prob))
        cross_entrophy=mean(cross_entrophy)
        
        Accuracy=confusionMatrix(factor(dstest$`t$BUY`), factor(xgbpred))
        
        if(Accuracy$byClass['Balanced Accuracy']>(100-(WRONG/(nrow(test_set)/100)))/100)
        {print(spinit)
          
          
          print(Accuracy$byClass['Balanced Accuracy'])
          
          Limit=10
          push=1
          Meancom=0
          threshold=0
          start=0.10
          end=0.3
          for (THR in (start*100):(end*100)){
            
            mixx= sample(nrow(test_set))
            test_set=test_set[mixx,]
            
            
            dstest=test_set
            
            dstest1=dstest[,1:ncol(dstest)-1]
            dtest <- xgb.DMatrix(data =as.matrix(dstest1))
            
            
            xgbpred <- predict (xgb1,dtest)
            xgbpred <- ifelse (xgbpred > THR/100,1,0)
            if(length(unique(xgbpred))==1){next}
            
            
            
            TAccuracy=confusionMatrix(factor(dstest$`t$BUY`), factor(xgbpred))
            
            threshold[push]=THR/100
            
            Meancom[push]=TAccuracy
            push=push+1
          }
          BTHRES=as.data.frame(cbind(threshold,Meancom))
          TTHRES= BTHRES[which.max(BTHRES$Meancom),1]
          xgbpred <- predict (xgb1,dtest)
          xgbpred <- ifelse (xgbpred > TTHRES,1,0)
          FAccuracy=confusionMatrix(factor(dstest$`t$BUY`), factor(xgbpred))
          print(paste("MEAN ACCURACY:",mean(Meancom)))
          break
        }
        
        
      }
      
      
      
      dfork <- xgb.DMatrix(data =as.matrix(dsfork))
      
      
      xgbpred <- predict (xgb1,dfork)
      xgbpred <- ifelse (xgbpred > TTHRES,1,0)
      if(length(unique(xgbpred))>1){
        
        
        
     
        comp=comp$Last
        
        if(xgbpred[length(xgbpred)]==1&Accuracy$byClass['Balanced Accuracy']>(100-(WRONG/(nrow(test_set)/100)))/100){
          print(Accuracy$table)
          print(paste("ADJUSTED ACCURACY:",BTHRES[which.max(BTHRES$Meancom),2]*100))
          print(FAccuracy$table)
          print(paste("BEST THRESHOLD:",BTHRES[which.max(BTHRES$Meancom),1]))
          print(paste("BEST THRESHOLD ACC:",BTHRES[which.max(BTHRES$Meancom),2]))
          print(paste("CLASS BALANCE:ZERO:",Balance[1]))
          print(paste("CLASS BALANCE:ONE:",Balance[2]))
          print(paste("LIVE PRICE:",comp))
          print(paste("PERCENTAGE GAIN:",(Cmean-comp)/(comp/100)))
          print(paste("MEAN:",Cmean))
          print(paste("DIRECTION:ABOVE MEAN:",Cmean))
          print(mfv(fork[nrow(fork),]==dsfork[nrow(dsfork),]))
          new_row[app]=list(c(Logo[gon],as.numeric(names(table(Accuracy$overall[1]*100))),BTHRES[which.max(BTHRES$Meancom),2],cross_entrophy,Balance[1],Balance[2],comp,(Cmean-comp)/(comp/100),Cmean))
          app=app+1
          
        }
      }
      
      end_time <- Sys.time()
      print(end_time-start_time)
      print(end_time-beg_time)
      cat("\n")
      
      
      
    }
    
    pholder=core[1]
    core[1]=core[gon+1]
    core[gon+1]=pholder
    TTHRES=0.50
    
    
  }
  
  print(paste("COIN:",coin))
  print(paste("LOGO:",length(Logo)))
  
  if(length(data.table(list.rbind(new_row)))==9){
    FinalCut=data.table(list.rbind(new_row))
    colnames(FinalCut)=names
    FinalCut <- FinalCut[order(-ACCURACY ),]
    write.csv(FinalCut, file = paste("FinalCut",Sys.Date(),".csv"))
  }
  cat("\n")
  print("--------------------THE END--------------------")
  
}


