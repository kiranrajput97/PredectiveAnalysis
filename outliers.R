library(class)
library(missForest)


#GETING DATA
data=read.csv(file.choose(),sep=',',header = T, stringsAsFactors = F)

str(data)
summary(data)
ncol(data)
is.na(data)
(data[!complete.cases(,data)])


#DATA CLEANING ANDD PREPERATION
data=data[-1]
ncol(data)
data=data[-32]
View(head(data))
summary(data)
data$diagnosis=factor(data$diagnosis,labels=c('MALIGANT+CANCER','BENIGN-NOT-CANCER'))
datacleaned=data 
table(datacleaned$diagnosis)
prop.table(table(datacleaned$diagnosis))*100


#handling outlier and feeding NA's to them

outliers=NULL
for(i in 1:ncol(datacleaned))
  if(class(datacleaned[,i])=='numeric' || class(datacleaned[,i])=='integer')
  {
    if(length((boxplot(datacleaned[,i]$out))==0)
       {
         print('no outlier')
    }else{
      print('outliers')
      outliers=boxplot()
    }
    }
  }