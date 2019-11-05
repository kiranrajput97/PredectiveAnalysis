# CALLING LIBRARIES

library(class)
library(naivebayes)
library(missForest)
#library(psych)
#install.packages("plotly", dependencies=TRUE, repos='http://cran.rstudio.com/')
#library(plotly)

# GETTING DATA
DATA<- read.csv(file.choose(), header = T, sep=',')

# DATA VIEWING
str(DATA)
summary(DATA)
is.na(DATA)
(DATA[!complete.cases(DATA),])
cor(DATA[c(-1,-4)])


# DATA CLEANING AND PREPARATION
str(DATA)
DATA$admit[DATA$admit == 0] <- 'No'
DATA$admit[DATA$admit == 1] <- 'Yes'
DATA$admit <- factor(DATA$admit)
DATA$rank<- factor(DATA$rank)
DATACLEANED=DATA
summary(DATACLEANED)


# IMPUTING OUTLIERS AND FEEDING NAs TO THEM

OUTLIERS=NULL
for (i in 1:ncol(DATACLEANED))
{
  if(class(DATACLEANED[,i])=='numeric'||class(DATACLEANED[,i])=='integer')
  {
    if (length((boxplot(DATACLEANED[,i])$out))==0)
    {
      print ('NO OUTLIERS')
    }else {
      print ('OUTLIERS')
      OUTLIERS=boxplot(DATACLEANED[,i], plot=FALSE)$out
      DATACLEANED[which(DATACLEANED[,i] %in% OUTLIERS),i]=NA
      OUTLIERS=NULL
    }
  }else{
    print ("NOT NUMERIC")
  }
}
summary(DATACLEANED)
DATANOOUTLIER=DATACLEANED  


# REMOVING NA

if (nrow(DATACLEANED[!complete.cases(DATACLEANED),])==0)
{
  DATANONA=DATACLEANED
}else{
  DATAMISSFOREST <- missForest(DATACLEANED)
  DATANONA=as.data.frame(DATAMISSFOREST[[1]])
}

summary(DATANONA)


# NORMALIZE DATASET
Normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
DATANORM=as.data.frame(lapply(DATANONA[2:3],Normalize))
DATANORM=cbind(DATANONA[,c(1,4)],DATANORM)
summary(DATANORM)


# DATA SUBSETTING AND PREPARATION
set.seed(1234)
ind <- sample(2, nrow(DATANORM), replace = T, prob = c(0.7, 0.3))
TRAINING<- DATANORM[ind == 1,]
TESTING<- DATANORM[ind == 2,]
TRAINING_LABEL=TRAINING[,1]
TESTING_LABEL=TESTING[,1]
TRAINING_KNN=TRAINING[-1]
TESTING_KNN=TESTING[-1]
TRAINING_NAIVE=TRAINING
TESTING_NAIVE=TESTING
#summary(TRAINING_KNN)
#summary(TESTING_KNN)
#summary(TRAINING_NAIVE)
#length(TRAINING_LABEL)
#length(TRAINING_KNN)

# CREATING KNN MODEL
nearestneighbours=round(sqrt(nrow(DATANORM)))
print(nearestneighbours)
df=NULL
for (i in 1:nearestneighbours)
{
  set.seed(1234)
  PREDICTION=knn(train=TRAINING_KNN,test=TESTING_KNN,cl=TRAINING_LABEL,k=i)
  VALIDATION=table(TESTING_LABEL,PREDICTION)
  (ACCURACY=sum(diag(VALIDATION))/sum(VALIDATION)*100)
  print(paste("When Nearest neighbour= ",i,"Then Accuracy = ",ACCURACY))
  df=rbind(df,data.frame(K=i,Acc=ACCURACY))
}
print(df)

# SELECTING BEST VALUE OF K
MAXK=subset(df,Acc==max(Acc),select=K)
MAXK

if (length(MAXK$K)>1)
{
  for (i in 1:length(MAXK$K))
  {
    if(MAXK[i,] %% 2==1){
      K=MAXK[i,]
    }else{
      K=MAXK[1,]
    }
  }
}else{
  K=MAXK
}
print(K)
set.seed(1234)
PREDICTION_KNN=knn(train=TRAINING_KNN,test=TESTING_KNN,cl=TRAINING_LABEL,k=K)
VALIDATION_KNN=table(TESTING_LABEL,PREDICTION_KNN)
print(VALIDATION_KNN)
(ACCURACY_KNN=sum(diag(VALIDATION_KNN))/sum(VALIDATION_KNN)*100)



# CREATING NAIVE BAYES MODEL
NAIVE_MODEL=naive_bayes(admit~.,data=TRAINING_NAIVE)
plot(NAIVE_MODEL)
PREDICTION_NAIVE=predict(NAIVE_MODEL,TESTING_NAIVE,type="prob")
head(cbind(PREDICTION_NAIVE,TESTING_NAIVE))

PREDICTION_NAIVE=predict(NAIVE_MODEL,TESTING_NAIVE)
(VALIDATION_NAIVE=table(TEST=TESTING_NAIVE$admit,PREDICTED=PREDICTION_NAIVE))
(ACCURACY_NAIVE=sum(diag(VALIDATION_NAIVE))/sum(VALIDATION_NAIVE)*100)

# MERGING ACCURACY OF ALGORITHMS 
MERGE_ACCURACY=data.frame(KNN=ACCURACY_KNN,NAIVEBAYES=ACCURACY_NAIVE)
row.names(MERGE_ACCURACY)='STUDENT ENTRANCE'


print(MERGE_ACCURACY)
MERGE_ACCURACY=as.matrix.data.frame(MERGE_ACCURACY)
barplot(MERGE_ACCURACY,xlab = 'ALGORITHMS',ylab = 'ACCURACY',ylim=c(0,100),main='ACCURACIES OF ALGOS ON STUDENT ENTRANCE')

