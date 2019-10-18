library(class)
library(missForest)
lung<-read.csv(file=file.choose())


str(lung)
summary(lung)

lung<-lung[,-1]

# Missing Values Handling

data_clean<-missForest(lung)
data_clean<-as.data.frame(data_clean[[1]])
summary(data_clean)

data_clean$Smoke<-as.numeric(data_clean$Smoke)
data_clean$Gender<-as.numeric(data_clean$Gender)

# DATA SUBSETTING AND PREPARATION
set.seed(123)
ind <- sample(2, nrow(data_clean), replace = T, prob = c(0.8, 0.2))
training <- data_clean[ind == 1,]
testing <- data_clean[ind == 2,]

training_label=training[,6]
testing_label=testing[,6]

training <- training[,-6]
testing <- testing[,-6]



# CREATING KNN MODEL
PREDICTION=knn(train=training,test=testing,cl=training_label,k=5)

# CHECKING ACCURACY AND CROSS VALIDATION
tab=table(PREDICTION, testing_label)
tab
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

###################################           Q2           ########################################

library(MASS)
library(class)
data<-painters


str(data)
summary(data)

# NORMALIZE DATASET
Normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_n=as.data.frame(lapply(data[1:4],Normalize))
summary(data_n)
data_norm <- cbind(data_n,data[5])

# DATA SUBSETTING AND PREPARATION
set.seed(1234)
ind <- sample(2, nrow(data_norm), replace = T, prob = c(0.7, 0.3))
training <- data_norm[ind == 1,]
testing <- data_norm[ind == 2,]

training_label=training[,5]
testing_label=testing[,5]

training <- training[,-5]
testing <- testing[,-5]


for (i in 1:20){
  
  # CREATING KNN MODEL
  PREDICTION=knn(train=training,test=testing,cl=training_label,k=i)
  
  # CHECKING ACCURACY AND CROSS VALIDATION
  tab=table(PREDICTION, testing_label)
  tab
  accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
  print(round(c(i,accuracy(tab)),0))
}


##################################             Q3          #########################################

#Find all the words with less than 6 or more than 8 characters in the vector 
a<- c("Maine", "Maryland",
      "Massachusetts", "Michigan", "Minnesota",
      "Mississippi", "Missouri", "Montana")

a[nchar(a)>8 | nchar(a)<6]
