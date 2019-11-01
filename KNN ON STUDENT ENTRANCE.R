# CALLING LIBRARIES

library(missForest)
library(class)

# GETTING DATA

data <- read.csv(file.choose(), header = T)

# DATA CLEANING AND PREPARATION


str(data)
summary(data)

data<-data[,-1]
str(data)
data$admit[data$admit == 0] <- 'No'
data$admit[data$admit == 1] <- 'Yes'
data$admit <- factor(data$admit)
data$rank<- factor(data$rank)

# NORMALIZE DATASET
Normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#data_n=as.data.frame(lapply(data[2:4],Normalize))
data_n=as.data.frame(lapply(data[2:3],Normalize))
summary(data_n)
data_norm=cbind(data[,c(1,4)],data_n)

# DATA SUBSETTING AND PREPARATION

set.seed(1234)
ind <- sample(2, nrow(data_norm), replace = T, prob = c(0.7, 0.3))
training <- data_norm[ind == 1,]
testing <- data_norm[ind == 2,]

training_label=training[,1]
testing_label=testing[,1]

training <- training[,-1]
testing <- testing[,-1]



# CREATING KNN MODEL
PREDICTION=knn(train=training,test=testing,cl=training_label,k=10)

# CHECKING ACCURACY AND CROSS VALIDATION
tab=table(PREDICTION, testing_label)
tab
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

