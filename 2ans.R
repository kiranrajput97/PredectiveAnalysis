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
