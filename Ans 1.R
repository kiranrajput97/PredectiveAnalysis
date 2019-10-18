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
