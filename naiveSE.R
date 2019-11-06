# CALLING LIBRARIES
#NaiveBayes
install.packages("naivebayes", dependencies=TRUE, repos='http://cran.rstudio.com/')
library(naivebayes)
library(e1071)

# GETTING DATA

data <- read.csv(file.choose(), header = T)

# DATA CLEANING AND PREPARATION
str(data)
summary(data)
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

summary(data_norm)
# DATA SUBSETTING AND PREPARATION
set.seed(1234)
ind <- sample(2, nrow(data_norm), replace = T, prob = c(0.7, 0.3))
training <- data_norm[ind == 1,]
testing <- data_norm[ind == 2,]

training_label=training[,1]
testing_label=testing[,1]

training <- training[,-1]
testing <- testing[,-1]
str(training)


# CREATING NAIVE BAYES MODEL

model<-naive_bayes(training_label ~ .,data=training,type='prob')

#Predict testing set
p1<-predict(model,testing)

# CHECKING ACCURACY AND CROSS VALIDATION
tab <- table(p1,testing_label)
tab
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
# PREDICTION
NEW1=data.frame(rank=1,gre=0.7,gpa=0.8)
NEW1$rank<- factor(NEW$rank,levels = 1:4)
summary(NEW1)
#NEW=testing[7,]
model<-naive_bayes(training_label ~ .,data=training,type='prob')
model
#Predict testing set
p1<-predict(model,NEW1)
p1

